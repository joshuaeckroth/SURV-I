
#include <QPainter>
#include <QTime>
#include <QDebug>
#include <QPen>
#include <QMap>
#include <QPair>
#include <QImage>
#include <QWidget>
#include <QMouseEvent>
#include <QMessageBox>

#include <cmath>

#include "renderarea.h"
#include "entities.h"
#include "detection.h"
#include "movement.h"
#include "path.h"
#include "frame.h"
#include "cameramodel.h"

RenderArea::RenderArea(QWidget* parent)
        : QWidget(parent), clear(true), entities(NULL)
{
    for(int i = 0; i < 10; i++)
    {
        imageData[i] = 0;
        imageWidth[i] = 0;
        imageHeight[i] = 0;
        scaleFactor[i] = 1.0;
        cameraRegion[i] = QRegion();
    }
    setAttribute(Qt::WA_OpaquePaintEvent, true);
    setAttribute(Qt::WA_PaintOnScreen, true);
    setSizePolicy(QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding));
    time = framesShown = 0;

    map = QImage("videos/ARL1-with-lines.jpg");
    if(map.isNull())
        qDebug() << "Unable to load map image.";

    detectionPen.setColor(Qt::blue);
    detectionPen.setWidth(1);

    detectionCenterPen.setColor(Qt::blue);
    detectionCenterPen.setWidth(3);

    movementPen.setColor(Qt::green);
    movementPen.setWidth(2);

    pathPen.setColor(Qt::black);
    pathPen.setWidth(3);
}

void RenderArea::setNumCameras(int n)
{
    numCameras = n;
    clear = true;
    update();
}

void RenderArea::onFrameSizeChanged(int width, int height, int camera)
{
    setFixedSize(width, height);
    imageWidth[camera] = width;
    imageHeight[camera] = height;
}

void RenderArea::updateEntities(Entities* e)
{
    mutex.lock();
    entities = e;
    mutex.unlock();
    update();
}

void RenderArea::showFrame(Frame* frame)
{
    clear = false;
    updatePixmap(frame->getImage(), frame->getCamera());
    update();
}

void RenderArea::updatePixmap(const IplImage* frameImg, int camera)
{
    QTime t;
    t.start();
    bool start = false;
    if(frameImg->width != imageWidth[camera] || frameImg->height != imageHeight[camera])
    {
        if(imageData[camera])
        {
            delete[] imageData[camera];
        }
        start = true;

        imageWidth[camera] = frameImg->width;
        imageHeight[camera] = frameImg->height;

        emit(frameSizeChanged(imageWidth[camera], imageHeight[camera], camera));

        imageData[camera] = new unsigned char[4 * imageWidth[camera] * imageHeight[camera]];
        for(int i = 0; i < imageWidth[camera] * imageHeight[camera]; i++)
        {
            imageData[camera][i * 4 + 3] = 0xFF; // alpha channel
        }
    }

    int pixels = imageWidth[camera] * imageHeight[camera];
    uchar* src = (uchar*)(frameImg->imageData);
    uchar* srcEnd = src + (3 * pixels);
    uchar* dest = imageData[camera];

    do
    {
        memcpy(dest, src, 3);
        dest += 4;
        src += 3;
    }
    while(src < srcEnd);

    if(!start)
    {
        ++framesShown;
        time += t.elapsed();
    }
}

void RenderArea::paintEvent(QPaintEvent*)
{
    QPainter painter;
    painter.begin(this);
    painter.setRenderHint(QPainter::Antialiasing);

    if(!clear)
    {
        // draw camera frames
        int maxHeight = 0;
        int eachWidth = width() / numCameras;
        for(int i = 0; i < numCameras; i++)
        {
            if(imageData[i] == NULL) continue;

            QImage tImg(imageData[i], imageWidth[i], imageHeight[i], QImage::Format_RGB32);
            QImage sImg = tImg.scaledToWidth(eachWidth, Qt::SmoothTransformation);
            scaleFactor[i] = static_cast<double>(eachWidth) / static_cast<double>(imageWidth[i]);
            cameraRegion[i] = QRegion(i * eachWidth, 0, sImg.width(), sImg.height());

            painter.drawImage(QPoint(i * eachWidth, 0), sImg);

            for(int i = 0; i < numCameras; i++)
            {
                if(sImg.height() > maxHeight)
                    maxHeight = sImg.height();
            }
        }

        // draw map
        QPoint mapcenter = QPoint(857, 577);
        int cropHeight = height() - maxHeight;
        int cropWidth = width();
        int mapTopLeftX = mapcenter.x() - (cropWidth / 2);
        int mapTopLeftY = mapcenter.y() - (cropHeight / 2);
        int mapBottomRightX = mapTopLeftX + cropWidth; // used to determine if warpToMap coordinates are in view
        int mapBottomRightY = mapTopLeftY + cropHeight;
        painter.drawImage(0, maxHeight, map, mapTopLeftX, mapTopLeftY, cropWidth, cropHeight);
        mapRegion = QRegion(0, maxHeight, cropWidth, cropHeight);

        mutex.lock();
        if(entities != NULL)
        {
            int scaledX, scaledY;
            int radius;


            entities->detections_begin();
            while(!entities->detections_end())
            {
                Detection* d = entities->detections_next();
                for(int i = 0; i < numCameras; i++)
                {
                    painter.setClipRegion(cameraRegion[i]);

                    // draw on camera image
                    QPair<int,int> p = CameraModel::warpToImage(i, QPair<double,double>(d->getLat(), d->getLon()));

                    scaledX = p.first * scaleFactor[i];
                    scaledY = p.second * scaleFactor[i];

                    // draw pixel at center
                    painter.setPen(detectionCenterPen);

                    painter.drawLine(i * eachWidth + scaledX, scaledY,
                                     i * eachWidth + scaledX, scaledY);

                    painter.setPen(detectionPen);

                    radius = 5.0 * scaleFactor[i];
                    painter.drawEllipse(QPoint(i * eachWidth + scaledX, scaledY), radius, radius);

                    // draw on map
                    painter.setClipRegion(mapRegion);

                    QPair<int,int> c = CameraModel::warpToMap(QPair<double,double>(d->getLat(), d->getLon()));
                    painter.drawLine(c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY,
                                     c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY);
                    radius = 3.0;
                    painter.drawEllipse(QPoint(c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY), radius, radius);
                }
            }


            painter.setPen(movementPen);

            entities->movements_begin();
            while(!entities->movements_end())
            {
                Movement* m = entities->movements_next();

                // should only be two detections
                QPoint points[2][numCameras];
                int j = 0;

                m->detections_begin();
                while(!m->detections_end())
                {
                    Detection* d = m->detections_next();
                    for(int i = 0; i < numCameras; i++)
                    {
                        QPair<int,int> p = CameraModel::warpToImage(i, QPair<double,double>(d->getLat(), d->getLon()));

                        scaledX = p.first * scaleFactor[i];
                        scaledY = p.second * scaleFactor[i];

                        points[j][i] = QPoint(i * eachWidth + scaledX, scaledY);
                    }
                    j++;
                }

                for(int i = 0; i < numCameras; i++)
                {
                    painter.setClipRegion(cameraRegion[i]);
                    painter.drawLine(points[0][i], points[1][i]);
                    // draw an "arrow head"
                    painter.drawEllipse(points[1][i], (int)(5.0 * scaleFactor[i]), (int)(5.0 * scaleFactor[i]));
                }
            }

            painter.setPen(pathPen);

            entities->paths_begin();
            while(!entities->paths_end())
            {
                std::vector<QPoint> points;
                std::vector<QPoint>::const_iterator points_iter;

                Path *p = entities->paths_next();
                p->movements_begin();
                while(!p->movements_end())
                {
                    Movement *m = p->movements_next();
                    m->detections_begin();
                    while(!m->detections_end())
                    {
                        Detection *d = m->detections_next();
                        for(int i = 0; i < numCameras; i++)
                        {
                            QPair<int,int> point = CameraModel::warpToImage(i, QPair<double,double>(d->getLat(), d->getLon()));

                            scaledX = point.first * scaleFactor[i];
                            scaledY = point.second * scaleFactor[i];

                            points.push_back(QPoint(i * eachWidth + scaledX, scaledY));
                        }
                    }
                }

                QPoint point1, point2;
                for(int i = 0; i < numCameras; i++)
                {
                    painter.setClipRegion(cameraRegion[i]);
                    for(points_iter = points.begin(); points_iter != points.end(); points_iter++)
                    {
                        point1 = *points_iter;
                        if((points_iter + 1) != points.end())
                            point2 = *(points_iter + 1);
                        else
                            point2 = point2;
                        painter.drawLine(point1, point2);
                    }
                }
            }
        }
        mutex.unlock();
    }
    else
    {
        painter.setBrush(Qt::black);
        painter.drawRect(rect());
    }
    painter.end();
}

void RenderArea::mousePressEvent(QMouseEvent *e)
{
    if(entities == NULL)
        return;

    int camera = -1;
    // find the camera that the user clicked on
    for(int i = 0; i < numCameras; i++)
    {
        if(cameraRegion[i].contains(e->pos()))
        {
            camera = i;
            break;
        }
    }
    if(camera == -1) // map clicked
        return;

    QString msg; // we will build a message to show to the user

    mutex.lock();
    int eachWidth = width() / numCameras;
    int scaledX, scaledY;
    int entityX, entityY;

    // check for detections under the mouse
    entities->detections_begin();
    while(!entities->detections_end())
    {
        Detection* d = entities->detections_next();
        QPair<int,int> p = CameraModel::warpToImage(camera, QPair<double,double>(d->getLat(), d->getLon()));
        scaledX = p.first * scaleFactor[camera];
        scaledY = p.second * scaleFactor[camera];
        entityX = camera * eachWidth + scaledX;
        entityY = scaledY;

        if(2.0 >= sqrt(pow(e->x() - entityX, 2) + pow(e->y() - entityY, 2)))
        {
            msg += QString("Detection %1 (%2 lat, %3 lon); area = %4, time = (%5/%6)\n")
                   .arg(d->getId()).arg(d->getLat()).arg(d->getLon()).arg(d->getArea(), 0, 'f', 0)
                   .arg(d->getStartTime(), 0, 'f', 1).arg(d->getEndTime(), 0, 'f', 1);
        }
    }

    // check for movements under the mouse
    entities->movements_begin();
    while(!entities->movements_end())
    {
        Movement* m = entities->movements_next();
        QPoint points[2];
        int j = 0;
        m->detections_begin();
        while(!m->detections_end())
        {
            Detection* d = m->detections_next();
            QPair<int,int> p = CameraModel::warpToImage(camera, QPair<double,double>(d->getLat(), d->getLon()));
            scaledX = p.first * scaleFactor[camera];
            scaledY = p.second * scaleFactor[camera];
            points[j] = QPoint(camera * eachWidth + scaledX, scaledY);
            j++;
        }
        // from Grumdrig's post (Oct 1) on http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
        double dist = 100.0;

        // ensure we are close to the line segment
        if(2.0 >= dist)
        {
            msg += QString("Movement %1\n").arg(m->getId());
        }
    }
    mutex.unlock();

    if(!msg.isNull())
    {
        QMessageBox msgBox;
        msgBox.setText(msg);
        msgBox.exec();
    }
}


