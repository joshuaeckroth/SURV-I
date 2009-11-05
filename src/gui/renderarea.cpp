
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

    detectionUnacceptedPen.setColor(Qt::yellow);
    detectionUnacceptedPen.setWidth(1);

    detectionCenterPen.setColor(Qt::blue);
    detectionCenterPen.setWidth(1);

    movementPen.setColor(Qt::green);
    movementPen.setWidth(2);

    movementUnacceptedPen.setColor(Qt::gray);
    movementUnacceptedPen.setWidth(1);

    pathPen.setColor(Qt::black);
    pathPen.setWidth(2);

    pathUnacceptedPen.setColor(Qt::white);
    pathUnacceptedPen.setWidth(1);
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
                    if(d->isAccepted())
                        painter.setPen(detectionPen);
                    else
                        painter.setPen(detectionUnacceptedPen);

                    painter.drawLine(i * eachWidth + scaledX, scaledY,
                                     i * eachWidth + scaledX, scaledY);

                    radius = 5.0 * scaleFactor[i];
                    painter.drawEllipse(QPoint(i * eachWidth + scaledX, scaledY), radius, radius);

                    // draw on map
                    painter.setClipRegion(mapRegion);

                    QPair<int,int> c = CameraModel::warpToMap(QPair<double,double>(d->getLat(), d->getLon()));
                    painter.drawLine(c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY,
                                     c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY);
                    if(d->isAccepted())
                        radius = 3.0;
                    else
                        radius = 5.0;
                    painter.drawEllipse(QPoint(c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY), radius, radius);
                }
            }

            entities->movements_begin();
            while(!entities->movements_end())
            {
                Movement* m = entities->movements_next();

                if(m->isAccepted())
                    painter.setPen(movementPen);
                else
                    painter.setPen(movementUnacceptedPen);

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
                }
            }

            entities->paths_begin();
            while(!entities->paths_end())
            {
                std::vector<QPair<double,double> > points;
                std::vector<QPair<double,double> >::const_iterator points_iter;

                Path *p = entities->paths_next();
                if(p->isAccepted())
                    painter.setPen(pathPen);
                else
                    painter.setPen(pathUnacceptedPen);
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
                            points.push_back(QPair<double,double>(d->getLat(), d->getLon()));
                        }
                    }
                }

                QPair<double,double> point1, point2;
                QPair<int,int> scaledPoint1, scaledPoint2;
                int scaledX1, scaledY1, scaledX2, scaledY2;
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

                        scaledPoint1 = CameraModel::warpToImage(i, point1);
                        scaledPoint2 = CameraModel::warpToImage(i, point2);
                        scaledX1 = scaledPoint1.first * scaleFactor[i];
                        scaledY1 = scaledPoint1.second * scaleFactor[i];
                        scaledX2 = scaledPoint2.first * scaleFactor[i];
                        scaledY2 = scaledPoint2.second * scaleFactor[i];
                        painter.drawLine(QPoint(i * eachWidth + scaledX1, scaledY1),
                                         QPoint(i * eachWidth + scaledX2, scaledY2));
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

        qDebug() << QString("Dist from movement %1: %2").arg(m->getId()).arg(clickDistance(points[0], points[1], e->pos()));
        // ensure we are close to the line segment
        if(2.0 >= clickDistance(points[0], points[1], e->pos()))
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

// from Grumdrig's post (Oct 1) on
// http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
double RenderArea::clickDistance(QPoint p1, QPoint p2, QPoint click)
{
    double pointDist = pointDistance(p1, p2);

    // if p1 == p2
    if(pointDist < 0.1) return pointDistance(click, p1);

    QPoint p1ClickDiff = p1 - click;
    QPoint p2ClickDiff = p2 - click;
    double projection = ((double)p1ClickDiff.x() * (double)p2ClickDiff.x()
                         + (double)p1ClickDiff.y() * (double)p2ClickDiff.y())
                        / pow(pointDist, 2.0);
    qDebug() << QString("projection: %1").arg(projection);
    // projection of click on segment is beyond p1
    if(projection < 0.0) return pointDistance(click, p1);
    // projection of click on segment is beyond p2
    else if(projection > 1.0) return pointDistance(click, p2);
    // projection is between p1 and p2
    QPoint projPoint = p1 + projection * (p2 - click);
    return pointDistance(click, projPoint);
}

double RenderArea::pointDistance(QPoint p1, QPoint p2)
{
    return sqrt(pow((double)p1.x() - (double)p2.x(), 2.0) +
                pow((double)p1.y() - (double)p2.y(), 2.0));
}
