
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
#include <QPaintEvent>
#include <QPixmap>

#include <cmath>
#include <vector>

#include <opencv2/opencv.hpp>
using namespace cv;

#include "renderarea.h"
#include "entities.h"
#include "entity.h"
#include "detection.h"
#include "movement.h"
#include "path.h"
#include "agent.h"
#include "behavior.h"
#include "frame.h"
#include "cameramodel.h"
#include "infobox.h"
#include "context.h"

RenderArea::RenderArea(QWidget* parent)
        : QWidget(parent), clear(true), entities(NULL),
        showDetailsState(Qt::Unchecked), showRegionsState(Qt::Unchecked), showPoisState(Qt::Unchecked)
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

    map = QImage(Context::getMap().file);
    if(map.isNull())
        qDebug() << "Unable to load map image.";

    detectionPen.setColor(Qt::blue);
    detectionPen.setWidth(1);

    detectionUnacceptedPen.setColor(Qt::darkBlue);
    detectionUnacceptedPen.setWidth(1);

    movementPen.setColor(Qt::green);
    movementPen.setWidth(1);

    movementUnacceptedPen.setColor(Qt::darkGreen);
    movementUnacceptedPen.setWidth(1);

    pathPen.setColor(Qt::black);
    pathPen.setWidth(1);

    pathUnacceptedPen.setColor(Qt::gray);
    pathUnacceptedPen.setWidth(1);

    regionColor = QColor(200, 0, 0, 40);
    regionOutline = QColor(200, 0, 0, 150);

    poiColor = QColor(0, 0, 200, 40);
    poiOutline = QColor(0, 0, 200, 150);

    highlightedPen.setColor(Qt::white);
    highlightedPen.setWidth(3);
    highlighted = NULL;

    behaviorIcon = QImage("src\\gui\\images\\info.png");
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

void RenderArea::newFrame(Frame* frame)
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
    QPixmap *doubleBuffer = new QPixmap(width(), height());
    QPainter painter(doubleBuffer);
    painter.setRenderHint(QPainter::Antialiasing);

    if(!clear)
    {
        // draw camera frames
        maxHeight = 0;
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
        mapTopLeftX = mapcenter.x() - (cropWidth / 2);
        mapTopLeftY = mapcenter.y() - (cropHeight / 2);
        painter.drawImage(0, maxHeight, map, mapTopLeftX, mapTopLeftY, cropWidth, cropHeight);
        mapRegion = QRegion(0, maxHeight, cropWidth, cropHeight);

        mutex.lock();
        if(entities != NULL)
        {
            int radius;

            if(showDetailsState == Qt::Checked)
            {
                entities->detections_begin();
                while(!entities->detections_end())
                {
                    Detection* d = entities->detections_next();
                    for(int i = 0; i < numCameras; i++)
                    {
                        painter.setClipRegion(cameraRegion[i]);
                        QPoint detCenter = warpToCameraRegion(i, d->getLat(), d->getLon());

                        // draw pixel at center
                        if(d == highlighted)
                            painter.setPen(highlightedPen);
                        else if(d->isAccepted())
                            painter.setPen(detectionPen);
                        else
                            painter.setPen(detectionUnacceptedPen);

                        painter.drawLine(detCenter, detCenter);

                        radius = (int)(5.0 * scaleFactor[i]);
                        painter.drawEllipse(detCenter, radius, radius);
                    }
                }

                entities->movements_begin();
                while(!entities->movements_end())
                {
                    Movement* m = entities->movements_next();

                    if(m == highlighted)
                        painter.setPen(highlightedPen);
                    else if(m->isAccepted())
                        painter.setPen(movementPen);
                    else
                        painter.setPen(movementUnacceptedPen);

                    QPoint points[2][numCameras];

                    Detection *d;
                    d = m->getDet1();
                    for(int i = 0; i < numCameras; i++)
                        points[0][i] = warpToCameraRegion(i, d->getLat(), d->getLon());
                    d = m->getDet2();
                    for(int i = 0; i < numCameras; i++)
                        points[1][i] = warpToCameraRegion(i, d->getLat(), d->getLon());

                    for(int i = 0; i < numCameras; i++)
                    {
                        painter.setClipRegion(cameraRegion[i]);
                        painter.drawLine(points[0][i], points[1][i]);
                    }
                }

                // unaccepted first, then accepted (so that accepted are drawn over unaccepted)
                int ordering = 0;
                while(ordering < 2)
                {
                    entities->paths_begin();
                    while(!entities->paths_end())
                    {
                        std::vector<QPair<double,double> > points;
                        std::vector<QPair<double,double> >::const_iterator points_iter;

                        Path *p = entities->paths_next();

                        if(ordering == 0 && p->isAccepted())
                            continue;
                        if(ordering == 1 && !p->isAccepted())
                            continue;

                        if(p == highlighted)
                            painter.setPen(highlightedPen);
                        else if(p->isAccepted())
                            painter.setPen(pathPen);
                        else
                            painter.setPen(pathUnacceptedPen);

                        p->movements_begin();
                        while(!p->movements_end())
                        {
                            Movement *m = p->movements_next();
                            Detection *d;
                            d = m->getDet1();
                            points.push_back(QPair<double,double>(d->getLat(), d->getLon()));
                            d = m->getDet2();
                            points.push_back(QPair<double,double>(d->getLat(), d->getLon()));
                        }

                        QPair<double,double> point1, point2;
                        for(int i = 0; i < numCameras; i++)
                        {
                            painter.setClipRegion(cameraRegion[i]);
                            for(points_iter = points.begin(); points_iter != points.end(); points_iter++)
                            {
                                point1 = *points_iter;
                                if((points_iter + 1) != points.end())
                                    point2 = *(points_iter + 1);
                                else
                                    point2 = point1;

                                QPoint p1 = warpToCameraRegion(i, point1.first, point1.second);
                                QPoint p2 = warpToCameraRegion(i, point2.first, point2.second);
                                painter.drawLine(p1, p2);

                                // draw arrowhead on last pair
                                if(p1 != p2 && (points_iter + 1) != points.end() && (points_iter + 2) == points.end())
                                    drawArrowHead(painter, p1, p2);
                            }
                        }
                        // draw on map only if accepted
                        if(p->isAccepted())
                        {
                            painter.setClipRegion(mapRegion);
                            for(points_iter = points.begin(); points_iter != points.end(); points_iter++)
                            {
                                point1 = *points_iter;
                                if((points_iter + 1) != points.end())
                                    point2 = *(points_iter + 1);
                                else
                                    point2 = point1;

                                QPoint p1 = warpToMapRegion(point1.first, point1.second);
                                QPoint p2 = warpToMapRegion(point2.first, point2.second);
                                painter.drawLine(p1, p2);

                                // draw arrowhead on last pair
                                if(p1 != p2 && (points_iter + 1) != points.end() && (points_iter + 2) == points.end())
                                    drawArrowHead(painter, p1, p2);
                            }
                        }
                    }

                    ordering++;
                }
            }

            entities->behaviors_begin();
            while(!entities->behaviors_end())
            {
                Behavior *b = entities->behaviors_next();
                if(!b->isAccepted())
                    continue;

                b->agents_begin();
                Agent *a = b->agents_next();
                if(!a->isAccepted())
                    continue;

                a->paths_begin();
                Path *p = a->paths_next();

                if(!p->isAccepted())
                    continue;

                // draw path if not already drawn by showing details
                if(showDetailsState == Qt::Unchecked)
                {
                    a->paths_begin();
                    while(!a->paths_end())
                    {
                        std::vector<QPair<double,double> > points;
                        std::vector<QPair<double,double> >::const_iterator points_iter;

                        Path *p = a->paths_next();

                        if(p == highlighted)
                            painter.setPen(highlightedPen);
                        else
                            painter.setPen(pathPen);

                        p->movements_begin();
                        while(!p->movements_end())
                        {
                            Movement *m = p->movements_next();
                            Detection *d;
                            d = m->getDet1();
                            points.push_back(QPair<double,double>(d->getLat(), d->getLon()));
                            d = m->getDet2();
                            points.push_back(QPair<double,double>(d->getLat(), d->getLon()));
                        }

                        QPair<double,double> point1, point2;
                        for(int i = 0; i < numCameras; i++)
                        {
                            painter.setClipRegion(cameraRegion[i]);
                            for(points_iter = points.begin(); points_iter != points.end(); points_iter++)
                            {
                                point1 = *points_iter;
                                if((points_iter + 1) != points.end())
                                    point2 = *(points_iter + 1);
                                else
                                    point2 = point1;

                                QPoint p1 = warpToCameraRegion(i, point1.first, point1.second);
                                QPoint p2 = warpToCameraRegion(i, point2.first, point2.second);
                                painter.drawLine(p1, p2);

                                // draw arrowhead on last pair
                                if(p1 != p2 && (points_iter + 1) != points.end() && (points_iter + 2) == points.end())
                                    drawArrowHead(painter, p1, p2);
                            }
                        }
                        // draw path on map
                        painter.setClipRegion(mapRegion);
                        for(points_iter = points.begin(); points_iter != points.end(); points_iter++)
                        {
                            point1 = *points_iter;
                            if((points_iter + 1) != points.end())
                                point2 = *(points_iter + 1);
                            else
                                point2 = point1;

                            QPoint p1 = warpToMapRegion(point1.first, point1.second);
                            QPoint p2 = warpToMapRegion(point2.first, point2.second);
                            painter.drawLine(p1, p2);

                            // draw arrowhead on last pair
                            if(p1 != p2 && (points_iter + 1) != points.end() && (points_iter + 2) == points.end())
                                drawArrowHead(painter, p1, p2);
                        }
                    }
                }
                // get last movement
                p->movements_begin();
                Movement *m = NULL;
                while(!p->movements_end())
                    m = p->movements_next();

                QPair<double,double> point(m->getDet2()->getLat(), m->getDet2()->getLon());
                for(int i = 0; i < numCameras; i++)
                {
                    painter.setClipRegion(cameraRegion[i]);
                    QPoint p = warpToCameraRegion(i, point.first, point.second);
                    painter.drawImage(QPoint(p.x() + 3, p.y() + 3), behaviorIcon);
                }
            }
        }

        if(showRegionsState == Qt::Checked)
        {
            // paint regions
            painter.setPen(regionOutline);
            painter.setBrush(regionColor);
            QPolygon regionPolygon;
            for(int regionIndex = 0; regionIndex < Context::regionCount(); regionIndex++)
            {
                ContextElements::Region region = Context::getRegion(regionIndex);

                for(int i = 0; i < numCameras; i++)
                {
                    regionPolygon.clear();
                    for(std::vector<QPointF>::iterator it = region.points.begin(); it != region.points.end(); it++)
                    {
                        regionPolygon.push_back(warpToCameraRegion(i, it->rx(), it->ry()));
                    }
                    painter.setClipRegion(cameraRegion[i]);
                    painter.drawPolygon(regionPolygon);
                }

                regionPolygon.clear();
                for(std::vector<QPointF>::iterator it = region.points.begin(); it != region.points.end(); it++)
                {
                    regionPolygon.push_back(warpToMapRegion(it->rx(), it->ry()));
                }
                painter.setClipRegion(mapRegion);
                painter.drawPolygon(regionPolygon, Qt::WindingFill);
            }
        }

        if(showPoisState == Qt::Checked)
        {
            // paint points of interest
            painter.setPen(poiOutline);
            painter.setBrush(poiColor);
            for(int poiIndex = 0; poiIndex < Context::pointOfInterestCount(); poiIndex++)
            {
                ContextElements::PointOfInterest poi = Context::getPointOfInterest(poiIndex);

                for(int i = 0; i < numCameras; i++)
                {
                    QPoint center = warpToCameraRegion(i, poi.point.rx(), poi.point.ry());
                    int rangeX = center.x() - (warpToCameraRegion(i, poi.point.rx() + poi.range, poi.point.ry()).x());
                    int rangeY = center.y() - (warpToCameraRegion(i, poi.point.rx(), poi.point.ry() + poi.range).y());
                    painter.setClipRegion(cameraRegion[i]);
                    painter.drawEllipse(center, rangeX, rangeY);

                    painter.drawEllipse(center, 2, 2);
                }

                QPoint center = warpToMapRegion(poi.point.rx(), poi.point.ry());
                int rangeX = center.x() - (warpToMapRegion(poi.point.rx() + poi.range, poi.point.ry()).x());
                int rangeY = center.y() - (warpToMapRegion(poi.point.rx(), poi.point.ry() + poi.range).y());
                painter.setClipRegion(mapRegion);
                painter.drawEllipse(center, rangeX, rangeY);

                painter.drawEllipse(center, 2, 2);
            }
        }

        mutex.unlock();
    }
    else
    {
        painter.setBrush(Qt::black);
        painter.drawRect(rect());
    }
    QPainter realPainter;
    realPainter.begin(this);
    realPainter.drawPixmap(rect(), *doubleBuffer);
    realPainter.end();
}

QPoint RenderArea::warpToCameraRegion(int camera, double lat, double lon)
{
    int eachWidth = width() / numCameras;
    QPair<int,int> p = CameraModel::warpToImage(camera, QPair<double,double>(lat, lon));
    int scaledX = (int)(p.first * scaleFactor[camera]);
    int scaledY = (int)(p.second * scaleFactor[camera]);
    int x = camera * eachWidth + scaledX;
    int y = scaledY;

    return QPoint(x, y);
}

QPoint RenderArea::warpToMapRegion(double lat, double lon)
{
    QPair<int,int> p = CameraModel::warpToMap(QPair<double,double>(lat, lon));
    return QPoint(p.first - mapTopLeftX, maxHeight + p.second - mapTopLeftY);
}

void RenderArea::mousePressEvent(QMouseEvent *e)
{
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
    // if camera == -1, map was clicked


    // this code helps us define regions and points of interest;
    // locations clicked on the map are shown in the debug log (using qDebug)
    if(camera == -1)
    {
        QPair<int,int> mapPoint = QPair<int,int>(e->pos().x() + mapTopLeftX, e->pos().y() - maxHeight + mapTopLeftY);
        QPair<double,double> groundPoint = CameraModel::mapToGround(mapPoint);
        qDebug() << QString("Unwarped map coordinates: (%1, %2)\nWarped map coordinates: (%3, %4)")
                .arg(mapPoint.first).arg(mapPoint.second)
                .arg(groundPoint.first).arg(groundPoint.second);
    }

    if(entities == NULL)
        return;

    double maxClickDist = 5.0;

    // unhighlight everything
    entities->detections_begin();
    while(!entities->detections_end())
        entities->detections_next()->setHighlighted(false);

    entities->movements_begin();
    while(!entities->movements_end())
        entities->movements_next()->setHighlighted(false);

    entities->paths_begin();
    while(!entities->paths_end())
        entities->paths_next()->setHighlighted(false);

    mutex.lock();

    if(camera != -1) // detections are not shown in map
    {
        // check for detections under the mouse
        entities->detections_begin();
        while(!entities->detections_end())
        {
            Detection *d = entities->detections_next();
            QPoint p;
            p = warpToCameraRegion(camera, d->getLat(), d->getLon());

            if(maxClickDist >= pointDistance(e->pos(), p))
            {
                d->setHighlighted(true);
                highlighted = d;
            }
        }
    }

    if(camera != -1) // movements are not shown in map
    {
        // check for movements under the mouse
        entities->movements_begin();
        while(!entities->movements_end())
        {
            Movement *m = entities->movements_next();
            QPoint points[2];
            Detection *d;
            d = m->getDet1();
            points[0] = warpToCameraRegion(camera, d->getLat(), d->getLon());
            d = m->getDet2();
            points[1] = warpToCameraRegion(camera, d->getLat(), d->getLon());

            // ensure we are close to the line segment
            if(maxClickDist >= clickDistance(points[0], points[1], e->pos()))
            {
                m->setHighlighted(true);
                highlighted = m;
            }
        }
    }

    bool found = false;

    if(camera != -1) // behavior icons are not shown in map
    {
        entities->behaviors_begin();
        while(!entities->behaviors_end() && !found)
        {
            Behavior *b = entities->behaviors_next();
            if(!b->isAccepted())
                continue;

            b->agents_begin();
            Agent *a = b->agents_next();

            if(!a->isAccepted())
                continue;

            a->paths_begin();
            Path *p = a->paths_next();

            if(!p->isAccepted())
                continue;

            // get last movement
            p->movements_begin();
            Movement *m = NULL;
            while(!p->movements_end())
                m = p->movements_next();

            QPair<double,double> point(m->getDet2()->getLat(), m->getDet2()->getLon());
            QPoint iconOrigin = warpToCameraRegion(camera, point.first, point.second);

            QRegion iconRegion(iconOrigin.x() + 3, iconOrigin.y() + 3, behaviorIcon.width(), behaviorIcon.height());
            if(iconRegion.contains(e->pos()))
            {
                // someday, need a way to delete these boxes
                InfoBox *infobox = new InfoBox;
                infobox->setWindowTitle(QString("Behavior %1").arg(b->getId()));
                infobox->setText(b->getContent());
                infobox->show();
                p->setHighlighted(true);
                highlighted = p;
                found = true;
                break;
            }
        }
    }

    // check for paths under the mouse
    entities->paths_begin();
    while(!entities->paths_end() && !found)
    {
        Path *p = entities->paths_next();

        // map does not show unaccepted paths
        if(camera == -1 && !p->isAccepted()) continue;

        // check for constituent movements under the mouse
        p->movements_begin();
        while(!p->movements_end() && !found)
        {
            Movement *m = p->movements_next();
            QPoint points[2];
            Detection *d1, *d2;
            d1 = m->getDet1();
            d2 = m->getDet2();

            if(camera == -1)
            {
                points[0] = warpToMapRegion(d1->getLat(), d1->getLon());
                points[1] = warpToMapRegion(d2->getLat(), d2->getLon());
            }
            else
            {
                points[0] = warpToCameraRegion(camera, d1->getLat(), d1->getLon());
                points[1] = warpToCameraRegion(camera, d2->getLat(), d2->getLon());
            }

            // ensure we are close to the line segment
            if(maxClickDist >= clickDistance(points[0], points[1], e->pos()))
            {
                p->setHighlighted(true);
                highlighted = p;
                found = true;
            }
        }
    }


    entities->updateHighlights();

    mutex.unlock();

    update();
}

// from http://www.gamedev.net/community/forums/viewreply.asp?ID=1250842
double RenderArea::clickDistance(QPoint p1, QPoint p2, QPoint click)
{
    QPair<int,int> p1ToClick(click.x() - p1.x(), click.y() - p1.y());
    double segmentLength = pointDistance(p1, p2);
    QPair<double,double> unitSegment((double)(p2.x() - p1.x())/segmentLength,
                                     (double)(p2.y() - p1.y())/segmentLength);

    double intersectionDist = unitSegment.first * (double)p1ToClick.first +
                              unitSegment.second * (double)p1ToClick.second;

    QPoint intersectionPoint;
    if(intersectionDist < 0.1) intersectionPoint = p1;
    else if(intersectionDist > segmentLength) intersectionPoint = p2;
    else intersectionPoint = QPoint((int)(unitSegment.first * intersectionDist) + p1.x(),
                                    (int)(unitSegment.second * intersectionDist) + p1.y());

    return pointDistance(intersectionPoint, click);
}

double RenderArea::pointDistance(QPoint p1, QPoint p2)
{
    return sqrt(pow((double)p1.x() - (double)p2.x(), 2.0) +
                pow((double)p1.y() - (double)p2.y(), 2.0));
}

// from http://forums.devx.com/archive/index.php/t-74981.html
// with some corrections
void RenderArea::drawArrowHead(QPainter &painter, QPoint p1, QPoint p2)
{
    const double pi = 3.141592654;
    const double arrowHeadLength = 5.0;
    // get angle of line
    double lineAngle = std::atan(((double)p2.y() - (double)p1.y())/((double)p2.x() - (double)p1.x()));
    // arrowhead angles
    double endAngle1 = lineAngle + 45.0 * pi / 180.0;
    double endAngle2 = lineAngle - 45.0 * pi / 180.0;
    // end points of arrowhead
    int modifier;
    if(p2.x() < p1.x()) modifier = -1;
    else modifier = 1;
    int ax1 = p2.x() - modifier * (int)(arrowHeadLength * std::cos(endAngle1));
    int ax2 = p2.x() - modifier * (int)(arrowHeadLength * std::cos(endAngle2));
    int ay1 = p2.y() - modifier * (int)(arrowHeadLength * std::sin(endAngle1));
    int ay2 = p2.y() - modifier * (int)(arrowHeadLength * std::sin(endAngle2));
    // draw it
    painter.drawLine(p2, QPoint(ax1, ay1));
    painter.drawLine(p2, QPoint(ax2, ay2));
}

void RenderArea::highlightEntity(Entity *e)
{
    highlighted = e;
    update();
}

void RenderArea::showDetails(int state)
{
    showDetailsState = state;
    update();
}

void RenderArea::showRegions(int state)
{
    showRegionsState = state;
    update();
}

void RenderArea::showPois(int state)
{
    showPoisState = state;
    update();
}
