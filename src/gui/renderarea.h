#ifndef RENDER_AREA_H
#define RENDER_AREA_H

#include <QWidget>
#include <QMap>
#include <QPair>
#include <QImage>
#include <QRegion>
#include <QPen>
#include <QMutex>
#include <QPoint>
#include <QFont>

#include "highgui.h"

class Entities;
class Entity;
class Frame;

class RenderArea : public QWidget
{
  Q_OBJECT;

public:
  RenderArea(QWidget* parent);
  void setNumCameras(int n);
  void highlightEntity(Entity *e);

public slots:
  void newFrame(Frame* frame);
  void onFrameSizeChanged(int width, int height, int camera);
  void updateEntities(Entities *e);

signals:
  void frameSizeChanged(int width, int height, int camera);

protected:
  void paintEvent(QPaintEvent*);
  void mousePressEvent(QMouseEvent*);

private:
  void updatePixmap(const IplImage* frame, int camera);
  QPixmap bufferPixmap;
  QImage map;
  uchar* imageData[10];
  int imageWidth[10], imageHeight[10];
  double scaleFactor[10];
  QRegion cameraRegion[10];
  QRegion mapRegion;
  int time;
  int framesShown;
  int numCameras;
  bool clear;
  Entities* entities;
  QMutex mutex;
  QPen detectionPen, detectionUnacceptedPen,
    movementPen, movementUnacceptedPen,
    pathPen, pathUnacceptedPen, highlightedPen, behaviorTextPen;
  QFont behaviorTextFont;
  int maxHeight, mapTopLeftX, mapTopLeftY;
  QPoint warpToCameraRegion(int camera, double lat, double lon);
  QPoint warpToMapRegion(double lat, double lon);
  double clickDistance(QPoint p1, QPoint p2, QPoint click);
  double pointDistance(QPoint p1, QPoint p2);
  void drawArrowHead(QPainter &painter, QPoint p1, QPoint p2);
  Entity *highlighted;
};

#endif
