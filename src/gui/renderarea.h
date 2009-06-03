#ifndef RENDER_AREA_H
#define RENDER_AREA_H

#include <QGLWidget>
#include <QMap>
#include <QPair>
#include <QImage>
#include <QRegion>
#include <QPen>

#include "highgui.h"

class Entities;
class Frame;

class RenderArea : public QGLWidget
{
  Q_OBJECT;

public:
  RenderArea(QWidget* parent);
  void setNumCameras(int n);
  void showFrames(QMap<int, QMap<int, QPair<Frame*, QString> > > detections, int number, Entities* e);

public slots:
  void onFrameSizeChanged(int width, int height, int camera);

signals:
  void frameSizeChanged(int width, int height, int camera);

protected:
  void paintEvent(QPaintEvent*);

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
  QPen detectionPen, detectionCenterPen, detectionTextPen,
    noisePen, trackHeadPen, trackTextPen, trackPathPen,
    trackExpectedPathPen, trackExpectedCirclePen, trackMapPen;
};

#endif
