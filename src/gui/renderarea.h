#ifndef RENDER_AREA_H
#define RENDER_AREA_H

#include <QWidget>
#include <QMap>
#include <QPair>
#include <QImage>

#include "opencv/highgui.h"

class Frame;

class RenderArea : public QWidget
{
  Q_OBJECT;

public:
  RenderArea(QWidget* parent);
  void setNumCameras(int n);
  void showFrames(QMap<int, QMap<int, QPair<Frame*, QString> > > detections, int number, Frame* f);

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
  int time;
  int framesShown;
  int numCameras;
  bool clear;
  Frame* entities;
};

#endif
