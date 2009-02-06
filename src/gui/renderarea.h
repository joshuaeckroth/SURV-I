#ifndef RENDER_AREA_H
#define RENDER_AREA_H

#include <QWidget>

#include "opencv/highgui.h"

class RenderArea : public QWidget
{
  Q_OBJECT;

public:
  RenderArea(QWidget* parent);
  void showFrame(const IplImage* frame, int camera);
  void setNumCameras(int n);

public slots:
  void onFrameSizeChanged(int width, int height, int camera);

signals:
  void frameSizeChanged(int width, int height, int camera);

protected:
  void paintEvent(QPaintEvent*);

private:
  void updatePixmap(const IplImage* frame, int camera);
  QPixmap bufferPixmap;
  uchar* imageData[10];
  int imageWidth[10], imageHeight[10];
  int time;
  int frames;
  int numCameras;
  bool clear;
};

#endif
