#ifndef FRAME_H
#define FRAME_H

#include "highgui.h"

class Frame
{
public:
  Frame(int n, double t, int c);
  void setImage(IplImage* img);

  int getNumber() const;
  double getTime() const;
  int getCamera() const;
  IplImage *getImage() const;

private:
  int number;
  double time;
  int camera;
  IplImage *image;
};

#endif
