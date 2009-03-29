#ifndef FRAME_H
#define FRAME_H

#include "opencv/highgui.h"

class Frame
{
public:
  Frame(int n, double t);
  void setImage(IplImage* img);

  int getNumber() const;
  double getTime() const;
  IplImage *getImage() const;

private:
  int number;
  double time;
  IplImage *image;
};

#endif
