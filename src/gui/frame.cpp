
#include <vector>

#include "highgui.h"

#include "frame.h"

Frame::Frame(int n, double t)
  : number(n), time(t)
{ }

void Frame::setImage(IplImage* img)
{
  image = img;
}

int Frame::getNumber() const
{
  return number;
}

double Frame::getTime() const
{
  return time;
}

IplImage *Frame::getImage() const
{
  return image;
}

