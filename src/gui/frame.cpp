
#include <vector>

#include <opencv2/opencv.hpp>
using namespace cv;

#include "frame.h"

Frame::Frame(int n, double t, int c)
  : number(n), time(t), camera(c)
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

int Frame::getCamera() const
{
    return camera;
}

IplImage *Frame::getImage() const
{
  return image;
}

