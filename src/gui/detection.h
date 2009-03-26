#ifndef DETECTION_H
#define DETECTION_H

class Frame;

class Detection
{
public:
  Detection(int i, int c, double a, double x, double y);
  void setFrame(Frame *f);

private:
  int id;
  int camera;
  double area;
  double cx;
  double cy;
  Frame *frame;
};

#endif
