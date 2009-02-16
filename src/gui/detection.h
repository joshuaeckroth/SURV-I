#ifndef DETECTION_H
#define DETECTION_H

class Frame;

class Detection
{
public:
  Detection(int i, double a, double x, double y);
  void setFrame(Frame *f);

private:
  int id;
  double area;
  double cx;
  double cy;
  Frame *frame;
};

#endif
