#ifndef NOISE_H
#define NOISE_H

class Frame;

class Noise
{
public:
  Noise(int i, double a, double x, double y);
  void setFrame(Frame *f);

private:
  int id;
  double area;
  double cx;
  double cy;
  Frame *frame;
};

#endif
