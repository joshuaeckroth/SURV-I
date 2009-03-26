#ifndef NOISE_H
#define NOISE_H

class Frame;

class Noise
{
public:
  Noise(int i, int c, double a, double x, double y);
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
