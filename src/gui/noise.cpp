
#include "noise.h"
#include "frame.h"

Noise::Noise(int i, int c, double a, double x, double y)
  : id(i), camera(c), area(a), cx(x), cy(y)
{ }

void Noise::setFrame(Frame *f)
{
  frame = f;
}

int Noise::getId() const
{
  return id;
}

int Noise::getCamera() const
{
  return camera;
}

double Noise::getArea() const
{
  return area;
}

double Noise::getCx() const
{
  return cx;
}

double Noise::getCy() const
{
  return cy;
}


