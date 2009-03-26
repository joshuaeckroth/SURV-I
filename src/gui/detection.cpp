
#include "detection.h"
#include "frame.h"

Detection::Detection(int i, int c, double a, double x, double y)
  : id(i), camera(c), area(a), cx(x), cy(y)
{ }

void Detection::setFrame(Frame *f)
{
  frame = f;
}

int Detection::getId() const
{
  return id;
}

int Detection::getCamera() const
{
  return camera;
}

double Detection::getArea() const
{
  return area;
}

double Detection::getCx() const
{
  return cx;
}

double Detection::getCy() const
{
  return cy;
}

