
#include "noise.h"
#include "entities.h"

Noise::Noise(int i, int c, double a, double x, double y)
  : id(i), camera(c), area(a), cx(x), cy(y)
{ }

void Noise::setEntities(Entities *e)
{
  entities = e;
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


