
#include "track.h"
#include "frame.h"

Track::Track(int i, double x, double y, double ox, double oy,
	     int p, int n, double ex, double ey, double r, bool thisF)
  : id(i), cx(x), cy(y), ocx(ox), ocy(oy),
    prevId(p), nextId(n), ecx(ex), ecy(ey), radius(r), thisFrame(thisF)
{ }

void Track::setFrame(Frame *f)
{
  frame = f;
}

int Track::getId() const
{
  return id;
}

double Track::getCx() const
{
  return cx;
}

double Track::getCy() const
{
  return cy;
}

double Track::getOcx() const
{
  return ocx;
}

double Track::getOcy() const
{
  return ocy;
}

int Track::getPrevId() const
{
  return prevId;
}

int Track::getNextId() const
{
  return nextId;
}

double Track::getEcx() const
{
  return ecx;
}

double Track::getEcy() const
{
  return ecy;
}

double Track::getRadius() const
{
  return radius;
}

bool Track::getThisFrame() const
{
  return thisFrame;
}

