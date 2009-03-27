
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

bool Track::getThisFrame() const
{
  return thisFrame;
}

