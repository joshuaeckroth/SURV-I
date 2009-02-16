
#include "track.h"
#include "frame.h"

Track::Track(int i, double x, double y, double ox, double oy,
	     int p, int n, double ex, double ey, double r)
  : id(i), cx(x), cy(y), ocx(ox), ocy(oy),
    prevId(p), nextId(n), ecx(ex), ecy(ey), radius(r)
{ }

void Track::setFrame(Frame *f)
{
  frame = f;
}
