
#include "noise.h"
#include "frame.h"

Noise::Noise(int i, int c, double a, double x, double y)
  : id(i), camera(c), area(a), cx(x), cy(y)
{ }

void Noise::setFrame(Frame *f)
{
  frame = f;
}

