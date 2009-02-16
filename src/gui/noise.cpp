
#include "noise.h"
#include "frame.h"

Noise::Noise(int i, double a, double x, double y)
  : id(i), area(a), cx(x), cy(y)
{ }

void Noise::setFrame(Frame *f)
{
  frame = f;
}

