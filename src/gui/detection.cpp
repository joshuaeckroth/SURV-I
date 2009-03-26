
#include "detection.h"
#include "frame.h"

Detection::Detection(int i, int c, double a, double x, double y)
  : id(i), camera(c), area(a), cx(x), cy(y)
{ }

void Detection::setFrame(Frame *f)
{
  frame = f;
}
