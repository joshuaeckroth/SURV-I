
#include "detection.h"
#include "frame.h"

Detection::Detection(int i, double a, double x, double y)
  : id(i), area(a), cx(x), cy(y)
{ }

void Detection::setFrame(Frame *f)
{
  frame = f;
}
