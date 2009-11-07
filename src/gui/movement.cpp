
#include "movement.h"
#include "detection.h"

Movement::Movement(int _id, Detection *_det1, Detection *_det2)
  : id(_id), det1(_det1), det2(_det2)
{ }

int Movement::getId() const
{
    return id;
}

void Movement::setAccepted(bool _accepted)
{
    accepted = _accepted;
}

bool Movement::isAccepted() const
{
    return accepted;
}

Detection *Movement::getDet1() const
{
    return det1;
}

Detection *Movement::getDet2() const
{
    return det2;
}
