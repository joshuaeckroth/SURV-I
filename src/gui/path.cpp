#include "path.h"
#include "movement.h"

Path::Path(int _id, std::vector<Movement*> _movements)
        : id(_id), movements(_movements)
{ }

int Path::getId() const
{
    return id;
}

void Path::movements_begin()
{
    movements_iter = movements.begin();
}

bool Path::movements_end() const
{
    return movements_iter == movements.end();
}

Movement *Path::movements_next()
{
    return *(movements_iter++);
}

void Path::setAccepted(bool _accepted)
{
    accepted = _accepted;
}

bool Path::isAccepted() const
{
    return accepted;
}
