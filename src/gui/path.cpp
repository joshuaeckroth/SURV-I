#include "path.h"
#include "movement.h"
#include "entities.h"

Path::Path(QString _id, bool _accepted, std::vector<Movement*> _movements)
        : id(_id), accepted(_accepted), entities(NULL), movements(_movements)
{ }

void Path::setEntities(Entities *e)
{
    entities = e;
}

QString Path::getId() const
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

bool Path::isAccepted() const
{
    return accepted;
}
