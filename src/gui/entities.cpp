
#include <map>
#include <QDebug>

#include "entities.h"
#include "detection.h"
#include "movement.h"
#include "path.h"

Entities::Entities(std::map<int,Detection*> _detections,
                   std::map<int,Movement*> _movements,
                   std::map<int,Path*> _paths)
    : detections(_detections), movements(_movements), paths(_paths)
{ }

void Entities::detections_begin()
{
    detections_iter = detections.begin();
}

bool Entities::detections_end() const
{
    return detections_iter == detections.end();
}

Detection *Entities::detections_next()
{
    Detection *d = (*detections_iter).second;
    detections_iter++;
    return d;
}

void Entities::movements_begin()
{
    movements_iter = movements.begin();
}

bool Entities::movements_end() const
{
    return movements_iter == movements.end();
}

Movement *Entities::movements_next()
{
    Movement *m = (*movements_iter).second;
    movements_iter++;
    return m;
}

void Entities::paths_begin()
{
    paths_iter = paths.begin();
}

bool Entities::paths_end() const
{
    return paths_iter == paths.end();
}

Path *Entities::paths_next()
{
    Path *p = (*paths_iter).second;
    paths_iter++;
    return p;
}
