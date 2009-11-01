
#include "movement.h"
#include "detection.h"
#include "entities.h"

Movement::Movement(QString _id, std::vector<Detection*> _detections)
  : id(_id), entities(NULL), detections(_detections)
{ }

void Movement::setEntities(Entities *e)
{
    entities = e;
}

QString Movement::getId() const
{
    return id;
}

void Movement::detections_begin()
{
    detections_iter = detections.begin();
}

bool Movement::detections_end() const
{
    return detections_iter == detections.end();
}

Detection *Movement::detections_next()
{
    return *(detections_iter++);
}
