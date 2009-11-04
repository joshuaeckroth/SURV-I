
#include <QDebug>

#include <vector>

#include "entities.h"
#include "detection.h"
#include "movement.h"
#include "path.h"

Entities::Entities()
{ }

void Entities::addDetection(Detection *d)
{
  d->setEntities(this);
  detections.push_back(d);
}

void Entities::addMovement(Movement *m)
{
  m->setEntities(this);
  movements.push_back(m);
}

void Entities::addPath(Path *p)
{
  p->setEntities(this);
  paths.push_back(p);
}

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
  return *(detections_iter++);
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
  return *(movements_iter++);
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
  return *(paths_iter++);
}

void Entities::appendLog(const QString l)
{
    log.append(l);
}

QString Entities::getLog() const
{
    return log;
}
