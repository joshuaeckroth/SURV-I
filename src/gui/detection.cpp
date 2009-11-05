
#include "detection.h"
#include "entities.h"

Detection::Detection(QString _id, double _lat, double _lon,
                     double _startTime, double _endTime,
                     double _area, bool _accepted)
  : id(_id), lat(_lat), lon(_lon), startTime(_startTime), endTime(_endTime),
  area(_area), accepted(_accepted)
{ }

void Detection::setEntities(Entities *e)
{
  entities = e;
}

QString Detection::getId() const
{
  return id;
}

double Detection::getLat() const
{
    return lat;
}

double Detection::getLon() const
{
    return lon;
}

double Detection::getStartTime() const
{
    return startTime;
}

double Detection::getEndTime() const
{
    return endTime;
}

double Detection::getArea() const
{
    return area;
}

bool Detection::isAccepted() const
{
    return accepted;
}
