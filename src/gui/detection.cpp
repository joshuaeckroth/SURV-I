
#include "detection.h"
#include "entities.h"

Detection::Detection(QString _id, double _lat, double _lon, int _startTime, int _endTime)
  : id(_id), lat(_lat), lon(_lon), startTime(_startTime), endTime(_endTime)
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

int Detection::getStartTime() const
{
    return startTime;
}

int Detection::getEndTime() const
{
    return endTime;
}

