
#include "detection.h"

Detection::Detection(int _id, double _lat, double _lon,
                     double _startTime, double _endTime,
                     double _area)
  : id(_id), lat(_lat), lon(_lon), startTime(_startTime), endTime(_endTime),
  area(_area)
{ }

int Detection::getId() const
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

void Detection::setAccepted(bool _accepted)
{
    accepted = _accepted;
}

bool Detection::isAccepted() const
{
    return accepted;
}
