
#include "detection.h"
#include "entity.h"

Detection::Detection(int _id, double _lat, double _lon,
                     double _startTime, double _endTime,
                     double _area, QString _score)
: Entity(), id(_id), lat(_lat), lon(_lon), startTime(_startTime), endTime(_endTime),
    area(_area), score(_score), highlighted(false)
{ }

Detection::~Detection() { }

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

QString Detection::getScore() const
{
    return score;
}

QStringList Detection::getData() const
{
    QStringList data;
    data << "Detection" << QString::number(id)
            << ""
            << (accepted ? "Accepted" : "Rejected")
            << score
            << QString::number(lat, 'f', 2)
            << QString::number(lon, 'f', 2)
            << QString::number(startTime, 'f', 2)
            << QString::number(endTime, 'f', 2)
            << "" << "" << ""
            << QString::number(area, 'f', 2)
            << "";
    return data;
}

void Detection::setHighlighted(bool h)
{
    highlighted = h;
}

bool Detection::isHighlighted() const
{
    return highlighted;
}
