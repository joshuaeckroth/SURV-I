#include <cmath>

#include "path.h"
#include "detection.h"
#include "movement.h"
#include "entity.h"

Path::Path(int _id, std::vector<Movement*> _movements, QString _score)
        : Entity(), id(_id), movements(_movements), score(_score), highlighted(false)
{
    distance = 0.0;
    for(std::vector<Movement*>::const_iterator it = movements.begin();
        it != movements.end(); it++)
    {
        distance += std::sqrt(std::pow((*it)->getDet2()->getLat() -
                                       (*it)->getDet1()->getLat(), 2.0) +
                              std::pow((*it)->getDet2()->getLon() -
                                       (*it)->getDet1()->getLon(), 2.0));
    }
    duration = (movements.back())->getDet2()->getEndTime() -
               (movements.front())->getDet1()->getStartTime();
}

Path::~Path()
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

QString Path::getScore() const
{
    return score;
}

QStringList Path::getData() const
{
    QStringList data;
    data << "Path" << QString::number(id)
            << (accepted ? "Accepted" : "Rejected")
            << score
            << "" << ""
            << QString::number((movements.front())->getDet1()->getStartTime(), 'f', 2)
            << QString::number((movements.back())->getDet2()->getEndTime(), 'f', 2)
            << QString::number(duration, 'f', 2)
            << QString::number(distance, 'f', 2)
            << QString::number(distance / duration, 'f', 2)
            << "";
    return data;
}

void Path::setHighlighted(bool h)
{
    highlighted = h;
}

bool Path::isHighlighted() const
{
    return highlighted;
}
