
#include <cmath>

#include "movement.h"
#include "detection.h"
#include "entity.h"

Movement::Movement(int _id, Detection *_det1, Detection *_det2, QString _score)
        : Entity(), id(_id), det1(_det1), det2(_det2), score(_score), highlighted(false)
{
    distance = std::sqrt(std::pow(det2->getLat() - det1->getLat(), 2.0) +
                         std::pow(det2->getLon() - det1->getLon(), 2.0));
}

Movement::~Movement() { }

int Movement::getId() const
{
    return id;
}

void Movement::setAccepted(bool _accepted)
{
    accepted = _accepted;
}

bool Movement::isAccepted() const
{
    return accepted;
}

Detection *Movement::getDet1() const
{
    return det1;
}

Detection *Movement::getDet2() const
{
    return det2;
}

QString Movement::getScore() const
{
    return score;
}

QStringList Movement::getData() const
{
    QStringList data;
    data << "Movement" << QString::number(id)
            << ""
            << (accepted ? "Accepted" : "Rejected")
            << score
            << "" << ""
            << QString::number(det1->getStartTime(), 'f', 2)
            << QString::number(det2->getEndTime(), 'f', 2)
            << QString::number(det2->getEndTime() - det1->getStartTime(), 'f', 2)
            << QString::number(distance, 'f', 2)
            << QString::number(distance / (det2->getEndTime() - det1->getStartTime()), 'f', 2)
            << "" << "";
    return data;
}

void Movement::setHighlighted(bool h)
{
    highlighted = h;
}

bool Movement::isHighlighted() const
{
    return highlighted;
}
