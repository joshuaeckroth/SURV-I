#ifndef MOVEMENT_H
#define MOVEMENT_H

#include <QString>
#include <vector>

class Entities;
class Detection;

class Movement
{
    public:
    Movement(QString _id, bool _accepted, std::vector<Detection*> _detections);
    void setEntities(Entities *e);

    QString getId() const;
    void detections_begin();
    bool detections_end() const;
    Detection *detections_next();
    bool isAccepted() const;

    private:
    QString id;
    bool accepted;
    Entities *entities;
    std::vector<Detection*> detections;
    std::vector<Detection*>::const_iterator detections_iter;
};

#endif // MOVEMENT_H
