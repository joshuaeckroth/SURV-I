#ifndef MOVEMENT_H
#define MOVEMENT_H

#include <QString>
#include <QStringList>

#include "entity.h"

class Detection;

class Movement : public Entity
{
public:
    Movement(int _id, Detection *_det1, Detection *_det2, QString _score);
    ~Movement();

    int getId() const;
    void setAccepted(bool);
    bool isAccepted() const;
    Detection *getDet1() const;
    Detection *getDet2() const;
    QString getScore() const;
    QStringList getData() const;
    void setHighlighted(bool h);
    bool isHighlighted() const;


private:
    int id;
    bool accepted;
    Detection *det1, *det2;
    QString score;
    double distance;
    bool highlighted;
};

#endif // MOVEMENT_H
