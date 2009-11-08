#ifndef ENTITY_H
#define ENTITY_H

#include <QString>

class Entity
{
public:
    Entity();
    virtual ~Entity();
    virtual QString getType() const = 0;
    virtual int getId() const = 0;
    virtual bool isAccepted() const = 0;
    virtual QString getScore() const = 0;
};

#endif // ENTITY_H
