#ifndef ENTITY_H
#define ENTITY_H

#include <QStringList>

class Entity
{
public:
    Entity();
    virtual ~Entity();
    virtual QStringList getData() const = 0;
};

#endif // ENTITY_H
