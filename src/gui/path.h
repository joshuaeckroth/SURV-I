#ifndef PATH_H
#define PATH_H

#include <QString>
#include <vector>

class Entities;
class Movement;

class Path
{
public:
    Path(QString _id, std::vector<Movement*> _movements);
    void setEntities(Entities *e);

    QString getId() const;
    void movements_begin();
    bool movements_end() const;
    Movement *movements_next();

private:
    QString id;
    Entities *entities;
    std::vector<Movement*> movements;
    std::vector<Movement*>::const_iterator movements_iter;
};

#endif // PATH_H
