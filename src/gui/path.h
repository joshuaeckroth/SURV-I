#ifndef PATH_H
#define PATH_H

#include <QString>
#include <vector>

class Movement;

class Path
{
public:
    Path(int _id, std::vector<Movement*> _movements);

    int getId() const;
    void movements_begin();
    bool movements_end() const;
    Movement *movements_next();
    void setAccepted(bool);
    bool isAccepted() const;

private:
    int id;
    bool accepted;
    std::vector<Movement*> movements;
    std::vector<Movement*>::const_iterator movements_iter;
};

#endif // PATH_H
