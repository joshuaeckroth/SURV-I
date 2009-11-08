#ifndef PATH_H
#define PATH_H

#include <QString>
#include <QStringList>
#include <vector>

#include "entity.h"

class Movement;

class Path : public Entity
{
public:
    Path(int _id, std::vector<Movement*> _movements, QString _score);
    ~Path();

    int getId() const;
    void movements_begin();
    bool movements_end() const;
    Movement *movements_next();
    void setAccepted(bool);
    bool isAccepted() const;
    QString getScore() const;
    QStringList getData() const;

private:
    int id;
    bool accepted;
    std::vector<Movement*> movements;
    std::vector<Movement*>::const_iterator movements_iter;
    QString score;
    double distance;
    double duration;
};

#endif // PATH_H
