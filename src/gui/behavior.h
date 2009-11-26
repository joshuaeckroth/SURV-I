#ifndef BEHAVIOR_H
#define BEHAVIOR_H

#include <QString>
#include <QStringList>
#include <vector>

#include "entity.h"

class Path;

class Behavior : public Entity
{
public:
    Behavior(int _id, std::vector<Path*> _paths, QString _score, QString _content);
    ~Behavior();

    int getId() const;
    void paths_begin();
    bool paths_end() const;
    Path *paths_next();
    void setAccepted(bool);
    bool isAccepted() const;
    QString getScore() const;
    QStringList getData() const;
    void setHighlighted(bool h);
    bool isHighlighted() const;


private:
    int id;
    bool accepted;
    std::vector<Path*> paths;
    std::vector<Path*>::const_iterator paths_iter;
    QString score;
    QString conflicts;
    QString content;
    bool highlighted;
};

#endif // BEHAVIOR_H
