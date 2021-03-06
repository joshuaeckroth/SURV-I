#ifndef BEHAVIOR_H
#define BEHAVIOR_H

#include <QString>
#include <QStringList>
#include <vector>

#include "entity.h"

class Agent;

class Behavior : public Entity
{
public:
    Behavior(int _id, std::vector<Agent*> _agents, QString _score, QString _content, QString _conflicts);
    ~Behavior();

    int getId() const;
    void agents_begin();
    bool agents_end() const;
    Agent *agents_next();
    void setAccepted(bool);
    bool isAccepted() const;
    QString getScore() const;
    QStringList getData() const;
    QString getContent() const;
    void setHighlighted(bool h);
    bool isHighlighted() const;


private:
    int id;
    bool accepted;
    std::vector<Agent*> agents;
    std::vector<Agent*>::const_iterator agents_iter;
    QString score;
    QString content;
    QString conflicts;
    bool highlighted;
};

#endif // BEHAVIOR_H
