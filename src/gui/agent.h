#ifndef AGENT_H
#define AGENT_H

#include <QString>
#include <QStringList>
#include <vector>

#include "entity.h"

class Path;

class Agent : public Entity
{
public:
    Agent(int _id, std::vector<Path*> _paths, QString _score, QString _content, QString _conflicts);
    ~Agent();

    int getId() const;
    void paths_begin();
    bool paths_end() const;
    Path *paths_next();
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
    std::vector<Path*> paths;
    std::vector<Path*>::const_iterator paths_iter;
    QString score;
    QString content;
    QString conflicts;
    bool highlighted;
};

#endif // AGENT_H
