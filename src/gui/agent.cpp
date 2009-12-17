#include "agent.h"
#include "path.h"
#include "entity.h"

Agent::Agent(int _id, std::vector<Path*> _paths, QString _score, QString _content, QString _conflicts)
        : Entity(), id(_id), paths(_paths), score(_score), content(_content), conflicts(_conflicts), highlighted(false)
{ }

Agent::~Agent()
{ }

int Agent::getId() const
{
    return id;
}

void Agent::paths_begin()
{
    paths_iter = paths.begin();
}

bool Agent::paths_end() const
{
    return paths_iter == paths.end();
}

Path *Agent::paths_next()
{
    return *(paths_iter++);
}

QString Agent::getContent() const
{
    return content;
}

void Agent::setAccepted(bool _accepted)
{
    accepted = _accepted;
}

bool Agent::isAccepted() const
{
    return accepted;
}

QString Agent::getScore() const
{
    return score;
}

QStringList Agent::getData() const
{
    QStringList data;
    data << "Agent" << QString::number(id)
            << content
            << (accepted ? "Accepted" : "Rejected")
            << score
            << ""
            << ""
            << ""
            << ""
            << ""
            << ""
            << ""
            << ""
            << conflicts;
    return data;
}

void Agent::setHighlighted(bool h)
{
    highlighted = h;
}

bool Agent::isHighlighted() const
{
    return highlighted;
}
