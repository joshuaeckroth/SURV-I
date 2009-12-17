#include "behavior.h"
#include "agent.h"
#include "entity.h"

Behavior::Behavior(int _id, std::vector<Agent*> _agents, QString _score, QString _content, QString _conflicts)
        : Entity(), id(_id), agents(_agents), score(_score), content(_content), conflicts(_conflicts), highlighted(false)
{ }

Behavior::~Behavior()
{ }

int Behavior::getId() const
{
    return id;
}

void Behavior::agents_begin()
{
    agents_iter = agents.begin();
}

bool Behavior::agents_end() const
{
    return agents_iter == agents.end();
}

Agent *Behavior::agents_next()
{
    return *(agents_iter++);
}

QString Behavior::getContent() const
{
    return content;
}

void Behavior::setAccepted(bool _accepted)
{
    accepted = _accepted;
}

bool Behavior::isAccepted() const
{
    return accepted;
}

QString Behavior::getScore() const
{
    return score;
}

QStringList Behavior::getData() const
{
    QStringList data;
    data << "Behavior" << QString::number(id)
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

void Behavior::setHighlighted(bool h)
{
    highlighted = h;
}

bool Behavior::isHighlighted() const
{
    return highlighted;
}
