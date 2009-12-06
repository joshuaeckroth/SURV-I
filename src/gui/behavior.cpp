#include "behavior.h"
#include "path.h"
#include "entity.h"

Behavior::Behavior(int _id, std::vector<Path*> _paths, QString _score, QString _content)
        : Entity(), id(_id), paths(_paths), score(_score), content(_content), highlighted(false)
{ }

Behavior::~Behavior()
{ }

int Behavior::getId() const
{
    return id;
}

void Behavior::paths_begin()
{
    paths_iter = paths.begin();
}

bool Behavior::paths_end() const
{
    return paths_iter == paths.end();
}

Path *Behavior::paths_next()
{
    return *(paths_iter++);
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
            << "";
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
