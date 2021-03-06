
#include <QFont>
#include <QDebug>
#include <QList>

#include "entities.h"
#include "entitiestreeitem.h"
#include "detection.h"
#include "movement.h"
#include "path.h"
#include "agent.h"
#include "behavior.h"

Entities::Entities(std::map<int,Detection*> _detections,
                   std::map<int,Movement*> _movements,
                   std::map<int,Path*> _paths,
                   std::map<int,Agent*> _agents,
                   std::map<int,Behavior*> _behaviors,
                   bool detailed)
: QAbstractItemModel(0), detections(_detections), movements(_movements),
    paths(_paths), agents(_agents), behaviors(_behaviors)
{
    rootEntitiesTreeItem = new EntitiesTreeItem(NULL, 0);

    if(detailed)
    {
        for(std::map<int,Detection*>::const_iterator it = detections.begin();
            it != detections.end(); it++)
        {
            EntitiesTreeItem *e = new EntitiesTreeItem((*it).second, rootEntitiesTreeItem);
            rootEntitiesTreeItem->appendChild(e);
        }
        for(std::map<int,Movement*>::const_iterator it = movements.begin();
            it != movements.end(); it++)
        {
            EntitiesTreeItem *e = new EntitiesTreeItem((*it).second, rootEntitiesTreeItem);
            rootEntitiesTreeItem->appendChild(e);

            EntitiesTreeItem *e1 = new EntitiesTreeItem(((*it).second)->getDet1(), e);
            e->appendChild(e1);

            EntitiesTreeItem *e2 = new EntitiesTreeItem(((*it).second)->getDet2(), e);
            e->appendChild(e2);
        }
        for(std::map<int,Path*>::const_iterator it = paths.begin();
            it != paths.end(); it++)
        {
            Path *p = (*it).second;
            EntitiesTreeItem *e = new EntitiesTreeItem(p, rootEntitiesTreeItem);
            rootEntitiesTreeItem->appendChild(e);

            p->movements_begin();
            while(!p->movements_end())
            {
                Movement *m = p->movements_next();
                EntitiesTreeItem *em = new EntitiesTreeItem(m, e);
                e->appendChild(em);

                EntitiesTreeItem *ed1 = new EntitiesTreeItem(m->getDet1(), em);
                em->appendChild(ed1);

                EntitiesTreeItem *ed2 = new EntitiesTreeItem(m->getDet2() ,em);
                em->appendChild(ed2);
            }
        }
        for(std::map<int,Agent*>::const_iterator it = agents.begin();
            it != agents.end(); it++)
        {
            Agent *a = (*it).second;
            EntitiesTreeItem *e = new EntitiesTreeItem(a, rootEntitiesTreeItem);
            rootEntitiesTreeItem->appendChild(e);

            a->paths_begin();
            while(!a->paths_end())
            {
                Path *p = a->paths_next();
                EntitiesTreeItem *ep = new EntitiesTreeItem(p, e);
                e->appendChild(ep);

                p->movements_begin();
                while(!p->movements_end())
                {
                    Movement *m = p->movements_next();
                    EntitiesTreeItem *em = new EntitiesTreeItem(m, ep);
                    ep->appendChild(em);

                    EntitiesTreeItem *ed1 = new EntitiesTreeItem(m->getDet1(), em);
                    em->appendChild(ed1);

                    EntitiesTreeItem *ed2 = new EntitiesTreeItem(m->getDet2() ,em);
                    em->appendChild(ed2);
                }
            }
        }
    } // if(detailed)

    for(std::map<int,Behavior*>::const_iterator it = behaviors.begin();
        it != behaviors.end(); it++)
    {
        Behavior *b = (*it).second;
        EntitiesTreeItem *e = new EntitiesTreeItem(b, rootEntitiesTreeItem);
        rootEntitiesTreeItem->appendChild(e);

        b->agents_begin();
        while(!b->agents_end())
        {
            Agent *a = b->agents_next();
            EntitiesTreeItem *ea = new EntitiesTreeItem(a, e);
            e->appendChild(ea);

            a->paths_begin();
            while(!a->paths_end())
            {
                Path *p = a->paths_next();
                EntitiesTreeItem *ep = new EntitiesTreeItem(p, ea);
                ea->appendChild(ep);

                if(detailed)
                {
                    p->movements_begin();
                    while(!p->movements_end())
                    {
                        Movement *m = p->movements_next();
                        EntitiesTreeItem *em = new EntitiesTreeItem(m, ep);
                        ep->appendChild(em);

                        EntitiesTreeItem *ed1 = new EntitiesTreeItem(m->getDet1(), em);
                        em->appendChild(ed1);

                        EntitiesTreeItem *ed2 = new EntitiesTreeItem(m->getDet2() ,em);
                        em->appendChild(ed2);
                    }
                }
            }
        }
    }
}

Entities::~Entities()
{
    delete rootEntitiesTreeItem;
}

void Entities::detections_begin()
{
    detections_iter = detections.begin();
}

bool Entities::detections_end() const
{
    return detections_iter == detections.end();
}

Detection *Entities::detections_next()
{
    Detection *d = (*detections_iter).second;
    detections_iter++;
    return d;
}

void Entities::movements_begin()
{
    movements_iter = movements.begin();
}

bool Entities::movements_end() const
{
    return movements_iter == movements.end();
}

Movement *Entities::movements_next()
{
    Movement *m = (*movements_iter).second;
    movements_iter++;
    return m;
}

void Entities::paths_begin()
{
    paths_iter = paths.begin();
}

bool Entities::paths_end() const
{
    return paths_iter == paths.end();
}

Path *Entities::paths_next()
{
    Path *p = (*paths_iter).second;
    paths_iter++;
    return p;
}

void Entities::agents_begin()
{
    agents_iter = agents.begin();
}

bool Entities::agents_end() const
{
    return agents_iter == agents.end();
}

Agent *Entities::agents_next()
{
    Agent *a = (*agents_iter).second;
    agents_iter++;
    return a;
}

void Entities::behaviors_begin()
{
    behaviors_iter = behaviors.begin();
}

bool Entities::behaviors_end() const
{
    return behaviors_iter == behaviors.end();
}

Behavior *Entities::behaviors_next()
{
    Behavior *b = (*behaviors_iter).second;
    behaviors_iter++;
    return b;
}

int Entities::columnCount(const QModelIndex &parent) const
{
    if(parent.isValid())
        return static_cast<EntitiesTreeItem*>(parent.internalPointer())->columnCount();
    else
        return rootEntitiesTreeItem->columnCount();
}

QVariant Entities::data(const QModelIndex &index, int role) const
{
    if(!index.isValid())
        return QVariant();

    if(role == Qt::FontRole)
    {
        QFont font;
        if(anyChildHighlighted(index)) font.setBold(true);
        return font;
    }
    else if(role == Qt::DisplayRole)
    {
        EntitiesTreeItem *e = static_cast<EntitiesTreeItem*>(index.internalPointer());

        return e->data(index.column());
    }
    else
        return QVariant();
}

bool Entities::anyChildHighlighted(QModelIndex index) const
{
    EntitiesTreeItem *e = static_cast<EntitiesTreeItem*>(index.internalPointer());
    if(e->getEntity()->isHighlighted())
        return true;

    int row = 0;
    index = index.child(row++, 0);
    while(index.isValid())
    {
        if(anyChildHighlighted(index)) return true;
        index = index.child(row++, 0);
    }
    return false;
}

Qt::ItemFlags Entities::flags(const QModelIndex &index) const
{
    if(!index.isValid())
        return 0;

    return Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

QVariant Entities::headerData(int section, Qt::Orientation orientation,
                              int role) const
{
    if(orientation == Qt::Horizontal && role == Qt::DisplayRole)
        return rootEntitiesTreeItem->data(section);

    return QVariant();
}

QModelIndex Entities::index(int row, int column, const QModelIndex &parent)
        const
{
    if(!hasIndex(row, column, parent))
        return QModelIndex();

    EntitiesTreeItem *parentEntitiesTreeItem;

    if(!parent.isValid())
        parentEntitiesTreeItem = rootEntitiesTreeItem;
    else
        parentEntitiesTreeItem = static_cast<EntitiesTreeItem*>(parent.internalPointer());

    EntitiesTreeItem *childEntitiesTreeItem = parentEntitiesTreeItem->child(row);
    if(childEntitiesTreeItem)
        return createIndex(row, column, childEntitiesTreeItem);
    else
        return QModelIndex();
}

QModelIndex Entities::parent(const QModelIndex &index) const
{
    if(!index.isValid())
        return QModelIndex();

    EntitiesTreeItem *childEntitiesTreeItem = static_cast<EntitiesTreeItem*>(index.internalPointer());
    EntitiesTreeItem *parentEntitiesTreeItem = childEntitiesTreeItem->parent();

    if(parentEntitiesTreeItem == rootEntitiesTreeItem)
        return QModelIndex();

    return createIndex(parentEntitiesTreeItem->row(), 0, parentEntitiesTreeItem);
}

int Entities::rowCount(const QModelIndex &parent) const
{
    EntitiesTreeItem *parentEntitiesTreeItem;
    if(parent.column() > 0)
        return 0;

    if(!parent.isValid())
        parentEntitiesTreeItem = rootEntitiesTreeItem;
    else
        parentEntitiesTreeItem = static_cast<EntitiesTreeItem*>(parent.internalPointer());

    return parentEntitiesTreeItem->childCount();
}

void Entities::updateHighlights()
{
    reset();
}
