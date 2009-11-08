
#include <QString>

#include "entitiestreeitem.h"
#include "entity.h"

EntitiesTreeItem::EntitiesTreeItem(Entity *e, EntitiesTreeItem *p)
{
    entity = e;
    parentEntitiesTreeItem = p;
}

EntitiesTreeItem::~EntitiesTreeItem()
{
    qDeleteAll(childEntitiesTreeItems);
}

void EntitiesTreeItem::appendChild(EntitiesTreeItem *child)
{
    childEntitiesTreeItems.append(child);
}

EntitiesTreeItem *EntitiesTreeItem::child(int row)
{
    return childEntitiesTreeItems.value(row);
}

int EntitiesTreeItem::childCount() const
{
    return childEntitiesTreeItems.count();
}

EntitiesTreeItem *EntitiesTreeItem::parent()
{
    return parentEntitiesTreeItem;
}

int EntitiesTreeItem::columnCount() const
{
    return 4;
}

QVariant EntitiesTreeItem::data(int column) const
{
    switch(column)
    {
    case 0: return (entity ? entity->getType() : "Entity Type");
    case 1: return (entity ? QString::number(entity->getId()) : "Id");
    case 2: return (entity ? (entity->isAccepted() ? "Accepted" : "Rejected") : "Accepted?");
    case 3: return (entity ? entity->getScore() : "Score");
    default: return QVariant();
    }
}

int EntitiesTreeItem::row() const
{
    if(parentEntitiesTreeItem)
        return parentEntitiesTreeItem->childEntitiesTreeItems.indexOf(const_cast<EntitiesTreeItem*>(this));
    return 0;
}
