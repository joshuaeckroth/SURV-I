
#include <QString>

#include "entitiestreeitem.h"
#include "entity.h"

EntitiesTreeItem::EntitiesTreeItem(Entity *e, EntitiesTreeItem *p)
{
    entity = e;
    parentEntitiesTreeItem = p;

    columns << "Entity Type" << "Id" << "Accepted?" << "Score"
            << "Lat" << "Lon" << "Start Time" << "End Time" << "Duration"
            << "Distance" << "Avg Speed" << "Area";
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
    return columns.count();
}

QVariant EntitiesTreeItem::data(int column) const
{
    QStringList data;
    if(entity) data = entity->getData();
    else data = columns;
    return data.at(column);
}

int EntitiesTreeItem::row() const
{
    if(parentEntitiesTreeItem)
        return parentEntitiesTreeItem->childEntitiesTreeItems.indexOf(const_cast<EntitiesTreeItem*>(this));
    return 0;
}
