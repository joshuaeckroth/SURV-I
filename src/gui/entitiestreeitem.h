#ifndef ENTITIESTREEITEM_H
#define ENTITIESTREEITEM_H

#include <QVariant>
#include <QStringList>

class Entity;

class EntitiesTreeItem
{
public:
    EntitiesTreeItem(Entity *e, EntitiesTreeItem *p);
    ~EntitiesTreeItem();

    void appendChild(EntitiesTreeItem *child);

    EntitiesTreeItem *child(int row);
    int childCount() const;
    int columnCount() const;
    QVariant data(int column) const;
    int row() const;
    EntitiesTreeItem *parent();

    Entity *getEntity() const;

private:
    QList<EntitiesTreeItem*> childEntitiesTreeItems;
    EntitiesTreeItem *parentEntitiesTreeItem;
    Entity *entity;
    QStringList columns;
};

#endif // ENTITIESTREEITEM_H
