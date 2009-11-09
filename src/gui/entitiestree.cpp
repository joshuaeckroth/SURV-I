#include <QStringList>
#include <QTreeWidgetItem>

#include "entitiestree.h"
#include "mainwindow.h"
#include "entities.h"
#include "renderarea.h"
#include "entitiestreeitem.h"
#include "entity.h"

EntitiesTree::EntitiesTree(MainWindow *m, RenderArea *r)
        : QDialog(0), mainWindow(m), renderer(r)
{
    ui.setupUi(this);
    connect(ui.entitiesTreeView, SIGNAL(clicked(QModelIndex)), this, SLOT(clicked(QModelIndex)));
}

void EntitiesTree::updateEntities(Entities *entities)
{
    ui.entitiesTreeView->setModel(entities);
}

void EntitiesTree::clicked(QModelIndex index)
{
    EntitiesTreeItem *eti = static_cast<EntitiesTreeItem*>(index.internalPointer());
    renderer->highlightEntity(eti->getEntity());
}
