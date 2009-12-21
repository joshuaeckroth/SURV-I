#include <QStringList>
#include <QTreeWidgetItem>

#include "entitiestree.h"
#include "mainwindow.h"
#include "entities.h"
#include "renderarea.h"
#include "entitiestreeitem.h"
#include "entity.h"

EntitiesTree::EntitiesTree(MainWindow *m, RenderArea *r)
        : QDialog(0), mainWindow(m), renderer(r), showDetailsState(Qt::Unchecked)
{
    ui.setupUi(this);
    connect(ui.entitiesTreeView, SIGNAL(clicked(QModelIndex)), this, SLOT(clicked(QModelIndex)));
}

void EntitiesTree::updateEntities(Entities *entities)
{
    if(showDetailsState == Qt::Unchecked)
        ui.entitiesTreeView->setModel(entities->getNotDetailedEntities());
    else
        ui.entitiesTreeView->setModel(entities->getDetailedEntities());
}

void EntitiesTree::clicked(QModelIndex index)
{
    EntitiesTreeItem *eti = static_cast<EntitiesTreeItem*>(index.internalPointer());
    renderer->highlightEntity(eti->getEntity());
}

void EntitiesTree::showDetails(int state)
{
    showDetailsState = state;

    Entities *entities = static_cast<Entities*>(ui.entitiesTreeView->model());

    if(entities != NULL)
    {
        if(showDetailsState == Qt::Unchecked)
            ui.entitiesTreeView->setModel(entities->getNotDetailedEntities());
        else
            ui.entitiesTreeView->setModel(entities->getDetailedEntities());
    }
}
