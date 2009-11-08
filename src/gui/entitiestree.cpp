#include <QStringList>
#include <QTreeWidgetItem>

#include "entitiestree.h"
#include "mainwindow.h"
#include "detection.h"
#include "movement.h"
#include "entities.h"

EntitiesTree::EntitiesTree(MainWindow *m) : QDialog(0), mainWindow(m)
{
    ui.setupUi(this);
}

void EntitiesTree::updateEntities(Entities *entities)
{
    ui.entitiesTreeView->setModel(entities);
}
