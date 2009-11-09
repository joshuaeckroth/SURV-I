#ifndef ENTITIESTREE_H
#define ENTITIESTREE_H

#include <QDialog>
#include <QModelIndex>
#include "ui_entitiesTree.h"

class RenderArea;
class MainWindow;
class Entities;

class EntitiesTree : public QDialog
{
    Q_OBJECT;

public:
    EntitiesTree(MainWindow *m, RenderArea *r);

public slots:
    void updateEntities(Entities *entities);
    void clicked(QModelIndex index);

private:
    Ui::EntitiesTree ui;
    MainWindow *mainWindow;
    RenderArea *renderer;
};

#endif // ENTITIESTREE_H
