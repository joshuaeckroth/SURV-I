#ifndef ENTITIESTREE_H
#define ENTITIESTREE_H

#include <QDialog>
#include "ui_entitiesTree.h"

class MainWindow;
class Entities;

class EntitiesTree : public QDialog
{
    Q_OBJECT;

public:
    EntitiesTree(MainWindow *m);

public slots:
    void updateEntities(Entities *entities);

private:
    Ui::EntitiesTree ui;
    MainWindow *mainWindow;
};

#endif // ENTITIESTREE_H
