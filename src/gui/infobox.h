#ifndef INFOBOX_H
#define INFOBOX_H

#include <QDialog>
#include <QString>
#include "ui_infobox.h"

class InfoBox : public QDialog
{
public:
    InfoBox();
    void setText(QString);

private:
    Ui::InfoBox ui;
};

#endif // INFOBOX_H
