#include "infobox.h"

InfoBox::InfoBox()
{
    ui.setupUi(this);
}

void InfoBox::setText(QString text)
{
    ui.infoarea->setText(text);
}

