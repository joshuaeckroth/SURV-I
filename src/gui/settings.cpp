
#include "settings.h"
#include "mainwindow.h"

SettingsWidget::SettingsWidget(MainWindow* m) : QWidget(0), mainWindow(m)
{
  ui.setupUi(this);

  connect(ui.numCamerasSpinBox, SIGNAL(valueChanged(int)),
	  mainWindow, SLOT(numCamerasChanged(int)));
}

