#ifndef SETTINGS_H
#define SETTINGS_H

#include "ui_settings.h"
#include "capturethread.h"

class MainWindow;

class SettingsWidget : public QWidget
{
  Q_OBJECT;

public:
  SettingsWidget(MainWindow* m);

private:
  Ui::SettingsWidget ui;
  MainWindow* mainWindow;
};

#endif
