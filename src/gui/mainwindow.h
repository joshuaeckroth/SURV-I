#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

#include "ui_mainwindow.h"
#include "capturethread.h"

class RenderArea;
class SettingsWidget;
class ProcessingController;

class MainWindow : public QMainWindow
{
  Q_OBJECT;

public:
  MainWindow();
  
public slots:
  void startProcessing();
  void stopProcessing();
  void startRecording();
  void updateStats();
  void numCamerasChanged(int);

protected:
  void closeEvent(QCloseEvent*);

private:
  Ui::MainWindow ui;
  RenderArea *renderArea;
  QTimer* updateTimer;
  QDockWidget* settingsDock;
  SettingsWidget* settingsWidget;
  ProcessingController* processingController;
};

#endif


