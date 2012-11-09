#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

#include "ui_mainwindow.h"
#include "capturethread.h"

class RenderArea;
class SettingsWidget;
class EntitiesTree;
class Entities;
class ProcessingController;
class Frame;
class AbducerReader;
class AbducerWriter;

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow();

public slots:
    void startProcessing();
    void stopProcessing();
    void updateStats();
    void numCamerasChanged(int);

protected:
    void closeEvent(QCloseEvent*);

private:
    Ui::MainWindow ui;
    EntitiesTree *entitiesTree;
    RenderArea *renderArea;
    QTimer* updateTimer;
    QDockWidget* settingsDock;
    SettingsWidget* settingsWidget;
    ProcessingController* processingController;
    AbducerReader *abducerReader;
    AbducerWriter *abducerWriter;
};

#endif


