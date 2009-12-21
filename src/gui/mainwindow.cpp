
#include <QTimer>
#include <QDockWidget>
#include <QThread>

#include "mainwindow.h"
#include "abducerreader.h"
#include "abducerwriter.h"
#include "renderarea.h"
#include "settings.h"
#include "entitiestree.h"
#include "entities.h"
#include "processingcontroller.h"


MainWindow::MainWindow() : QMainWindow(0)
{
    ui.setupUi(this);

    int numCameras = 2;

    renderArea = new RenderArea(this);
    renderArea->setNumCameras(numCameras);
    ui.verticalLayout->insertWidget(0, renderArea);

    entitiesTree = new EntitiesTree(this, renderArea);
    connect(ui.entitiesButton, SIGNAL(clicked()), entitiesTree, SLOT(show()));
    connect(ui.entitiesButton, SIGNAL(clicked()), entitiesTree, SLOT(raise()));
    connect(ui.showDetails, SIGNAL(stateChanged(int)), entitiesTree, SLOT(showDetails(int)));
    connect(ui.showDetails, SIGNAL(stateChanged(int)), renderArea, SLOT(showDetails(int)));
    connect(ui.showRegions, SIGNAL(stateChanged(int)), renderArea, SLOT(showRegions(int)));

    processingController = new ProcessingController(numCameras);

    abducerReader = new AbducerReader();
    abducerWriter = new AbducerWriter();

    connect(processingController, SIGNAL(newFrame(Frame*)), renderArea, SLOT(newFrame(Frame*)));
    connect(processingController, SIGNAL(sendDetections(QString)), abducerWriter, SLOT(sendDetections(QString)));

    connect(abducerReader, SIGNAL(newEntities(Entities*)), renderArea, SLOT(updateEntities(Entities*)));
    connect(abducerReader, SIGNAL(newEntities(Entities*)), entitiesTree, SLOT(updateEntities(Entities*)));
    connect(abducerReader, SIGNAL(newEntities(Entities*)), processingController, SLOT(newEntities(Entities*)));

    abducerReader->start(QThread::HighestPriority);
    abducerWriter->start(QThread::HighestPriority);

    processingController->start(QThread::HighestPriority);

    // settings dock widget
    /*
  settingsDock = new QDockWidget("Settings", this);
  settingsDock->setAllowedAreas(Qt::RightDockWidgetArea);
  settingsWidget = new SettingsWidget(this);
  settingsDock->setWidget(settingsWidget);
  settingsDock->setFeatures(QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetFloatable);
  addDockWidget(Qt::RightDockWidgetArea, settingsDock);
  */

    updateTimer = new QTimer(this);
    connect(updateTimer, SIGNAL(timeout()), this, SLOT(updateStats()));
    updateTimer->start(100);

    connect(ui.actionStart, SIGNAL(clicked()), this, SLOT(startProcessing()));
    connect(ui.actionStart, SIGNAL(clicked()), processingController, SLOT(startProcessing()));
    connect(ui.actionStop, SIGNAL(clicked()), this, SLOT(stopProcessing()));
    connect(ui.actionStop, SIGNAL(clicked()), processingController, SLOT(stopProcessing()));
}

void MainWindow::startProcessing()
{
    ui.actionStart->setEnabled(false);
    ui.actionStop->setEnabled(true);
}

void MainWindow::stopProcessing()
{
    ui.actionStart->setEnabled(true);
    ui.actionStop->setEnabled(false);
}

void MainWindow::updateStats()
{
    statusBar()->showMessage(processingController->getCameraTimes());
}

void MainWindow::numCamerasChanged(int n)
{
    ui.actionStart->setEnabled(true);
    ui.actionStop->setEnabled(false);
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    processingController->stopProcessing();
}


