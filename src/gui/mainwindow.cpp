
#include <QTimer>
#include <QDockWidget>

#include "mainwindow.h"
#include "renderarea.h"
#include "settings.h"
#include "processingcontroller.h"


MainWindow::MainWindow() : QMainWindow(0)
{
  ui.setupUi(this);

  renderArea = new RenderArea(this);
  ui.verticalLayout->insertWidget(0, renderArea);

  processingController = new ProcessingController(renderArea, 2 /* num of cameras */);

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
  connect(ui.actionStop, SIGNAL(clicked()), this, SLOT(stopProcessing()));
  connect(ui.actionRecord, SIGNAL(clicked()), this, SLOT(startRecording()));
}

void MainWindow::closeEvent(QCloseEvent*)
{
  if(processingController->isProcessing())
    {
      processingController->stopProcessing();
    }
}

void MainWindow::startRecording()
{
}

void MainWindow::startProcessing()
{
  processingController->startProcessing();
  ui.actionStart->setEnabled(false);
  ui.actionStop->setEnabled(true);
}

void MainWindow::stopProcessing()
{
  processingController->stopProcessing();
  ui.actionStart->setEnabled(true);
  ui.actionStop->setEnabled(false);
}

void MainWindow::updateStats()
{
  statusBar()->showMessage(QString("Frame ") + 
			   QString::number(processingController->getFrameNumber()) +
			   QString(" (") +
			   QString::number(processingController->getFrameTime(), 'f', 2) +
			   QString("s) - ") +
			   QString("Processing FPS: ") +
			   QString::number(processingController->getCalculatedFPS(), 'f', 1));
}

void MainWindow::numCamerasChanged(int n)
{
  processingController->stopProcessing();
  ui.actionStart->setEnabled(true);
  ui.actionStop->setEnabled(false);
  processingController->numCamerasChanged(n);
}


