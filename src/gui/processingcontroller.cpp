
#include <QDebug>

#include "processingcontroller.h"
#include "imagebuffer.h"
#include "renderthread.h"
#include "renderarea.h"
#include "decoder.h"
#include "abducerthread.h"

ProcessingController::ProcessingController(RenderArea* r, int n)
  : frameRate(6), numCameras(n), renderer(r)
{
  renderer->setNumCameras(n);
  init();
}

void ProcessingController::init()
{
  abducerThread = new AbducerThread();

  for(int i = 0; i < numCameras; i++)
    {
      imageBuffer[i] = new ImageBuffer(20);
      decoder[i] = new Decoder(i, numCameras);
      captureThread[i] = new CaptureThread(imageBuffer[i], decoder[i], i);
      renderThread[i] = new RenderThread(imageBuffer[i], renderer, i);
      if(captureThread[i]->hasError())
	{
	  qDebug() << "Error in camera " << QString::number(i);
	}
      else
	{
	  connect(captureThread[i], SIGNAL(newDetections(QString)), this, SLOT(newDetections(QString)));
	  renderThread[i]->start();
	  connect(abducerThread, SIGNAL(newTracks(QString)), this, SLOT(newTracks(QString)));
	  abducerThread->start();
	}
    }
}

void ProcessingController::startProcessing()
{
  for(int i = 0; i < numCameras; i++)
    {
      if(!captureThread[i]->hasError())
	{
	  captureThread[i]->startCapture(frameRate);
	  captureThread[i]->start(QThread::IdlePriority);
	}
    }
}

bool ProcessingController::isProcessing()
{
  bool retval = true;
  for(int i = 0; i < numCameras; i++)
    {
      retval &= captureThread[i]->isCapturing();
    }
  return retval;
}

void ProcessingController::stopProcessing()
{
  for(int i = 0; i < numCameras; i++)
    {
      captureThread[i]->stopCapture();
    }
}

double ProcessingController::getCalculatedFPS()
{
  double fps = 0.0;
  for(int i = 0; i < numCameras; i++)
    {
      fps += captureThread[i]->getCalculatedFPS();
    }
  return (fps / (double)numCameras);
}

void ProcessingController::numCamerasChanged(int n)
{
  for(int i = 0; i < numCameras; i++)
    {
      captureThread[i]->stopCapture();
      imageBuffer[i]->exit();
      //delete imageBuffer[i];
      captureThread[i]->exit();
      //delete captureThread[i];
      //delete decoder[i];
      renderThread[i]->exit();
      //delete renderThread[i];
    }

  numCameras = n;

  renderer->setNumCameras(n);

  init();
}

void ProcessingController::newDetections(QString detections)
{
  qDebug() << "new detections";
  abducerThread->newDetections(detections);
}

void ProcessingController::newTracks(QString tracks)
{
  qDebug() << "new tracks" << tracks;
}
