
#include <QDebug>

#include "processingcontroller.h"
#include "renderarea.h"
#include "decoder.h"
#include "abducerthread.h"
#include "frames.h"

ProcessingController::ProcessingController(RenderArea* r, int n)
  : numCameras(n), renderer(r)
{
  frames = new Frames();
  renderer->setNumCameras(n);
  connect(frames, SIGNAL(newFrame(const Frame*)), renderer, SLOT(newFrame(const Frame*)));
  init();
}

void ProcessingController::init()
{
  abducerThread = new AbducerThread(frames);

  for(int i = 0; i < numCameras; i++)
    {
      decoder[i] = new Decoder(i, numCameras);
      captureThread[i] = new CaptureThread(frames, decoder[i], i);
      if(captureThread[i]->hasError())
	{
	  qDebug() << "Error in camera " << QString::number(i);
	}
      else
	{
	  connect(captureThread[i], SIGNAL(newDetections(QString)), this, SLOT(newDetections(QString)));
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
	  captureThread[i]->startCapture();
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
      captureThread[i]->exit();
      //delete captureThread[i];
      //delete decoder[i];
    }

  numCameras = n;

  renderer->setNumCameras(n);

  init();
}

void ProcessingController::newDetections(QString detections)
{
  qDebug() << "decoder found new detections";
  abducerThread->newDetections(detections);
}

void ProcessingController::newTracks(QString tracks)
{
  qDebug() << "abducer returned new tracks" << tracks;
}
