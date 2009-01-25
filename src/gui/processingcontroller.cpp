
#include <QDebug>

#include "processingcontroller.h"
#include "imagebuffer.h"
#include "renderthread.h"
#include "renderarea.h"
#include "decoder.h"

ProcessingController::ProcessingController(RenderArea* r, int n)
  : frameRate(6), numCameras(n), renderer(r)
{
  renderer->setNumCameras(n);
  init();
}

void ProcessingController::init()
{
  for(int i = 0; i < numCameras; i++)
    {
      imageBuffer[i] = new ImageBuffer(20);
      captureThread[i] = new CaptureThread(imageBuffer[i], i);
      decoder[i] = new Decoder(i);
      renderThread[i] = new RenderThread(imageBuffer[i], decoder[i], renderer, i);
      if(captureThread[i]->hasError())
	{
	  qDebug() << "Error in camera " << QString::number(i);
	}
      else
	{
	  renderThread[i]->start();
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
