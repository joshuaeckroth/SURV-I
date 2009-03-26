
#include <QDebug>

#include <iostream>

#include "processingcontroller.h"
#include "renderarea.h"
#include "decoder.h"
#include "abducerthread.h"
#include "frame.h"

ProcessingController::ProcessingController(RenderArea* r, int n)
  : curFrame(NULL), curFrameNumber(1), numCameras(n), renderer(r)
{
  frames = new Frame*[numCameras];
  detections = new QString[numCameras];

  for(int i = 0; i < numCameras; i++)
    {
      frames[i] = NULL;
      detections[i] = QString();
    }

  renderer->setNumCameras(numCameras);
  init();
}

void ProcessingController::init()
{
  abducerThread = new AbducerThread();

  for(int i = 0; i < numCameras; i++)
    {
      decoder[i] = new Decoder(i);
      captureThread[i] = new CaptureThread(decoder[i], i);
      if(captureThread[i]->hasError())
	{
	  qDebug() << "Error in camera " << QString::number(i);
	}
      else
	{
	  connect(captureThread[i], SIGNAL(newDetections(QString, int, Frame*)),
		  this, SLOT(newDetections(QString, int, Frame*)));
	  connect(abducerThread, SIGNAL(newTracks()), this, SLOT(newTracks()));
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

double ProcessingController::getCalculatedFPS() const
{
  double fps = 0.0;
  for(int i = 0; i < numCameras; i++)
    {
      fps += captureThread[i]->getCalculatedFPS();
    }
  return (fps / (double)numCameras);
}

int ProcessingController::getFrameNumber() const
{
  if(curFrame != NULL)
    return curFrame->getNumber();
  else
    return -1;
}

double ProcessingController::getFrameTime() const
{
  if(curFrame != NULL)
    return curFrame->getTime();
  else
    return -1.0;
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

void ProcessingController::newDetections(QString ds, int camera, Frame* f)
{
  bool allDetected = true;
  for(int i = 0; i < numCameras; i++)
    {
      if(frames[i] == NULL || frames[i]->getNumber() < curFrameNumber)
	allDetected = false;
    }

  // if all cameras have produced detections,
  // then give these detections to the abducer thread
  if(allDetected)
    {
      // build collected detections string
      // get frame info from a single frame (they should all have same info)
      QString allDetections;
      QTextStream stream(&allDetections);
      stream << "<Frame number=\"" << frames[0]->getNumber() << "\" "
	     << "time=\"" << frames[0]->getTime() << "\">";

      for(int i = 0; i < numCameras; i++)
	{
	  stream << detections[i];
	  detections[i].clear();
	}

      stream << "</Frame>";

      qDebug() << "Sending to abducer" << allDetections;
      
      abducerThread->newDetections(allDetections);

      curFrame = frames[0]; // update current frame
      curFrameNumber++;

      // stop grabbing frames until the abducer returns
      //for(int i = 0; i < numCameras; i++)
      //captureThread[i]->stopCapture();
    }
  else
    {
      detections[camera] = ds;
      if(frames[camera] != NULL)
	delete frames[camera];
      frames[camera] = f;
    }
}

void ProcessingController::newTracks()
{
  qDebug() << "Showing frames in GUI.";
  renderer->showFrames(frames);

  for(int i = 0; i < numCameras; i++)
    {
      //delete frames[i];
      //frames[i] = NULL;

      // start capturing again
      //if(!captureThread[i]->hasError())
      //{
      //  captureThread[i]->startCapture();
      //  captureThread[i]->start(QThread::IdlePriority);
      //}
      //else
      //qDebug() << "capture thread for camera " << QString::number(i) << " has an error";
    }
}
