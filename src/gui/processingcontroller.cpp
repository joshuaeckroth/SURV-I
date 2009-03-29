
#include <QDebug>
#include <QMap>
#include <QPair>

#include <iostream>

#include "processingcontroller.h"
#include "renderarea.h"
#include "decoder.h"
#include "abducerthread.h"
#include "frame.h"
#include "cameramodel.h"
#include "entities.h"

ProcessingController::ProcessingController(RenderArea* r, int n)
  : curFrame(NULL), curFrameNumber(1), numCameras(n), renderer(r)
{
  renderer->setNumCameras(numCameras);
  CameraModel::setNumCameras(numCameras);
  init();
}

void ProcessingController::init()
{
  abducerThread = new AbducerThread();
  connect(abducerThread, SIGNAL(newTracks(Entities*)), this, SLOT(newTracks(Entities*)));
  abducerThread->start();

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
  // if already some detections for this frame
  if(detections.contains(f->getNumber()))
    {
      // add this camera's detections for this frame
      QPair<Frame*, QString> pair(f, ds);
      QMap<int, QPair<Frame*, QString> > t = detections.value(f->getNumber());
      t.insert(camera, pair);
      detections.insert(f->getNumber(), t);
    }
  else
    {
      // create new map for detections for this frame
      QMap<int, QPair<Frame*, QString> > m;
      QPair<Frame*, QString> pair(f, ds);
      m.insert(camera, pair);
      detections.insert(f->getNumber(), m);
    }

  bool allDetected = true;
  for(int i = 0; i < numCameras; i++)
    {
      // determine if the detections map contains a camera detection for
      // the current frame and this camera
      if(!detections.value(curFrameNumber).contains(i))
	allDetected = false;
    }

  // if all cameras have produced detections,
  // then give these detections to the abducer thread
  if(allDetected)
    {
      // update current frame with frame from camera 0
      curFrame = detections.value(curFrameNumber).value(0).first;

      // wait for the next frame
      // build collected detections string
      // get frame info from a single frame (they should all have same info)
      QString allDetections;
      QTextStream stream(&allDetections);
      stream << "<Frame number=\"" << getFrameNumber() << "\" "
	     << "time=\"" << getFrameTime() << "\">";

      for(int i = 0; i < numCameras; i++)
	{
	  // grab string of detections for current frame and this camera
	  stream << detections.value(curFrameNumber).value(i).second;
	}

      stream << "</Frame>";

      //qDebug() << "Sending to abducer" << allDetections;
      std::cout << allDetections.toStdString() << std::endl;
      
      abducerThread->newDetections(allDetections);

      // wait for the next frame
      curFrameNumber++;

      // stop grabbing frames until the abducer returns
      //for(int i = 0; i < numCameras; i++)
      //captureThread[i]->stopCapture();
    }
}

void ProcessingController::newTracks(Entities* e)
{
  renderer->showFrames(detections, curFrameNumber, e);

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
