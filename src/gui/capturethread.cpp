
#include <QTime>
#include <QDebug>
#include <QFile>

#include "opencv/highgui.h"

#include "capturethread.h"
#include "imagebuffer.h"
#include "decoder.h"

CaptureThread::CaptureThread(ImageBuffer* buffer, Decoder* d, int c)
  : QThread(), imageBuffer(buffer), decoder(d), captureActive(false),
    calculatedFps(0.0), fps(0.0), frameTime(0.0), frameNum(0), camera(c),
    error(false)
{
  char filename[20];
  sprintf(filename, "camera-%d.avi", camera);
  QFile file(filename);
  if(!file.exists() || !(capture = cvCaptureFromFile(filename)))
    {
      error = true;
    }

  fps = cvGetCaptureProperty(capture, CV_CAP_PROP_FPS);
}

void CaptureThread::run()
{
  QTime time;
  time.start();
  IplImage *frame;
  QString detections;
  while(true)
    {
      if(!captureActive)
	{
	  captureLock.lock();
	  calculatedFps = 0;
	  frameTimes.clear();
	  captureWait.wait(&captureLock);
	  time.restart();
	  updateFPS(time.elapsed());
	  captureLock.unlock();
	}
      frame = cvQueryFrame(capture);
      if(frame)
	{
	  frameNum++;
	  frameTime = frameNum / fps;
	  imageBuffer->addFrame(frame);
	  detections = decoder->decodeFrame(frame, frameNum, frameTime);
	  emit newDetections(detections);
	}
      updateFPS(time.elapsed());
    }
}

void CaptureThread::updateFPS(int time)
{
  frameTimes.enqueue(time);
  if(frameTimes.size() > 15)
    {
      frameTimes.dequeue();
    }
  if(frameTimes.size() > 2)
    {
      calculatedFps = frameTimes.size() / ((double)time - frameTimes.head()) * 1000.0;
    }
  else
    {
      calculatedFps = 0;
    }
}

bool CaptureThread::startCapture(int frameRate)
{
  if(!captureActive)
    {
      if(!capture || !imageBuffer)
	{
	  qDebug() << "Error: capture not initialized or invalid buffer";
	  return false;
	}
      captureActive = true;
      captureWait.wakeAll();
      return true;
    }
  return false;
}

void CaptureThread::stopCapture()
{
  captureActive = false;
}

