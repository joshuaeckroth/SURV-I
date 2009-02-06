#ifndef PROCESSING_CONTROLLER_H
#define PROCESSING_CONTROLLER_H

#include "capturethread.h"

class ImageBuffer;
class RenderArea;
class RenderThread;
class Decoder;
class AbducerThread;

class ProcessingController : public QObject
{
  Q_OBJECT;
  
public:
  ProcessingController(RenderArea* r, int n);
  bool isProcessing();
  void setFrameRate(int rate) { frameRate = rate; }
  int getFrameRate() { return frameRate; }
  double getCalculatedFPS();
  void numCamerasChanged(int n);

public slots:
  void startProcessing();
  void stopProcessing();

private slots:
  void newDetections(QString);
  void newTracks(QString);

private:
  int frameRate;
  int numCameras;
  RenderArea* renderer;
  CaptureThread::FrameSize frameSize;
  CaptureThread* captureThread[10];
  RenderThread* renderThread[10];
  ImageBuffer* imageBuffer[10];
  Decoder* decoder[10];
  AbducerThread* abducerThread;
  void init();
};

#endif

