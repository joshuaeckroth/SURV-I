#ifndef PROCESSING_CONTROLLER_H
#define PROCESSING_CONTROLLER_H

#include "capturethread.h"

class RenderArea;
class Decoder;
class AbducerThread;
class Frames;

class ProcessingController : public QObject
{
  Q_OBJECT;
  
public:
  ProcessingController(RenderArea* r, int n);
  bool isProcessing();
  double getCalculatedFPS();
  void numCamerasChanged(int n);

public slots:
  void startProcessing();
  void stopProcessing();

private slots:
  void newDetections(QString);
  void newTracks(QString);

private:
  Frames *frames;
  int numCameras;
  RenderArea* renderer;
  CaptureThread::FrameSize frameSize;
  CaptureThread* captureThread[10];
  Decoder* decoder[10];
  AbducerThread* abducerThread;
  void init();
};

#endif

