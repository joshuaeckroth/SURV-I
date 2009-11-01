#ifndef PROCESSING_CONTROLLER_H
#define PROCESSING_CONTROLLER_H

#include "capturethread.h"

#include <QMap>
#include <QPair>
#include <QMutex>
#include <QTimer>

class RenderArea;
class Decoder;
class AbducerThread;
class Frame;
class Entities;

class ProcessingController : public QObject
{
  Q_OBJECT;
  
public:
  ProcessingController(RenderArea* r, int n);
  QString getCameraTimes() const;
  void numCamerasChanged(int n);

public slots:
  void startProcessing();
  void stopProcessing();

private slots:
  void newDetections(QString, Frame*);
  void newEntities(Entities*);
  void sendDetections();

private:
  int numCameras;
  RenderArea* renderer;
  CaptureThread::FrameSize frameSize;
  CaptureThread* captureThread[10];
  Decoder* decoder[10];
  AbducerThread* abducerThread;
  void init();
  QMutex mutex;
  bool isProcessing;
  QTimer* abducerTimer;
  Frame** curFrame;
  QString detections;
};

#endif

