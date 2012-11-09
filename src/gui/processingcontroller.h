#ifndef PROCESSING_CONTROLLER_H
#define PROCESSING_CONTROLLER_H

#include "capturethread.h"

#include <QMap>
#include <QPair>
#include <QTimer>
#include <QThread>
#include <QMutex>

class Decoder;
class Frame;
class Entities;

class ProcessingController : public QThread
{
  Q_OBJECT
  
public:
  ProcessingController(int n);
  void run();
  QString getCameraTimes() const;
  void numCamerasChanged(int n);

signals:
  void newFrame(Frame*);
  void sendDetections(QString);

public slots:
  void startProcessing();
  void stopProcessing();
  void newEntities(Entities*);

private slots:
  void newDetections(QString, Frame*);
  void timeoutDetections();

private:
  int numCameras;
  CaptureThread::FrameSize frameSize;
  CaptureThread* captureThread[10];
  Decoder* decoder[10];
  QMutex mutex;
  bool isProcessing;
  QTimer* abducerTimer;
  Frame** curFrame;
  QString detections;
  int detectionsCount;
};

#endif

