#ifndef CAPTURE_THREAD_H
#define CAPTURE_THREAD_H

#include <QThread>
#include <QMutex>
#include <QWaitCondition>
#include <QQueue>

class Decoder;
class CvCapture;
class Frame;

class CaptureThread : public QThread
{
  Q_OBJECT

public:
  enum FrameSize { Size640, Size320 };

  CaptureThread(Decoder* d, int c);
  void run();
  bool startCapture();
  void stopCapture();
  double getCalculatedFPS() { return calculatedFps; }
  double getFPS() { return fps; }
  bool isCapturing() { return captureActive; }
  bool hasError() { return error; }

signals:
  void newDetections(QString, Frame*);

private:
  void updateFPS(int time);
  QMutex captureLock;
  QMutex frameLock;
  QMutex mutex;
  QWaitCondition captureWait;
  Decoder* decoder;
  bool captureActive;
  CvCapture* capture;
  double calculatedFps;
  double fps;
  int frameNum;
  QQueue<int> frameTimes;
  int camera;
  bool error;
};

#endif

