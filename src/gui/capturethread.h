#ifndef CAPTURE_THREAD_H
#define CAPTURE_THREAD_H

#include <QThread>
#include <QMutex>
#include <QWaitCondition>
#include <QQueue>

class ImageBuffer;
class Decoder;
class CvCapture;

class CaptureThread : public QThread
{
  Q_OBJECT

public:
  enum FrameSize { Size640, Size320 };
  CaptureThread(ImageBuffer* buffer, Decoder* d, int c);
  void run();
  bool startCapture(int frameRate);
  void stopCapture();
  double getCalculatedFPS() { return calculatedFps; }
  double getFPS() { return fps; }
  bool isCapturing() { return captureActive; }
  bool hasError() { return error; }

signals:
  void newDetections(QString);

private:
  void updateFPS(int time);
  QMutex captureLock;
  QWaitCondition captureWait;
  ImageBuffer* imageBuffer;
  Decoder* decoder;
  bool captureActive;
  CvCapture* capture;
  double calculatedFps;
  double fps;
  double frameTime;
  int frameNum;
  QQueue<int> frameTimes;
  int camera;
  bool error;
};

#endif

