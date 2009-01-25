#ifndef IMAGE_BUFFER_H
#define IMAGE_BUFFER_H

#include <QWaitCondition>
#include <QMutex>
#include <QQueue>

#include "opencv/cxcore.h"

class ImageBuffer
{
public:
  ImageBuffer(int size);
  void addFrame(const IplImage* image);
  IplImage* getFrame();
  void clear();
  void exit();

private:
  QWaitCondition bufferNotEmpty;
  QWaitCondition bufferNotFull;
  QMutex mutex;
  int bufferSize;
  QQueue<IplImage*> imageQueue;
};

#endif
