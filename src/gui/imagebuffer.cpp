
#include <QTime>
#include <QDebug>

#include "imagebuffer.h"

ImageBuffer::ImageBuffer(int size) : bufferSize(size) {}

void ImageBuffer::addFrame(const IplImage* image)
{
  if(!image)
    {
      qDebug() << "Error: Imagebuffer received a null image";
      return;
    }

  mutex.lock();
  if(imageQueue.size() == bufferSize)
    {
      bufferNotFull.wait(&mutex);
    }
  mutex.unlock();

  IplImage* temp = cvCloneImage(image);
  imageQueue.enqueue(temp);
  mutex.lock();
  bufferNotEmpty.wakeAll();
  mutex.unlock();
}

// Note: callee must release image memory
IplImage* ImageBuffer::getFrame()
{
  mutex.lock();
  if(imageQueue.isEmpty())
    {
      bufferNotEmpty.wait(&mutex);
    }
  mutex.unlock();

  IplImage* temp = 0;

  mutex.lock();
  if(!imageQueue.isEmpty())
    {
      temp = imageQueue.dequeue();
    }
  bufferNotFull.wakeAll();
  mutex.unlock();
  return temp;
}

void ImageBuffer::clear()
{
  imageQueue.clear();
  bufferNotEmpty.wakeAll();
}

void ImageBuffer::exit()
{
}


