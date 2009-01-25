
#include <QDebug>

#include "renderthread.h"
#include "imagebuffer.h"
#include "renderarea.h"
#include "decoder.h"

RenderThread::RenderThread(ImageBuffer* buffer, Decoder* d, RenderArea* r, int c)
  : QThread(), imageBuffer(buffer), decoder(d), renderer(r), camera(c)
{ }

void RenderThread::run()
{
  QString detections;
  while(true)
    {
      IplImage* currentFrame = imageBuffer->getFrame();
      if(currentFrame)
	{
	  detections = decoder->processFrame(currentFrame);
	  qDebug() << detections;
	  renderer->processFrame(currentFrame, camera);
	  cvReleaseImage(&currentFrame);
	}
    }
}
