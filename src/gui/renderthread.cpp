
#include <QDebug>

#include "renderthread.h"
#include "imagebuffer.h"
#include "renderarea.h"

RenderThread::RenderThread(ImageBuffer* buffer, RenderArea* r, int c)
  : QThread(), imageBuffer(buffer), renderer(r), camera(c)
{ }

void RenderThread::run()
{
  while(true)
    {
      IplImage* currentFrame = imageBuffer->getFrame();
      if(currentFrame)
	{
	  renderer->showFrame(currentFrame, camera);
	  cvReleaseImage(&currentFrame);
	}
    }
}
