#ifndef RENDER_THREAD_H
#define RENDER_THREAD_H

#include <QThread>

class ImageBuffer;
class RenderArea;

class RenderThread : public QThread
{
public:
  RenderThread(ImageBuffer* buffer, RenderArea* r, int c);
  void run();

private:
  ImageBuffer* imageBuffer;
  RenderArea* renderer;
  int camera;
};

#endif

