#ifndef RENDER_THREAD_H
#define RENDER_THREAD_H

#include <QThread>

class ImageBuffer;
class RenderArea;
class Decoder;

class RenderThread : public QThread
{
public:
  RenderThread(ImageBuffer* buffer, Decoder* d, RenderArea* r, int c);
  void run();

private:
  ImageBuffer* imageBuffer;
  Decoder* decoder;
  RenderArea* renderer;
  int camera;
};

#endif

