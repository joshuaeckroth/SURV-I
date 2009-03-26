
#include <QPainter>
#include <QTime>
#include <QDebug>
#include <QPen>

#include "renderarea.h"
#include "frame.h"
#include "detection.h"
#include "noise.h"
#include "track.h"

RenderArea::RenderArea(QWidget* parent)
  : QWidget(parent), clear(true), entities(NULL)
{
  for(int i = 0; i < 10; i++)
    {
      imageData[i] = 0;
      imageWidth[i] = 0;
      imageHeight[i] = 0;
    }
  setAttribute(Qt::WA_OpaquePaintEvent, true);
  setAttribute(Qt::WA_PaintOnScreen, true);
  setSizePolicy(QSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding));
  time = framesShown = 0;
}

void RenderArea::setNumCameras(int n)
{
  numCameras = n;
  clear = true;
  update();
}

void RenderArea::onFrameSizeChanged(int width, int height, int camera)
{
  setFixedSize(width, height);
  imageWidth[camera] = width;
  imageHeight[camera] = height;
}

void RenderArea::showFrames(Frame** frames, Frame* f)
{
  entities = f;

  clear = false;
  for(int i = 0; i < numCameras; i++)
    {
      updatePixmap(frames[i]->getImage(), i);
    }

  update();
}

void RenderArea::updatePixmap(const IplImage* frame, int camera)
{
  QTime t;
  t.start();
  bool start = false;
  if(frame->width != imageWidth[camera] || frame->height != imageHeight[camera])
    {
      if(imageData[camera])
	{
	  delete[] imageData[camera];
	}
      start = true;

      imageWidth[camera] = frame->width;
      imageHeight[camera] = frame->height;

      emit(frameSizeChanged(imageWidth[camera], imageHeight[camera], camera));

      imageData[camera] = new unsigned char[4 * imageWidth[camera] * imageHeight[camera]];
      for(int i = 0; i < imageWidth[camera] * imageHeight[camera]; i++)
	{
	  imageData[camera][i * 4 + 3] = 0xFF; // alpha channel
	}
    }

  int pixels = imageWidth[camera] * imageHeight[camera];
  uchar* src = (uchar*)(frame->imageData);
  uchar* srcEnd = src + (3 * pixels);
  uchar* dest = imageData[camera];

  do
    {
      memcpy(dest, src, 3);
      dest += 4;
      src += 3;
    }
  while(src < srcEnd);

  if(!start)
    {
      ++framesShown;
      time += t.elapsed();
    }
}

void RenderArea::paintEvent(QPaintEvent*)
{
  QPainter painter(this);
  painter.setRenderHint(QPainter::Antialiasing);

  bool hasAllFrames = true;
  for(int i = 0; i < numCameras; i++)
    {
      if(!imageData[i])
	{
	  hasAllFrames = false;
	  break;
	}
    }

  if(!clear && hasAllFrames)
    {
      int eachWidth = width() / numCameras;
      for(int i = 0; i < numCameras; i++)
	{
	  QImage tImg(imageData[i], imageWidth[i], imageHeight[i], QImage::Format_RGB32);
	  QImage sImg = tImg.scaledToWidth(eachWidth, Qt::SmoothTransformation);
	  int heightTop = (height() - sImg.height()) / 2;
	  painter.drawImage(QPoint(i * eachWidth, heightTop), sImg);

	  painter.setBrush(Qt::black);
	  painter.drawRect(QRect(QPoint(i * eachWidth, 0),
				 QPoint(i * eachWidth + eachWidth, heightTop - 1)));
	  painter.drawRect(QRect(QPoint(i * eachWidth, height() - heightTop),
				 QPoint(i * eachWidth + eachWidth, height())));
	}

      QPen pen;

      pen.setColor(Qt::blue);
      pen.setWidth(3);
      painter.setPen(pen);

      entities->detections_begin();
      while(!entities->detections_end())
	{
	  Detection* d = entities->detections_next();
	  painter.drawArc(d->getCx(), d->getCy(), 20, 20, 0, 5760);
	}


      pen.setColor(Qt::black);
      painter.setPen(pen);

      entities->noise_begin();
      while(!entities->noise_end())
	{
	  Noise* n = entities->noise_next();
	  painter.drawArc(n->getCx(), n->getCy(), 20, 20, 0, 5760);
	}


      pen.setColor(Qt::green);
      painter.setPen(pen);

      entities->tracks_begin();
      while(!entities->tracks_end())
	{
	  Track* t = entities->tracks_next();
	  painter.drawArc(t->getCx(), t->getCy(), 20, 20, 0, 5760);
	}
    }
  else
    {
      painter.setBrush(Qt::black);
      painter.drawRect(rect());
    }
}


