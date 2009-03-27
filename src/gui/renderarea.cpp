
#include <QPainter>
#include <QTime>
#include <QDebug>
#include <QPen>
#include <QMap>
#include <QPair>
#include <QImage>

#include "renderarea.h"
#include "frame.h"
#include "detection.h"
#include "noise.h"
#include "track.h"
#include "cameramodel.h"

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

  map = QImage("../videos/ARL1.jpg");
  if(map.isNull())
    qDebug() << "Unable to load map image.";
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

void RenderArea::showFrames(QMap<int, QMap<int, QPair<Frame*, QString> > > detections, int number, Frame* f)
{
  entities = f;

  clear = false;
  for(int i = 0; i < numCameras; i++)
    {
      updatePixmap(detections.value(number).value(i).first->getImage(), i);
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
      // draw camera frames
      int maxHeight = 0;
      int eachWidth = width() / numCameras;
      for(int i = 0; i < numCameras; i++)
	{
	  QImage tImg(imageData[i], imageWidth[i], imageHeight[i], QImage::Format_RGB32);
	  QImage sImg = tImg.scaledToWidth(eachWidth, Qt::SmoothTransformation);
	  painter.drawImage(QPoint(i * eachWidth, 0), sImg);

	  for(int i = 0; i < numCameras; i++)
	    {
	      if(sImg.height() > maxHeight)
		maxHeight = sImg.height();
	    }
	}

      // draw map
      QPoint mapcenter = QPoint(857, 577);
      int cropHeight = height() - maxHeight;
      int cropWidth = width();
      painter.drawImage(0, maxHeight, map,
			mapcenter.x() - (cropWidth / 2), mapcenter.y() - (cropHeight / 2), // top left of map
			cropWidth, cropHeight); // size of map

      if(entities != NULL)
	{
	  QPen pen;
	  
	  pen.setColor(Qt::blue);
	  pen.setWidth(3);
	  painter.setPen(pen);
	  
	  entities->detections_begin();
	  while(!entities->detections_end())
	    {
	      Detection* d = entities->detections_next();
	      int camera = d->getCamera();
	      QPair<int,int> p = CameraModel::warpToImage(camera, QPair<double,double>(d->getCx(), d->getCy()));
	      painter.drawArc(camera * eachWidth + p.first, p.second, 20, 20, 0, 5760);
	    }
	  
	  
	  pen.setColor(Qt::black);
	  painter.setPen(pen);
	  
	  entities->noise_begin();
	  while(!entities->noise_end())
	    {
	      Noise* n = entities->noise_next();
	      int camera = n->getCamera();
	      QPair<int,int> p = CameraModel::warpToImage(camera, QPair<double,double>(n->getCx(), n->getCy()));
	      painter.drawArc(camera * eachWidth + p.first, p.second, 20, 20, 0, 5760);
	    }
	  
	  
	  pen.setColor(Qt::green);
	  painter.setPen(pen);
	  
	  entities->tracks_begin();
	  while(!entities->tracks_end())
	    {
	      Track* t = entities->tracks_next();
	      // show the track on all cameras
	      for(int i = 0; i < numCameras; i++)
		{
		  QPair<int,int> p = CameraModel::warpToImage(i, QPair<double,double>(t->getCx(), t->getCy()));
		  if(t->getThisFrame())
		    painter.drawArc(i * eachWidth + p.first, p.second, 20, 20, 0, 5760);
		}
	    }
	}
    }
  else
    {
      painter.setBrush(Qt::black);
      painter.drawRect(rect());
    }
}


