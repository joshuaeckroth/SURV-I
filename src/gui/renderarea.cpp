
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
      scaleFactor[i] = 1.0;
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
	  scaleFactor[i] = static_cast<double>(eachWidth) / static_cast<double>(imageWidth[i]);

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
      int mapTopLeftX = mapcenter.x() - (cropWidth / 2);
      int mapTopLeftY = mapcenter.y() - (cropHeight / 2);
      int mapBottomRightX = mapTopLeftX + cropWidth; // used to determine if warpToMap coordinates are in view
      int mapBottomRightY = mapTopLeftY + cropHeight;
      painter.drawImage(0, maxHeight, map, mapTopLeftX, mapTopLeftY, cropWidth, cropHeight);

      if(entities != NULL)
	{
	  int scaledX, scaledY;

	  QPen pen;
	  
	  pen.setColor(Qt::blue);
	  pen.setWidth(3);
	  painter.setPen(pen);
	  
	  entities->detections_begin();
	  while(!entities->detections_end())
	    {
	      Detection* d = entities->detections_next();
	      int camera = d->getCamera();

	      // draw on camera image
	      QPair<int,int> p = CameraModel::warpToImage(camera, QPair<double,double>(d->getCx(), d->getCy()));

	      scaledX = p.first * scaleFactor[camera];
	      scaledY = p.second * scaleFactor[camera];

	      painter.drawArc(camera * eachWidth + scaledX, scaledY, 20, 20, 0, 5760);
	    }
	  
	  
	  pen.setColor(Qt::black);
	  painter.setPen(pen);
	  
	  entities->noise_begin();
	  while(!entities->noise_end())
	    {
	      Noise* n = entities->noise_next();
	      int camera = n->getCamera();

	      // draw on camera image
	      QPair<int,int> p = CameraModel::warpToImage(camera, QPair<double,double>(n->getCx(), n->getCy()));

	      scaledX = p.first * scaleFactor[camera];
	      scaledY = p.second * scaleFactor[camera];

	      painter.drawArc(camera * eachWidth + scaledX, scaledY, 20, 20, 0, 5760);
	    }
	  
	  
	  pen.setColor(Qt::green);
	  painter.setPen(pen);
	  
	  entities->tracks_begin();
	  while(!entities->tracks_end())
	    {
	      Track* t = entities->tracks_next();

	      // draw the track on all cameras (if within camera view)
	      for(int i = 0; i < numCameras; i++)
		{
		  QPair<int,int> c = CameraModel::warpToImage(i, QPair<double,double>(t->getCx(), t->getCy()));
		  if(c.first >= 0 && c.first <= imageWidth[i]
		     && c.second >= 0 && c.second <= imageHeight[i])
		    {
		      scaledX = c.first * scaleFactor[i];
		      scaledY = c.second * scaleFactor[i];

		      if(t->getThisFrame())
			painter.drawArc(i * eachWidth + scaledX, scaledY, 20, 20, 0, 5760);

		    }
		  
		  // draw line to previous track point (on the camera view)
		  QPair<int,int> o = CameraModel::warpToImage(i, QPair<double,double>(t->getOcx(), t->getOcy()));

		  // either the center point or old point need to be in-view
		  if((c.first >= 0 && c.first <= imageWidth[i]
		      && c.second >= 0 && c.second <= imageHeight[i])
		     || (o.first >= 0 && o.first <= imageWidth[i]
			 && o.second >= 0 && o.second <= imageHeight[i]))
		    {
		      // don't draw onto other camera views
		      if(c.first < 0) c.first = 0;
		      if(c.first > imageWidth[i]) c.first = imageWidth[i];
		      if(c.second < 0) c.second = 0;
		      if(c.second > imageHeight[i]) c.second = imageHeight[i];
		      if(o.first < 0) o.first = 0;
		      if(o.first > imageWidth[i]) o.first = imageWidth[i];
		      if(o.second < 0) o.second = 0;
		      if(o.second > imageWidth[i]) o.second = imageWidth[i];

		      // scale the points
		      int scaledCx = c.first * scaleFactor[i];
		      int scaledCy = c.second * scaleFactor[i];
		      int scaledOx = o.first * scaleFactor[i];
		      int scaledOy = o.second * scaleFactor[i];

		      // draw the line
		      painter.drawLine(i * eachWidth + scaledCx, scaledCy,
				       i * eachWidth + scaledOx, scaledOy);
		    }
		      
		}

	      // draw on map
	      QPair<int,int> c = CameraModel::warpToMap(QPair<double,double>(t->getCx(), t->getCy()));
	      if(c.first >= mapTopLeftX && c.first <= mapBottomRightX
		 && c.second >= mapTopLeftY && c.second <= mapBottomRightY)
		{
		  painter.drawArc(c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY, 20, 20, 0, 5760);
		}

	      // draw line to previous track point (on the map)
	      QPair<int,int> o = CameraModel::warpToMap(QPair<double,double>(t->getOcx(), t->getOcy()));
	      
	      // either the center point or old point need to be in-view
	      if((c.first >= mapTopLeftX && c.first <= mapBottomRightX
		  && c.second >= mapTopLeftY && c.second <= mapBottomRightY)
		 || (o.first >= mapTopLeftX && o.first <= mapBottomRightX
		     && o.second >= mapTopLeftY && o.second <= mapBottomRightY))
		{
		  // don't draw onto other camera views
		  if(c.first < mapTopLeftX) c.first = mapTopLeftX;
		  if(c.first > mapBottomRightX) c.first = mapBottomRightX;
		  if(c.second < mapTopLeftY) c.second = mapTopLeftY;
		  if(c.second > mapBottomRightY) c.second = mapBottomRightY;
		  if(o.first < mapTopLeftX) o.first = mapTopLeftX;
		  if(o.first > mapBottomRightX) o.first = mapBottomRightX;
		  if(o.second < mapTopLeftY) o.second = mapTopLeftY;
		  if(o.second > mapBottomRightX) o.second = mapBottomRightX;
		  
		  // draw the line
		  painter.drawLine(c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY,
				   o.first - mapTopLeftX, maxHeight + o.second - mapTopLeftY);
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


