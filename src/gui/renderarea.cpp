
#include <QPainter>
#include <QTime>
#include <QDebug>
#include <QPen>
#include <QMap>
#include <QPair>
#include <QImage>
#include <QGLWidget>

#include <cmath>

#include "renderarea.h"
#include "entities.h"
#include "detection.h"
#include "noise.h"
#include "track.h"
#include "frame.h"
#include "cameramodel.h"

RenderArea::RenderArea(QWidget* parent)
  : QGLWidget(QGLFormat(QGL::SampleBuffers), parent), clear(true), entities(NULL)
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

  map = QImage("videos/ARL1-with-lines.jpg");
  if(map.isNull())
    qDebug() << "Unable to load map image.";

  detectionPen.setColor(Qt::blue);
  detectionPen.setWidth(1);
  
  detectionCenterPen.setColor(Qt::blue);
  detectionCenterPen.setWidth(3);
  
  detectionTextPen.setColor(Qt::blue);
  
  noisePen.setColor(Qt::black);
  noisePen.setWidth(1);
  
  trackHeadPen.setColor(Qt::black);
  trackHeadPen.setWidth(3);
  
  trackTextPen.setColor(Qt::black);
  
  trackPathPen.setColor(Qt::black);
  trackPathPen.setWidth(3);
  
  trackExpectedPathPen.setColor(Qt::red);
  trackExpectedPathPen.setWidth(1);
  trackExpectedPathPen.setStyle(Qt::DotLine);
  
  trackExpectedCirclePen.setColor(Qt::red);
  trackExpectedCirclePen.setWidth(1);
  trackExpectedCirclePen.setStyle(Qt::DotLine);
  
  trackMapPen.setColor(Qt::black);
  trackMapPen.setWidth(3);
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

void RenderArea::showFrames(QMap<int, QMap<int, QPair<Frame*, QString> > > detections, int number, Entities* e)
{
  entities = e;

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
  QPainter painter;
  painter.begin(this);
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
	  cameraRegion[i] = QRegion(i * eachWidth, 0, sImg.width(), sImg.height());

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
      mapRegion = QRegion(0, maxHeight, cropWidth, cropHeight);

      if(entities != NULL)
	{
	  int scaledX, scaledY;
	  int radius;


	  entities->detections_begin();
	  while(!entities->detections_end())
	    {
	      Detection* d = entities->detections_next();
	      int camera = d->getCamera();
	      painter.setClipRegion(cameraRegion[camera]);

	      // draw on camera image
	      QPair<int,int> p = CameraModel::warpToImage(camera, QPair<double,double>(d->getCx(), d->getCy()));

	      scaledX = p.first * scaleFactor[camera];
	      scaledY = p.second * scaleFactor[camera];

	      // draw pixel at center
	      painter.setPen(detectionCenterPen);

	      painter.drawLine(camera * eachWidth + scaledX, scaledY,
			       camera * eachWidth + scaledX, scaledY);

	      painter.setPen(detectionPen);
	  
	      // draw circle whose size is determined by the detection's area
	      radius = std::sqrt(d->getArea()) * scaleFactor[camera];
	      painter.drawEllipse(QPoint(camera * eachWidth + scaledX, scaledY), radius, radius);

	      // draw id
	      //painter.drawText(camera * eachWidth + scaledX + 7, scaledY - 7, QString::number(d->getId()));
	    }

	  
	  painter.setPen(noisePen);
	  
	  entities->noise_begin();
	  while(!entities->noise_end())
	    {
	      Noise* n = entities->noise_next();
	      int camera = n->getCamera();
	      painter.setClipRegion(cameraRegion[camera]);

	      // draw on camera image
	      QPair<int,int> p = CameraModel::warpToImage(camera, QPair<double,double>(n->getCx(), n->getCy()));

	      scaledX = p.first * scaleFactor[camera];
	      scaledY = p.second * scaleFactor[camera];

	      radius = std::sqrt(n->getArea()) * scaleFactor[camera];

	      painter.drawEllipse(QPoint(camera * eachWidth + scaledX, scaledY), radius, radius);
	    }
	  
	  
	  entities->tracks_begin();
	  while(!entities->tracks_end())
	    {
	      Track* t = entities->tracks_next();

	      if(t->getCy() > 1100) continue;

	      if(t->getPrevId() != 0 || t->getNextId() != 0) // skip tracks that have only one point
		{
		  // draw the track on all cameras (if within camera view)
		  for(int i = 0; i < numCameras; i++)
		    {
		      painter.setClipRegion(cameraRegion[i]);

		      QPair<int,int> c = CameraModel::warpToImage(i, QPair<double,double>(t->getCx(), t->getCy()));
		      if(c.first >= 0 && c.first <= imageWidth[i]
			 && c.second >= 0 && c.second <= imageHeight[i])
			{
			  scaledX = c.first * scaleFactor[i];
			  scaledY = c.second * scaleFactor[i];
			  
			  if(t->getThisFrame())
			    {
			      // draw an X
			      painter.setPen(trackHeadPen);
			      
			      painter.drawLine((i * eachWidth + scaledX) - 5, scaledY - 5,
					       (i * eachWidth + scaledX) + 5, scaledY + 5);
			      painter.drawLine((i * eachWidth + scaledX) - 5, scaledY + 5,
					       (i * eachWidth + scaledX) + 5, scaledY - 5);
			    }
			}
		      
		      // draw line to previous track point (on the camera view)
		      QPair<int,int> o = CameraModel::warpToImage(i, QPair<double,double>(t->getOcx(), t->getOcy()));
		      
		      // and expected point
		      QPair<int,int> e = CameraModel::warpToImage(i, QPair<double,double>(t->getEcx(), t->getEcy()));
		      
		      // either the center point or old point need to be in-view
		      if((c.first >= 0 && c.first <= imageWidth[i]
			  && c.second >= 0 && c.second <= imageHeight[i])
			 || (o.first >= 0 && o.first <= imageWidth[i]
			     && o.second >= 0 && o.second <= imageHeight[i]))
			{
			  // scale the points
			  int scaledCx = c.first * scaleFactor[i];
			  int scaledCy = c.second * scaleFactor[i];
			  int scaledOx = o.first * scaleFactor[i];
			  int scaledOy = o.second * scaleFactor[i];
			  int scaledEx = e.first * scaleFactor[i];
			  int scaledEy = e.second * scaleFactor[i];
			  
			  // draw the line
			  painter.setPen(trackPathPen);
			  
			  painter.drawLine(i * eachWidth + scaledCx, scaledCy,
					   i * eachWidth + scaledOx, scaledOy);
			  
			  if(t->getThisFrame())
			    {
			      // draw expected line
			      painter.setPen(trackExpectedPathPen);
			      
			      painter.drawLine(i * eachWidth + scaledCx, scaledCy,
					       i * eachWidth + scaledEx, scaledEy);
			      
			      // draw expected circle
			      painter.setPen(trackExpectedCirclePen);
			      
			      //painter.drawEllipse(QPoint(i * eachWidth + scaledEx, scaledEy),
			      //		  static_cast<int>(t->getRadius() * scaleFactor[i]),
			      //		  static_cast<int>(t->getRadius() * scaleFactor[i]));
			    }
			  
			  // draw track id
			  if(t->getThisFrame())
			    {
			      painter.setPen(trackTextPen);
			      
			      painter.drawText(i * eachWidth + scaledCx + 3, scaledCy - 3, QString::number(t->getId()));
			    }
			}
		      
		    }
		  
		  // draw on map
		  painter.setPen(trackMapPen);
		  painter.setClipRegion(mapRegion);
		  
		  QPair<int,int> c = CameraModel::warpToMap(QPair<double,double>(t->getCx(), t->getCy()));
		  if(c.first >= mapTopLeftX && c.first <= mapBottomRightX
		     && c.second >= mapTopLeftY && c.second <= mapBottomRightY)
		    {
		      // draw single pixel "line"
		      painter.drawLine(c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY,
				       c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY);
		    }
		  
		  // draw line to previous track point (on the map)
		  QPair<int,int> o = CameraModel::warpToMap(QPair<double,double>(t->getOcx(), t->getOcy()));
		  
		  // either the center point or old point need to be in-view
		  if((c.first >= mapTopLeftX && c.first <= mapBottomRightX
		      && c.second >= mapTopLeftY && c.second <= mapBottomRightY)
		     || (o.first >= mapTopLeftX && o.first <= mapBottomRightX
			 && o.second >= mapTopLeftY && o.second <= mapBottomRightY))
		    {
		      // draw the line
		      painter.drawLine(c.first - mapTopLeftX, maxHeight + c.second - mapTopLeftY,
				       o.first - mapTopLeftX, maxHeight + o.second - mapTopLeftY);
		      
		      // draw track id
		      if(t->getThisFrame())
			{
			  painter.setPen(trackTextPen);
			  
			  painter.drawText(o.first - mapTopLeftX + 7, maxHeight + o.second - mapTopLeftY + 8, QString::number(t->getId()));
			}
		    }
		}
	    }
	}
    }
  else
    {
      painter.setBrush(Qt::black);
      painter.drawRect(rect());
    }
  painter.end();
}


