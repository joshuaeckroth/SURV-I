
#include <QFile>
#include <QDebug>
#include <QPair>

#include "highgui.h"

#include "cameramodel.h"

CameraModel* CameraModel::pinstance = NULL;

CameraModel* CameraModel::instance()
{
  if(pinstance == NULL)
    pinstance = new CameraModel;

  return pinstance;
}

void CameraModel::setNumCameras(int n)
{
  instance()->numCameras = n;
  instance()->updateModel();
}

CameraModel::CameraModel()
{
  numCameras = 0;
}

void CameraModel::updateModel()
{
  warp = new CvMat*[numCameras];

  for(int i = 0; i < numCameras; i++)
    {
      QFile warp_file(QString("camera-") + QString::number(i) + "-warp.xml");
      warp[i] = (CvMat*)cvLoad(warp_file.fileName().toAscii().constData());
      if(warp == NULL)
	{
	  qDebug() << "Error loading " << warp_file.fileName();
	}
    }

  QFile warp_file(QString("map-warp.xml"));
  mapWarp = (CvMat*)cvLoad(warp_file.fileName().toAscii().constData());
  if(mapWarp == NULL)
    {
      qDebug() << "Error loading map warp file map-warp.xml";
    }
}

QPair<double,double> CameraModel::warpToGround(int camera, QPair<int,int> p)
{
  CvMat* warp = instance()->warp[camera];

  CvMat *pmat = cvCreateMat(3, 1, CV_32FC1);
  CvMat *rmat = cvCreateMat(3, 1, CV_32FC1);
  
  cvSetReal1D(pmat, 0, p.first);
  cvSetReal1D(pmat, 1, p.second);
  cvSetReal1D(pmat, 2, 1);

  cvMatMul(warp, pmat, rmat);

  double z = cvGetReal1D(rmat, 2);
  return QPair<double,double>(cvGetReal1D(rmat, 0) / z, cvGetReal1D(rmat, 1) / z);
}

QPair<int,int> CameraModel::warpToImage(int camera, QPair<double,double> p)
{
  CvMat* warp = instance()->warp[camera];
  CvMat* unwarp = cvCreateMat(3, 3, CV_32FC1);
  cvInvert(warp, unwarp);

  CvMat *pmat = cvCreateMat(3, 1, CV_32FC1);
  CvMat *rmat = cvCreateMat(3, 1, CV_32FC1);

  cvSetReal1D(pmat, 0, p.first);
  cvSetReal1D(pmat, 1, p.second);
  cvSetReal1D(pmat, 2, 1);

  cvMatMul(unwarp, pmat, rmat);

  double z = cvGetReal1D(rmat, 2);
  return QPair<int,int>((int)(cvGetReal1D(rmat, 0) / z), (int)(cvGetReal1D(rmat, 1) / z));
}

QPair<int,int> CameraModel::warpToMap(QPair<double,double> p)
{
  CvMat* mapWarp = instance()->mapWarp;
  CvMat* unwarp = cvCreateMat(3, 3, CV_32FC1);
  cvInvert(mapWarp, unwarp);

  CvMat *pmat = cvCreateMat(3, 1, CV_32FC1);
  CvMat *rmat = cvCreateMat(3, 1, CV_32FC1);

  cvSetReal1D(pmat, 0, p.first);
  cvSetReal1D(pmat, 1, p.second);
  cvSetReal1D(pmat, 2, 1);

  cvMatMul(unwarp, pmat, rmat);

  double z = cvGetReal1D(rmat, 2);
  return QPair<int,int>((int)(cvGetReal1D(rmat, 0) / z), (int)(cvGetReal1D(rmat, 1) / z));
}
