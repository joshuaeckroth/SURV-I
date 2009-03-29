#ifndef CAMERA_MODEL_H
#define CAMERA_MODEL_H

#include <QPoint>

class CameraModel
{
public:
  static void setNumCameras(int n);
  static QPair<double,double> warpToGround(int camera, QPair<int,int> p);
  static QPair<int,int> warpToImage(int camera, QPair<double,double> p);
  static QPair<int,int> warpToMap(QPair<double,double> p);

protected:
  CameraModel();

private:
  static CameraModel* pinstance;
  static CameraModel* instance();
  void updateModel();
  int numCameras;
  CvMat** warp;
  CvMat* mapWarp;
};

#endif
