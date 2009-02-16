#ifndef DECODER_H
#define DECODER_H

#include "opencv/cvaux.h"

class Frame;

class Decoder
{
public:
  Decoder(int c, int n);
  QString decodeFrame(Frame *frame);

private:
  QString findBlobs();
  void findBlobsByCCClasters(CvSeq** clasters, int& claster_num, CvSeq** cnt_list);
  void multVectorMatrix(float rv[4], const float v[4], const float m[4][4]);

  int camera;
  int numCameras;
  CvBGStatModel* bg_model;
  CvMat** fmat;
};

#endif
