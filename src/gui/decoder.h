#ifndef DECODER_H
#define DECODER_H

#include "opencv/cvaux.h"

class Decoder
{
public:
  Decoder(int c);
  QString processFrame(IplImage* f);

private:
  QString findBlobs();
  void findBlobsByCCClasters(CvSeq** clasters, int& claster_num, CvSeq** cnt_list);
  void multVectorMatrix(float rv[4], const float v[4], const float m[4][4]);

  int camera;
  CvBGStatModel* bg_model;
  IplImage* frame;
  CvMat* warp;
};

#endif
