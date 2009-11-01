#ifndef DECODER_H
#define DECODER_H

#include "cvaux.h"

class Frame;

class Decoder
{
public:
  Decoder();
  QString decodeFrame(Frame *frame);

private:
  QString findBlobs(double, double, int);
  void findBlobsByCCClasters(CvSeq** clasters, int& claster_num, CvSeq** cnt_list);
  CvBGStatModel* bg_model;
};

#endif
