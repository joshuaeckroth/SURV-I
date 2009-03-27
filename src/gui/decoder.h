#ifndef DECODER_H
#define DECODER_H

#include "opencv/cvaux.h"

class Frame;

class Decoder
{
public:
  Decoder(int c);
  QString decodeFrame(Frame *frame);

private:
  QString findBlobs();
  void findBlobsByCCClasters(CvSeq** clasters, int& claster_num, CvSeq** cnt_list);

  int camera;
  CvBGStatModel* bg_model;
};

#endif
