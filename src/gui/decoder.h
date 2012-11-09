#ifndef DECODER_H
#define DECODER_H

#include <opencv2/opencv.hpp>

class Frame;

class Decoder
{
public:
  Decoder();
  QString decodeFrame(Frame *frame);

private:
  QString findBlobs(Frame *frame, bool drawContours);
  void findBlobsByCCClasters(CvSeq** clasters, int& claster_num, CvSeq** cnt_list);
  cv::BackgroundSubtractorMOG* bg_model;
  IplImage *fgmask;
};

#endif
