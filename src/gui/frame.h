#ifndef FRAME_H
#define FRAME_H

#include <vector>

#include "opencv/highgui.h"

class Detection;
class Noise;
class Track;

class Frame
{
public:
  Frame(int n, double t);
  void addDetection(Detection *d);
  void addNoise(Noise *n);
  void addTrack(Track *t);
  void setImage(IplImage* img);

  int getNumber() const;
  double getTime() const;
  IplImage *getImage() const;

  void detections_begin();
  bool detections_end() const;
  Detection *detections_next();

  void noise_begin();
  bool noise_end() const;
  Noise *noise_next();

  void tracks_begin();
  bool tracks_end() const;
  Track *tracks_next();

private:
  int number;
  double time;
  IplImage *image;
  std::vector<Detection*> detections;
  std::vector<Detection*>::const_iterator detections_iter;

  std::vector<Noise*> noise;
  std::vector<Noise*>::const_iterator noise_iter;

  std::vector<Track*> tracks;
  std::vector<Track*>::const_iterator tracks_iter;
};

#endif
