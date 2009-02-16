
#include <vector>

#include "opencv/highgui.h"

#include "frame.h"
#include "detection.h"
#include "noise.h"
#include "track.h"

Frame::Frame(int c, int n, double t)
  : camera(c), number(n), time(t)
{ }

void Frame::addDetection(Detection *d)
{
  d->setFrame(this);
  detections.push_back(d);
}

void Frame::addNoise(Noise *n)
{
  n->setFrame(this);
  noise.push_back(n);
}

void Frame::addTrack(Track *t)
{
  t->setFrame(this);
  tracks.push_back(t);
}

void Frame::setImage(IplImage* img)
{
  image = img;
}

int Frame::getCamera() const
{
  return camera;
}

int Frame::getNumber() const
{
  return number;
}

double Frame::getTime() const
{
  return time;
}

IplImage *Frame::getImage() const
{
  return image;
}

void Frame::detections_begin()
{
  detections_iter = detections.begin();
}

bool Frame::detections_end() const
{
  return detections_iter == detections.end();
}

Detection *Frame::detections_next()
{
  return *(detections_iter++);
}

void Frame::noise_begin()
{
  noise_iter = noise.begin();
}

bool Frame::noise_end() const
{
  return noise_iter == noise.end();
}

Noise *Frame::noise_next()
{
  return *(noise_iter++);
}

void Frame::tracks_begin()
{
  tracks_iter = tracks.begin();
}

bool Frame::tracks_end() const
{
  return tracks_iter == tracks.end();
}

Track *Frame::tracks_next()
{
  return *(tracks_iter++);
}
