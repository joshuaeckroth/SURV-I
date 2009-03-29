
#include <QDebug>

#include <vector>

#include "entities.h"
#include "detection.h"
#include "noise.h"
#include "track.h"

Entities::Entities(int n, double t)
  : number(n), time(t)
{ }

void Entities::addDetection(Detection *d)
{
  d->setEntities(this);
  detections.push_back(d);
}

void Entities::addNoise(Noise *n)
{
  n->setEntities(this);
  noise.push_back(n);
}

void Entities::addTrack(Track *t)
{
  t->setEntities(this);
  tracks.push_back(t);
}

int Entities::getNumber() const
{
  return number;
}

double Entities::getTime() const
{
  return time;
}

void Entities::detections_begin()
{
  detections_iter = detections.begin();
}

bool Entities::detections_end() const
{
  return detections_iter == detections.end();
}

Detection *Entities::detections_next()
{
  return *(detections_iter++);
}

void Entities::noise_begin()
{
  noise_iter = noise.begin();
}

bool Entities::noise_end() const
{
  return noise_iter == noise.end();
}

Noise *Entities::noise_next()
{
  return *(noise_iter++);
}

void Entities::tracks_begin()
{
  tracks_iter = tracks.begin();
}

bool Entities::tracks_end() const
{
  return tracks_iter == tracks.end();
}

Track *Entities::tracks_next()
{
  return *(tracks_iter++);
}
