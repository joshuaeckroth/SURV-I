#ifndef ENTITIES_H
#define ENTITIES_H

#include <vector>
#include <QString>

class Detection;
class Movement;
class Track;

class Entities
{
public:
  Entities();

  void addDetection(Detection *d);
  void addMovement(Movement *n);
  void addTrack(Track *t);

  void detections_begin();
  bool detections_end() const;
  Detection *detections_next();

  void movements_begin();
  bool movements_end() const;
  Movement *movements_next();

  void tracks_begin();
  bool tracks_end() const;
  Track *tracks_next();

  void appendLog(const QString l);
  QString getLog() const;

private:
  std::vector<Detection*> detections;
  std::vector<Detection*>::const_iterator detections_iter;

  std::vector<Movement*> movements;
  std::vector<Movement*>::const_iterator movements_iter;

  std::vector<Track*> tracks;
  std::vector<Track*>::const_iterator tracks_iter;

  QString log;
};

#endif
