#ifndef ENTITIES_H
#define ENTITIES_H

#include <vector>
#include <QString>

class Detection;
class Movement;
class Path;

class Entities
{
public:
  Entities();

  void addDetection(Detection *d);
  void addMovement(Movement *n);
  void addPath(Path *p);

  void detections_begin();
  bool detections_end() const;
  Detection *detections_next();

  void movements_begin();
  bool movements_end() const;
  Movement *movements_next();

  void paths_begin();
  bool paths_end() const;
  Path *paths_next();

  void appendLog(const QString l);
  QString getLog() const;

private:
  std::vector<Detection*> detections;
  std::vector<Detection*>::const_iterator detections_iter;

  std::vector<Movement*> movements;
  std::vector<Movement*>::const_iterator movements_iter;

  std::vector<Path*> paths;
  std::vector<Path*>::const_iterator paths_iter;

  QString log;
};

#endif
