#ifndef ENTITIES_H
#define ENTITIES_H

#include <map>
#include <QString>

class Detection;
class Movement;
class Path;

class Entities
{
public:
  Entities(std::map<int,Detection*>,
           std::map<int,Movement*>,
           std::map<int,Path*>);

  void detections_begin();
  bool detections_end() const;
  Detection *detections_next();

  void movements_begin();
  bool movements_end() const;
  Movement *movements_next();

  void paths_begin();
  bool paths_end() const;
  Path *paths_next();

private:
  std::map<int,Detection*> detections;
  std::map<int,Detection*>::const_iterator detections_iter;

  std::map<int,Movement*> movements;
  std::map<int,Movement*>::const_iterator movements_iter;

  std::map<int,Path*> paths;
  std::map<int,Path*>::const_iterator paths_iter;
};

#endif
