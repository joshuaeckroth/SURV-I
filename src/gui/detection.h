#ifndef DETECTION_H
#define DETECTION_H

#include <QString>

class Entities;

class Detection
{
public:
  Detection(QString _id, double _lat, double _lon, int _startTime, int _endTime);
  void setEntities(Entities *e);

  QString getId() const;
  double getLat() const;
  double getLon() const;
  int getStartTime() const;
  int getEndTime() const;

private:
  QString id;
  double lat;
  double lon;
  int startTime;
  int endTime;
  Entities *entities;
};

#endif
