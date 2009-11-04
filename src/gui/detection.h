#ifndef DETECTION_H
#define DETECTION_H

#include <QString>

class Entities;

class Detection
{
public:
  Detection(QString _id, double _lat, double _lon, double _startTime, double _endTime, double _area);
  void setEntities(Entities *e);

  QString getId() const;
  double getLat() const;
  double getLon() const;
  double getStartTime() const;
  double getEndTime() const;
  double getArea() const;

private:
  QString id;
  double lat;
  double lon;
  double startTime;
  double endTime;
  double area;
  Entities *entities;
};

#endif
