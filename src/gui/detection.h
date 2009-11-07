#ifndef DETECTION_H
#define DETECTION_H

#include <QString>

class Detection
{
public:
  Detection(int _id, double _lat, double _lon,
            double _startTime, double _endTime,
            double _area);

  int getId() const;
  double getLat() const;
  double getLon() const;
  double getStartTime() const;
  double getEndTime() const;
  double getArea() const;
  void setAccepted(bool);
  bool isAccepted() const;

private:
  int id;
  double lat;
  double lon;
  double startTime;
  double endTime;
  double area;
  bool accepted;
};

#endif
