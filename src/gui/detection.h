#ifndef DETECTION_H
#define DETECTION_H

class Entities;

class Detection
{
public:
  Detection(int i, int c, double a, double x, double y);
  void setEntities(Entities *e);

  int getId() const;
  int getCamera() const;
  double getArea() const;
  double getCx() const;
  double getCy() const;

private:
  int id;
  int camera;
  double area;
  double cx;
  double cy;
  Entities *entities;
};

#endif
