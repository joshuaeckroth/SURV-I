#ifndef TRACK_H
#define TRACK_H

class Entities;

class Track
{
public:
  Track(int i, double x, double y, double ox, double oy,
	int p, int n, double ex, double ey, double r, bool thisF);
  void setEntities(Entities *e);
  int getId() const;
  double getCx() const;
  double getCy() const;
  double getOcx() const;
  double getOcy() const;
  int getPrevId() const;
  int getNextId() const;
  double getEcx() const;
  double getEcy() const;
  double getRadius() const;
  bool getThisFrame() const;

private:
  int id;
  double cx;
  double cy;
  double ocx;
  double ocy;
  int prevId;
  int nextId;
  double ecx;
  double ecy;
  double radius;
  bool thisFrame;
  Entities *entities;
};

#endif
