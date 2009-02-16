#ifndef TRACK_H
#define TRACK_H

class Frame;

class Track
{
public:
  Track(int i, double x, double y, double ox, double oy,
	int p, int n, double ex, double ey, double r);
  void setFrame(Frame *f);

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
  Frame *frame;
};

#endif