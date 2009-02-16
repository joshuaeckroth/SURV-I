#ifndef FRAMES_H
#define FRAMES_H

#include <vector>

#include <QObject>
#include <QMutex>

class Frame;

class Frames : public QObject
{
  Q_OBJECT;

public:
  void addFrame(Frame *f);
  Frame *findFrame(int camera, double time) const;

signals:
  void newFrame(const Frame *f);

private:
  QMutex mutex;
  std::vector<Frame*> frames;
};

#endif
