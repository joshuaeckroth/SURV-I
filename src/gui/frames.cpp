
#include <vector>

#include <QObject>

#include "frames.h"
#include "frame.h"

void Frames::addFrame(Frame *f)
{
  mutex.lock();
  frames.push_back(f);
  mutex.unlock();

  emit newFrame(f);
}

/* find a frame that seems to match withing +/- 0.05 seconds */
/* from a given camera; if not found, return NULL            */
Frame *Frames::findFrame(int camera, double time) const
{
  for(std::vector<Frame*>::const_iterator it = frames.begin();
      it != frames.end();
      it++)
    {
      if(camera == (*it)->getCamera()
	 && time >= ((*it)->getTime() - 0.05)
	 && time <= ((*it)->getTime() + 0.05))
	{
	  return *it;
	}
    }
  return NULL;
}
