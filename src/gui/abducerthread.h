#ifndef ABDUCER_THREAD_H
#define ABDUCER_THREAD_H

#include <QThread>
#include <QWaitCondition>
#include <QMutex>

class TracksReader;
class QXmlSimpleReader;
class QXmlInputSource;
class QTcpSocket;
class Entities;

class AbducerThread : public QThread
{
  Q_OBJECT;

public:
  AbducerThread();
  void run();
  void newDetections(QString d);

signals:
  void newTracks(Entities*);

private:
  int frameNum;
  QWaitCondition detectionsBuffer;
  QMutex mutex;
  QString detections;
  QXmlSimpleReader* reader;
  TracksReader* handler;
  QXmlInputSource *responseXml;
  QTcpSocket *socket;
};

#endif

