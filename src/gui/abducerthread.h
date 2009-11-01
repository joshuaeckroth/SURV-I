#ifndef ABDUCER_THREAD_H
#define ABDUCER_THREAD_H

#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QQueue>
#include <QTcpSocket>

class ResultsReader;
class QXmlSimpleReader;
class QXmlInputSource;
class Entities;

class AbducerThread : public QThread
{
  Q_OBJECT;

public:
  AbducerThread();
  void run();
  void newDetections(QString d);

signals:
  void newEntities(Entities*);

private:
  int frameNum;
  QWaitCondition detectionsBuffer;
  QMutex mutex;
  QQueue<QString> detections;
  QXmlSimpleReader* reader;
  ResultsReader* handler;
  QXmlInputSource *xmlInput;
  QTcpSocket *socket;
};

#endif

