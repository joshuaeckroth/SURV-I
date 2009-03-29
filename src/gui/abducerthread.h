#ifndef ABDUCER_THREAD_H
#define ABDUCER_THREAD_H

#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QProcess>

class TracksReader;
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
  void newTracks(Entities*);

private slots:
  void readyTracks();
  void abducerStarted();
  void abducerError(QProcess::ProcessError);

private:
  int frameNum;
  QWaitCondition detectionsBuffer;
  QMutex mutex;
  QString detections;
  QXmlSimpleReader* reader;
  TracksReader* handler;
  QProcess* abducer;
  bool notParsing;
  QXmlInputSource *abducerSource;
};

#endif

