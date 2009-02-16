
#include <QThread>
#include <QProcess>
#include <QXmlSimpleReader>
#include <QDebug>

#include "abducerthread.h"
#include "tracksreader.h"
#include "frames.h"

AbducerThread::AbducerThread(Frames *frames)
  : QThread()
{
  reader = new QXmlSimpleReader;
  handler = new TracksReader(frames);
  reader->setContentHandler(handler);
  reader->setErrorHandler(handler);

  QObject* parent = new QObject;
  abducer = new QProcess(parent);

  connect(abducer, SIGNAL(readyReadStandardOutput()), this, SLOT(readyTracks()));
  connect(abducer, SIGNAL(started()), this, SLOT(abducerStarted()));
  connect(abducer, SIGNAL(error(QProcess::ProcessError)), this, SLOT(abducerError(QProcess::ProcessError)));

  abducer->start("build/abducer");

  if(!abducer->waitForStarted())
    {
      qDebug() << "abducer did not start";
    }

  abducerSource = new QXmlInputSource(abducer);
  notParsing = true;
}

void AbducerThread::run()
{
  while(true)
    {
      mutex.lock();
      detectionsBuffer.wait(&mutex);

      abducer->write(detections.toAscii());
      abducer->write("\n");
      detections = QString();

      mutex.unlock();
    }
}

void AbducerThread::newDetections(QString d)
{
  mutex.lock();
  detections += d;
  detectionsBuffer.wakeAll();
  mutex.unlock();
}

void AbducerThread::readyTracks()
{
  if(notParsing)
    {
      reader->parse(abducerSource, true);
      notParsing = false;
    }
  else
    reader->parseContinue();

  QString tracks = "";
  emit newTracks(tracks);
}

void AbducerThread::abducerStarted()
{
  qDebug() << "Abducer started.";
}

void AbducerThread::abducerError(QProcess::ProcessError e)
{
  qDebug() << "Abducer error: " << e;
}
