
#include <QThread>
#include <QProcess>
#include <QXmlSimpleReader>
#include <QDebug>

#include "abducerthread.h"
#include "tracksreader.h"

AbducerThread::AbducerThread()
  : QThread()
{
  reader = new QXmlSimpleReader;
  handler = new TracksReader();
  reader->setContentHandler(handler);
  reader->setErrorHandler(handler);

  QObject* parent = new QObject;
  abducer = new QProcess(parent);

  //connect(abducer, SIGNAL(readyReadStandardOutput()), this, SLOT(readyTracks()));
  connect(abducer, SIGNAL(started()), this, SLOT(abducerStarted()));
  connect(abducer, SIGNAL(error(QProcess::ProcessError)), this, SLOT(abducerError(QProcess::ProcessError)));

  //abducer->start("build/abducer");

  if(!abducer->waitForStarted())
    {
      qDebug() << "abducer did not start";
    }

  abducerSource = new QXmlInputSource(abducer);
  notParsing = true;

  frameNum = 1;
}

void AbducerThread::run()
{
  while(true)
    {
      mutex.lock();
      detectionsBuffer.wait(&mutex);

      abducer->write(detections.toAscii());
      abducer->write("\n");

      mutex.unlock();
    }
}

void AbducerThread::newDetections(QString d)
{
  mutex.lock();
  detections = d;
  detectionsBuffer.wakeAll();
  mutex.unlock();

  readyTracks();
}

void AbducerThread::readyTracks()
{
  QFile tracks(QString("tracks/frame-") + QString::number(frameNum) + QString(".xml"));
  if(tracks.exists())
    {
      QXmlInputSource* source = new QXmlInputSource(&tracks);
      reader->parse(source);
    }

  frameNum++;

  /*
  if(notParsing)
    {
      reader->parse(abducerSource, true);
      notParsing = false;
    }
  else
    reader->parseContinue();
  */

  emit newTracks(handler->getFrame());
}

void AbducerThread::abducerStarted()
{
  qDebug() << "Abducer started.";
}

void AbducerThread::abducerError(QProcess::ProcessError e)
{
  qDebug() << "Abducer error: " << e;
}
