#ifndef ABDUCERREADER_H
#define ABDUCERREADER_H

#include <QThread>
#include <QTcpSocket>

class ResultsReader;
class QXmlSimpleReader;
class QXmlInputSource;
class Entities;

class AbducerReader : public QThread
{
  Q_OBJECT;

public:
  AbducerReader();
  void run();

signals:
  void newEntities(Entities*);

private:
  int frameNum;
  QXmlSimpleReader* reader;
  ResultsReader* handler;
  QXmlInputSource *xmlInput;
  QTcpSocket *inSocket;
};

#endif // ABDUCERREADER_H
