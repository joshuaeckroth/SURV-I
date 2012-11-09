#ifndef ABDUCERWRITER_H
#define ABDUCERWRITER_H

#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QQueue>
#include <QTcpSocket>

class AbducerWriter : public QThread
{
  Q_OBJECT

public:
  AbducerWriter();
  void run();

public slots:
  void sendDetections(QString d);

private:
  QWaitCondition detectionsBuffer;
  QMutex mutex;
  QQueue<QString> detections;
  QTcpSocket *outSocket;
};

#endif // ABDUCERWRITER_H
