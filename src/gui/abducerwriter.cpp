#include <QThread>
#include <QProcess>
#include <QDebug>
#include <QTcpSocket>
#include <QHostAddress>

#include "abducerwriter.h"

AbducerWriter::AbducerWriter()
        : QThread()
{
    outSocket = NULL;
}

void AbducerWriter::run()
{
    outSocket = new QTcpSocket;

    outSocket->connectToHost(QHostAddress::LocalHost, 10001);

    if(!outSocket->waitForConnected(5000))
    {
        qDebug() << "Error connecting to outSocket: " << outSocket->errorString();
        return;
    }

    while(true)
    {
        mutex.lock();
        if(detections.isEmpty())
            detectionsBuffer.wait(&mutex);
        outSocket->write("NEW DETECTIONS\n");
        outSocket->write(QString::number(detections.head().length()).toAscii());
        outSocket->write("\n");
        outSocket->write(detections.dequeue().toAscii());
        mutex.unlock();

        if(!outSocket->waitForBytesWritten(-1))
        {
            qDebug() << "Error writing bytes.";
            return;
        }
    }
}

void AbducerWriter::newDetections(QString d)
{
    mutex.lock();
    detections.enqueue(QString("<?xml version=\"1.0\" ?>\n"
                               "<CameraDetections>\n%1</CameraDetections>\n").arg(d));
    detectionsBuffer.wakeAll();
    mutex.unlock();
}

