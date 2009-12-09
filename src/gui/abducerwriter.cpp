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

    int chunkSize = 4000;
    while(true)
    {
        mutex.lock();
        if(detections.isEmpty())
            detectionsBuffer.wait(&mutex);
        outSocket->write("NEW DETECTIONS\n");
        outSocket->write(QString::number(detections.head().length()).toAscii());
        outSocket->write("\n");
        qint64 size = detections.back().length();
        QString output = detections.dequeue();;
        qint64 pos = 0;
        while(pos < size)
        {
            qint64 next = ((size - pos) >= chunkSize ? chunkSize : size - pos);
            qint64 count = outSocket->write(output.mid(pos, next).toAscii());
            pos += count;
        }
        mutex.unlock();

        if(!outSocket->waitForBytesWritten(-1))
        {
            qDebug() << "Error writing bytes.";
            return;
        }
    }
}

void AbducerWriter::sendDetections(QString d)
{
    mutex.lock();
    detections.enqueue(d);
    //detections.enqueue(QString("<?xml version=\"1.0\" ?>\n"
    //                           "<CameraDetections>\n%1</CameraDetections>\n").arg(d));
    detectionsBuffer.wakeAll();
    mutex.unlock();
}

