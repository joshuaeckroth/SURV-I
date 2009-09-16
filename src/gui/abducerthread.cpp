
#include <QThread>
#include <QProcess>
#include <QXmlSimpleReader>
#include <QDebug>
#include <QTcpSocket>

#include "abducerthread.h"
#include "tracksreader.h"

AbducerThread::AbducerThread()
        : QThread()
{
    reader = new QXmlSimpleReader;
    handler = new TracksReader();
    reader->setContentHandler(handler);
    reader->setErrorHandler(handler);
}

void AbducerThread::run()
{
    socket = new QTcpSocket;
    responseXml = new QXmlInputSource(socket);

    socket->connectToHost("localhost", 10000);
    if(!socket->waitForConnected(5000))
    {
        qDebug() << "Error connecting.";
        return;
    }

    while(true)
    {
        mutex.lock();
        detectionsBuffer.wait(&mutex);
        socket->write(detections.toAscii());
        mutex.unlock();

        if(!socket->waitForReadyRead(10000))
        {
            qDebug() << "Timed out waiting for response from abducer.";
            return;
        }

        mutex.lock();
        reader->parse(responseXml);
        mutex.unlock();

        emit newTracks(handler->getEntities());
    }
}

void AbducerThread::newDetections(QString d)
{
    mutex.lock();
    detections = d;
    detectionsBuffer.wakeAll();
    mutex.unlock();
}
