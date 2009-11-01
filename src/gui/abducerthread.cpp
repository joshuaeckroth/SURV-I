
#include <QThread>
#include <QProcess>
#include <QXmlSimpleReader>
#include <QDebug>
#include <QTcpSocket>
#include <QHostAddress>

#include "abducerthread.h"
#include "resultsreader.h"

AbducerThread::AbducerThread()
        : QThread()
{
    reader = new QXmlSimpleReader;
    handler = new ResultsReader();
    reader->setContentHandler(handler);
    reader->setErrorHandler(handler);
    xmlInput = new QXmlInputSource;
    socket = NULL;
}

void AbducerThread::run()
{
    socket = new QTcpSocket;

    socket->connectToHost(QHostAddress::LocalHost, 10000);

    if(!socket->waitForConnected(5000))
    {
        qDebug() << "Error connecting: " << socket->errorString();
        return;
    }

    while(true)
    {
        mutex.lock();
        if(detections.isEmpty())
            detectionsBuffer.wait(&mutex);
        socket->write("NEW DETECTIONS\n");
        socket->write(QString::number(detections.head().length()).toAscii());
        socket->write("\n");
        socket->write(detections.dequeue().toAscii());
        mutex.unlock();

        if(!socket->waitForBytesWritten(-1))
        {
            qDebug() << "Error writing bytes.";
            return;
        }

        if(!socket->waitForReadyRead(10000))
        {
            qDebug() << "Timed out waiting for response from abducer.";
            return;
        }

        QByteArray response = socket->read(12);
        if(QString(response) == "NEW RESULTS\n")
        {
            char sizeString[100];
            socket->readLine(sizeString, 100);
            qint64 size = QString(sizeString).toInt();
            QString partialXml;
            qint64 pos = 0;
            while(pos < size)
            {
                qint64 next = ((size - pos) >= 8192 ? 8192 : size - pos);
                partialXml = socket->read(next);
                pos += next;
                qDebug() << QString("size: %1, pos: %2\n").arg(size).arg(pos);
                xmlInput->setData(partialXml);
                if(size <= 8192)
                {
                    // only iteration; parse all
                    reader->parse(xmlInput, false);
                }
                else if(size > 8192 && pos == 8192)
                {
                    // first iteration; start parsing
                    reader->parse(xmlInput, true);
                }
                else
                    reader->parseContinue();
            }
            emit newEntities(handler->getEntities());
        }
    }
}

void AbducerThread::newDetections(QString d)
{
    mutex.lock();
    detections.enqueue(QString("<?xml version=\"1.0\" ?>\n"
                               "<CameraDetections>\n%1</CameraDetections>\n").arg(d));
    detectionsBuffer.wakeAll();
    mutex.unlock();
}
