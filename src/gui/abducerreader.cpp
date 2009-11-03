#include <QThread>
#include <QProcess>
#include <QXmlSimpleReader>
#include <QDebug>
#include <QTcpSocket>
#include <QHostAddress>

#include "abducerreader.h"
#include "resultsreader.h"

AbducerReader::AbducerReader()
        : QThread()
{
    reader = new QXmlSimpleReader;
    handler = new ResultsReader();
    reader->setContentHandler(handler);
    reader->setErrorHandler(handler);
    xmlInput = new QXmlInputSource;
    inSocket = NULL;
}

void AbducerReader::run()
{
    inSocket = new QTcpSocket;

    inSocket->connectToHost(QHostAddress::LocalHost, 10000);

    if(!inSocket->waitForConnected(5000))
    {
        qDebug() << "Error connecting to inSocket: " << inSocket->errorString();
        return;
    }

    int chunkSize = 4000;
    while(true)
    {
        if(!inSocket->waitForReadyRead(60000))
        {
            qDebug() << "Timed out waiting for response from abducer.";
            return;
        }

        QByteArray response = inSocket->read(12);
        qDebug() << QString("First 12 bytes: %1").arg(QString(response));
        if(QString(response) == "NEW RESULTS\n")
        {
            qDebug() << "New results.";
            char sizeString[100];
            char partialXml[chunkSize + 1];
            inSocket->readLine(sizeString, 100);
            qint64 size = QString(sizeString).toInt();
            qint64 pos = 0;
            while(pos < size)
            {
                qint64 next = ((size - pos) >= chunkSize ? chunkSize : size - pos);
                qint64 count = inSocket->read(partialXml, next);
                if(count == -1)
                {
                    qDebug() << "Error reading!";
                    return;
                }
                if(count == 0)
                {
                    continue;
                }
                partialXml[count] = '\0';
                pos += count;
                xmlInput->setData(QString(partialXml));
                if(size <= chunkSize)
                {
                    // only iteration; parse all
                    reader->parse(xmlInput, false);
                }
                else if(size > chunkSize && pos == chunkSize)
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
