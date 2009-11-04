
#include <QXmlDefaultHandler>
#include <QDebug>

#include "resultsreader.h"
#include "detection.h"
#include "movement.h"
#include "entities.h"

ResultsReader::ResultsReader()
        : curEntities(NULL)
{ }

bool ResultsReader::startElement(const QString&, const QString&,
                                 const QString& qName, const QXmlAttributes& attributes)
{
    if(qName == "Results")
    {
        curEntities = new Entities;
    }
    else if(qName == "Log")
    {

    }
    else if(qName == "Frame")
    {
        int frameNumber = attributes.value("number").toInt();
        double frameTime = attributes.value("time").toDouble();

        //curEntities = new Entities(frameNumber, frameTime);
    }
    // <Detection> NOT inside <Movement>
    else if(qName == "Detection" && movementId.isEmpty())
    {
        QString id = attributes.value("id");
        double lat = attributes.value("lat").toDouble();
        double lon = attributes.value("lon").toDouble();
        double startTime = attributes.value("startTime").toDouble();
        double endTime = attributes.value("endTime").toDouble();
        double area = attributes.value("area").toDouble();

        curEntities->addDetection(new Detection(id, lat, lon, startTime, endTime, area));
    }
    // <Detection> inside <Movement>
    else if(qName == "Detection" && !movementId.isEmpty())
    {
        QString id = attributes.value("id");
        double lat = attributes.value("lat").toDouble();
        double lon = attributes.value("lon").toDouble();
        double startTime = attributes.value("startTime").toDouble();
        double endTime = attributes.value("endTime").toDouble();
        double area = attributes.value("area").toDouble();

        detections.push_back(new Detection(id, lat, lon, startTime, endTime, area));
    }
    else if(qName == "Movement")
    {
        movementId = attributes.value("id");
    }
    else if(qName == "Track")
    {
        int id = attributes.value("id").toInt();
        double cx = attributes.value("cx").toDouble();
        double cy = attributes.value("cy").toDouble();
        double ocx = attributes.value("ocx").toDouble();
        double ocy = attributes.value("ocy").toDouble();
        int prevId = attributes.value("prevID").toInt();
        int nextId = attributes.value("nextID").toInt();
        double ecx = attributes.value("ecx").toDouble();
        double ecy = attributes.value("ecy").toDouble();
        double radius = attributes.value("radius").toDouble();
        bool thisFrame = (attributes.value("thisFrame") == QString("True") ? true : false);

        //curEntities->addTrack(new Track(id, cx, cy, ocx, ocy, prevId, nextId, ecx, ecy, radius, thisFrame));
    }
    return true;
}

bool ResultsReader::endElement(const QString&, const QString&, const QString& qName)
{
    if(qName == "Movement")
    {
        curEntities->addMovement(new Movement(movementId, detections));
        movementId.clear();
        detections.clear();
    }
    return true;
}

bool ResultsReader::characters(const QString& str)
{
    curEntities->appendLog(str);
    return true;
}

bool ResultsReader::fatalError(const QXmlParseException& exception)
{
    qDebug() << QString("ResultsReader: parse error at line %1, column %2:\n%3")
            .arg(exception.lineNumber())
            .arg(exception.columnNumber())
            .arg(exception.message());

    return true;
}

QString ResultsReader::errorString() const
{
    return errorStr;
}

Entities* ResultsReader::getEntities() const
{
    return curEntities;
}
