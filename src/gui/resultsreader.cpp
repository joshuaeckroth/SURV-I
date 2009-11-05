
#include <QXmlDefaultHandler>
#include <QDebug>

#include "resultsreader.h"
#include "detection.h"
#include "movement.h"
#include "path.h"
#include "entities.h"

ResultsReader::ResultsReader()
        : curEntities(NULL)
{ }

bool ResultsReader::startElement(const QString&, const QString&,
                                 const QString& qName, const QXmlAttributes& attributes)
{
    if(qName == "Accepted")
    {
        accepted = true;
    }
    else if(qName == "Rejected")
    {
        accepted = false;
    }
    else if(qName == "Results")
    {
        curEntities = new Entities;
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
        
        curEntities->addDetection(new Detection(id, lat, lon, startTime, endTime, area, accepted));
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

        detections.push_back(new Detection(id, lat, lon, startTime, endTime, area, accepted));
    }
    else if(qName == "Movement")
    {
        movementId = attributes.value("id");
    }
    else if(qName == "Path")
    {
        pathId = attributes.value("id");
    }

    return true;
}

bool ResultsReader::endElement(const QString&, const QString&, const QString& qName)
{
    // <Movement> NOT inside <Path>
    if(qName == "Movement" && pathId.isEmpty())
    {
        curEntities->addMovement(new Movement(movementId, accepted, detections));
        movementId.clear();
        detections.clear();
    }
    // <Movement> inside <Path>
    if(qName == "Movement" && !pathId.isEmpty())
    {
        movements.push_back(new Movement(movementId, accepted, detections));
        movementId.clear();
        detections.clear();
    }
    else if(qName == "Path")
    {
        curEntities->addPath(new Path(pathId, accepted, movements));
        pathId.clear();
        movements.clear();
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
