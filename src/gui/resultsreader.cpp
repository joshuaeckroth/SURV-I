
#include <QXmlDefaultHandler>
#include <QDebug>

#include "resultsreader.h"
#include "detection.h"
#include "movement.h"
#include "path.h"
#include "entities.h"

ResultsReader::ResultsReader()
        : entities(NULL), inPath(false)
{ }

bool ResultsReader::startElement(const QString&, const QString&,
                                 const QString& qName, const QXmlAttributes& attributes)
{
    if(qName == "Results")
    {
        detections.clear();
        movements.clear();
        paths.clear();
    }
    else if(qName == "Entities")
    { }
    else if(qName == "Detection")
    {
        int id = attributes.value("id").toInt();
        double lat = attributes.value("lat").toDouble();
        double lon = attributes.value("lon").toDouble();
        double startTime = attributes.value("startTime").toDouble();
        double endTime = attributes.value("endTime").toDouble();
        double area = attributes.value("area").toDouble();

        detections[id] = new Detection(id, lat, lon, startTime, endTime, area);
    }
    if(qName == "Accepted")
    {
        accepted = true;
    }
    else if(qName == "Rejected")
    {
        accepted = false;
    }
    else if(qName == "DetectionRef")
    {
        int id = attributes.value("detId").toInt();
        detections[id]->setAccepted(accepted);
    }
    else if(qName == "Movement")
    {
        int id = attributes.value("id").toInt();
        int detId1 = attributes.value("detId1").toInt();
        int detId2 = attributes.value("detId2").toInt();
        movements[id] = new Movement(id, detections[detId1], detections[detId2]);
    }
    else if(qName == "Path")
    {
        pathId = attributes.value("id").toInt();
        inPath = true;
        pathMovements.clear();
    }
    else if(qName == "MovementRef" && inPath)
    {
        int movId = attributes.value("movId").toInt();
        pathMovements.push_back(movements[movId]);
    }
    else if(qName == "MovementRef" && !inPath)
    {
        int id = attributes.value("movId").toInt();
        movements[id]->setAccepted(accepted);
    }
    else if(qName == "PathRef")
    {
        int id = attributes.value("pathId").toInt();
        paths[id]->setAccepted(accepted);
    }

    return true;
}

bool ResultsReader::endElement(const QString&, const QString&, const QString& qName)
{
    if(qName == "Results")
    {
        entities = new Entities(detections, movements, paths);
    }
    else if(qName == "Path")
    {
        paths[pathId] = new Path(pathId, pathMovements);
        inPath = false;
    }
    return true;
}

bool ResultsReader::characters(const QString& str)
{
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
    return entities;
}
