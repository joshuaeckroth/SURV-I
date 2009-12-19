#include <QDebug>

#include "contextreader.h"
#include "context.h"

ContextReader::ContextReader()
        : inCameras(false), inRegions(false), inRegion(false), inPois(false), inAgentTemplates(false)
{ }

bool ContextReader::startElement(const QString&, const QString&,
                                 const QString& qName, const QXmlAttributes& attributes)
{
    if(qName == "Context")
    { }
    else if(qName == "Cameras")
    {
        inCameras = true;
    }
    else if(qName == "FileInput")
    {
        Context::addCamera(attributes.value("name"), attributes.value("file"), attributes.value("warpFile"));
    }
    else if(qName == "Map")
    {
        Context::setMap(attributes.value("mapFile"), attributes.value("warpFile"));
    }
    else if(qName == "Regions")
    {
        inRegions = true;
    }
    else if(qName == "Region")
    {
        inRegion = true;
        regionName = attributes.value("name");
        regionPoints.clear();
    }
    else if(qName == "RegionPoint")
    {
        QPointF p(attributes.value("lat").toDouble(), attributes.value("lon").toDouble());
        regionPoints.push_back(p);
    }
    else if(qName == "PointsOfInterest")
    {
        inPois = true;
    }
    else if(qName == "PointOfInterest")
    {
        Context::addPointOfInterest(attributes.value("name"),
                                    QPointF(attributes.value("lat").toDouble(), attributes.value("lon").toDouble()),
                                    attributes.value("range").toDouble());
    }
    else if(qName == "AgentTemplates")
    {
        inAgentTemplates = true;
    }
    else if(qName == "AgentTemplate")
    {
        Context::addAgentTemplate(attributes.value("name"), attributes.value("area").toDouble(), attributes.value("speed").toDouble());
    }
    return true;
}

bool ContextReader::endElement(const QString&, const QString&, const QString& qName)
{
    if(qName == "Context")
    { }
    else if(qName == "Cameras")
    {
        inCameras = false;
    }
    else if(qName == "FileInput")
    { }
    else if(qName == "Map")
    { }
    else if(qName == "Regions")
    {
        inRegions = false;
    }
    else if(qName == "Region")
    {
        inRegion = false;
        Context::addRegion(regionName, regionPoints);
    }
    else if(qName == "RegionPoint")
    { }
    else if(qName == "PointsOfInterest")
    {
        inPois = false;
    }
    else if(qName == "PointOfInterest")
    { }
    else if(qName == "AgentTemplates")
    {
        inAgentTemplates = false;
    }
    else if(qName == "AgentTemplate")
    { }
    return true;
}

bool ContextReader::characters(const QString& str)
{
    return true;
}

bool ContextReader::fatalError(const QXmlParseException& exception)
{
    qDebug() << QString("ContextReader: parse error at line %1, column %2:\n%3")
            .arg(exception.lineNumber())
            .arg(exception.columnNumber())
            .arg(exception.message());

    return true;
}

QString ContextReader::errorString() const
{
    return errorStr;
}
