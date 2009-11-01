
#include <QXmlDefaultHandler>
#include <QDebug>

#include "tracksreader.h"
#include "detection.h"
#include "track.h"
#include "noise.h"
#include "entities.h"

TracksReader::TracksReader()
  : curEntities(NULL)
{ }

bool TracksReader::startElement(const QString&, const QString&,
				const QString& qName, const QXmlAttributes& attributes)
{
  if(qName == "Frame")
    {
      int frameNumber = attributes.value("number").toInt();
      double frameTime = attributes.value("time").toDouble();

      //curEntities = new Entities(frameNumber, frameTime);
    }
  else if(qName == "Detection")
    {
      int id = attributes.value("id").toInt();
      int camera = attributes.value("camera").toInt();
      double area = attributes.value("area").toDouble();
      int cx = attributes.value("cx").toDouble();
      int cy = attributes.value("cy").toDouble();

      //curEntities->addDetection(new Detection(id, camera, area, cx, cy));
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

      curEntities->addTrack(new Track(id, cx, cy, ocx, ocy, prevId, nextId, ecx, ecy, radius, thisFrame));
    }
  else if(qName == "FrameLog")
    {
    }
  return true;
}

bool TracksReader::endElement(const QString&, const QString&, const QString& qName)
{
  return true;
}

bool TracksReader::characters(const QString& str)
{
  return true;
}

bool TracksReader::fatalError(const QXmlParseException& exception)
{
  qDebug() << QString("TracksReader: parse error at line %1, column %2:\n%3")
    .arg(exception.lineNumber())
    .arg(exception.columnNumber())
    .arg(exception.message());

  return true;
}

QString TracksReader::errorString() const
{
  return errorStr;
}

Entities* TracksReader::getEntities() const
{
  return curEntities;
}
