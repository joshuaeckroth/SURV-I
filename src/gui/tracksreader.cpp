
#include <QXmlDefaultHandler>
#include <QDebug>

#include "tracksreader.h"
#include "frame.h"
#include "frames.h"
#include "noise.h"

TracksReader::TracksReader(Frames *fs)
  : frames(fs), curFrame(NULL)
{ }

bool TracksReader::startElement(const QString&, const QString&,
				const QString& qName, const QXmlAttributes& attributes)
{
  if(qName == "Frame")
    {
      frameCamera = attributes.value("camera").toInt();
      frameNumber = attributes.value("number").toInt();
      frameTime = attributes.value("time").toDouble();

      curFrame = frames->findFrame(frameCamera, frameTime);
    }
  else if(qName == "Noise")
    {
      int id = attributes.value("id").toInt();
      double area = attributes.value("area").toDouble();
      int cx = attributes.value("cx").toInt();
      int cy = attributes.value("cy").toInt();

      curFrame->addNoise(new Noise(id, area, cx, cy));
    }
  else if(qName == "Track")
    {

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
