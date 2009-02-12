
#include <QXmlDefaultHandler>
#include <QDebug>

#include "tracksreader.h"

TracksReader::TracksReader()
{ }

bool TracksReader::startElement(const QString&, const QString&,
				const QString& qName, const QXmlAttributes& attributes)
{
  if(qName == "Frame")
    {
      qDebug() << "Got frame";
    }
  else if(qName == "Detection")
    {
      qDebug() << "Got detection";
    }
  else if(qName == "Track")
    {
      qDebug() << "Got track";
    }
  else if(qName == "FrameLog")
    {
      qDebug() << "Got frame log";
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
