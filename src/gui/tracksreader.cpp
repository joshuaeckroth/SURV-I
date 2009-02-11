
#include <QXmlDefaultHandler>
#include <QDebug>

#include "tracksreader.h"

TracksReader::TracksReader()
{ }

bool TracksReader::startElement(const QString&, const QString&,
				const QString& qName, const QXmlAttributes& attributes)
{
  qDebug() << "start: " << qName;
  return true;
}

bool TracksReader::endElement(const QString&, const QString&, const QString& qName)
{
  qDebug() << "end: " << qName;
  return true;
}

bool TracksReader::characters(const QString& str)
{
  qDebug() << "characters: " << str;
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
