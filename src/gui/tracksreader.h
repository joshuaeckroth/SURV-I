#ifndef TRACKS_READER_H
#define TRACKS_READER_H

#include <vector>

#include <QXmlDefaultHandler>

class Detection;
class Noise;
class Track;
class Entities;

class TracksReader : public QXmlDefaultHandler
{
public:
  TracksReader();
  bool startElement(const QString& namespaceURI, const QString& localName,
		    const QString& qName, const QXmlAttributes& attributes);
  bool endElement(const QString& namespaceURI, const QString& localName,
		  const QString& qName);
  bool characters(const QString& str);
  bool fatalError(const QXmlParseException& exception);
  QString errorString() const;
  Entities* getEntities() const;

private:
  Entities *curEntities;
  QString currentText;
  QString errorStr;
};

#endif
