#ifndef TRACKS_READER_H
#define TRACKS_READER_H

#include <vector>

#include <QXmlDefaultHandler>

class Frames;
class Frame;

class TracksReader : public QXmlDefaultHandler
{
public:
  TracksReader(Frames *fs);
  bool startElement(const QString& namespaceURI, const QString& localName,
		    const QString& qName, const QXmlAttributes& attributes);
  bool endElement(const QString& namespaceURI, const QString& localName,
		  const QString& qName);
  bool characters(const QString& str);
  bool fatalError(const QXmlParseException& exception);
  QString errorString() const;

private:
  Frames *frames;
  Frame *curFrame;
  QString currentText;
  QString errorStr;
  int frameCamera;
  int frameNumber;
  double frameTime;
};

#endif
