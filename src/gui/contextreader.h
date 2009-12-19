#ifndef CONTEXTREADER_H
#define CONTEXTREADER_H

#include <QXmlDefaultHandler>
#include <QPoint>

class ContextReader : public QXmlDefaultHandler
{
public:
    ContextReader();
    bool startElement(const QString& namespaceURI, const QString& localName,
                      const QString& qName, const QXmlAttributes& attributes);
    bool endElement(const QString& namespaceURI, const QString& localName,
                    const QString& qName);
    bool characters(const QString& str);
    bool fatalError(const QXmlParseException& exception);
    QString errorString() const;

private:
    QString errorStr;
    bool inCameras, inRegions, inRegion, inPois, inAgentTemplates;
    QString regionName;
    std::vector<QPointF> regionPoints;
};

#endif // CONTEXTREADER_H
