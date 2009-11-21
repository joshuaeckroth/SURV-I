#ifndef RESULTS_READER_H
#define RESULTS_READER_H

#include <vector>
#include <map>

#include <QXmlDefaultHandler>

class Detection;
class Movement;
class Path;
class Behavior;
class Entities;

class ResultsReader : public QXmlDefaultHandler
{
public:
  ResultsReader();
  bool startElement(const QString& namespaceURI, const QString& localName,
		    const QString& qName, const QXmlAttributes& attributes);
  bool endElement(const QString& namespaceURI, const QString& localName,
		  const QString& qName);
  bool characters(const QString& str);
  bool fatalError(const QXmlParseException& exception);
  QString errorString() const;
  Entities* getEntities() const;

private:
  Entities *entities;
  bool accepted;
  QString currentText;
  QString errorStr;
  int pathId;
  bool inPath;
  QString pathScore;
  QString pathConflicts;
  int behaviorId;
  bool inBehavior;
  QString behaviorScore;
  QString behaviorContent;
  std::map<int,Detection*> detections;
  std::map<int,Movement*> movements;
  std::map<int,Path*> paths;
  std::vector<Movement*> pathMovements;
  std::map<int,Behavior*> behaviors;
  std::vector<Path*>behaviorPaths;
};

#endif
