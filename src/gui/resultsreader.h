#ifndef RESULTS_READER_H
#define RESULTS_READER_H

#include <vector>
#include <map>

#include <QXmlDefaultHandler>

class Detection;
class Movement;
class Path;
class Agent;
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
  Entities *detailedEntities, *notDetailedEntities;
  bool accepted;
  QString currentText;
  QString errorStr;
  int pathId;
  bool inPath;
  QString pathScore;
  QString pathConflicts;
  int agentId;
  bool inAgent;
  QString agentScore;
  QString agentContent;
  QString agentConflicts;
  int behaviorId;
  bool inBehavior;
  QString behaviorScore;
  QString behaviorContent;
  QString behaviorConflicts;
  std::map<int,Detection*> detections;
  std::map<int,Movement*> movements;
  std::map<int,Path*> paths;
  std::vector<Movement*> pathMovements;
  std::map<int,Agent*> agents;
  std::vector<Path*> agentPaths;
  std::map<int,Behavior*> behaviors;
  std::vector<Agent*> behaviorAgents;
};

#endif
