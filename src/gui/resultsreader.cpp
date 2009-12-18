
#include <QXmlDefaultHandler>
#include <QDebug>

#include "resultsreader.h"
#include "detection.h"
#include "movement.h"
#include "path.h"
#include "agent.h"
#include "behavior.h"
#include "entities.h"

ResultsReader::ResultsReader()
        : detailedEntities(NULL), notDetailedEntities(NULL), inPath(false), inAgent(false), inBehavior(false)
{ }

bool ResultsReader::startElement(const QString&, const QString&,
                                 const QString& qName, const QXmlAttributes& attributes)
{
    if(qName == "Results")
    {
        detections.clear();
        movements.clear();
        paths.clear();
        agents.clear();
        behaviors.clear();
    }
    else if(qName == "Entities")
    { }
    else if(qName == "Accepted")
    {
        accepted = true;
    }
    else if(qName == "Rejected")
    {
        accepted = false;
    }
    else if(qName == "Detection")
    {
        int id = attributes.value("id").toInt();
        double lat = attributes.value("lat").toDouble();
        double lon = attributes.value("lon").toDouble();
        double startTime = attributes.value("startTime").toDouble();
        double endTime = attributes.value("endTime").toDouble();
        double area = attributes.value("area").toDouble();
        QString score = attributes.value("score");

        detections[id] = new Detection(id, lat, lon, startTime, endTime, area, score);
    }
    else if(qName == "DetectionRef")
    {
        int id = attributes.value("detId").toInt();
        detections[id]->setAccepted(accepted);
    }
    else if(qName == "Movement")
    {
        int id = attributes.value("id").toInt();
        int detId1 = attributes.value("detId1").toInt();
        int detId2 = attributes.value("detId2").toInt();
        QString score = attributes.value("score");
        movements[id] = new Movement(id, detections[detId1], detections[detId2], score);
    }
    else if(qName == "Path")
    {
        pathId = attributes.value("id").toInt();
        pathScore = attributes.value("score");
        pathConflicts = attributes.value("conflicts");
        inPath = true;
        pathMovements.clear();
    }
    else if(qName == "Agent")
    {
        agentId = attributes.value("id").toInt();
        agentScore = attributes.value("score");
        agentContent = attributes.value("content");
        agentConflicts = attributes.value("conflicts");
        inAgent = true;
        agentPaths.clear();
    }
    else if(qName == "Behavior")
    {
        behaviorId = attributes.value("id").toInt();
        behaviorScore = attributes.value("score");
        behaviorContent = attributes.value("content");
        behaviorConflicts = attributes.value("conflicts");
        inBehavior = true;
        behaviorAgents.clear();
    }
    else if(qName == "MovementRef" && inPath)
    {
        int movId = attributes.value("movId").toInt();
        pathMovements.push_back(movements[movId]);
    }
    else if(qName == "MovementRef" && !inPath)
    {
        int id = attributes.value("movId").toInt();
        movements[id]->setAccepted(accepted);
    }
    else if(qName == "PathRef" && inAgent)
    {
        int pathId = attributes.value("pathId").toInt();
        agentPaths.push_back(paths[pathId]);
    }
    else if(qName == "PathRef" && !inAgent)
    {
        int id = attributes.value("pathId").toInt();
        paths[id]->setAccepted(accepted);
    }
    else if(qName == "AgentRef" && inBehavior)
    {
        int agentId = attributes.value("agentId").toInt();
        behaviorAgents.push_back(agents[agentId]);
    }
    else if(qName == "AgentRef" && !inBehavior)
    {
        int id = attributes.value("agentId").toInt();
        agents[id]->setAccepted(accepted);
    }
    else if(qName == "BehaviorRef")
    {
        int id = attributes.value("behavId").toInt();
        behaviors[id]->setAccepted(accepted);
    }

    return true;
}

bool ResultsReader::endElement(const QString&, const QString&, const QString& qName)
{
    if(qName == "Results")
    {
        detailedEntities = new Entities(detections, movements, paths, agents, behaviors, true);
        notDetailedEntities = new Entities(detections, movements, paths, agents, behaviors, false);

        detailedEntities->setDetailedEntities(detailedEntities);
        detailedEntities->setNotDetailedEntities(notDetailedEntities);

        notDetailedEntities->setNotDetailedEntities(notDetailedEntities);
        notDetailedEntities->setDetailedEntities(detailedEntities);
    }
    else if(qName == "Path")
    {
        paths[pathId] = new Path(pathId, pathMovements, pathScore, pathConflicts);
        inPath = false;
    }
    else if(qName == "Agent")
    {
        agents[agentId] = new Agent(agentId, agentPaths, agentScore, agentContent, agentConflicts);
        inAgent = false;
    }
    else if(qName == "Behavior")
    {
        behaviors[behaviorId] = new Behavior(behaviorId, behaviorAgents, behaviorScore, behaviorContent, behaviorConflicts);
        inBehavior = false;
    }
    return true;
}

bool ResultsReader::characters(const QString& str)
{
    return true;
}

bool ResultsReader::fatalError(const QXmlParseException& exception)
{
    qDebug() << QString("ResultsReader: parse error at line %1, column %2:\n%3")
            .arg(exception.lineNumber())
            .arg(exception.columnNumber())
            .arg(exception.message());

    return true;
}

QString ResultsReader::errorString() const
{
    return errorStr;
}

Entities* ResultsReader::getEntities() const
{
    return notDetailedEntities;
}
