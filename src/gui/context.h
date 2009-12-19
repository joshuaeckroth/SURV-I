#ifndef CONTEXT_H
#define CONTEXT_H

#include <QString>
#include <QPoint>
#include <vector>

namespace ContextElements
{
    typedef struct Camera
    {
        QString name;
        QString file;
        QString warp;
    } Camera;

    typedef struct Map
    {
        QString file;
        QString warp;
    } Map;

    typedef struct Region
    {
        QString name;
        std::vector<QPointF> points;
    } Region;

    typedef struct PointOfInterest
    {
        QString name;
        QPointF point;
        double range;
    } PointOfInterest;

    typedef struct AgentTemplate
    {
        QString name;
        double area;
        double speed;
    } AgentTemplate;
}

class Context
{
public:
    static void addCamera(QString name, QString file, QString warp);
    static void setMap(QString file, QString warp);
    static void addRegion(QString name, std::vector<QPointF> points);
    static void addPointOfInterest(QString name, QPointF point, double range);
    static void addAgentTemplate(QString name, double area, double speed);

    static int cameraCount();
    static ContextElements::Camera getCamera(int index);

    static ContextElements::Map getMap();

    static int regionCount();
    static ContextElements::Region getRegion(int index);

    static int pointOfInterestCount();
    static ContextElements::PointOfInterest getPointOfInterest(int index);

protected:
    Context();

private:
    static Context *pinstance;
    static Context *instance();
    ContextElements::Map map;
    std::vector<ContextElements::Camera> cameras;
    std::vector<ContextElements::Region> regions;
    std::vector<ContextElements::PointOfInterest> pois;
    std::vector<ContextElements::AgentTemplate> agentTemplates;
};

#endif // CONTEXT_H
