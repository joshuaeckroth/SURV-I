#include "context.h"

Context *Context::pinstance = NULL;

Context *Context::instance()
{
    if(pinstance == NULL)
        pinstance = new Context;
    return pinstance;
}

Context::Context()
{ }

void Context::addCamera(QString name, QString file, QString warp)
{
    ContextElements::Camera c;
    c.name = name;
    c.file = file;
    c.warp = warp;
    instance()->cameras.push_back(c);
}

void Context::setMap(QString file, QString warp)
{
    instance()->map.file = file;
    instance()->map.warp = warp;
}

void Context::addRegion(QString name, std::vector<QPointF> points)
{
    ContextElements::Region r;
    r.name = name;
    r.points = points;
    instance()->regions.push_back(r);
}

void Context::addPointOfInterest(QString name, QPointF point, double range)
{
    ContextElements::PointOfInterest poi;
    poi.name = name;
    poi.point = point;
    poi.range = range;
    instance()->pois.push_back(poi);
}

void Context::addAgentTemplate(QString name, double area, double speed)
{
    ContextElements::AgentTemplate a;
    a.name = name;
    a.area = area;
    a.speed = speed;
    instance()->agentTemplates.push_back(a);
}

int Context::cameraCount()
{
    return instance()->cameras.size();
}

ContextElements::Camera Context::getCamera(int index)
{
    return instance()->cameras.at(index);
}

ContextElements::Map Context::getMap()
{
    return instance()->map;
}

int Context::regionCount()
{
    return instance()->regions.size();
}

ContextElements::Region Context::getRegion(int index)
{
    return instance()->regions.at(index);
}

int Context::pointOfInterestCount()
{
    return instance()->pois.size();
}

ContextElements::PointOfInterest Context::getPointOfInterest(int index)
{
    return instance()->pois.at(index);
}
