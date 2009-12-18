#ifndef ENTITIES_H
#define ENTITIES_H

#include <map>
#include <QString>
#include <QAbstractItemModel>
#include <QModelIndex>
#include <QVariant>

class Detection;
class Movement;
class Path;
class Agent;
class Behavior;
class Entity;
class EntitiesTreeItem;

class Entities : public QAbstractItemModel
{
    Q_OBJECT;

public:
    Entities(std::map<int,Detection*>,
             std::map<int,Movement*>,
             std::map<int,Path*>,
             std::map<int,Agent*>,
             std::map<int,Behavior*>,
             bool detailed);
    ~Entities();

    void detections_begin();
    bool detections_end() const;
    Detection *detections_next();

    void movements_begin();
    bool movements_end() const;
    Movement *movements_next();

    void paths_begin();
    bool paths_end() const;
    Path *paths_next();

    void agents_begin();
    bool agents_end() const;
    Agent *agents_next();

    void behaviors_begin();
    bool behaviors_end() const;
    Behavior *behaviors_next();

    QVariant data(const QModelIndex &index, int role) const;
    Qt::ItemFlags flags(const QModelIndex &index) const;
    QVariant headerData(int section, Qt::Orientation orientation,
                        int role = Qt::DisplayRole) const;
    QModelIndex index(int row, int column,
                      const QModelIndex &parent = QModelIndex()) const;
    QModelIndex parent(const QModelIndex &index) const;
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    int columnCount(const QModelIndex &parent = QModelIndex()) const;

    void updateHighlights();

    Entities *getNotDetailedEntities() const { return notDetailed; }
    void setNotDetailedEntities(Entities *e) { notDetailed = e; }
    Entities *getDetailedEntities() const { return detailed; }
    void setDetailedEntities(Entities *e) { detailed = e; }

private:
    std::map<int,Detection*> detections;
    std::map<int,Detection*>::const_iterator detections_iter;

    std::map<int,Movement*> movements;
    std::map<int,Movement*>::const_iterator movements_iter;

    std::map<int,Path*> paths;
    std::map<int,Path*>::const_iterator paths_iter;

    std::map<int,Agent*> agents;
    std::map<int,Agent*>::const_iterator agents_iter;

    std::map<int,Behavior*> behaviors;
    std::map<int,Behavior*>::const_iterator behaviors_iter;

    EntitiesTreeItem *rootEntitiesTreeItem;

    Entities *notDetailed;
    Entities *detailed;
};

#endif
