#ifndef DETECTION_H
#define DETECTION_H

#include <QString>
#include <QStringList>

#include "entity.h"

class Detection : public Entity
{
public:
    Detection(int _id, double _lat, double _lon,
              double _startTime, double _endTime,
              double _area, QString _score);
    ~Detection();

    int getId() const;
    double getLat() const;
    double getLon() const;
    double getStartTime() const;
    double getEndTime() const;
    double getArea() const;
    void setAccepted(bool);
    bool isAccepted() const;
    QString getScore() const;
    QStringList getData() const;
    void setHighlighted(bool h);
    bool isHighlighted() const;

private:
    int id;
    double lat;
    double lon;
    double startTime;
    double endTime;
    double area;
    bool accepted;
    QString score;
    bool highlighted;
};

#endif
