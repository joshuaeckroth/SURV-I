#ifndef MOVEMENT_H
#define MOVEMENT_H

class Detection;

class Movement
{
public:
    Movement(int _id, Detection *_det1, Detection *_det2);

    int getId() const;
    void setAccepted(bool);
    bool isAccepted() const;
    Detection *getDet1() const;
    Detection *getDet2() const;

private:
    int id;
    bool accepted;
    Detection *det1, *det2;
};

#endif // MOVEMENT_H
