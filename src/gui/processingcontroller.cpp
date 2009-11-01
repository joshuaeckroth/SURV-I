
#include <QDebug>
#include <QMap>
#include <QPair>

#include <iostream>

#include "processingcontroller.h"
#include "renderarea.h"
#include "decoder.h"
#include "abducerthread.h"
#include "frame.h"
#include "cameramodel.h"
#include "entities.h"

ProcessingController::ProcessingController(RenderArea* r, int n)
        : numCameras(n), renderer(r)
{
    renderer->setNumCameras(numCameras);
    CameraModel::setNumCameras(numCameras);
    curFrame = new Frame*[numCameras];
    init();
}

void ProcessingController::init()
{
    mutex.lock();
    isProcessing = false;
    mutex.unlock();

    abducerThread = new AbducerThread();
    connect(abducerThread, SIGNAL(newEntities(Entities*)), this, SLOT(newEntities(Entities*)));
    abducerThread->start();

    for(int i = 0; i < numCameras; i++)
    {
        curFrame[i] = NULL;
        decoder[i] = new Decoder;
        captureThread[i] = new CaptureThread(decoder[i], i);
        if(captureThread[i]->hasError())
        {
            qDebug() << "Error in camera " << QString::number(i);
        }
        else
        {
            connect(captureThread[i], SIGNAL(newDetections(QString, Frame*)),
                    this, SLOT(newDetections(QString, Frame*)));
        }
    }

    abducerTimer = new QTimer;
    connect(abducerTimer, SIGNAL(timeout()), this, SLOT(sendDetections()));
}

void ProcessingController::startProcessing()
{
    mutex.lock();
    for(int i = 0; i < numCameras; i++)
    {
        if(!captureThread[i]->hasError())
        {
            captureThread[i]->startCapture();
            captureThread[i]->start(QThread::IdlePriority);
        }
    }

    abducerTimer->start(2000);
    isProcessing = true;
    mutex.unlock();
}

void ProcessingController::stopProcessing()
{
    mutex.lock();
    for(int i = 0; i < numCameras; i++)
    {
        captureThread[i]->stopCapture();
    }

    abducerTimer->stop();
    isProcessing = false;
    mutex.unlock();
}

QString ProcessingController::getCameraTimes() const
{
    QString times;
    for(int i = 0; i < numCameras; i++)
    {
        if(curFrame[i] == NULL)
        {
            times.append(QString("Camera %1 not active")
                         .arg(i));
        }
        else
        {
            times.append(QString("Camera %1: Frame %2 / %3s (%4 FPS)")
                         .arg(i)
                         .arg(curFrame[i]->getNumber())
                         .arg(curFrame[i]->getTime(), 0, 'f', 1)
                         .arg(captureThread[i]->getCalculatedFPS(), 0, 'f', 1));
        }
        if(i != (numCameras - 1)) times.append(", ");
    }
    return times;
}

void ProcessingController::numCamerasChanged(int n)
{
    for(int i = 0; i < numCameras; i++)
    {
        captureThread[i]->stopCapture();
        captureThread[i]->exit();
        delete curFrame[i];
        //delete captureThread[i];
        //delete decoder[i];
    }
    delete curFrame;

    numCameras = n;
    curFrame = new Frame*[numCameras];

    renderer->setNumCameras(n);

    init();
}

void ProcessingController::newDetections(QString ds, Frame* frame)
{
    mutex.lock();
    detections.append(ds);

    curFrame[frame->getCamera()] = frame;
    renderer->showFrame(frame);

    if(isProcessing)
    {
        // stop processing the faster cameras if they are out of sync
        double oldestTime = frame->getTime();
        for(int i = 0; i < numCameras; i++)
        {
            if(curFrame[i] != NULL)
            {
                if(curFrame[i]->getTime() < (oldestTime + 0.01))
                {
                    oldestTime = curFrame[i]->getTime();
                    captureThread[i]->startCapture();
                }
                else
                {
                    captureThread[i]->stopCapture();
                }
            }
        }
    }
    mutex.unlock();
}

void ProcessingController::sendDetections()
{
    mutex.lock();
    abducerThread->newDetections(detections);
    detections.clear();
    mutex.unlock();
}

void ProcessingController::newEntities(Entities* e)
{
    //qDebug() << QString("Log: %1\n").arg(e->getLog());
    renderer->updateEntities(e);
}

