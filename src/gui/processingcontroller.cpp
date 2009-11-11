
#include <QDebug>
#include <QMap>
#include <QPair>

#include <iostream>

#include "processingcontroller.h"
#include "renderarea.h"
#include "decoder.h"
#include "abducerreader.h"
#include "abducerwriter.h"
#include "frame.h"
#include "cameramodel.h"
#include "entities.h"
#include "entitiesTree.h"

ProcessingController::ProcessingController(RenderArea* r, int n, EntitiesTree *e)
        : numCameras(n), renderer(r), entitiesTree(e)
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

    abducerReader = new AbducerReader();
    connect(abducerReader, SIGNAL(newEntities(Entities*)), renderer, SLOT(updateEntities(Entities*)));
    connect(abducerReader, SIGNAL(newEntities(Entities*)), entitiesTree, SLOT(updateEntities(Entities*)));
    abducerReader->start();

    abducerWriter = new AbducerWriter();
    abducerWriter->start();

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

    // send detections to abducer every 3 seconds
    abducerTimer->start(3000);
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

        // get oldest time
        double oldestTime = frame->getTime(); // initial reasonable value
        for(int i = 0; i < numCameras; i++)
        {
            if(curFrame[i] != NULL && curFrame[i]->getTime() < oldestTime)
            {
                oldestTime = curFrame[i]->getTime();
            }
        }
        // stop cameras later than oldestTime + 0.2,
        // start/resume all others
        for(int i = 0; i < numCameras; i++)
        {
            if(curFrame[i] != NULL && curFrame[i]->getTime() > (oldestTime + 0.2))
            {
                captureThread[i]->stopCapture();
            }
            else
            {
                captureThread[i]->startCapture();
            }
        }
    }
    mutex.unlock();
}

void ProcessingController::sendDetections()
{
    abducerWriter->newDetections(detections);
    detections.clear();
}
