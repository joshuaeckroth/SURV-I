
#include <QDebug>
#include <QMap>
#include <QPair>
#include <QThread>
#include <QFile>

#include <iostream>

#include "processingcontroller.h"
#include "entities.h"
#include "decoder.h"
#include "frame.h"
#include "cameramodel.h"

ProcessingController::ProcessingController(int n)
        : QThread(), numCameras(n), detectionsCount(1)
{
    CameraModel::setNumCameras(numCameras);
    curFrame = new Frame*[numCameras];
}

void ProcessingController::run()
{
    mutex.lock();
    isProcessing = false;
    mutex.unlock();

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

    abducerTimer = NULL;

    exec();
}

void ProcessingController::startProcessing()
{
    mutex.lock();
    // send detections to abducer every 3 seconds
    if(abducerTimer == NULL)
    {
        abducerTimer = new QTimer;
        connect(abducerTimer, SIGNAL(timeout()), this, SLOT(timeoutDetections()));
    }
    abducerTimer->start(3000);

    for(int i = 0; i < numCameras; i++)
    {
        if(!captureThread[i]->hasError())
        {
            captureThread[i]->startCapture();
            captureThread[i]->start(QThread::IdlePriority);
        }
    }

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

    //renderer->setNumCameras(n);
}

void ProcessingController::newDetections(QString ds, Frame* frame)
{
    mutex.lock();
    detections.append(ds);

    curFrame[frame->getCamera()] = frame;

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

    emit newFrame(frame);
    mutex.unlock();
}

void ProcessingController::timeoutDetections()
{
    /*
    mutex.lock();
    for(int i = 0; i < numCameras; i++)
    {
        captureThread[i]->stopCapture();
    }
    qDebug() << "stopped cameras.";
    isProcessing = false;
    abducerTimer->stop();
    */

    QFile detFile(QString("detections/detections-chunk-%1.xml").arg(detectionsCount));
    detFile.open(QFile::ReadOnly);

    detections = detFile.readAll();

    detectionsCount++;

    emit sendDetections(detections);
    detections.clear();
    //mutex.unlock();
}

void ProcessingController::newEntities(Entities*)
{
    /*
    mutex.lock();
    for(int i = 0; i < numCameras; i++)
    {
        captureThread[i]->startCapture();
    }
    qDebug() << "started cameras";
    isProcessing = true;
    abducerTimer->start(3000);
    mutex.unlock();
    */
}
