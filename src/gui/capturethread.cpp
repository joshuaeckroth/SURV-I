
#include <QTime>
#include <QDebug>
#include <QFile>

#include <unistd.h>

#include <opencv2/opencv.hpp>
using namespace cv;

#include "capturethread.h"
#include "decoder.h"
#include "frame.h"
#include "context.h"

CaptureThread::CaptureThread(Decoder* d, int c)
        : QThread(), decoder(d), captureActive(false),
        calculatedFps(0.0), fps(0.0), frameNum(0), camera(c),
        error(false)
{
    QFile file(Context::getCamera(camera).file);
    if(!file.exists())
    {
        error = true;
    } else {
        capture = new VideoCapture(Context::getCamera(camera).file.toStdString());
        if(!capture)
        {
            error = true;
        }
    }

    fps = capture->get(CV_CAP_PROP_FPS);
}

void CaptureThread::run()
{
    QTime time;
    time.start();
    IplImage *image;
    Frame *frame;
    double frameTime;
    QString detections;
    double lastFrameTime = 0.0;

    while(true)
    {
        if(!captureActive)
        {
            captureLock.lock();
            calculatedFps = 0;
            frameTimes.clear();
            captureWait.wait(&captureLock);
            time.restart();
            updateFPS(time.elapsed());
            captureLock.unlock();
        }
        Mat img_mat;
        *capture >> img_mat;
        IplImage img = img_mat.operator IplImage();
        image = cvCloneImage(&img);
        if(image)
        {
            frameNum++;

            //if(frameNum == 453) break;

            frameTime = frameNum / fps;
            frame = new Frame(frameNum, frameTime, camera);
            frame->setImage(image);

            //detections = decoder->decodeFrame(frame);
            double toSleep = 2.0 * (frameTime - lastFrameTime) * 1000; //convert from seconds to ms
            lastFrameTime = frameTime;
            //qDebug() << "frameTime: " << frameTime << " lastFrameTime: " << lastFrameTime << " toSleep: " << (int)toSleep;
            this->msleep((int)toSleep);
            detections = QString();
            emit newDetections(detections, frame);
        }
        updateFPS(time.elapsed());
    }
}

void CaptureThread::updateFPS(int time)
{
    frameLock.lock();

    frameTimes.enqueue(time);
    if(frameTimes.size() > 15)
    {
        frameTimes.dequeue();
    }
    if(frameTimes.size() > 2)
    {
        calculatedFps = frameTimes.size() / ((double)time - frameTimes.head()) * 1000.0;
    }
    else
    {
        calculatedFps = 0;
    }
    frameLock.unlock();
}

bool CaptureThread::startCapture()
{
    mutex.lock();
    if(!captureActive)
    {
        if(!capture)
        {
            qDebug() << "Error: capture not initialized";
            return false;
        }
        captureActive = true;
        captureWait.wakeAll();
        mutex.unlock();
        return true;
    }
    mutex.unlock();
    return false;
}

void CaptureThread::stopCapture()
{
    mutex.lock();
    captureActive = false;
    mutex.unlock();
}

