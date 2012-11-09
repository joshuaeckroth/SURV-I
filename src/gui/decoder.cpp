#include <QString>
#include <QFile>
#include <QTextStream>
#include <QDebug>
#include <QPair>
#include <QPointF>

#include <math.h>
#include <vector>

#include <opencv2/opencv.hpp>
using namespace cv;

#include "decoder.h"
#include "frame.h"
#include "cameramodel.h"

Decoder::Decoder()
        : bg_model(0)
{}

QString Decoder::decodeFrame(Frame *frame)
{
    IplImage *image = frame->getImage();

    QString result;

    if(!bg_model)
    {
        bg_model = new BackgroundSubtractorMOG;
    }
    fgmask = cvCloneImage(image);
    Mat tmp_fgmask = cvarrToMat(fgmask);
    Mat tmp_image = cvarrToMat(image);
    (*bg_model)(tmp_image, tmp_fgmask, 0.1);
    *fgmask = tmp_fgmask.operator IplImage();
    result = findBlobs(frame, true);

    return result;
}

//void group_blobs(std::vector<coordinate> blobs_in, double threshold, std::vector<coordinate>& bigblobs);

// copied from OpenCV 1.1.0 enteringblobdetection.cpp
int compareContour(const void* a, const void* b, void*)
{
    float           dx, dy;
    float           h, w, ht, wt;
    CvPoint2D32f    pa, pb;
    CvRect          ra, rb;
    CvSeq*          pCA = *(CvSeq**)a;
    CvSeq*          pCB = *(CvSeq**)b;
    ra = ((CvContour*)pCA)->rect;
    rb = ((CvContour*)pCB)->rect;
    pa.x = ra.x + ra.width * 0.5f;
    pa.y = ra.y + ra.height * 0.5f;
    pb.x = rb.x + rb.width * 0.5f;
    pb.y = rb.y + rb.height * 0.5f;
    w = (ra.width + rb.width) * 0.5f;
    h = (ra.height + rb.height) * 0.5f;

    dx = (float)(fabs(pa.x - pb.x) - w);
    dy = (float)(fabs(pa.y - pb.y) - h);

    //wt = MAX(ra.width, rb.width) * 0.1f;
    wt = 0;
    ht = MAX(ra.height, rb.height) * 0.3f;
    return (dx < wt && dy < ht);
}

// copied from OpenCV 1.1.0 enteringblobdetection.cpp
void Decoder::findBlobsByCCClasters(CvSeq** clasters, int&
                                    claster_num, CvSeq** cnt_list)
{
    /* Create contours: */
    IplImage* pIB = NULL;
    CvSeq* cnt = NULL;
    CvMemStorage* storage = cvCreateMemStorage();
    *cnt_list = cvCreateSeq(0, sizeof(CvSeq), sizeof(CvSeq*), storage);
    *clasters = NULL;

    pIB = cvCloneImage(fgmask);
    cvThreshold(pIB, pIB, 128, 255, CV_THRESH_BINARY);
    cvFindContours(pIB, storage, &cnt, sizeof(CvContour), CV_RETR_EXTERNAL);
    cvReleaseImage(&pIB);

    for(; cnt; cnt = cnt->h_next)
    {
        cvSeqPush(*cnt_list, &cnt);
    }

    claster_num = cvSeqPartition(*cnt_list, storage, clasters, compareContour, NULL);
}

struct blob
{
    QPointF center;
    double area;
};

QString Decoder::findBlobs(Frame *frame, bool drawContours)
{
    double startTime = frame->getTime();
    double endTime = frame->getTime();
    int camera = frame->getCamera();
    CvSeq* clasters = NULL;
    CvSeq* cnt_list = NULL;
    int claster_num;
    std::vector<struct blob> blobs;

    QString result;
    QTextStream stream(&result);

    findBlobsByCCClasters(&clasters, claster_num, &cnt_list);

    double m00, m01, m10;
    int cx, cy;
    for(int claster_cur = 0; claster_cur < claster_num; ++claster_cur)
    {
        for(int cnt_cur = 0; cnt_cur < clasters->total; ++cnt_cur)
        {
            CvSeq* cnt;
            int k = *(int*)cvGetSeqElem(clasters, cnt_cur);
            if(k != claster_cur) continue;
            cnt = *(CvSeq**)cvGetSeqElem(cnt_list, cnt_cur);

            CvTreeNodeIterator iterator;
            int maxLevel = 0;
            cvInitTreeNodeIterator(&iterator, cnt, maxLevel);
            while((cnt = (CvSeq*)cvNextTreeNode(&iterator)) != 0)
            {
                CvMoments moments;
                cvMoments(cnt, &moments);

                m00 = cvGetSpatialMoment(&moments, 0, 0);
                m01 = cvGetSpatialMoment(&moments, 0, 1);
                m10 = cvGetSpatialMoment(&moments, 1, 0);
                cx = (int)(m10 / m00);
                cy = (int)(m01 / m00);

                QPair<double,double> p = CameraModel::warpToGround(camera,  QPair<int,int>(cx, cy));

                struct blob b;
                b.center = QPointF(p.first, p.second);
                b.area = m00;
                blobs.push_back(b);

                if(drawContours)
                    cvDrawContours(frame->getImage(), cnt, cvScalarAll(255), cvScalarAll(255), 100);
            }
        }
    }

    //double threshold = 20.0; // in feet
    //std::vector<coordinate> bigblobs;
    //group_blobs(blobs_temp, threshold, bigblobs);

    for(std::vector<struct blob>::iterator it = blobs.begin(); it != blobs.end(); ++it)
    {
        stream << "\t<CameraDetection camera=\"" << camera << "\" "
                << "area=\"" << it->area << "\" "
                << "lat=\"" << (it->center).x() << "\" "
                << "lon=\"" << (it->center).y() << "\" "
                << "startTime=\"" << startTime << "\" "
                << "endTime=\"" << endTime << "\" />\n";
    }

    return result;
}


/*
void group_blobs(std::vector<coordinate> blobs_in, double threshold, std::vector<coordinate>& bigblobs)
{
    std::vector<coordinate> nearme;
    std::vector<coordinate> ob, *othera, *otherb, *t;
    double dist, cx, cy, w, h;
    int count;

    othera = &blobs_in;
    otherb = &ob;

    while (othera->size() > 0) {
        // Start with some blob, any blob, call it the focus
        coordinate focus = othera->back();
        othera->pop_back();

        // Sums of coordinates for centroid
        cx = focus.x;
        cy = focus.y;
        w = focus.w;
        h = focus.h;
        count = 1;

        for (;;) {
            // Identify every blob near focus
            while (othera->size() > 0) {
                // Pop a potential neighbor
                coordinate consider = othera->back();
                othera->pop_back();

                // How far from focus?
                dist = focus.distance(consider.x, consider.y);

                if (dist <= threshold) {
                    // Put it in neighbor bin
                    nearme.push_back(consider);
                } else {
                    // Put in not-neighbor bin
                    otherb->push_back(consider);
                }
            }

            // At this point, othera is empty
            // otherb contains everything not (so far) in the group
            // Swap the pointers so that we can iterate
            // those not in the group
            t = othera;
            othera = otherb;
            otherb = t;

            // If there are no more neighbors to consider, we are done
            if (nearme.size() <= 0) break;

            // Get a neighbor, so we can look for its neighbors
            // Make it the new focus.
            focus = nearme.back();
            nearme.pop_back();

            // Add the focus to the centroid.  Every blob in the
            // group becomes focus at some point.
            cx += focus.x;
            cy += focus.y;
            w += focus.w;
            h += focus.h;
            count++;
        }

        // Compute the centroid for the group and add it to the list
        cx /= count;
        cy /= count;
        w /= count;
        h /= count;
        bigblobs.push_back(coordinate(cx, cy, w, h));
    }
}
*/
