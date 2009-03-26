
#include <QString>
#include <QFile>
#include <QTextStream>
#include <QDebug>

#include <math.h>

#include "opencv/highgui.h"

#include "decoder.h"
#include "frame.h"

Decoder::Decoder(int c)
  : camera(c), bg_model(0)
{
  QFile warp_file(QString("camera-") +
		  QString::number(camera) + "-warp.xml");
  warp = (CvMat*)cvLoad(warp_file.fileName().toAscii().constData());
  if(warp == NULL)
    {
      qDebug() << "Error loading " << warp_file.fileName();
    }
}

QString Decoder::decodeFrame(Frame *frame)
{
  IplImage *image = frame->getImage();

  QString result;

  if(!bg_model)
    {
      bg_model = cvCreateFGDStatModel(image);
    }
  else
    {
      cvUpdateBGStatModel(image, bg_model);
      result = findBlobs();
    }

  return result;
}

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
void Decoder::findBlobsByCCClasters(CvSeq** clasters, int& claster_num, CvSeq** cnt_list)
{
  /* Create contours: */
  IplImage* pIB = NULL;
  CvSeq* cnt = NULL;
  CvMemStorage* storage = cvCreateMemStorage();
  *cnt_list = cvCreateSeq(0, sizeof(CvSeq), sizeof(CvSeq*), storage);
  *clasters = NULL;
  
  pIB = cvCloneImage(bg_model->foreground);
  cvThreshold(pIB, pIB, 128, 255, CV_THRESH_BINARY);
  cvFindContours(pIB, storage, &cnt, sizeof(CvContour), CV_RETR_EXTERNAL);
  cvReleaseImage(&pIB);
  
  for(; cnt; cnt = cnt->h_next)
    {
      cvSeqPush(*cnt_list, &cnt);
    }
  
  claster_num = cvSeqPartition(*cnt_list, storage, clasters, compareContour, NULL);
}

QString Decoder::findBlobs()
{
  CvSeq* clasters = NULL;
  CvSeq* cnt_list = NULL;
  int claster_num;

  QString result;
  QTextStream stream(&result);

  findBlobsByCCClasters(&clasters, claster_num, &cnt_list);

  int contour_id = 0;

  CvRect rect;
  double area;
  int cx, cy;
  CvMat *pmat = cvCreateMat(3, 1, CV_32FC1);
  CvMat *rmat = cvCreateMat(3, 1, CV_32FC1);
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
	      area = fabs(cvContourArea(cnt));
	      rect = cvBoundingRect(cnt);

	      cx = rect.x + rect.width / 2;
	      cy = rect.y + rect.height / 2;

	      cvSetReal1D(pmat, 0, cx);
	      cvSetReal1D(pmat, 1, cy);
	      cvSetReal1D(pmat, 2, 1);

	      cvMatMul(warp, pmat, rmat);

	      cx = cvGetReal1D(rmat, 0) / cvGetReal1D(rmat, 2);
	      cy = cvGetReal1D(rmat, 1) / cvGetReal1D(rmat, 2);

	      stream << "<Detection camera=\"" << camera << "\" "
		     << "id=\"" << contour_id << "\" "
		     << "area=\"" << area << "\" "
		     << "cx=\"" << cx << "\" "
		     << "cy=\"" << cy << "\" />";

	      contour_id++;
	    }
	}
    }

  return result;
}
