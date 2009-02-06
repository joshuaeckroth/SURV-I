
#include <QString>
#include <QFile>
#include <QTextStream>
#include <QDebug>

#include <math.h>

#include "decoder.h"
#include "imagebuffer.h"

Decoder::Decoder(int c, int n)
  : camera(c), numCameras(n), bg_model(0), frame(0)
{
  fmat = (CvMat**)malloc(numCameras * sizeof(void*));
  fmat[0] = NULL; // self
  for(int i = 1; i < numCameras; i++)
    {
      QFile fmat_file(QString("camera-fmat-") +
		      QString::number(camera) + "-to-" +
		      QString::number((camera + i) % numCameras) + ".xml");
      fmat[i] = (CvMat*)cvLoad(fmat_file.fileName().toAscii().constData());
      if(fmat[i] == NULL)
	{
	  qDebug() << "Error loading " << fmat_file.fileName();
	}
    }
}

QString Decoder::decodeFrame(IplImage* f, int frameNum, double time)
{
  frame = f;
  QString result;
  QTextStream stream(&result);
  stream << "<Frame camera=\"" << camera << "\" "
	 << "number=\"" << frameNum << "\" "
	 << "time=\"" << time << "\">\n";

  if(!bg_model)
    {
      bg_model = cvCreateFGDStatModel(frame);
    }
  else
    {
      cvUpdateBGStatModel(frame, bg_model);
      stream << findBlobs();
    }

  stream << "</Frame>";

  return result;
}


// copied from OpenCV 1.1.0 cv3dtracker.cpp
void Decoder::multVectorMatrix(float rv[4], const float v[4], const float m[4][4])
{
    for (int i=0; i<=3; i++)
    {
        rv[i] = 0.f;
        for (int j=0;j<=3;j++)
            rv[i] += v[j] * m[j][i];
    }
}

// copied from OpenCV 1.1.0 cv3dtracker.cpp
/*
CvPoint3D32f Decoder::imageCStoWorldCS(CvPoint2D32f p)
{
  float tp[4];
  tp[0] = (float)p.x - camera_info.principal_point.x;
  tp[1] = (float)p.y - camera_info.principal_point.y;
  tp[2] = 1.f;
  tp[3] = 1.f;
  
  float tr[4];
  //multiply tp by mat to get tr
  multVectorMatrix(tr, tp, camera_info.mat);

  return cvPoint3D32f(tr[0]/tr[3], tr[1]/tr[3], tr[2]/tr[3]);
}
*/

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
  double ea, eb, ec;
  CvMat *pmat = cvCreateMat(1, 3, CV_32FC1);
  CvMat *rmat = cvCreateMat(1, 3, CV_32FC1);
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

	      stream << "<Acquisition id=\"" << contour_id << "\" "
		     << "area=\"" << area << "\" "
		     << "cx=\"" << cx << "\" "
		     << "cy=\"" << cy << "\">\n";

	      for(int i = 1; i < numCameras; i++)
		{
		  cvSetReal1D(pmat, 0, cx);
		  cvSetReal1D(pmat, 1, cy);
		  cvSetReal1D(pmat, 2, 1);

		  cvMatMul(pmat, fmat[i], rmat);

		  ea = cvGetReal1D(rmat, 0);
		  eb = cvGetReal1D(rmat, 1);
		  ec = cvGetReal1D(rmat, 2);
		  
		  stream << "<Epiline to=\"" << ((camera + i) % numCameras) << "\" "
			 << "ea=\"" << ea << "\" "
			 << "eb=\"" << eb << "\" "
			 << "ec=\"" << ec << "\" />\n";
		}

	      contour_id++;

	      CvSeqReader reader;
	      int i, count = cnt->total;
	      cvStartReadSeq(cnt, &reader, 0);
	      CvPoint pt1, pt2;
	      count -= !CV_IS_SEQ_CLOSED(cnt);

	      CV_READ_SEQ_ELEM(pt1, reader);

	      for(i = 0; i < count; i++)
		{
		  CV_READ_SEQ_ELEM(pt2, reader);

		  stream << "<Contour x1=\"" << pt1.x << "\" y1=\"" << pt1.y << "\" x2=\"" << pt2.x << "\" y2=\"" << pt2.y << "\" />\n";
		
		  pt1 = pt2;
		}

	      stream << "</Acquisition>\n";
	    }
	}
    }

  return result;
}
