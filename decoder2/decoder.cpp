#include "cvaux.h"
#include "highgui.h"
#include <stdio.h>
#include <math.h>

bool live;

int cleanup(int num_cameras, CvCapture** captures, CvBGStatModel** bg_models, FILE *out)
{
  fprintf(out, "</Frames>\n");

  fclose(out);

  for(int i = 0; i < num_cameras; i++)
    {
      cvReleaseBGStatModel(&bg_models[i]);
      cvReleaseCapture(&captures[i]);
    }

  return 0;
} 


// copied from OpenCV 1.1.0 cv3dtracker.cpp
void MultVectorMatrix(float rv[4], const float v[4], const float m[4][4])
{
    for (int i=0; i<=3; i++)
    {
        rv[i] = 0.f;
        for (int j=0;j<=3;j++)
            rv[i] += v[j] * m[j][i];
    }
}

// copied from OpenCV 1.1.0 cv3dtracker.cpp
static CvPoint3D32f ImageCStoWorldCS(const Cv3dTrackerCameraInfo &camera_info, CvPoint2D32f p)
{
  float tp[4];
  tp[0] = (float)p.x - camera_info.principal_point.x;
  tp[1] = (float)p.y - camera_info.principal_point.y;
  tp[2] = 1.f;
  tp[3] = 1.f;
  
  float tr[4];
  //multiply tp by mat to get tr
  MultVectorMatrix(tr, tp, camera_info.mat);

  return cvPoint3D32f(tr[0]/tr[3], tr[1]/tr[3], tr[2]/tr[3]);
}

// copied from OpenCV 1.1.0 enteringblobdetection.cpp
static int CompareContour(const void* a, const void* b, void* )
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
void cvFindBlobsByCCClasters(IplImage* sample, IplImage* pFG, CvBlobSeq* pBlobs, CvMemStorage* storage, int camera)
{
  /* Create contours: */
  IplImage*       pIB = NULL;
  CvSeq*          cnt = NULL;
  CvSeq*          cnt_list = cvCreateSeq(0, sizeof(CvSeq), sizeof(CvSeq*), storage );
  CvSeq*          clasters = NULL;
  int             claster_cur, claster_num;
  
  pIB = cvCloneImage(pFG);
  cvThreshold(pIB, pIB, 128, 255, CV_THRESH_BINARY);
  cvFindContours(pIB, storage, &cnt, sizeof(CvContour), CV_RETR_EXTERNAL);
  cvReleaseImage(&pIB);
  
  /* Create cnt_list.      */
  /* Process each contour: */
  for(; cnt; cnt = cnt->h_next)
    {
      cvSeqPush(cnt_list, &cnt);
    }
  
  claster_num = cvSeqPartition(cnt_list, storage, &clasters, CompareContour, NULL);
  
  for(claster_cur = 0; claster_cur < claster_num; ++claster_cur)
    {
      int         cnt_cur;
      CvBlob      NewBlob;
      double      M00,X,Y,XX,YY; /* image moments */
      CvMoments   m;
      CvRect      rect_res = cvRect(-1,-1,-1,-1);
      CvMat       mat;
      
      for(cnt_cur=0; cnt_cur<clasters->total; ++cnt_cur)
        {
	  CvRect  rect;
	  CvSeq*  cnt;
	  int k = *(int*)cvGetSeqElem(clasters, cnt_cur);
	  if(k!=claster_cur) continue;
	  cnt = *(CvSeq**)cvGetSeqElem(cnt_list, cnt_cur);
	  rect = ((CvContour*)cnt)->rect;
	  
	  if(rect_res.height < 0)
            {
	      rect_res = rect;
            }
	  else
            {
	      /* Unite rects: */
	      int x0, x1, y0, y1;
	      x0 = MIN(rect_res.x, rect.x);
	      y0 = MIN(rect_res.y, rect.y);
	      x1 = MAX(rect_res.x + rect_res.width, rect.x + rect.width);
	      y1 = MAX(rect_res.y + rect_res.height, rect.y + rect.height);
	      rect_res.x = x0;
	      rect_res.y = y0;
	      rect_res.width = x1 - x0;
	      rect_res.height = y1 - y0;
            }
        }
      
      if(rect_res.height < 1 || rect_res.width < 1)
        {
	  X = 0;
	  Y = 0;
	  XX = 0;
	  YY = 0;
        }
      else
        {
	  cvMoments(cvGetSubRect(pFG, &mat, rect_res), &m, 0);
	  M00 = cvGetSpatialMoment(&m, 0, 0);
	  if(M00 <= 0) continue;
	  X = cvGetSpatialMoment(&m, 1, 0) / M00;
	  Y = cvGetSpatialMoment(&m, 0, 1) / M00;
	  XX = (cvGetSpatialMoment(&m, 2, 0) / M00) - X * X;
	  YY = (cvGetSpatialMoment(&m, 0, 2) / M00) - Y * Y;
        }
      NewBlob = cvBlob(rect_res.x + (float)X, rect_res.y + (float)Y, (float)(4 * sqrt(XX)), (float)(4 * sqrt(YY)));
      pBlobs->AddBlob(&NewBlob);
      
    }

  for(claster_cur = 0; claster_cur < claster_num; ++claster_cur)
    {
      int         cnt_cur;
      CvScalar    color = CV_RGB(rand()%256, rand()%256, rand()%256);
      
      for(cnt_cur = 0; cnt_cur < clasters->total; ++cnt_cur)
	{
	  CvSeq*  cnt;
	  int k = *(int*)cvGetSeqElem(clasters, cnt_cur);
	  if(k != claster_cur) continue;
	  cnt = *(CvSeq**)cvGetSeqElem(cnt_list, cnt_cur);
	  cvDrawContours(sample, cnt, color, color, 0, 1, 8);
	}
      
      CvBlob* pB = pBlobs->GetBlob(claster_cur);
      int x = cvRound(CV_BLOB_RX(pB)), y = cvRound(CV_BLOB_RY(pB));
      cvEllipse(sample,
		cvPointFrom32f(CV_BLOB_CENTER(pB)),
		cvSize(MAX(1, x), MAX(1, y)),
		0, 0, 360, 
		color, 1 );
    }
}


void findAndPrintBlobs(IplImage* sample, CvBGStatModel* bg_model, int camera,
		       Cv3dTrackerCameraInfo* camera_info, CvMat** warp,
		       int fr, int fps, FILE *out)
{
  CvBlobSeq *blobs;
  CvMemStorage *storage;
  CvPoint2D32f real_center;

  blobs = new CvBlobSeq();
  storage = cvCreateMemStorage();
  cvFindBlobsByCCClasters(sample, bg_model->foreground, blobs, storage, camera);

  for(int i = blobs->GetBlobNum(); i > 0; i--)
    {
      CvBlob* b = blobs->GetBlob(i - 1);

      if(live)
	{
	  CvPoint2D32f center = cvPoint2D32f(b->x, b->y);
	  CvPoint3D32f center3d = ImageCStoWorldCS(camera_info[i], center);
	  real_center = center;
	}
      else
	{
	  CvMat* pixel = cvCreateMat(3, 1, CV_32FC1);
	  cvmSet(pixel, 0, 0, b->x);
	  cvmSet(pixel, 1, 0, b->y);
	  cvmSet(pixel, 2, 0, 1.0);

	  CvMat* real = cvCreateMat(3, 1, CV_32FC1);
	  cvMatMul(warp[camera], pixel, real);

	  real_center = cvPoint2D32f(cvmGet(real, 0, 0) / cvmGet(real, 2, 0),
				     cvmGet(real, 1, 0) / cvmGet(real, 2, 0));
	}

      fprintf(out, "<Acquisition source=\"camera-%d\" units=\"ft\" px=\"%.0f\" py=\"%.0f\" x=\"%.5f\" y=\"%.5f\" t=\"%.4f\" />\n",
	      camera, b->x, b->y, real_center.x, real_center.y, (double)fr * (1.0/(double)fps));
    }
}

int getNextFrame(int i, CvCapture** captures, IplImage **samples, IplImage** mapx, IplImage **mapy)
{
  samples[i] = cvQueryFrame(captures[i]);
  if(samples[i] == NULL)
    {
      return 0;
    }

  if(live)
    {
      IplImage* clone = cvCloneImage(samples[i]);
      cvRemap(clone, samples[i], mapx[i], mapy[i]);
      cvReleaseImage(&clone);
    }

  return 1;
}

int main(int argc, char** argv)
{
  if(argc < 3)
    {
      printf("Usage: %s <v|c> <number>\n", argv[0]);
      exit(0);
    }

  if(strncmp(argv[1], "v", 1) == 0)
    {
      live = false;
    }
  else if(strncmp(argv[1], "c", 1) == 0)
    {
      live = true;
    }
  else
    {
      printf("Must specific 'v' or 'c'\n");
      return -1;
    }
  int num_cameras = atoi(argv[2]);
  int psize = num_cameras * sizeof(void*);

  int fps = 6;

  Cv3dTrackerCameraInfo* camera_info = (Cv3dTrackerCameraInfo*)malloc(num_cameras * sizeof(Cv3dTrackerCameraInfo));
  CvCapture** captures = (CvCapture**)malloc(psize);
  IplImage** samples = (IplImage**)malloc(psize);
  IplImage** mapx = (IplImage**)malloc(psize);
  IplImage** mapy = (IplImage**)malloc(psize);
  CvMat** intrinsics = (CvMat**)malloc(psize);
  CvMat** distortions = (CvMat**)malloc(psize);
  CvMat** warp = (CvMat**)malloc(psize);
  CvBGStatModel** bg_models = (CvBGStatModel**)malloc(psize);
  char** window_names = (char**)malloc(psize);
  char** fg_window_names = (char**)malloc(psize);
  char* intrinsics_file = (char*)malloc(30);
  char* distortion_file = (char*)malloc(30);
  char* warp_file = (char*)malloc(30);
  char* calib3d_file = (char*)malloc(50);
  char* calib3d_principal_file = (char*)malloc(50);
  CvMat** M_calib3d = (CvMat**)malloc(psize);
  CvMat** M_calib3d_principal = (CvMat**)malloc(psize);

  for(int i = 0; i < num_cameras; i++)
    {
      if(live)
	{
	  captures[i] = cvCreateCameraCapture(i);
	  if(captures[i] == NULL)
	    {
	      printf("Failure capturing camera %d\n", i);
	      return -1;
	    }
	  printf("Got camera %d\n", i);
	}
      else
	{
	  char* videofile = (char*)malloc(30);
	  sprintf(videofile, "camera-%d.avi", i);
	  captures[i] = cvCreateFileCapture(videofile);
	  if(captures[i] == NULL)
	    {
	      printf("Failure opening %s\n", videofile);
	      return -1;
	    }
	}

      window_names[i] = (char*)malloc(20);
      sprintf(window_names[i], "Camera %d", i);
      cvNamedWindow(window_names[i]);

      //fg_window_names[i] = (char*)malloc(20);
      //sprintf(fg_window_names[i], "Camera %d (FG)", i);
      //cvNamedWindow(fg_window_names[i]);

      if(live)
	{
	  sprintf(intrinsics_file, "camera-%d-intrinsics.xml", i);
	  sprintf(distortion_file, "camera-%d-distortion.xml", i);
	  intrinsics[i] = (CvMat*)cvLoad(intrinsics_file);
	  if(intrinsics[i] == NULL)
	    {
	      printf("Error loading %s\n", intrinsics_file);
	      return -1;
	    }
	  distortions[i] = (CvMat*)cvLoad(distortion_file);
	  if(distortions[i] == NULL)
	    {
	      printf("Error loading %s\n", distortion_file);
	      return -1;
	    }
	  
	  sprintf(calib3d_file, "camera-%d-calib3d.xml", i);
	  M_calib3d[i] = (CvMat*)cvLoad(calib3d_file);
	  if(M_calib3d[i] == NULL)
	    {
	      printf("Error loading %s\n", calib3d_file);
	      return -1;
	    }
	  
	  sprintf(calib3d_principal_file, "camera-%d-calib3d-principal.xml", i);
	  M_calib3d_principal[i] = (CvMat*)cvLoad(calib3d_principal_file);
	  if(M_calib3d_principal[i] == NULL)
	    {
	      printf("Error loading %s\n", calib3d_principal_file);
	      return -1;
	    }
      
	  CvScalar s;
	  for(int r = 0; r < 4; r++)
	    {
	      for(int c = 0; c < 4; c++)
		{
		  s = cvGet2D(M_calib3d[i], r, c);
		  camera_info[i].mat[r][c] = s.val[0];
		}
	    }
	  camera_info[i].principal_point.x = cvmGet(M_calib3d_principal[i], 0, 0);
	  camera_info[i].principal_point.y = cvmGet(M_calib3d_principal[i], 0, 1);
	}
      else
	{
	  sprintf(warp_file, "camera-%d-warp.xml", i);
	  warp[i] = (CvMat*)cvLoad(warp_file);
	  if(warp[i] == NULL)
	    {
	      printf("Error loading %s\n", warp_file);
	      return -1;
	    }
	}
	  
      if(live)
	{
	  printf("Skipping 60 frames for camera %d\n", i);
	  // get the Nth frame (so the camera can calm down and stop changing colors)
	  for(int j = 0; j < 60; j++)
	    {
	      samples[i] = cvQueryFrame(captures[i]);
	    }
	}
      else
	{
	  // get first frame
	  samples[i] = cvQueryFrame(captures[i]);
	}

      if(live)
	{
	  mapx[i] = cvCreateImage(cvGetSize(samples[i]), IPL_DEPTH_32F, 1);
	  mapy[i] = cvCreateImage(cvGetSize(samples[i]), IPL_DEPTH_32F, 1);
	  cvInitUndistortMap(intrinsics[i], distortions[i], mapx[i], mapy[i]);
	}

      if(!getNextFrame(i, captures, samples, mapx, mapy))
	{
	  printf("Error getting next frame for camera %d\n", i);
	  return -1;
	}
      
      printf("Creating BG subtraction model for camera %d\n", i);
      bg_models[i] = cvCreateFGDStatModel(samples[i]);
    }
  
  FILE *out = fopen("acquisitions.xml", "w");
  fprintf(out, "<Frames>\n");
  
  int c;
  for(int fr = 1; ; fr++)
    {
      fprintf(out, "<Frame time=\"%.4f\" number=\"%d\">\n", (double)fr * (1.0/(double)fps), fr);
      
      for(int i = 0; i < num_cameras; i++)
	{
	  if(!getNextFrame(i, captures, samples, mapx, mapy))
	    {
	      printf("Error getting next frame for camera %d\n", i);
	      return -1;
	    }

	  cvUpdateBGStatModel(samples[i], bg_models[i]);
	  findAndPrintBlobs(samples[i], bg_models[i], i, camera_info, warp, fr, fps, out);
	  cvShowImage(window_names[i], samples[i]);

	  //cvShowImage(fg_window_names[i], bg_models[i]->foreground);
      
	  c = cvWaitKey(5);
	  if(c == 'p')
	    {
	      c = 0;
	      while(c != 'p' && c != 27)
		{
		  c = cvWaitKey(250);
		}
	    }
	  if(c == 27)
	    {
	      fprintf(out, "</Frame>\n");
	      return cleanup(num_cameras, captures, bg_models, out);
	    }
	}
      fprintf(out, "</Frame>\n");
    }
  return cleanup(num_cameras, captures, bg_models, out);
}
