#include "cvaux.h"
#include "highgui.h"
#include <stdio.h>

void cvFindBlobsByCCClasters(IplImage* pFG, CvBlobSeq* pBlobs, CvMemStorage* storage);

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

void findAndPrintBlobs(CvBGStatModel* bg_model, int camera, Cv3dTrackerCameraInfo* camera_info, int fr, int fps, FILE *out)
{
  CvBlobSeq *blobs;
  CvMemStorage *storage;

  blobs = new CvBlobSeq();
  storage = cvCreateMemStorage();
  cvFindBlobsByCCClasters(bg_model->foreground, blobs, storage);

  for(int i = blobs->GetBlobNum(); i > 0; i--)
    {
      CvBlob* b = blobs->GetBlob(i - 1);
      CvPoint2D32f center = cvPoint2D32f(b->x, b->y);
      CvPoint3D32f center3d = ImageCStoWorldCS(camera_info[i], center);

      if((center3d.x > 1 || center3d.x < -1) && center3d.x < 1000000000
	  && (center3d.y > 1 || center3d.y < -1) && center3d.y < 1000000000
	  && (center3d.z > 1 || center3d.z < -1) && center3d.z < 1000000000
	  && b->w > 20 && b->h > 20)
	{
	  fprintf(out,
		  "<Acquisition source=\"camera-%d\" units=\"cm\" px=\"%.0f\" py=\"%.0f\" x=\"%.0f\" y=\"%.0f\" z=\"%.0f\" w=\"%.0f\" h=\"%.0f\" t=\"%.4f\" />\n",
		  camera, b->x, b->y, center3d.x, center3d.y, center3d.z, b->w, b->h, (double)fr * (1.0/(double)fps));
	}
    }
}

int getNextFrame(int i, CvCapture** captures, IplImage **samples, IplImage** mapx, IplImage **mapy)
{
  samples[i] = cvQueryFrame(captures[i]);
  if(samples[i] == NULL)
    {
      return 0;
    }

  IplImage* clone = cvCloneImage(samples[i]);
  cvRemap(clone, samples[i], mapx[i], mapy[i]);
  cvReleaseImage(&clone);

  return 1;
}

int main(int argc, char** argv)
{
  if(argc < 2)
    {
      printf("Usage: %s <number of cameras>\n", argv[0]);
      exit(0);
    }

  int num_cameras = atoi(argv[1]);
  int psize = num_cameras * sizeof(void*);

  int fps = 15;

  Cv3dTrackerCameraInfo* camera_info = (Cv3dTrackerCameraInfo*)malloc(num_cameras * sizeof(Cv3dTrackerCameraInfo));
  CvCapture** captures = (CvCapture**)malloc(psize);
  IplImage** samples = (IplImage**)malloc(psize);
  IplImage** mapx = (IplImage**)malloc(psize);
  IplImage** mapy = (IplImage**)malloc(psize);
  CvMat** intrinsics = (CvMat**)malloc(psize);
  CvMat** distortions = (CvMat**)malloc(psize);
  CvBGStatModel** bg_models = (CvBGStatModel**)malloc(psize);
  char** window_names = (char**)malloc(psize);
  char** fg_window_names = (char**)malloc(psize);
  char* intrinsics_file = (char*)malloc(30);
  char* distortion_file = (char*)malloc(30);
  char* calib3d_file = (char*)malloc(50);
  char* calib3d_principal_file = (char*)malloc(50);
  CvMat** M_calib3d = (CvMat**)malloc(psize);
  CvMat** M_calib3d_principal = (CvMat**)malloc(psize);

  for(int i = 0; i < num_cameras; i++)
    {
      captures[i] = cvCreateCameraCapture(i);
      if(captures[i] == NULL)
	{
	  printf("Failure capturing camera %d\n", i);
	  return -1;
	}
      printf("Got camera %d\n", i);

      window_names[i] = (char*)malloc(20);
      sprintf(window_names[i], "Camera %d", i);
      cvNamedWindow(window_names[i]);

      fg_window_names[i] = (char*)malloc(20);
      sprintf(fg_window_names[i], "Camera %d (FG)", i);
      cvNamedWindow(fg_window_names[i]);

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

      printf("Skipping 60 frames for camera %d\n", i);
      // get the Nth frame (so the camera can calm down and stop changing colors)
      for(int j = 0; j < 60; j++)
	{
	  samples[i] = cvQueryFrame(captures[i]);
	}

      mapx[i] = cvCreateImage(cvGetSize(samples[i]), IPL_DEPTH_32F, 1);
      mapy[i] = cvCreateImage(cvGetSize(samples[i]), IPL_DEPTH_32F, 1);
      cvInitUndistortMap(intrinsics[i], distortions[i], mapx[i], mapy[i]);

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

	  cvShowImage(window_names[i], samples[i]);

	  cvUpdateBGStatModel(samples[i], bg_models[i]);
	  findAndPrintBlobs(bg_models[i], i, camera_info, fr, fps, out);

	  cvShowImage(fg_window_names[i], bg_models[i]->foreground);
      
	  cvWaitKey(5);
	}
      fprintf(out, "</Frame>\n");
    }
  
  fprintf(out, "</Frames>\n");

  fclose(out);

  for(int i = 0; i < num_cameras; i++)
    {
      cvReleaseBGStatModel(&bg_models[i]);
      cvReleaseCapture(&captures[i]);
    }
  
  return 0;
}
