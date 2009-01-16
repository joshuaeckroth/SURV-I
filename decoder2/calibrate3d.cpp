#include <cv.h>
#include <cvaux.h>
#include <highgui.h>
#include <stdio.h>
#include <stdlib.h>

int board_w;
int board_h;
int num_cameras;

int main(int argc, char *argv[])
{
  if(argc != 4)
    {
      printf("Usage: %s <board width> <board height> <number of cameras>\n", argv[0]);
      return -1;
    }

  board_w = atoi(argv[1]);
  board_h = atoi(argv[2]);
  num_cameras = atoi(argv[3]);

  CvSize board_sz = cvSize(board_w, board_h);

  Cv3dTrackerCameraInfo* infos = (Cv3dTrackerCameraInfo*)malloc(num_cameras * sizeof(Cv3dTrackerCameraInfo));
  CvCapture** captures = (CvCapture**)malloc(num_cameras * sizeof(CvCapture*));
  IplImage** samples = (IplImage**)malloc(num_cameras * sizeof(IplImage*));
  IplImage** mapx = (IplImage**)malloc(num_cameras * sizeof(IplImage*));
  IplImage** mapy = (IplImage**)malloc(num_cameras * sizeof(IplImage*));
  CvMat** intrinsics = (CvMat**)malloc(num_cameras * sizeof(CvMat**));
  CvMat** distortions = (CvMat**)malloc(num_cameras * sizeof(CvMat**));
  Cv3dTrackerCameraIntrinsics* intrinsics3d =
    (Cv3dTrackerCameraIntrinsics*)malloc(num_cameras * sizeof(Cv3dTrackerCameraIntrinsics));
  char** window_names = (char**)malloc(num_cameras * sizeof(char*));
  char* intrinsics_file = (char*)malloc(30);
  char* distortion_file = (char*)malloc(30);
  for(int i = 0; i < num_cameras; i++)
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

      intrinsics3d[i].principal_point = cvPoint2D32f(cvmGet(intrinsics[i], 0, 2),
						      cvmGet(intrinsics[i], 1, 2));
      intrinsics3d[i].focal_length[0] = cvmGet(intrinsics[i], 0, 0);
      intrinsics3d[i].focal_length[1] = cvmGet(intrinsics[i], 1, 1);

      intrinsics3d[i].distortion[0] = cvmGet(distortions[i], 0, 0);
      intrinsics3d[i].distortion[1] = cvmGet(distortions[i], 1, 0);
      intrinsics3d[i].distortion[2] = cvmGet(distortions[i], 2, 0);
      intrinsics3d[i].distortion[3] = cvmGet(distortions[i], 3, 0);

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
    }

  int display = 1;
  int c;
  CvBool result = 0;
  IplImage* clone;
  char calib3d_file[20];
  char calib3d_principal_file[40];
  CvMat M_result;
  CvMat M_principal;
  float principal[1][2];
  while(display == 1)
    {
      for(int i = 0; i < num_cameras; i++)
	{
	  samples[i] = cvQueryFrame(captures[i]);
	  if(samples[i] == NULL)
	    {
	      display = 0;
	      break;
	    }

	  mapx[i] = cvCreateImage(cvGetSize(samples[i]), IPL_DEPTH_32F, 1);
	  mapy[i] = cvCreateImage(cvGetSize(samples[i]), IPL_DEPTH_32F, 1);
	  cvInitUndistortMap(intrinsics[i], distortions[i], mapx[i], mapy[i]);
	  clone = cvCloneImage(samples[i]);
	  cvRemap(clone, samples[i], mapx[i], mapy[i]);
	  cvReleaseImage(&clone);
	}

      if(display)
	{
	  c = cvWaitKey(15);
	  if(c == ' ')
	    {
	      result = cv3dTrackerCalibrateCameras(num_cameras, intrinsics3d, board_sz, 3.0, samples, infos);
	      if(result)
		{
		  printf("Success.\n");

		  for(int i = 0; i < num_cameras; i++)
		    {
		      M_result = cvMat(4, 4, CV_32FC2, &infos[i].mat);
		      sprintf(calib3d_file, "camera-%d-calib3d.xml", i);
		      cvSave(calib3d_file, &M_result);

		      M_principal = cvMat(1, 2, CV_32FC1, &principal);
		      cvmSet(&M_principal, 0, 0, infos[i].principal_point.x);
		      cvmSet(&M_principal, 0, 1, infos[i].principal_point.y);
		      sprintf(calib3d_principal_file, "camera-%d-calib3d-principal.xml", i);
		      cvSave(calib3d_principal_file, &M_principal);
		    }
		}
	      else
		{
		  printf("Failure.\n");
		}
	    }
	  else if(c == 'p')
	    {
	      c = 0;
	      while(c != 'p' && c != 27)
		{
		  c = cvWaitKey(250);
		}
	    }
	  if(c == 27)
	    break;

	  for(int i = 0; i < num_cameras; i++)
	    {
	      cvShowImage(window_names[i], samples[i]);
	    }
	}
    }

  return 0;
}
