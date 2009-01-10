#include "cvaux.h"
#include "highgui.h"
#include <stdio.h>


void cvFindBlobsByCCClasters(IplImage* pFG, CvBlobSeq* pBlobs, CvMemStorage* storage);

void findAndPrintBlobs(CvBGStatModel* bg_model, char* source, int fr, int fps)
{
  CvBlobSeq *blobs;
  CvMemStorage *storage;

  blobs = new CvBlobSeq();
  storage = cvCreateMemStorage();
  cvFindBlobsByCCClasters(bg_model->foreground, blobs, storage);
  
  for(int i = blobs->GetBlobNum(); i > 0; i--)
    {
      CvBlob* b = blobs->GetBlob(i - 1);
      printf("<Acquisition source=\"%s\" x=\"%.0f\" y=\"%.0f\" w=\"%.0f\" h=\"%.0f\" t=\"%.4f\" />\n",
	     source, b->x, b->y, b->w, b->h, (double)fr * (1.0/(double)fps));
    }
}

int main(int argc, char** argv)
{
  IplImage* tmp_frame_0 = NULL;
  IplImage* tmp_frame_1 = NULL;
  CvCapture* cap_0 = NULL;
  CvCapture* cap_1 = NULL;

  if(argc < 4)
    {
      printf("Usage: %s <video file 0> <source id 0> <video file 1> <source id 1> <frames per second>\n", argv[0]);
      exit(0);
    }
  
  char *vid_0 = argv[1];
  char *source_0 = argv[2];
  char *vid_1 = argv[3];
  char *source_1 = argv[4];
  int fps = atoi(argv[5]);
  
  cap_0 = cvCaptureFromFile(vid_0);
  cap_1 = cvCaptureFromFile(vid_1);
  
  tmp_frame_0 = cvQueryFrame(cap_0);
  if(!tmp_frame_0)
    {
      printf("bad video for file 0\n");
      exit(0);
    }
  
  tmp_frame_1 = cvQueryFrame(cap_1);
  if(!tmp_frame_1)
    {
      printf("bad video for file 1\n");
      exit(0);
    }
  
  //cvNamedWindow("BG", 1);
  //cvNamedWindow("FG", 1);
  
  //create BG models
  CvBGStatModel* bg_model_0 = cvCreateFGDStatModel(tmp_frame_0);
  CvBGStatModel* bg_model_1 = cvCreateFGDStatModel(tmp_frame_1);
  
  printf("<Frames>\n");
  
  for(int fr = 1;
      (tmp_frame_0 && tmp_frame_1);
      tmp_frame_0 = cvQueryFrame(cap_0), tmp_frame_1 = cvQueryFrame(cap_1), fr++)
    {
      printf("<Frame time=\"%.4f\" number=\"%d\">\n", (double)fr * (1.0/(double)fps), fr);
      
      //double t = (double)cvGetTickCount();
      
      // video 0
      cvUpdateBGStatModel(tmp_frame_0, bg_model_0);
      findAndPrintBlobs(bg_model_0, source_0, fr, fps);
      
      // video 1
      cvUpdateBGStatModel(tmp_frame_1, bg_model_1);
      findAndPrintBlobs(bg_model_1, source_1, fr, fps);
      
      //t = (double)cvGetTickCount() - t;
      //printf( "%.1f\n", t/(cvGetTickFrequency()*1000.) );
      //cvShowImage("BG", bg_model->background);
      //cvShowImage("FG", bg_model->foreground);
      
      cvWaitKey(5);
      
      printf("</Frame>\n");
    }
  
  printf("</Frames>\n");
  
  cvReleaseBGStatModel(&bg_model_0);
  cvReleaseCapture(&cap_0);
  
  cvReleaseBGStatModel(&bg_model_1);
  cvReleaseCapture(&cap_1);
  
  return 0;
}
