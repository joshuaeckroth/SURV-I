
#include <opencv/cv.h>
#include <opencv/highgui.h>

int main(int argc, char **argv)
{
  int numpoints = 28;
  CvMat *east_points = cvCreateMat(2, numpoints, CV_32FC1);
  CvMat *west_points = cvCreateMat(2, numpoints, CV_32FC1);

  /* Set east points */
  cvmSet(east_points, 0, 0, 613.0);
  cvmSet(east_points, 1, 0, 134.0);
  
  cvmSet(east_points, 0, 1, 605.0);
  cvmSet(east_points, 1, 1, 137.0);
  
  cvmSet(east_points, 0, 2, 587.0);
  cvmSet(east_points, 1, 2, 141.0);

  cvmSet(east_points, 0, 3, 511.0);
  cvmSet(east_points, 1, 3, 158.0);
  
  cvmSet(east_points, 0, 4, 507.0);
  cvmSet(east_points, 1, 4, 163.0);

  cvmSet(east_points, 0, 5, 462.0);
  cvmSet(east_points, 1, 5, 173.0);

  cvmSet(east_points, 0, 6, 616.0);
  cvmSet(east_points, 1, 6, 161.0);
  
  cvmSet(east_points, 0, 7, 473.0);
  cvmSet(east_points, 1, 7, 168.0);

  cvmSet(east_points, 0, 8, 133.0);
  cvmSet(east_points, 1, 8, 139.0);

  cvmSet(east_points, 0, 9, 613.0);
  cvmSet(east_points, 1, 9, 162.0);

  cvmSet(east_points, 0, 10, 584.0);
  cvmSet(east_points, 1, 10, 112.0);

  cvmSet(east_points, 0, 11, 575.0);
  cvmSet(east_points, 1, 11, 114.0);

  cvmSet(east_points, 0, 12, 144.0);
  cvmSet(east_points, 1, 12, 108.0);

  cvmSet(east_points, 0, 13, 165.0);
  cvmSet(east_points, 1, 13, 113.0);

  cvmSet(east_points, 0, 14, 120.0);
  cvmSet(east_points, 1, 14, 113.0);

  cvmSet(east_points, 0, 15, 188.0);
  cvmSet(east_points, 1, 15, 132.0);

  cvmSet(east_points, 0, 16, 95.0);
  cvmSet(east_points, 1, 16, 127.0);

  cvmSet(east_points, 0, 17, 62.0);
  cvmSet(east_points, 1, 17, 148.0);

  cvmSet(east_points, 0, 18, 119.0);
  cvmSet(east_points, 1, 18, 137.0);

  cvmSet(east_points, 0, 19, 224.0);
  cvmSet(east_points, 1, 19, 139.0);

  cvmSet(east_points, 0, 20, 424.0);
  cvmSet(east_points, 1, 20, 138.0);

  cvmSet(east_points, 0, 21, 636.0);
  cvmSet(east_points, 1, 21, 139.0);

  cvmSet(east_points, 0, 22, 568.0);
  cvmSet(east_points, 1, 22, 136.0);

  cvmSet(east_points, 0, 23, 497.0);
  cvmSet(east_points, 1, 23, 96.0);

  cvmSet(east_points, 0, 24, 448.0);
  cvmSet(east_points, 1, 24, 126.0);

  cvmSet(east_points, 0, 25, 591.0);
  cvmSet(east_points, 1, 25, 130.0);

  cvmSet(east_points, 0, 26, 80.0);
  cvmSet(east_points, 1, 26, 70.0);

  cvmSet(east_points, 0, 27, 110.0);
  cvmSet(east_points, 1, 27, 88.0);

  /* West points */
  cvmSet(west_points, 0, 0, 169.0);
  cvmSet(west_points, 1, 0, 289.0);
  
  cvmSet(west_points, 0, 1, 154.0);
  cvmSet(west_points, 1, 1, 284.0);

  cvmSet(west_points, 0, 2, 137.0);
  cvmSet(west_points, 1, 2, 279.0);

  cvmSet(west_points, 0, 3, 76.0);
  cvmSet(west_points, 1, 3, 257.0);

  cvmSet(west_points, 0, 4, 62.0);
  cvmSet(west_points, 1, 4, 256.0);

  cvmSet(west_points, 0, 5, 40.0);
  cvmSet(west_points, 1, 5, 247.0);

  cvmSet(west_points, 0, 6, 72.0);
  cvmSet(west_points, 1, 6, 289.0);

  cvmSet(west_points, 0, 7, 52.0);
  cvmSet(west_points, 1, 7, 248.0);

  cvmSet(west_points, 0, 8, 108.0);
  cvmSet(west_points, 1, 8, 185.0);

  cvmSet(west_points, 0, 9, 70.0);
  cvmSet(west_points, 1, 9, 286.0);

  cvmSet(west_points, 0, 10, 173.0);
  cvmSet(west_points, 1, 10, 254.0);

  cvmSet(west_points, 0, 11, 161.0);
  cvmSet(west_points, 1, 11, 252.0);

  cvmSet(west_points, 0, 12, 134.0);
  cvmSet(west_points, 1, 12, 170.0);

  cvmSet(west_points, 0, 13, 144.0);
  cvmSet(west_points, 1, 13, 177.0);

  cvmSet(west_points, 0, 14, 121.0);
  cvmSet(west_points, 1, 14, 169.0);

  cvmSet(west_points, 0, 15, 125.0);
  cvmSet(west_points, 1, 15, 191.0);

  cvmSet(west_points, 0, 16, 91.0);
  cvmSet(west_points, 1, 16, 171.0);

  cvmSet(west_points, 0, 17, 78.0);
  cvmSet(west_points, 1, 17, 178.0);

  cvmSet(west_points, 0, 18, 100.0);
  cvmSet(west_points, 1, 18, 180.0);

  cvmSet(west_points, 0, 19, 66.0);
  cvmSet(west_points, 1, 19, 189.0);

  cvmSet(west_points, 0, 20, 74.0);
  cvmSet(west_points, 1, 20, 221.0);

  cvmSet(west_points, 0, 21, 148.0);
  cvmSet(west_points, 1, 21, 298.0);

  cvmSet(west_points, 0, 22, 150.0);
  cvmSet(west_points, 1, 22, 271.0);

  cvmSet(west_points, 0, 23, 226.0);
  cvmSet(west_points, 1, 23, 229.0);

  cvmSet(west_points, 0, 24, 136.0);
  cvmSet(west_points, 1, 24, 230.0);

  cvmSet(west_points, 0, 25, 178.0);
  cvmSet(west_points, 1, 25, 279.0);

  cvmSet(west_points, 0, 26, 233.0);
  cvmSet(west_points, 1, 26, 149.0);

  cvmSet(west_points, 0, 27, 189.0);
  cvmSet(west_points, 1, 27, 160.0);

  CvPoint2D32f __east_points[numpoints], __west_points[numpoints];
  CvMat _east_points = cvMat(1, numpoints, CV_32FC2, __east_points);
  CvMat _west_points = cvMat(1, numpoints, CV_32FC2, __west_points);
  for(int i = 0; i < numpoints; i++)
    {
      __east_points[i] = cvPoint2D32f(cvmGet(east_points, 0, i),
				      cvmGet(east_points, 1, i));

      __west_points[i] = cvPoint2D32f(cvmGet(west_points, 0, i),
				      cvmGet(west_points, 1, i));
    }

  IplImage *east = cvLoadImage("../videos/plse1.png");
  IplImage *west = cvLoadImage("../videos/plsw1.png");

  CvMat *fmat = cvCreateMat(3, 3, CV_32FC1);
  CvMat *status = cvCreateMat(1, numpoints, CV_8UC1);

  int fm_count = cvFindFundamentalMat(west_points, east_points, fmat,
				      CV_FM_RANSAC, 3, 0.99, status);

  CvSize imgsize = cvSize(west->width, west->height);
  CvMat *east_homo = cvCreateMat(3, 3, CV_64F);
  CvMat *west_homo = cvCreateMat(3, 3, CV_64F);
  cvStereoRectifyUncalibrated(&_west_points, &_east_points, fmat,
			      imgsize, west_homo, east_homo, 3);

  //cvSave("east_fmat.xml", fmat);
  cvSave("west_fmat.xml", fmat);

  printf("result of FindFundamentalMat: %d\n", fm_count);

  printf("point statuses:\n");
  for(int i = 0; i < numpoints; i++)
    {
      printf(" point %d: %f\n", i, cvGetReal1D(status, i));
    }

  for(int i = 0; i < 3; i++)
    {
      for(int j = 0; j < 3; j++)
	{
	  printf("fmat[%d][%d] = %f\n", i, j, cvmGet(fmat, i, j));
	}
    }

  for(int i = 0; i < 3; i++)
    {
      for(int j = 0; j < 3; j++)
	{
	  printf("east_homo[%d][%d] = %f\n", i, j, cvmGet(east_homo, i, j));
	}
    }

  for(int i = 0; i < 3; i++)
    {
      for(int j = 0; j < 3; j++)
	{
	  printf("west_homo[%d][%d] = %f\n", i, j, cvmGet(west_homo, i, j));
	}
    }

  for(int view = 0; view < 2; view++)
    {
      CvMat *corrLines = cvCreateMat(3, numpoints, CV_32F);
      if(view == 1)
	{
	  cvComputeCorrespondEpilines(west_points, 1, fmat, corrLines);
	}
      else
	{
	  cvComputeCorrespondEpilines(east_points, 2, fmat, corrLines);
	}
      
      CvMat *epiLine = cvCreateMat(1, 3, CV_32F);
      
      for(int i = 0; i < numpoints; i++)
	{
	  int j;
	  
	  for(j = 0; j < 3; j++)
	    {
	      cvmSet(epiLine, 0, j, cvmGet(corrLines, j, i));
	    }
	  
	  CvPoint epPoint1, epPoint2;
	  
	  CvMat *a = cvCreateMat(3, 1, CV_32F);
	  CvMat *b = cvCreateMat(3, 1, CV_32F);
	  CvMat *c = cvCreateMat(3, 1, CV_32F);
	  CvMat *d = cvCreateMat(3, 1, CV_32F);
	  
	  for(int j = 0; j < 3; j++)
	    {
	      cvSetReal2D(a, j, 0,
			  cvGetReal2D(epiLine, 0, j) / cvGetReal2D(epiLine, 0, 2));
	    }
	  
	  if (abs(cvGetReal2D(epiLine,0,0)) > abs(cvGetReal2D(epiLine,0,1)) )
	    {
	      
	      double ylim = west->height;
	      
	      cvSetReal2D(b,0,0,0);
	      cvSetReal2D(b,1,0,1);
	      cvSetReal2D(b,2,0,0);
	      
	      cvCrossProduct(a,b,c);
	      
	      for(j = 0; j < 3; j++)
		{
		  cvSetReal2D(c,j,0,cvGetReal2D(c,j,0)/cvGetReal2D(c,2,0));
		}
	      
	      cvSetReal2D(b,0,0,0);
	      cvSetReal2D(b,1,0,-1.0/ylim);
	      cvSetReal2D(b,2,0,1);
	      cvCrossProduct(a,b,d);
	      
	      for(j= 0; j < 3; j++)
		{
		  cvSetReal2D(d,j,0,cvGetReal2D(d,j,0)/cvGetReal2D(d,2,0));
		} 
	    }
	  else
	    {
	      double xlim = west->width;
	      cvSetReal2D(b,0,0,1);
	      cvSetReal2D(b,1,0,0);
	      cvSetReal2D(b,2,0,0);
	      
	      cvCrossProduct(a,b,c);
	      for(j = 0; j < 3; j++)
		{
		  cvSetReal2D(c,j,0,cvGetReal2D(c,j,0)/cvGetReal2D(c,2,0));
		}
	      
	      cvSetReal2D(b,0,0,-1.0/xlim);
	      cvSetReal2D(b,1,0,0);
	      cvSetReal2D(b,2,0,1);
	      cvCrossProduct(a,b,d);
	      for(j = 0; j < 3; j++)
		{
		  cvSetReal2D(d,j,0,cvGetReal2D(d,j,0)/cvGetReal2D(d,2,0));
		}
	      
	    }
	  
	  CvPoint p;
	  for(j = 0; j < numpoints; j++)
	    {
	      if(view == 1)
		{
		  p.x = cvmGet(east_points, 0, j);
		  p.y = cvmGet(east_points, 1, j);
		  //cvCircle(east, p, 10, CV_RGB(255, 255, 255));
		}
	      else
		{
		  p.x = cvmGet(west_points, 0, j);
		  p.y = cvmGet(west_points, 1, j);
		  //cvCircle(west, p, 10, CV_RGB(255, 255, 255));
		}
	    }
	  
	  epPoint1.x = cvmGet(c,0,0);
	  epPoint1.y = cvmGet(c,1,0);
	  
	  epPoint2.x = cvmGet(d,0,0);
	  epPoint2.y = cvmGet(d,1,0);
	  
	  if(view == 1)
	    {
	      //cvLine(east, epPoint1, epPoint2, CV_RGB(255,255,255));
	    }
	  else
	    {
	      //cvLine(west, epPoint1, epPoint2, CV_RGB(255,255,255));
	    }
	}
    }

  IplImage *east_trans = cvCreateImage(imgsize, east->depth, east->nChannels);
  IplImage *west_trans = cvCreateImage(imgsize, west->depth, west->nChannels);
  cvWarpPerspective(east, east_trans, east_homo);
  cvWarpPerspective(west, west_trans, west_homo);

  IplImage *east_trans_8bit = cvCreateImage(imgsize, east->depth, 1);
  IplImage *west_trans_8bit = cvCreateImage(imgsize, west->depth, 1);
  cvCvtColor(east_trans, east_trans_8bit, CV_BGR2GRAY);
  cvCvtColor(west_trans, west_trans_8bit, CV_BGR2GRAY);

  CvStereoBMState *bm = cvCreateStereoBMState();
  IplImage *disparity = cvCreateImage(imgsize, IPL_DEPTH_16S, 1);
  IplImage *vdisparity = cvCreateImage(imgsize, IPL_DEPTH_8U, 1);
  cvFindStereoCorrespondenceBM(east_trans_8bit, west_trans_8bit, disparity, bm);
  cvNormalize(disparity, vdisparity, 0, 256, CV_MINMAX );

  cvNamedWindow("east", CV_WINDOW_AUTOSIZE);
  cvNamedWindow("west", CV_WINDOW_AUTOSIZE);

  cvShowImage("east", east_trans_8bit);
  cvShowImage("west", west_trans_8bit);

  cvSaveImage("east-trans.png", east_trans);
  cvSaveImage("west-trans.png", west_trans);

  cvNamedWindow("disparity");
  cvShowImage("disparity", vdisparity);

  cvWaitKey(0);

  return 0;
}
