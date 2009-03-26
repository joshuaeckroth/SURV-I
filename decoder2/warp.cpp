// Usage: warp <image>
//
#include <cv.h>
#include <highgui.h>

int main(int argc, char** argv) {

  CvPoint2D32f srcQuad[4], dstQuad[4];
  CvMat*       warp_matrix  = cvCreateMat(3,3,CV_32FC1);
  IplImage     *src, *dst;
if( argc == 2 && ((src=cvLoadImage(argv[1],1)) != 0 )) {

  dst = cvCloneImage(src);
  dst->origin = src->origin;
  cvZero(dst);

  srcQuad[0].x = 232;
  srcQuad[0].y = 267;
  srcQuad[1].x = 397;
  srcQuad[1].y = 209;
  srcQuad[2].x = 167;
  srcQuad[2].y = 167;
  srcQuad[3].x = 6;
  srcQuad[3].y = 206;

  dstQuad[0].x = 0;
  dstQuad[0].y = 0;
  dstQuad[1].x = 0;
  dstQuad[1].y = 90;
  dstQuad[2].x = 120;
  dstQuad[2].y = 90;
  dstQuad[3].x = 120;
  dstQuad[3].y = 0;

  cvGetPerspectiveTransform(
    srcQuad,
    dstQuad,
    warp_matrix
  );
  for(int i = 0; i < 3; i++)
    {
      for(int j = 0; j < 3; j++)
	{
	  printf("warp[%d][%d] = %f\n", i, j, cvmGet(warp_matrix, i, j));
	}
    }
  cvWarpPerspective( src, dst, warp_matrix );
  cvNamedWindow( "Perspective_Warp", 1 );
    cvShowImage( "Perspective_Warp", dst );
    cvWaitKey();
  }
  cvReleaseImage(&dst);
  cvReleaseMat(&warp_matrix);
  return 0;
}
