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

  srcQuad[0].x = 852;
  srcQuad[0].y = 523;
  srcQuad[1].x = 852;
  srcQuad[1].y = 560;
  srcQuad[2].x = 925;
  srcQuad[2].y = 560;
  srcQuad[3].x = 925;
  srcQuad[3].y = 523;

  dstQuad[0].x = 1260;
  dstQuad[0].y = 1115;
  dstQuad[1].x = 1260;
  dstQuad[1].y = 1300;
  dstQuad[2].x = 1625;
  dstQuad[2].y = 1300;
  dstQuad[3].x = 1625;
  dstQuad[3].y = 1115;

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
