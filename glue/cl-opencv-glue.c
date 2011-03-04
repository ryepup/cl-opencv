/* cl-opencv-glue.c */
#include "cl-opencv-glue.h"

void cvAbsDiffS_glue(const CvArr *src, CvArr *dest,
		     double s1, double s2, double s3, double s4)
{
    cvAbsDiffS(src, dest, cvScalar(s1, s2, s3, s4));
}
