/* cl-opencv-glue.h
 * OpenCV glue functions. Most of these handle the cases were a struct is
 * either passed or returned by value.
 */

#include "cxcore.h"

/* void cvAbsDiffS(const CvArr* src, CvArr* dst, CvScalar value) */
void cvAbsDiffS_glue(const CvArr *src, CvArr *dest,
		     double s1, double s2, double s3, double s4);

