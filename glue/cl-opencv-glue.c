/* cl-opencv-glue.c */
#include "cl-opencv-glue.h"

void cvAbsDiffS_glue(const CvArr *src, CvArr *dest, CV_SCALAR_DECL(value))
{
    cvAbsDiffS(src, dest, CV_SCALAR(value));
}

void cvSubS_glue(const CvArr *src, CV_SCALAR_DECL(value), CvArr *dest, 
		 const CvArr *mask)
{
    cvSubS(src, CV_SCALAR(value), dest, mask);
}

void cvSubRS_glue(const CvArr* src, CV_SCALAR_DECL(value), CvArr* dest, 
		  const CvArr* mask)
{
     cvSubRS(src, CV_SCALAR(value), dest, mask);
}

void cvCopyMakeBorder_glue(const CvArr* src, CvArr* dst, CvPoint offset, 
			   int bordertype, CV_SCALAR_DECL(value))
{
    cvCopyMakeBorder(src, dst, offset, bordertype, CV_SCALAR(value));
}

