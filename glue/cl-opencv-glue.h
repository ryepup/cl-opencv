/* cl-opencv-glue.h
 * OpenCV glue functions. Most of these handle the cases were a struct is
 * either passed or returned by value.
 */

#include "cxcore.h"
#include "cv.h"

#define CV_SCALAR_DECL(x) double x##1, double x##2, double x##3, double x##4
#define CV_SCALAR(x) cvScalar(x##1, x##2, x##3, x##4)

/* void cvAbsDiffS(const CvArr* src, CvArr* dst, CvScalar value) */
void cvAbsDiffS_glue(const CvArr *src, CvArr *dest, CV_SCALAR_DECL(value));

/* void cvSubS(const CvArr* src, CvScalar value, CvArr* dst, 
               const CvArr* mask=NULL) */
void cvSubS_glue(const CvArr *src, CV_SCALAR_DECL(value), CvArr *dest,
		 const CvArr *mask);

/* void cvSubRS(const CvArr* src, CvScalar value, CvArr* dst, 
                const CvArr* mask=NULL) */
void cvSubRS_glue(const CvArr* src, CV_SCALAR_DECL(value), CvArr* dest, 
		  const CvArr* mask);

/* void cvCopyMakeBorder(const CvArr* src, CvArr* dst, CvPoint offset, 
                         int bordertype, CvScalar value=cvScalarAll(0)) */
void cvCopyMakeBorder_glue(const CvArr* src, CvArr* dst, CvPoint offset, 
			   int bordertype, CV_SCALAR_DECL(value));
