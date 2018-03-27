SUBROUTINE DIMSD_F90()

USE ModuleIoPort

IMPLICIT NONE

WRITE(RunDIMSD,200)
200 FORMAT(1X,/,'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< DIMSD_F90 v1.0 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>',//,&
'      * * DIMSD_F90 v1.0* * : A Fortran package for solving structural dynamics  by		',/,&
'                                                     using direct Time Integration methods.        			 		',/)!,&

WRITE (RunDIMSD,300)
300 FORMAT (1X,'      Copyright (c) 2018. Jin-ze Li ')

WRITE (RunDIMSD,400)
400 FORMAT (1X,'      All rights reserved.'/)

WRITE(RunDIMSD,900)
900 FORMAT (1X,&
'===============================================================================',//,&
'	This version was developed and run on Gfortran  6.3.0 20170516 (Debian)		         ',/,&
'	Date: May 25, 2018                                                     							          		',/,&
'	Author: Jin-ze Li   ( Harbin Institute of Technology, China )          		          		',/,&
'	For any problems with the code, please contact the author:                                       		',/,&
'	E-mail: pinkie.ljz@gmail.com                                        						        		',/,&
'	       	       pinkie_li@163.com   													',//,&
'	Note: this code comes with no guarantee or warranty of any kind.					',//,&
'===============================================================================',/)
END SUBROUTINE DIMSD_F90