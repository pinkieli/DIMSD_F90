
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< DIMSD_F90 v1.0 >>>>>>>>>>>>>>>>>>>>>>>>>>> >>>>>>|

!///////////////////////////////////////////////////////////////////////////////|
!      * * DIMSD_F90 v1.0* * : A Fortran package for solving structural dynamics  by	|
!                                                using direct Time Integration methods.        			 	|
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
!          																    	|
!      Copyright (c) 2018, Jin-ze Li                                         						    	|
!      All rights reserved.     										              	|
!																	   	|
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 																	  	|
!	This version was developed and run on Gfortran  6.3.0 20170516 (Debian)		|
!	Date: May 25, 2018                                                     							          |
!	Author: Jin-ze Li   ( Harbin Institute of Technology, China )          		          |
!	For any problems with the code, please contact the author:                                       |
!	E-mail: pinkie.ljz@gmail.com                                        						          |
!	       	       pinkie_li@163.com   											|
!																		|
!	Note: this code comes with no guarantee or warranty of any kind.			|
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
PROGRAM MAIN
!///////////////////////////////////////////////////////////////////////////////|
!	This MAIN.f90 is a main function which solves motion of equations for 	          |
! 	structral dynamics.													|
!																	          |
! 	Require:															          |
! 		module_ioport.f90 												|
!		ReadPara.f90 													|
! 		ReadData.f90 													|
!		DirectTimeIntegration.f90										|
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
! USE ModuleIoPort
USE ModuleParameter

IMPLICIT NONE

! Declare deferred-length string : InputFileName
CHARACTER (LEN=:), ALLOCATABLE :: InputFileName
LOGICAL :: ExistInFile
INTEGER :: IERROR

OPEN(UNIT=RunDIMSD,FILE='DIMSD_F90',STATUS='UNKNOWN',&
	ACTION='WRITE',IOSTAT=IERROR)

CALL DIMSD_F90()

OPENIF: IF (IERROR==0) THEN
	! OPEN SUCCESSFULLY.
	ExistInFile = .FALSE.

	READFILE: DO WHILE (.NOT. ExistInFile)
		! Initial deferred-length string
		ALLOCATE (CHARACTER(LEN=255):: InputFileName)

		WRITE(*,100)
		100 FORMAT ('Specify the Input Filename:')
		READ(*,'(A)') InputFileName
		InputFileName = TRIM(InputFileName)

		INQUIRE(FILE=InputFileName,EXIST=ExistInFile)

		EXISTIF : IF (.NOT. ExistInFile) THEN
			WRITE(*,110)
			110 FORMAT(/' **ERROR** FILENAME: Specified input file does not' &
			 ' exist,reinput filename.'/)
			DEALLOCATE (InputFileName)
		END IF EXISTIF
	END DO READFILE

	WRITE(RunDIMSD,120) InputFileName
	120 FORMAT('The Input Filename is : "', A,'".'/ )

	CALL ReadPara(InputFileName)

	CALL ReadData()

	CALL DirectTimeIntegration()

ELSE OPENIF
	WRITE(*,10000) IERROR
	10000 FORMAT (1X,'*ERROR OPENING FILE* : IOSTAT = ', I6, &
		'values in the file.')

END IF OPENIF

CLOSE(UNIT=RunDIMSD)

DEALLOCATE (InputFileName,IntegrationTyle)

END PROGRAM MAIN
