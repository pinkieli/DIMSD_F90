SUBROUTINE OpenOutFiles()
!////////////////////////////////////////////////////////////////////////// |
!	OpenOutFiles:													|
!	 	open files for outputs										|
! 																	|
!	Require:															|
! 		ModuleParameter.f90											|
! 		ModuleIoPort.f90												|
! 		Cksep.f90													|
! 		Pcomp.f90													|
!		Command2Number.f90											|
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ |

USE ModuleParameter

IMPLICIT NONE
INTEGER :: I

IF (DispFlag) THEN
	DO I=1,N_DispDof
		OPEN(UNIT=OutputDisp(I), FILE=DispFileName(I), STATUS='UNKNOWN')
	END DO
END IF

IF (VeloFlag) THEN
	DO I=1,N_VeloDof
		OPEN(UNIT=OutputVelo(I), FILE=VeloFileName(I), STATUS='UNKNOWN')
	END DO
END IF

IF (AcceFlag) THEN
	DO I=1,N_AcceDof
		OPEN(UNIT=OutputAcce(I), FILE=AcceFileName(I), STATUS='UNKNOWN')
	END DO
END IF

END SUBROUTINE OpenOutFiles