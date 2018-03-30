SUBROUTINE CloseOutFiles()
! Purpose: CLOSE  output files
USE ModuleIoport
USE ModuleParameter

IMPLICIT NONE

INTEGER :: I

IF(DispFlag) THEN
	DO I=1,N_DispDof
		CLOSE (UNIT=OutputDisp(I))
	END DO
END IF

IF(VeloFlag) THEN
	DO I=1,N_VeloDof
		CLOSE (UNIT=OutputVelo(I))
	END DO
END IF

IF(AcceFlag) THEN
	DO I=1,N_AcceDof
		CLOSE (UNIT=OutputAcce(I))
	END DO
END IF

DEALLOCATE(OutputDisp,OutputVelo,OutputAcce)

END SUBROUTINE CloseOutFiles