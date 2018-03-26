SUBROUTINE Write2File(A,B,C)
!////////////////////////////////////////////////////////////////////////// |
!	Write2Files:														|
!	 	write time and value to files.									|
! 																	|
! 	Input:															|
! 		A --> time													|
! 		B --> vector of displacement, velocity or acceleration.			|
! 		C --> whose value is   											|
! 			'd' --> displacement										|
! 			'v' --> velocity											|
! 			'A' --> acceleration										|
!	Require:															|
! 		ModuleParameter.f90											|
! 		ModuleIoPort.f90												|
! 		Cksep.f90													|
! 		Pcomp.f90													|
!		Command2Number.f90											|
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ |

USE ModuleParameter
USE ModuleIoport

IMPLICIT NONE 

REAL :: A,B(*)
INTEGER :: I
CHARACTER :: C

LOGICAL :: pcomp

! Write value of displacement to Disp_filename.
IF (DispFlag .AND. pcomp(C,'d',1)) THEN
	DO I=1,N_DispDof
		WRITE (OutputDisp(I), 201) A,B(Disp_Dof(I))
	END DO
END IF  

! WRITE  value of displacement to Velo_filename.		
IF (VeloFlag .AND. pcomp(C,'v',1)) THEN
	DO I=1,N_VeloDof
		WRITE (OutputVelo(I), 201) A,B(Velo_Dof(I))
	END DO
END IF  

! WRITE  value of displacement to Disp_filename.
IF (AcceFlag .AND. pcomp(C,'a',1)) THEN
	DO I=1,N_AcceDof
		WRITE (OutputAcce(I), 201) A,B(Acce_Dof(I))
	END DO
END IF  
201 FORMAT (F12.6, 4X, F12.6)

END SUBROUTINE Write2File