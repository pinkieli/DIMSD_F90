SUBROUTINE DirectTimeIntegration ()
	!USE linear_operators
USE ModuleParameter
USE ModuleData
USE ModuleIoport
USE Force
USE m_gauss

IMPLICIT NONE

REAL :: f0(1:NDof), d0k(1:NDof),v0C(1:NDof),f_eff1(1:NDof)
! REAL, DIMENSION(NDof,NDof) :: M_Matrix1

! REAL :: ExternalForce
LOGICAL ::  pcomp

ALLOCATE (a0Vector(NDof))

CALL OpenOutFiles

WRITE(RunDIMSD, 302)
302   FORMAT (//,42X,  '========================================'/,&
			62X,'Begin to compute'/,&
			42X,'========================================')

! calculate initial acceleration
f0 = ExternalForce(0.0)*NodalForceId

! calculate initial acceleration
d0K = MATMUL(K_matrix,d0Vector)
v0C = MATMUL(C_matrix,v0Vector)
f_eff1 = f0- d0K - v0C
! M_Matrix1 = M_Matrix
!CALL sgesv(NDof,1,M_Matrix1,NDof,IPIV,f_eff1,NDof,INFO)
CALL solve(M_Matrix,f_eff1,a0Vector,NDof)

CALL Write2File(0.0, d0Vector,'d') ! WRITE d(0) to file
CALL Write2File(0.0, v0Vector,'v') ! WRITE v(0) to file
CALL Write2File(0.0, a0Vector,'a') ! WRITE a(0) to file


nstep= INT(TotalTime/dt)  ! number of time steps
WRITE(RunDIMSD, 301) nstep
301 FORMAT (/,'Total number of time intervals is : ', i5)
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
IF(pcomp(IntegrationTyle, 'Newmark',7)) THEN ! Newmark Method
	WRITE(RunDIMSD,300) 'Newmark'
	300 FORMAT (/,'Run ', a ,' method for time integration...')
	CALL Newmark()

ELSE
	WRITE(RunDIMSD, 304)
	304 FORMAT (/,'***Error : type fo Time Integration Method is wrong !')
	STOP '***Error : type fo Time Integration Method is wrong !'

END IF
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CALL CloseOutFiles
WRITE(RunDIMSD, 303)
303   FORMAT (/,42X,  '========================================'/,&
			62X,'End to compute'/,&
			42X,'========================================')

DEALLOCATE(a0Vector,v0Vector,d0Vector,Disp_Dof,Velo_Dof,Acce_Dof,&
	AlgoPara,DispFileName,VeloFileName,AcceFileName)

END SUBROUTINE DirectTimeIntegration