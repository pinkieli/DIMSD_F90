MODULE ModuleParameter

IMPLICIT NONE
!
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Define some UNITS
INTEGER :: InputFile=1000, InputData=1001
INTEGER :: RunDIMSD=2000
INTEGER, DIMENSION(:),ALLOCATABLE :: OutputDisp,OutputVelo,OutputAcce

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
INTEGER ::  N_DispDof, N_VeloDof, N_AcceDof,NDof, F_Dof, nstep,N_AlgoPara

REAL :: RaylCoef(1:2), dt, TotalTime,NodalForceId

LOGICAL :: DispFlag=.false., VeloFlag=.false.,&
		    AcceFlag=.false.,C_Exist=.false.

CHARACTER(4) :: M_Type, C_Type, IniD_Type, IniV_Type, F_Type

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Define ALLOCATABLE variables.
INTEGER, DIMENSION (:), ALLOCATABLE :: Disp_Dof,Velo_Dof,Acce_Dof,AlgoPara

REAL,DIMENSION(:,:), ALLOCATABLE:: K_Matrix, M_Matrix, C_Matrix

REAL,DIMENSION(:), ALLOCATABLE :: d0Vector, v0Vector, a0Vector

CHARACTER(LEN=:), ALLOCATABLE :: K_FileName,M_FileName,C_FileName, &
		 IniD_FileName,IniV_FileName, IntegrationTyle

CHARACTER(LEN = 255), DIMENSION (:), ALLOCATABLE :: DispFileName,&
		VeloFileName,AcceFileName

! In next content, DOF means Degree of Freedom.
! K_FileName - name the file where stiff matrix K is stored.
! M_FileName - name the file where mass matrix M is stored.
! C_FileName - name the file where damping matrix C is stored.
! IniD_FileName - name the file where initial displacement is stored.
! IniV_FileName - name the file where initial velocity is stored.
! ForceFileName - name the file where nodal force ID is stored.
! M_Type - type of the mass matrix. 'lump' for lumped, or 'cons' for consistent.
! C_Type - type of the damping matrix. 'file' means C will be imported from file.
!          'rayl' means Rayleigh Damping will be adopted.
! IniD_Type, IniV_Type - 'file' means initial conditions will be imported from files.
!                        'zero' means initial conditions are all zero.
! IntegrationTyle - Time Integration Method to be used.
! F_Type - type of nodal force ID. 'file' means reading ID from file.
!          'sing' (single) means only one node has force on it, number of DOF of this
!          node must be declared. 'zero' means no external force.
! MaxOutputDof - perhaps you want to output the results of some DOFs, the max number is
!                  defined by this varibale.
! N_DispDof - the number of the DOFs whose displacement will be outputted.
! N_VeloDof, N_AcceDof - similar to N_DispDof
! n_dof - total number of the DOFs in space.
! F_dof - if F_Type = 'sing', F_dof is the number of the DOF on which nodal force is imposed
! nstep - total number of time intervals.
! Disp_dof - an array. If you want to output the displacement of DOF 1, 3 and 4, then
!            Disp_dof is /1,3,4,0,0/.
! Velo_dof, Acce_dof - similar to Disp_dof
! DispFileName - a character-type array. to store the filenames where you want to output
!                 the displacement.
! VeloFileName,AcceFileName - similar to DispFileName
! DispFlag - if .true., displacement will be outputted to file.
! VeloFlag - if .true., velocity will be outputted to file.
! AcceFlag - if .true., acceleration  will be outputted to file.
! C_Exist - if .true., damping exists in this problem; .false. means no damping.
! RaylCoef - coefficients of Rayleigh damping. C = RaylCoef(1)*M + RaylCoef(2)*K
! dt - length of time step
! TotalTime - total time to be analyzed.
! prop_para - parameters for proportional force. let a(i)=prop_para(i),i=1,5
!             propforce(t) = a(1) + a(2)*t + a(3)*sin( a(4)*t + a(5) )
! TIM_para1,TIM_para2,TIM_para3 - parameters for Time Integration Method
END MODULE ModuleParameter