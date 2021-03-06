SUBROUTINE ReadPara(InputFileName)
!///////////////////////////////////////////////////////////////////|
!	ReadPara														|
!	 	Read commands in InputFileName and parameters for each 			|
! 		command.													|
!	Input:															|
!		InputFileName --> Name of input file provided by user.			|
! 																	|
!	Require:														|
! 		ModuleParameter.f90											|
! 		ModuleIoPort.f90											|
! 		Cksep.f90													|
! 		Pcomp.f90													|
!		Command2Number.f90											|
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
USE ModuleParameter
USE ModuleIoPort

IMPLICIT NONE

! Declare local variables.
CHARACTER (LEN =*), INTENT(IN) :: InputFileName
CHARACTER (LEN=4) :: Command

CHARACTER (LEN=255) :: String
! CHARACTER (LEN = :), ALLOCATABLE :: StringStripped
!CHARACTER(LEN = :), ALLOCATABLE :: String!,StringStripped

LOGICAL :: ExistInFile,EndofFile
INTEGER :: I,J,K,LSS,IERROR

! Declare functions.
LOGICAL :: Cksep, Pcomp
INTEGER :: Command2Number,IndexCommaSpace
!
WRITE(RunDIMSD,100)
100   FORMAT (42X,  '========================================'/,&
			44X,'Begin to read commands and parameters'/,&
			42X,'========================================')

OPEN(UNIT=InputFile,FILE=InputFileName,STATUS='OLD',&
	ACTION='READ',IOSTAT = IERROR)

FILEOPEN: IF (IERROR == 0) THEN

	EndofFile = .FALSE.

	!ALLOCATE (CHARACTER(LEN=255) :: String)
	LOOPCOMMAND : DO WHILE (.NOT. EndofFile)

		READ(InputFile,'(A)') String
		Command(1:4) = ' '

		CALL String2CommandLine(String ,LSS)

		IF (LSS .EQ. 0) CYCLE

		I = IndexCommaSpace(String)

		Command(1:I-1) = String(1:I-1)

		WRITE(*,101) TRIM(Command)
		WRITE(RunDIMSD,101) TRIM(Command)
		101   FORMAT(/,'Current running command is "', A,'"')

		CASES: SELECT CASE (Command2Number(Command))

		CASE(1) ! 'END'
			EndofFile = .TRUE.

		CASE(2) ! 'K'
			CALL ReadStiffMatrix

		CASE(3) ! 'm    '
			CALL ReadMassMatrix

		CASE(4) ! 'c    '
			CALL ReadDampMatrix

		CASE(5) ! 'inid' - initial displacement
			CALL ReadInitialPara(IniD_type, IniD_filename)

		CASE(6) ! 'iniv' - initial velocity
			CALL ReadInitialPara(IniV_type, IniV_filename)

		CASE(7) ! 'dt  '
			READ(String(I+1:LSS), *) dt
			WRITE(RunDIMSD, 1113) dt
			1113 FORMAT (11X,'dt = ',F10.2,' (sec.)')
		CASE(8) ! 'time'
			READ(String(I+1:LSS), *) TotalTime
			WRITE(RunDIMSD, 1112)  TotalTime
			1112 FORMAT (11X,'Time = ',F10.2,' (sec.)')
		CASE(9) ! 'disp' - parameters of displacement outputs
			CALL ReadOutputPara(DispFlag,N_DispDof,Disp_Dof,DispFileName)

		CASE(10) ! 'velo' - parameters of velocity outputs
			CALL ReadOutputPara(VeloFlag,N_VeloDof,Velo_Dof,VeloFileName)

		CASE(11) ! 'acce' - parameters of acceleration outputs
			CALL ReadOutputPara(AcceFlag,N_AcceDof,Acce_Dof,AcceFileName)

		CASE(12) ! 'ndof' - total number of degree of freedom (in space)
			READ(String(I+1:LSS), '(I4)') NDof
			WRITE(RunDIMSD,1111) NDof
			1111 FORMAT (11X,'NDof = ',I4)

 		CASE(13) ! 'meth' - Time Integration method
			CALL ReadMethodPara

		CASE(14) ! 'fpro' - parameters of propotional force
			CALL ReadPropPara

		CASE(15) ! 'forc' - Read nodal force information
			CALL ReadNodalForce

		CASE DEFAULT
			WRITE(*,102) TRIM(Command)
			WRITE(RunDIMSD,102) TRIM(Command)
			102   FORMAT(/,'**Error **: "',A4,'" is a wrong' &
				'command ! Check input file!')
		END SELECT CASES

	END DO LOOPCOMMAND
	CLOSE(InputFile)
	WRITE(RunDIMSD,103)
	103   FORMAT (/,42X,'========================================'/,&
			          44X,'End of reading commands and parameters'/,&
				42X,'========================================')

ELSE FILEOPEN

	WRITE(*,104) IERROR
	104 FORMAT (1X,'** ERROR OPENING FILE ** : IOSTAT = ', I6, &
		'values in the file.')

END IF FILEOPEN
!
!###################################################################|
!													             	   |
!						SOME OF SUBROUTINES					   |
!                                        											   |
!###################################################################|
!
CONTAINS
!===================================================================
SUBROUTINE ReadStiffMatrix()

ALLOCATE( CHARACTER(LEN = LSS-I) :: K_FileName)
K_FileName(1:LSS-I) = String(I+1:LSS)

WRITE(RunDIMSD,131) K_FileName(1:LSS-I)
131	FORMAT(11X,'Stiff matrix in file :', A)

END SUBROUTINE ReadStiffMatrix
!===================================================================
SUBROUTINE ReadMassMatrix()

LOGICAL :: pcomp,Cksep
INTEGER :: IndexCommaSpace

M_Type = String(I+1:I+4)

WRITE(RunDIMSD,132) M_Type
132 FORMAT(11X,'Type of mass matrix is :', A)

J = IndexCommaSpace(String(I+1:LSS))

IF (5 .NE. J) THEN
	WRITE(*,134) InputFileName
	WRITE(RunDIMSD,134) InputFileName
	134	FORMAT(1X,'** ERROR **: There is a wrong command line for mass' &
				' matrix in input file :',A,'.')
	STOP 'Error occurs !!!'
ELSE
	ALLOCATE( CHARACTER(LEN = LSS-J) :: M_FileName)
	M_FileName(1:LSS-J) = String(I+J+1:LSS)

	WRITE(RunDIMSD,133) M_FileName(1:LSS-J)
	133	FORMAT(11X,'Mass matrix in file : ', A)
END IF
!
END SUBROUTINE ReadMassMatrix
!===================================================================
SUBROUTINE ReadDampMatrix()
LOGICAL :: pcomp,Cksep

C_Exist=.true. ! Damp exists.
C_Type = String(I+1:I+4)

WRITE(RunDIMSD,107) C_Type
107  FORMAT(11X, 'Type of damp matrix is :', A)

CTYPE: IF(pcomp(C_Type,'rayl',4)) THEN
	CRAYL: DO J=I+1,LSS
		IF( Cksep(String(J:J)) ) THEN
			DO K=J+1,LSS
				IF( Cksep(String(K:K)) ) THEN

					read(String(J+1:K-1), *) RaylCoef(1)
					read(String(K+1:LSS), *) RaylCoef(2)

					WRITE(RunDIMSD,108) RaylCoef(1)
					108 FORMAT(11X, 'The 1st rayleigh coefficient is ', f15.4)
					WRITE(RunDIMSD,109) RaylCoef(2)
					109 FORMAT(11X, 'The 2nd rayleigh coefficient is ', f15.4)
					RETURN
				END IF
			END DO
		END IF
	END DO CRAYL

ELSE IF (pcomp(C_Type, 'file',4)) THEN CTYPE
	CFILE: DO J= I+1, LSS
		IF( Cksep(String(J:J)) ) THEN
			C_filename(1:LSS-J)=String(J+1:LSS)
			WRITE(RunDIMSD,110) C_filename(1:LSS-J)
			110 FORMAT(11X, 'Damping matrix in file :---> ', A)
			RETURN
		END IF
	END DO CFILE

END IF CTYPE

END SUBROUTINE ReadDampMatrix
!
!===================================================================
!
SUBROUTINE ReadInitialPara(IniType, IniFile)
! Purpose: read parameters for initial conditions
! Outputs:
!          IniType - type of initial conditions ( 'zero' or 'file')
!          IniFile - name of the file where non-zero initial conditions are stored.

CHARACTER(LEN=4) :: IniType
CHARACTER(LEN=12) ::  IniFile

LOGICAL :: Cksep

IniType = String(I+1:I+4)
WRITE(RunDIMSD,112) IniType
112  FORMAT(11X, 'Method of specifying initial condition is :',A)

DO J= I+1, LSS
	IF( Cksep(String(J:J)) ) THEN
		IniFile(1:LSS-J)=String(J+1:LSS)
		WRITE(RunDIMSD,111) IniFile(1:LSS-J)
		111 FORMAT(11X, 'Data in file : ', A)
		RETURN
	END IF
END DO

END SUBROUTINE ReadInitialPara
!
!===================================================================
!
SUBROUTINE ReadOutputPara(OutputFlag, N_OutputDof, OutputDof, OutputFile)
! Purpose: read parameters for output macro command, such as 'disp','velo' and 'acce'
! Outputs:
!           OutputFlag - .true. means results of this type will be outputted.
!		  	OutputDof  - which degree of freedom DO you want ouput?
!			OutputFile - name of the file to which the results will be wrote.
LOGICAL :: OutputFlag
INTEGER, DIMENSION(MaxOutputDof) :: OutputDof
INTEGER :: N_OutputDof, k2
CHARACTER(12), DIMENSION(MaxOutputDof) :: OutputFile

LOGICAL :: Cksep

OutputFlag = .TRUE.
N_OutputDof=0

DO J=I+1,LSS
	IF( Cksep(String(J:J)) ) N_OutputDof = N_OutputDof + 1
END DO

N_OutputDof = int((N_OutputDof +1)/2)  ! total number of dofs to be outputted.
N_OutputDof = min(N_OutputDof, MaxOutputDof)

WRITE(RunDIMSD,121) N_OutputDof
121 FORMAT(11X,'Total number of outputted DOF is:', i2, &
				/,11X, 'No.',4X,'Number of DOF',4X,'Filename',&
				/,10X, '---',4X,'--------------',3X,'---------')
!
k2=1
J=I+1

DO while(k2 .le. N_OutputDof)
	J=J+1
	IF(Cksep(String(J:J))) THEN  ! find the separator after the interger No.k2
		K=J+1
		DO while (.not. Cksep(String(K:K)))
			K=K+1
		END DO

		read(String(I+1:J-1), '(i3)') OutputDof(k2)
		OutputFile(k2)=String(J+1:K-1)
		WRITE(RunDIMSD, 122) k2,OutputDof(k2),OutputFile(k2)
		122 FORMAT(11X,i2,18X,i4,21X,a12)

		k2=k2+1
		I=K
		J=I+1

	END IF
END DO

END SUBROUTINE ReadOutputPara
!
!===================================================================
!
SUBROUTINE ReadMethodPara()
! Declare local variables
LOGICAL :: pcomp,Cksep
INTEGER :: k2

IntegrationTyle = String(I+1:I+4)
WRITE(*,201) IntegrationTyle
201 FORMAT(1X,'Selected Direct Time Integration Method is :', A)
WRITE(RunDIMSD,202) IntegrationTyle
202 FORMAT(11X,'Selected Direct Time Integration Method is :', A)

DO J=I+1,LSS  ! search for the 2nd separator, after which is the TIM_para1
	IF( Cksep(String(J:J)) ) THEN
		! find the 2nd separator.
		DO K=J+1,LSS
			! search for the 3rd separator, after which is the TIM_para2
			IF( Cksep(String(K:K)) ) THEN
				! find the 3rd separator.
				READ(String(J+1:K-1), '(F5.2)') TIM_para1
				DO k2=K+1,LSS
					! search for the 4th separator, after which is the TIM_para3
					IF( Cksep(String(k2:k2)) ) THEN
						! find the 4th separator.
						READ(String(K+1:k2-1), '(F5.2)') TIM_para2
						READ(String(k2+1:LSS), '(F5.2)') TIM_para3
						RETURN
					END IF
				END DO
				! cann't find the 4th separator, so there are only 2 parameters.
		                READ(String(K+1:LSS), '(F5.2)') TIM_para2
				RETURN
			 END IF
		END DO
		! cann't find the 3rd separator, so there is only one parameter in this line.
		READ(String(J+1:LSS),'(F5.2)') TIM_para1
		RETURN
	END IF
END DO

END SUBROUTINE ReadMethodPara
!
!===================================================================
!
SUBROUTINE ReadPropPara()
!IMPLICIT NONE
INTEGER :: iprop

READ(String(I+1:LSS), *) (prop_para(iprop),iprop=1,5)

WRITE(RunDIMSD,211) (prop_para(iprop),iprop=1,5)
211 FORMAT(11X, 'Parameters of proptational force :',/,6X,5f12.4)

END SUBROUTINE ReadPropPara
!
!===================================================================
!
SUBROUTINE ReadNodalForce()
!implicit none
LOGICAL :: pcomp,Cksep

F_Type=String(I+1:I+4)

WRITE(RunDIMSD,141) F_Type
141 FORMAT(11X, 'Type of nodal force ID is : ', A4)

IF(pcomp(F_Type, 'file', 4)) THEN
	DO J= I+1, LSS
		IF( Cksep(String(J:J)) ) THEN
			ForceFileName(1:LSS-J)=String(J+1:LSS)
			WRITE(RunDIMSD, 142) ForceFileName(1:LSS-J)
			142 FORMAT(11X, 'Nodal force ID in file : ', A)
			RETURN
		END IF
	END DO

ELSE IF (pcomp(F_Type, 'sing', 4)) THEN
	DO J= I+1, LSS
		IF( Cksep(String(J:J)) ) THEN
			READ(String(J+1:LSS), '(i3)') F_Dof
			WRITE(RunDIMSD,143) F_Dof
			143 FORMAT(11X,'Nodal force is imposed on DOF ', I3)
			RETURN
		END IF
	END DO

ELSE IF(pcomp(F_Type, 'zero', 4) .OR. pcomp(F_Type, '    ', 4)) THEN
	WRITE(RunDIMSD,144)
	144 FORMAT(11X,'All nodal forces are zero')

ELSE
	WRITE(RunDIMSD,*) '** Error **: the type of nodal force is wrong !!!'
	WRITE(RunDIMSD,*) 'The type of nodal force is : ', F_Type
	STOP '** Error ** : the type of nodal force is wrong !!!'
END IF

END SUBROUTINE ReadNodalForce
!===================================================================
! END OF SUBROUTINE ReadPara

END SUBROUTINE ReadPara