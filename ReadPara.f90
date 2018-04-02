SUBROUTINE ReadPara(InputFileName)
!///////////////////////////////////////////////////////////////////|
!	ReadPara
!	 	Read commands in InputFileName and parameters for each
! 		command.
!	Input:
!		InputFileName ---> Name of input file provided by user.
!
!	Require:
! 		ModuleParameter.f90
!		IndexCommaSpace.f90
! 		Pcomp.f90
!		Command2Number.f90
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
USE ModuleParameter

IMPLICIT NONE

! Declare local variables.
CHARACTER (LEN =*), INTENT(IN) :: InputFileName
CHARACTER (LEN=4) :: Command
CHARACTER (LEN=255) :: String
LOGICAL :: ExistInFile,EndofFile
INTEGER :: I,J,K,LSS,IERROR

! Declare functions.
LOGICAL :: Pcomp
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
			CALL ReadInitialPara(Command)

		CASE(6) ! 'iniv' - initial velocity
			CALL ReadInitialPara(Command)

		CASE(7) ! 'dt  '
			READ(String(I+1:LSS), *) dt
			WRITE(RunDIMSD, 1113) dt
			1113 FORMAT (11X,'dt = ',F10.2,' (sec.)')

		CASE(8) ! 'time'
			READ(String(I+1:LSS), *) TotalTime
			WRITE(RunDIMSD, 1112)  TotalTime
			1112 FORMAT (11X,'Time = ',F10.2,' (sec.)')

		CASE(9) ! 'disp' - parameters of displacement outputs
			CALL ReadOutputDisp()

		CASE(10) ! 'velo' - parameters of velocity outputs
			CALL ReadOutputVelo()

		CASE(11) ! 'acce' - parameters of acceleration outputs
			CALL ReadOutputAcce()

		CASE(12) ! 'ndof' - total number of degree of freedom (in space)
			READ(String(I+1:LSS), '(I4)') NDof
			WRITE(RunDIMSD,1111) NDof
			1111 FORMAT (11X,'NDof = ',I4)

 		CASE(13) ! 'meth' - Time Integration method
			CALL ReadMethodPara()

		CASE(14) ! 'forc' - Read nodal force information
			CALL ReadNodalForce()

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
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CONTAINS!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!//////////////////////////////////////////////////////////////////|
!
! 					Subroutine : ReadStiffMatrix
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
SUBROUTINE ReadStiffMatrix()

ALLOCATE( CHARACTER(LEN = LSS-I) :: K_FileName)
K_FileName(1:LSS-I) = String(I+1:LSS)

WRITE(RunDIMSD,131) K_FileName(1:LSS-I)
131	FORMAT(11X,'Stiff matrix in file :', A)

END SUBROUTINE ReadStiffMatrix
!//////////////////////////////////////////////////////////////////|
!
! 					Subroutine : ReadMassMatrix
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
SUBROUTINE ReadMassMatrix()

LOGICAL :: pcomp
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
!//////////////////////////////////////////////////////////////////|
!
! 					Subroutine : ReadDampMatrix
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
SUBROUTINE ReadDampMatrix()
LOGICAL :: pcomp
C_Exist=.TRUE. ! Damp exists.
C_Type = String(I+1:I+4)

CTYPE: IF(pcomp(C_Type,'rayl',4)) THEN

	WRITE(RunDIMSD,107) C_Type
	107  FORMAT(11X, 'Type of damp matrix is :', A, '.')

	J = IndexCommaSpace(String(I+1:LSS))
	J = I+J
	K = IndexCommaSpace(String(J+1:LSS))

	READ (String(J+1:J+K-1), *) RaylCoef(1)
	READ (String(J+K+1:LSS), *)     RaylCoef(2)

	WRITE(RunDIMSD,108) RaylCoef(1)
	108 FORMAT(11X, 'The 1st rayleigh coefficient is ', F15.4)
	WRITE(RunDIMSD,109) RaylCoef(2)
	109 FORMAT(11X, 'The 2nd rayleigh coefficient is ', F15.4)

	ALLOCATE( CHARACTER(LEN =1) :: C_FileName)
	C_FileName = ' '

ELSE IF (pcomp(C_Type, 'file',4)) THEN CTYPE

	J = IndexCommaSpace(String(I+1:LSS))
	J = I+J
	ALLOCATE( CHARACTER(LEN = LSS-J) :: C_FileName)
	C_filename(1:LSS-J)=String(J+1:LSS)
	WRITE(RunDIMSD,110) C_filename(1:LSS-J)
	110 FORMAT(11X, 'Damping matrix in file : ', A)

END IF CTYPE

END SUBROUTINE ReadDampMatrix
!//////////////////////////////////////////////////////////////////|
!
! 					Subroutine : ReadInitialPara
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
SUBROUTINE ReadInitialPara(COMM)
! Purpose: read parameters for initial conditions.
! COMM ---> inid : read the initial displacement.
! 	    ---> iniv : read the initial velocity.
CHARACTER(LEN=4), INTENT(IN) :: COMM
CHARACTER(LEN=4) :: IniType

LOGICAL :: pcomp

IniType = String(I+1:I+4)

IF (pcomp(IniType,'zero',4)) THEN

	IF (pcomp(COMM,'inid',4)) THEN
		IniD_Type = IniType
		WRITE(RunDIMSD,112)
		112  FORMAT(11X, 'The initial displacement conditions are zero.')
		ALLOCATE( CHARACTER(LEN = 1) :: IniD_FileName)
		IniD_FileName = ' '
	ELSE
		IniV_Type = IniType
		WRITE(RunDIMSD,114)
		114  FORMAT(11X, 'The initial velocity conditions are zero.')
		ALLOCATE( CHARACTER(LEN = 1) :: IniV_FileName)
		IniV_FileName = ' '
	END IF

ELSE IF (pcomp(IniType,'FILE',4)) THEN

	IF (pcomp(COMM,'inid',4)) THEN
		IniD_Type = IniType
		ALLOCATE( CHARACTER(LEN = LSS-10) :: IniD_FileName)
		IniD_FileName = String(11:LSS)
		WRITE(RunDIMSD,115) IniD_FileName
		115  FORMAT(11X, 'The initial displacement conditions are in file : '&
			A, '.')
	ELSE
		IniV_Type = IniType
		ALLOCATE( CHARACTER(LEN = LSS-10) :: IniV_FileName)
		IniV_FileName = String(11:LSS)
		WRITE(RunDIMSD,116) IniV_FileName
		116  FORMAT(11X, 'The initial velocity conditions are in file : '&
			A, '.')
	END IF

ELSE
	WRITE(*,117) InputFileName
	WRITE(RunDIMSD,117) InputFileName
	117 FORMAT (1X,'** ERROR ** : There is a wrong command line about ' &
		'"inid" or "iniv"in file :',A, '.')
	STOP
END IF

END SUBROUTINE ReadInitialPara
!//////////////////////////////////////////////////////////////////|
!
! 					Subroutine : ReadMethodPara
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
SUBROUTINE ReadMethodPara()
! Declare local variables
LOGICAL :: pcomp
INTEGER :: K2

J = IndexCommaSpace(String(I+1:LSS+1))

ALLOCATE(CHARACTER(LEN=J-1):: IntegrationTyle)
IntegrationTyle = String(I+1:I+J-1)

WRITE(*,201) IntegrationTyle
201 FORMAT(1X,'Selected Direct Time Integration Method is :', A)
WRITE(RunDIMSD,202) IntegrationTyle
202 FORMAT(11X,'Selected Direct Time Integration Method is :', A)

I = I+J
K2 = I
!
! COMPUATE THE NUMBER OF INTEGRATIONAL PARAMETERS.
!
N_AlgoPara=0
DO WHILE (K2 .LE. LSS)
	J = IndexCommaSpace(String(K2+1:LSS+1))
	N_AlgoPara = N_AlgoPara + 1
	K2 = K2 + J
END DO

ALLOCATE(AlgoPara(N_AlgoPara))

DO K = 1,N_AlgoPara
	J = IndexCommaSpace(String(I+1:K2))
	READ (String(I+1:I+J-1), '(F6.3)') AlgoPara(K)
	I = I+J
END DO

END SUBROUTINE ReadMethodPara
!//////////////////////////////////////////////////////////////////|
!
! 					Subroutine : ReadNodalForce
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
SUBROUTINE ReadNodalForce()

LOGICAL :: pcomp

F_Type = String(I+1:I+4)

IF(pcomp(F_Type,'file', 4)) THEN
	WRITE(RunDIMSD, 142)
	142 FORMAT(11X, 'External Force is saved in function : Force.f90')

ELSE IF(pcomp(F_Type,'zero', 4) .OR. pcomp(F_Type, '    ', 4)) THEN
	WRITE(RunDIMSD,144)
	144 FORMAT(11X,'All nodal external forces are zero')

ELSE
	WRITE(RunDIMSD,*) '** Error **: the type of nodal force is wrong !!!'
	WRITE(RunDIMSD,*) 'The type of nodal force is : ', F_Type
	STOP '** Error ** : the type of nodal force is wrong !!!'

END IF

END SUBROUTINE ReadNodalForce
!//////////////////////////////////////////////////////////////////|
!
! 					Subroutine : ReadOutputDisp
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
SUBROUTINE ReadOutputDisp()

INTEGER :: K2

INTEGER :: IndexCommaSpace

DispFlag = .TRUE.
N_DispDof=0

K2 = I
J = INDEX(String(K2+1:LSS),',')
DO WHILE (J .NE. 0)
	N_DispDof = N_DispDof + 1
	K2 = K2+J
	J = INDEX(String(K2+1:LSS),',')
END DO

N_DispDof = INT ((N_DispDof +1.0)/2.0)

ALLOCATE(OutputDisp(N_DispDof))
OutputDisp = 10000
DO K2 = 1,N_DispDof
	OutputDisp(K2) = OutputDisp(K2) +K2
END DO

ALLOCATE(Disp_Dof(N_DispDof))
ALLOCATE(DispFileName(N_DispDof))

WRITE(RunDIMSD,121) N_DispDof
121 FORMAT(11X,'Total number of outputted DOF is:', i2, &
				/,11X, 'No.',4X,'Number of DOF',4X,'Filename',&
				/,10X, '---',4X,'--------------',3X,'---------')

K2=1
DO while(K2 .LE. N_DispDof)
	J = INDEX(String(I+1:LSS),',')
	J = I+J

	K = IndexCommaSpace(String(J+1:LSS+1))
	K = K+J

	READ (String(I+1:J-1), '(I3)') Disp_Dof(K2)
	DispFileName(K2)=String(J+1:K-1)
	WRITE (RunDIMSD, 122) K2,Disp_Dof(K2),TRIM(DispFileName(K2))
	122 FORMAT(11X,i2,18X,i4,21X,A)

	K2 = K2+1
	I = K

END DO

END SUBROUTINE ReadOutputDisp
!//////////////////////////////////////////////////////////////////|
!
! 					Subroutine : ReadOutputVelo
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
SUBROUTINE ReadOutputVelo()

INTEGER :: K2

INTEGER :: IndexCommaSpace

VeloFlag = .TRUE.
N_VeloDof=0

K2 = I
J = INDEX(String(K2+1:LSS),',')
DO WHILE (J .NE. 0)
	N_VeloDof = N_VeloDof + 1
	K2 = K2+J
	J = INDEX(String(K2+1:LSS),',')
END DO

N_VeloDof = INT ((N_VeloDof +1.0)/2.0)

ALLOCATE(OutputVelo(N_VeloDof))
OutputVelo = 20000
DO K2 = 1,N_VeloDof
	OutputVelo(K2) = OutputVelo(K2) +K2
END DO

ALLOCATE(Velo_Dof(N_VeloDof))
ALLOCATE(VeloFileName(N_VeloDof))

WRITE(RunDIMSD,121) N_VeloDof
121 FORMAT(11X,'Total number of outputted DOF is:', i2, &
				/,11X, 'No.',4X,'Number of DOF',4X,'Filename',&
				/,10X, '---',4X,'--------------',3X,'---------')

K2=1
DO while(K2 .LE. N_VeloDof)
	J = INDEX(String(I+1:LSS),',')
	J = I+J

	K = IndexCommaSpace(String(J+1:LSS+1))
	K = K+J

	READ (String(I+1:J-1), '(I3)') Velo_Dof(K2)
	VeloFileName(K2)=String(J+1:K-1)
	WRITE (RunDIMSD, 122) K2,Velo_Dof(K2),TRIM(VeloFileName(K2))
	122 FORMAT(11X,i2,18X,i4,21X,A)

	K2 = K2+1
	I = K

END DO

END SUBROUTINE ReadOutputVelo
!//////////////////////////////////////////////////////////////////|
!
! 					Subroutine : ReadOutputAcce
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
SUBROUTINE ReadOutputAcce()

INTEGER :: K2

INTEGER :: IndexCommaSpace

AcceFlag = .TRUE.
N_AcceDof=0

K2 = I
J = INDEX(String(K2+1:LSS),',')
DO WHILE (J .NE. 0)
	N_AcceDof = N_AcceDof + 1
	K2 = K2+J
	J = INDEX(String(K2+1:LSS),',')
END DO

N_AcceDof = int((N_AcceDof +1.0)/2.0)

ALLOCATE(OutputAcce(N_AcceDof))
OutputAcce = 30000
DO K2 = 1,N_AcceDof
	OutputAcce(K2) = OutputAcce(K2) +K2
END DO

ALLOCATE(Acce_Dof(N_AcceDof))
ALLOCATE(AcceFileName(N_AcceDof))

WRITE(RunDIMSD,121) N_AcceDof
121 FORMAT(11X,'Total number of outputted DOF is:', i2, &
				/,11X, 'No.',4X,'Number of DOF',4X,'Filename',&
				/,10X, '---',4X,'--------------',3X,'---------')

K2=1
DO while(K2 .LE. N_AcceDof)
	J = INDEX(String(I+1:LSS),',')
	J = I+J

	K = IndexCommaSpace(String(J+1:LSS+1))
	K = K+J

	READ (String(I+1:J-1), '(I3)') Acce_Dof(K2)
	AcceFileName(K2)=String(J+1:K-1)
	WRITE (RunDIMSD, 122) K2,Acce_Dof(K2),TRIM(AcceFileName(K2))
	122 FORMAT(11X,i2,18X,i4,21X,A)

	K2 = K2+1
	I = K

END DO

END SUBROUTINE ReadOutputAcce
! END OF SUBROUTINE ReadPara
END SUBROUTINE ReadPara