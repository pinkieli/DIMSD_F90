SUBROUTINE ReadData()
!///////////////////////////////////////////////////////////////////|
!	ReadData
!	 	Read data for each parameters in InputFileName
!
!	Require:
! 		ModuleParameter.f90
! 		InquireFile.f90
! 		Pcomp.f90
!		Command2Number.f90
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
USE ModuleParameter

IMPLICIT NONE

LOGICAL :: InquireFile, pcomp
INTEGER :: I,J

ALLOCATE( K_Matrix(NDof,NDof), M_Matrix(NDof,NDof), C_Matrix(NDof,NDof))
ALLOCATE( d0Vector(NDof), v0Vector(NDof))

WRITE(RunDIMSD, 200)
200   FORMAT (//,42X,'========================================'/,&
			56X,'Begin to read data files'/,&
			42X,'========================================')
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! READ THE STIFF MATRIX.
IF (InquireFile(K_FileName)) THEN
	WRITE(RunDIMSD, 210)
	210 FORMAT(/,'Read data of stiff matrix...')

	OPEN(UNIT=InputData,FILE=K_FileName, STATUS='OLD')
	READ(InputData,*) ((K_Matrix(I,J),J=1,NDof),I=1,NDof)
	CLOSE(InputData)

	WRITE(RunDIMSD,211)
	211 FORMAT(11X,'Stiff matrix is :')!, /,(11X,2(F12.4)))
	WRITE(RunDIMSD,N_Format("(11X,<n>f12.4)", NDof )) &
		((K_Matrix(I,J),J=1,NDof),I=1,NDof)

ELSE
	WRITE(*,212) K_FileName
	WRITE(RunDIMSD, 212)  K_FileName
	212 FORMAT(1X,'** ERROR **: ',A, 'file is not found ! Check input file!')
	STOP '** Error ** : The file of Stiff Matrix is not found ! Check input file!'

END IF
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! READ THE MASS MATRIX.
M_Matrix=0.0
IF (InquireFile(M_FileName)) THEN ! M_FileName exists

	OPEN(UNIT=InputData,file=M_FileName, STATUS='OLD')

	IF (pcomp(M_Type, 'cons',4)) THEN
		READ(InputData,*) ((M_Matrix(I,J),J=1,NDof),I=1,NDof)
		WRITE(RunDIMSD, 220)

		WRITE(RunDIMSD,221)
		221 FORMAT(11X,'Consistent mass matrix is :')
		WRITE(RunDIMSD,N_Format("(11X,<n>f12.4)", NDof )) &
		((M_Matrix(I,J),J=1,NDof),I=1,NDof)

	ELSE IF (pcomp(M_Type, 'lump',4)) THEN
		READ(InputData,*) (M_Matrix(I,I),I=1,NDof)
		WRITE(RunDIMSD, 220)

		WRITE(RunDIMSD, 222)
		222 FORMAT(11X, 'Lumped mass matrix is :')
		WRITE(RunDIMSD, N_Format("(11X,<n>f12.4)", NDof )) (M_Matrix(I,I),I=1,NDof)

	ELSE
		WRITE(RunDIMSD, 223)
		223 FORMAT(/,'** Error ** : The type of Mass Matrix is wrong!' &
			' Check input file!')
		STOP '** Error **: The type of Mass Matrix is wrong! Check input file!'

	END IF
	220 FORMAT(/,'Read data of mass matrix...')
	CLOSE(InputData)

ELSE
	WRITE(*,224) M_FileName
	WRITE(RunDIMSD, 224)  M_FileName
	224 FORMAT(1X,'** ERROR**: ',A, 'file is not found ! Check input file!')
	STOP '**Error** : The file of Mass Matrix is not found ! Check input file!'

END IF
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! READ THE DAMP MATRIX.
IF (C_Exist) THEN
	IF (pcomp(C_Type, 'file',4)) THEN
		IF (InquireFile(C_FileName)) THEN
			OPEN(UNIT=InputData,file=C_FileName, STATUS='OLD')

			WRITE(RunDIMSD, 230)
			230 FORMAT(/,'Read data of damping matrix...')

			READ(InputData,*) ((C_Matrix(I,J),J=1,NDof),I=1,NDof)
			CLOSE(InputData)
		END IF

	ELSE IF(pcomp(C_Type, 'rayl', 4)) THEN
		WRITE(RunDIMSD, 233)
		233 FORMAT(/,'Compute Rayleigh damping matrix ...')
		C_Matrix = K_Matrix*RaylCoef(1) + M_Matrix*RaylCoef(2)
	ELSE
		WRITE(RunDIMSD, 232)
		232 FORMAT(/,'** Error ** : The type of Damping Matrix is wrong! '&
			'Check input file!')
		STOP '** Error ** : The type of Damping Matrix is wrong! Check input file!'
	END IF

ELSE
	C_Matrix = 0.0
	WRITE(RunDIMSD, 234)
	234 FORMAT(/,'The damping matrix is omitted, that is zero.')

END IF

WRITE(RunDIMSD,231)
231 FORMAT(11X,'Damping matrix is :')
WRITE(RunDIMSD,N_Format("(11X,<n>f12.4)", NDof )) &
		((C_Matrix(I,J),J=1,NDof),I=1,NDof)
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! READ THE INITIAL DISPLACEMENT.
IF (pcomp(IniD_Type, 'file',4)) THEN

	IF (InquireFile(IniD_FileName)) THEN

		OPEN(UNIT=InputData,file=IniD_FileName, STATUS='OLD')
		read(InputData,*) (d0Vector(I),I=1,NDof)
		CLOSE(InputData)

		WRITE(RunDIMSD, 240)
		240 FORMAT(/,'Read data of initial displacement ...')
		WRITE(RunDIMSD, 241)
		241 FORMAT(/,11X, 'Initial displacement is :')
		WRITE(RunDIMSD, N_Format("(11X,<n>f12.4)", NDof)) (d0Vector(I),I=1,NDof)

	END IF

ELSE IF(pcomp(IniD_Type, 'zero',4) .OR. pcomp(IniD_Type, '    ',4)) THEN
	d0Vector = 0.0
	WRITE(RunDIMSD, 242)
	242 FORMAT (/,11X, 'Initial displacement is :')
	WRITE(RunDIMSD, N_Format("(11X,<n>f12.4)", NDof)) (d0Vector(I),I=1,NDof)
ELSE
	WRITE(RunDIMSD, 243)
	243 FORMAT(/, '** Error ** : The type of u0 is wrong! Check input file!')
	STOP '** Error ** : The type of u0 is wrong! Check input file!'

END IF
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! READ THE INITIAL VELOCITY.
IF (pcomp(IniV_Type, 'file',4)) THEN

	IF (InquireFile(IniV_FileName)) THEN

		OPEN (UNIT=InputData,FILE=IniV_FileName, STATUS='OLD')
		READ (InputData,*) (v0Vector(I),I=1,NDof)
		CLOSE (InputData)

		WRITE(RunDIMSD, 250)
		250 FORMAT(/,'Read data of initial velocity ...')
		WRITE(RunDIMSD, 251)
		251 FORMAT(/,11X, 'Initial velocity is :')
		WRITE(RunDIMSD,N_Format("(11X,<n>f12.4)", NDof)) &
			(v0Vector(I),I=1,NDof)
	END IF

ELSE IF(pcomp(IniV_Type, 'zero',4) .OR. pcomp(IniV_Type, '    ',4)) THEN
	v0Vector = 0.0
	WRITE(RunDIMSD, 252)
	252 FORMAT(/,11X, 'Initial velocity is :')
	WRITE(RunDIMSD,N_Format("(11X,<n>f12.4)", NDof)) &
		(v0Vector(I),I=1,NDof)

ELSE
	WRITE(RunDIMSD, 253)
	253 FORMAT(/, '** Error ** : The type of v0 is wrong! Check input file!')
	STOP '** Error ** : The type of v0 is wrong! Check input file!'

END IF
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! READ THE RIGHT-HAND TERM, NAMELY, THE EXTERNAL FORCE.
WRITE(RunDIMSD, 260)
260 FORMAT(/,'Read data of external force  ...')

IF (pcomp(F_Type, 'file',4)) THEN
	WRITE(RunDIMSD, 261)
	261 FORMAT(11X, 'The external force is in file :Force.f90. ')
	! END IF
	NodalForceId=1.0

ELSE IF (pcomp(F_Type,'zero', 4) .OR. pcomp(F_Type, '    ', 4)) THEN
	WRITE(RunDIMSD, 262)
	262 FORMAT(11X, 'The external force is zero for all free-dom. ')
	NodalForceId=0.0

END IF
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
WRITE(RunDIMSD, 201)
201   FORMAT (/,42X,  '========================================'/,&
			59X,'End to read data files'/,&
			42X,'========================================')
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
DEALLOCATE(K_FileName,M_FileName,C_FileName,IniD_FileName,IniV_FileName)

CONTAINS
!//////////////////////////////////////////////////////////////////|
!
! 					Function : N_Format
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|
CHARACTER ( LEN = 256 ) FUNCTION N_Format( C , N )

CHARACTER ( LEN = * ) , Intent( IN ) :: C
INTEGER , Intent( IN ) :: N
INTEGER :: I , J
CHARACTER ( LEN = 16 ) :: CN
I = INDEX( C , '<' )
J = INDEX( C , '>' )
write( CN , '(G0)' ) N
N_Format = C(:I-1) // Trim(ADJUSTL(CN)) // C(J+1:)
End FUNCTION N_Format

END SUBROUTINE ReadData