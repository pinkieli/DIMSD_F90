SUBROUTINE ReadData()
!///////////////////////////////////////////////////////////////////|
!	ReadPara														|
!	 	Read data for each parameters in InputFileName					|
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
USE ModuleData

IMPLICIT NONE
LOGICAL :: InquireFile, pcomp
INTEGER :: i,j
ALLOCATE( K_Matrix(NDof,NDof), M_Matrix(NDof,NDof), C_Matrix(NDof,NDof))
ALLOCATE( d0Vector(NDof), v0Vector(NDof), NodalForceId(NDof))
	
WRITE(RunDIMSD, 200)
200 FORMAT(/, '====== Begin to read data file ... ======') 
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
! 																v
!				Read Stiff Matrix from K_FileName.				v
! 																v
! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<</
IF (InquireFile(K_FileName)) THEN ! K_FileName exists
	
	OPEN(UNIT=InputData,FILE=K_FileName, STATUS='OLD')
	READ(InputData,*) ((K_Matrix(i,j),j=1,NDof),i=1,NDof)
	
	CLOSE(InputData)

	WRITE(RunDIMSD, 210) 
	210 FORMAT(/,'Read data of stiff matrix...')
	WRITE(RunDIMSD,211) ((K_Matrix(i,j),j=1,NDof),i=1,NDof)
	211 FORMAT(4X,'Stiff matrix is :', /,(4X,2F12.4))

ELSE ! ................................................................
	WRITE(*,212) K_FileName
	WRITE(RunDIMSD, 222)  K_FileName
	212 FORMAT(1X,'** ERROR**: ',A, 'file is not found ! Check input file!')
	STOP '**Error** : The file of Stiff Matrix is not found ! Check input file!'

END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
! 																v
!					Read Mass Matrix from M_FileName.				v
! 																v
! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<</
M_Matrix=0.0
IF (InquireFile(M_FileName)) THEN ! K_FileName exists

	OPEN(UNIT=InputData,file=M_FileName, STATUS='OLD')

	IF (pcomp(M_Type, 'cons',4)) THEN
		READ(InputData,*) ((M_Matrix(i,j),j=1,NDof),i=1,NDof)
		WRITE(RunDIMSD, 220)
		WRITE(RunDIMSD,221) ((M_Matrix(i,j),j=1,NDof),i=1,NDof)
		221 FORMAT(4X,'Consistent mass matrix is :', /,(4X,2f12.4))

	ELSE IF (pcomp(M_Type, 'lump',4)) THEN
		READ(InputData,*) (M_Matrix(i,i),i=1,NDof)
		WRITE(RunDIMSD, 220)
		WRITE(RunDIMSD, 222) (M_Matrix(i,i),i=1,NDof)
		222 FORMAT(4X, 'Lumped mass matrix is :', /, 4X, 2f12.4)

	ELSE
		WRITE(RunDIMSD, 223)
		223 FORMAT(/,'**Error** : The type of Mass Matrix is wrong! Check input file!')
		STOP '**Error : The type of Mass Matrix is wrong! Check input file!'

	END IF 
	220 FORMAT(/,'Read data of mass matrix...')
	CLOSE(InputData)

ELSE
	WRITE(*,224) M_FileName
	WRITE(RunDIMSD, 224)  M_FileName
	224 FORMAT(1X,'** ERROR**: ',A, 'file is not found ! Check input file!')
	STOP '**Error** : The file of Mass Matrix is not found ! Check input file!'

END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
! 															v
!					Construct Damping Matrix.					v
! 															v
! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<</
!
! IF  C_Type is 'file', read Damping Matrix from C_FileName;
! IF  C_Type is 'rayl', compute Damping Matrix from K_Matrix and 
! 			M_Matrix.
!________________________________________________________________
IF (C_Exist) THEN
	IF (pcomp(C_Type, 'file',4)) THEN
		IF (InquireFile(C_FileName)) THEN
			OPEN(UNIT=InputData,file=C_FileName, STATUS='OLD')
			READ(InputData,*) ((C_Matrix(i,j),j=1,NDof),i=1,NDof)
			CLOSE(InputData)
			WRITE(RunDIMSD, 230)
			230 FORMAT(/,'Read data of damping matrix...')
		END IF
	ELSE IF(pcomp(C_Type, 'rayl', 4)) THEN
		C_Matrix = K_Matrix*RaylCoef(1) + M_Matrix*RaylCoef(2)
	ELSE
		WRITE(RunDIMSD, 232)
		232 FORMAT(/,'**Error** : The type of Damping Matrix is wrong! Check input file!')
		STOP '**Error** : The type of Damping Matrix is wrong! Check input file!'
	END IF
ELSE
	C_Matrix = 0.0
END IF 
WRITE(RunDIMSD,231) ((C_Matrix(i,j),j=1,NDof),i=1,NDof)
231 FORMAT(4X,'Damping matrix is :', /,(4X,2F12.4))
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
! 																v
!					Read Initial Displacement.					v
! 																v
! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<</
IF (pcomp(IniD_Type, 'file',4)) THEN

	IF (InquireFile(IniD_FileName)) THEN
		OPEN(UNIT=InputData,file=IniD_FileName, STATUS='OLD')
		read(InputData,*) (d0Vector(i),i=1,NDof)
		CLOSE(InputData)
		WRITE(RunDIMSD, 240) 
		240 FORMAT(/,'Read data of initial displacement ...')
		WRITE(RunDIMSD, 241) (d0Vector(i),i=1,NDof)
		241 FORMAT(4X, 'Initial displacement is :',/, 4X, 2f12.4)
	END IF

ELSE IF(pcomp(IniD_Type, 'zero',4) .OR. pcomp(IniV_Type, '    ',4)) THEN
	d0Vector = 0.0

ELSE
	WRITE(RunDIMSD, 242)
	242 FORMAT(/, '**Error** : The type of u0 is wrong! Check input file!')
	STOP '**Error** : The type of u0 is wrong! Check input file!'

END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
! 																v
!					Read Initial Velocity.						v
! 																v
! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<</
IF (pcomp(IniV_Type, 'file',4)) THEN

	IF (InquireFile(IniV_FileName)) THEN
		OPEN(UNIT=InputData,FILE=IniV_FileName, STATUS='OLD')
		read(InputData,*) (v0Vector(i),i=1,NDof)
		CLOSE(InputData)
		WRITE(RunDIMSD, 250) 
		250 FORMAT(/,'Read data of initial velocity ...')
		WRITE(RunDIMSD, 251) (v0Vector(i),i=1,NDof)
		251 FORMAT(4X, 'Initial velocity is :',/, 4X, 2f12.4)
	END IF

ELSE IF(pcomp(IniV_Type, 'zero',4) .OR. pcomp(IniV_Type, '    ',4)) THEN
	v0Vector = 0.0

ELSE
	WRITE(RunDIMSD, 252)
	252 FORMAT(/, '**Error** : The type of v0 is wrong! Check input file!')
	STOP '**Error** : The type of v0 is wrong! Check input file!'

END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
! 																v
!					Read nodal force ID.							v
! 																v
! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<</
IF (pcomp(F_Type, 'file',4)) THEN

	IF (InquireFile(ForceFileName)) THEN
		OPEN(UNIT=InputData,file=ForceFileName, STATUS='OLD')
		READ(InputData,*) (NodalForceId(i),i=1,NDof)
		CLOSE(InputData)
		WRITE(RunDIMSD, 260) 
		260 FORMAT(/,'Read data of nodal force ID ...')
		WRITE(RunDIMSD, 261) (NodalForceId(i),i=1,NDof)
		261 FORMAT(4X, 'Nodal force ID is :',/, 4X, 2I4)
	END IF

ELSE IF(pcomp(F_Type, 'sing',4)) THEN
	NodalForceId=0.0
	NodalForceId(F_Dof)=1.0

ELSE
	NodalForceId=0.0

END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\
! 																v
!					END OF READING DATA.							v
! 																v
! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<</
WRITE(RunDIMSD, 201)
201 FORMAT(/, '------ End of reading data file ------') 
END SUBROUTINE ReadData