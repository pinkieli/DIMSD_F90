LOGICAL FUNCTION InquireFile(FileName)
!///////////////////////////////////////////////////////////////////|
!	InquireFile:
!	 	determine if the file named FileName exists. If exists, return .TRUE.
! 			or, retrun .FALSE.
!
!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|

IMPLICIT NONE

CHARACTER(LEN=*) :: FileName
LOGICAL :: ExistFile

INQUIRE(FILE=FileName, EXIST=ExistFile)

IF(.NOT. ExistFile) THEN
	InquireFile = .FALSE.
	write(*,200) FileName
	200 format(' ** ERROR ** : Specified file (',A,') does not exist ! ')
	STOP 'Error occurs !!!'

ELSE
	InquireFile = .TRUE.

END IF

END FUNCTION InquireFile

