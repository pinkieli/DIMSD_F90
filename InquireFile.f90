LOGICAL FUNCTION InquireFile(FileName)
! Purpose: inquire IF the file exists in current directory.
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

