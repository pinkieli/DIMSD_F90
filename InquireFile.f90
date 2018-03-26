LOGICAL FUNCTION InquireFile(FileName)
! Purpose: inquire IF the file exists in current directory.
IMPLICIT NONE

CHARACTER(LEN=12) :: FileName
LOGICAL :: ExistFile

INQUIRE(FILE=FileName, EXIST=ExistFile)

IF(.NOT. ExistFile) THEN ! file does not exist
	InquireFile = .FALSE.
	write(*,200) FileName
	200 format(' *ERROR* : Specified file (',A12,') does not exist ! ')
	STOP 'Error occurs !!!'

ELSE ! file exists
	InquireFile = .TRUE.

END IF

END FUNCTION InquireFile

