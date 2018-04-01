SUBROUTINE Lower2Upper(String)

IMPLICIT NONE

CHARACTER(LEN=*), INTENT(INOUT) :: String

INTEGER :: I,LENGTH

LENGTH = LEN(String)

DO I = 1,LENGTH
	IF (LGE(String(I:I),'a') .AND. LLE(String(I:I),'z')) THEN
		String(I:I) = ACHAR(IACHAR(String(I:I))-32)
	END IF
END DO

END SUBROUTINE Lower2Upper