SUBROUTINE String2CommandLine(String ,LengthCL)

IMPLICIT NONE

CHARACTER(LEN=*), INTENT(INOUT) :: String
INTEGER, INTENT(OUT) :: LengthCL

INTEGER :: I,J

J = LEN_TRIM(String)

DO I = 1,J
    IF (IACHAR(String(I:I)).EQ. 9 .OR. IACHAR(String(I:I)) .EQ. 13) THEN
        String(I:I) = ' '
    END IF
END DO

I = INDEX(String,'!')
IF (I .NE. 0) THEN
	String(I:J) = ' '
END IF

String = ADJUSTL(String)

111   J = LEN_TRIM(String)
I = INDEX(String,' ')
IF (I .LE. J) THEN

	String(I:J-1) = String(I+1:J)
	String(J:J) = ' '
	GO TO 111

ELSE

	LengthCL = LEN_TRIM(String)
	IF (String(LengthCL:LengthCL) .EQ. ',') THEN
		String(LengthCL:LengthCL) = ' '
		LengthCL = LEN_TRIM(String)
	END IF

END IF

END SUBROUTINE String2CommandLine