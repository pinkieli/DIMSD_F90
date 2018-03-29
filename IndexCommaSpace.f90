INTEGER FUNCTION IndexCommaSpace(string)

IMPLICIT NONE

CHARACTER(LEN=*), INTENT(IN) :: string
INTEGER :: I,J

I = INDEX(string,',')

J = INDEX(string,' ')

IF (I .NE. 0 .AND. J .NE. 0) THEN
	IndexCommaSpace = MIN(I,J)
ELSE
	IndexCommaSpace = MAX(I,J)
END IF

END FUNCTION IndexCommaSpace