LOGICAL FUNCTION Cksep(X)
!----------------------------------------------------------------------------------
!      Purpose: Check for existence of separator characters in data.
!      Inputs:
!         X  -  Character to check
!      Outputs:
!         cksep - True of character is a valid separator; else false.
!----------------------------------------------------------------------------------
IMPLICIT NONE

CHARACTER :: X


cksep = (X.EQ.' ') .OR. (X.EQ.',')

END FUNCTION Cksep