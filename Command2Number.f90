INTEGER FUNCTION Command2Number(CommandName)

! -------------------------------------------------------------------------
!    Purpose: Calculate the sequence number of a macro in the macro list.
!    Input:
!      CommandName - current macro's name
!    Output:
!      Command2Number - current macro's sequence number.
!--------------------------------------------------------------------------
IMPLICIT NONE

INTEGER :: I
INTEGER,PARAMETER:: N = 14

LOGICAL :: pcomp
CHARACTER :: CommandName*4
CHARACTER(4),PARAMETER:: CommandList(N) = (/'end ', 'k   ', 'm   ', 'c   ', 'inid', &
		'iniv', 'dt  ', 'time', 'disp', 'velo','acce', 'ndof', 'meth', 'forc' /)

DO I=1, N
	IF (pcomp(CommandName, CommandList(I),4)) THEN
		Command2Number = I
		RETURN
	END IF
END DO

END FUNCTION Command2Number