LOGICAL FUNCTION pcomp(A,B,N)
!---------------------------------------------------------------------------------
!      Purpose: Compare character strings for match
!               Ignores upper/lower case differences.
!      Inputs:
!         A(*)   - Character string 1
!         B(*)   - Character string 2
!         N      - Number of characters to compare
!      Outputs:
!         pcomp  - Flag, TRUE if A = B
!---------------------------------------------------------------------------------
IMPLICIT NONE

INTEGER :: N, INC, I, IA,IB
CHARACTER(LEN=*) ::  A,B

!     Compute increment between an upper and lower case letter

INC = IACHAR('A') - IACHAR('a')

!     Compare for match
pcomp = .FALSE.

DO I = 1,N
          IA = IACHAR(A(I:I))
          IB = IACHAR(B(I:I))

!       Test all permutations of characters for match
          if(IA.ne.IB .and. IA+INC.ne.IB .and. IA.ne.IB+INC ) return

END DO ! I

pcomp = .TRUE.

END FUNCTION pcomp
