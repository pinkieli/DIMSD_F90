REAL FUNCTION PropForce(T)
!-------------------------------------------------------------
!  Purpose: calculate proportional force at T
!  Input:   T - time
!  Output:  PropForce - value of force at T.
!-------------------------------------------------------------
USE ModuleParameter, ONLY: A=>prop_para

IMPLICIT NONE

REAL :: T
PropForce = A(1) + A(2)*T + A(3)*SIN(A(4)*T+A(5))

END FUNCTION PropForce