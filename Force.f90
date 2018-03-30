MODULE Force

USE ModuleParameter, ONLY: N=>NDof

IMPLICIT NONE

CONTAINS

FUNCTION ExternalForce(T)
!-------------------------------------------------------------
!  Purpose: calculate proportional force at T
!  Input:   T - time
!  Output:  ExternalForce - value of force at T.
!-------------------------------------------------------------

IMPLICIT NONE

REAL, INTENT(IN) :: T
REAL :: ExternalForce(N)
!  Define the external force f(t)
!
ExternalForce = 0.

ExternalForce(2) = 2.0*sin(1.2*T)

END FUNCTION ExternalForce


END MODULE Force