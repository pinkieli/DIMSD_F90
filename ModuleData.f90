MODULE ModuleData

IMPLICIT NONE

REAL,ALLOCATABLE:: K_Matrix(:,:), M_Matrix(:,:), C_Matrix(:,:), &
					d0Vector(:), v0Vector(:), a0Vector(:),    &
                   				NodalForceId(:)

END MODULE ModuleData