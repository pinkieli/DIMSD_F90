SUBROUTINE pstrip(XXX,YYY,I,J)
!----------------------------------------------------------------------------------
!      Purpose: Strip off comments (begin with !) & leading blanks.
!      Inputs:
!         YYY(*)    - Input string
!         I         - First character to look for ! on comments
!                     N.B. Command language uses !x and !! for
!                          re-execution of command.
!      Outputs:
!         XXX(*)    - Output string after strips
!         J         - length of string after strips
!----------------------------------------------------------------------------------
IMPLICIT NONE

LOGICAL :: cksep
CHARACTER(LEN=255) :: XXX,YYY 
INTEGER :: I,N,M,J

!     Strip comments

do N = I,255
   if(ichar(YYY(N:N)).eq.13) then
    YYY(N:N) = ' '
   elseif(YYY(N:N).eq.'!') then
    YYY(N:255) = ' '
     go to 100
  end if
 end do

!     Strip leading blanks

100   XXX = ' '
      do N = 1,255
        if(YYY(N:N).ne.' ') then
          XXX(1:256-N) = YYY(N:255)
		  go to 200
        end if
      end do
	  J=0
      return

!     Find last character

200   do M = 255,1,-1
        if(XXX(M:M).ne.' ') go to 300
      end do
      M = 2  

!     Remove extra blanks

300   N = 1
! remove blank before comma and blank
301   if(XXX(N:N).eq.' ' .and. cksep(XXX(N+1:N+1))) then  
        XXX(N:M-1) = XXX(N+1:M)
        XXX(M:M) = ' '
        M = M - 1
        go to 301
      endif
      N = N + 1
      if(N.lt.M) go to 301
      J=M

! remove blank after comma
      do N = 1,M-2
        if(XXX(N:N).eq.',' .and. XXX(N+1:N+1).eq.' ' ) then
          XXX(N+1:M-1) = XXX(N+2:M)
          XXX(M:M) = ' '
		  J=J-1
        end if
      end do

end SUBROUTINE pstrip