module ModuleIoPort

integer :: InputFile=1000, InputData=1001
integer :: RunDIMSD=2000 !oport_runmsg
! integer :: OutputDisp(1:5)=(/2011,2012,2013,2014,2015/), &
! 		   OutputVelo(1:5)=(/2021,2022,2023,2024,2025/), &
! 		   OutputAcce(1:5)=(/2031,2032,2033,2034,2035/)
INTEGER, DIMENSION(:),ALLOCATABLE :: OutputDisp,OutputVelo,OutputAcce



end module ModuleIoPort