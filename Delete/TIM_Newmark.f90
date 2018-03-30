subroutine TIM_Newmark()

! Purpose: Newmark Method.
	!use linear_operators
	use ModuleParameter
	use ModuleData
	use ModuleIoport

	use m_gauss

	implicit none
	integer i
	real :: c0, c1, c2, c3, c4, c5, c6, c7,t
	real ::K_eff(1:NDof,1:NDof), &
            f_eff(1:NDof), f_t(1:NDof), d1(1:NDof), v1(1:NDof),   &
            a1(1:NDof),d2(1:NDof), v2(1:NDof), a2(1:NDof), Mcb(1:NDof), &
            Mc(1:NDof),Cc(1:NDof),Ccb(1:NDof)
	real :: gamma,beta
	real PropForce
	!==================================
	!integer, DIMENSION(NDof) :: IPIV
	!integer :: INFO
!====================================================

	if(TIM_para1 .eq. 0.0) then
		gamma = 0.5
	else
		gamma=TIM_para1
	endif
	if(TIM_para2 .eq. 0.0) then
		beta = 0.25
	else
		beta=TIM_para2
	endif
	write(RunDIMSD,301) gamma, beta

	c0=1.0/(beta*dt*dt); c1=gamma/beta/dt;
    c2=1.0/beta/dt; c3=0.5/beta-1.0
	c4=gamma/beta-1.0; c5=dt*(gamma/2.0/beta-1.0);
    c6=dt*(1-gamma); c7=gamma*dt

	K_eff = K_Matrix + c0*M_Matrix + c1*C_Matrix
	!invK = .i. K_eff
	!invK = K_eff
	Mc = 0.
	Cc = 0.
	!
	d1=d0Vector; v1=v0Vector; a1=a0Vector
	t=0.0
	do i=1,nstep
		t=t+dt
		f_t=PropForce(t)*NodalForceId
		!==============================
		Mcb = c0*d1 + c2*v1 +c3*a1
		Ccb = c1*d1 + c4*v1 +c5*a1
		!call dgemm('N','N',NDof,1,NDof,1.,M_matrix,NDof,Mcb,NDof,0.,Mc,NDof)
		!call dgemm('N','N',NDof,1,NDof,1.,C_matrix,NDof,Ccb,NDof,0.,Cc,NDof)
		Mc = matmul(M_Matrix,Mcb)
		Cc = matmul(C_Matrix,Ccb)
		f_eff = f_t + Mc + Cc
		!d2=invK .x. f_eff
		!call sgesv(NDof,1,K_eff,NDof,IPIV,f_eff,NDof,INFO)
		!IF (INFO==0) then
		!	d2 = f_eff
		!	K_eff = invK
		!else
		!	write (RunDIMSD,*) 'Failed : when solving linear equations by using the LAPACK!'
		!endif
		call solve(K_eff,f_eff,d2,NDof)
		!=============================================
		a2=c0*(d2-d1) - c2*v1 - c3*a1
		v2=v1 + c6*a1 +c7*a2
		call Write2File(t,d2,'d')
		call Write2File(t,v2,'v')
		call Write2File(t,a2,'a')
		d1=d2; v1=v2; a1=a2
	end do

301 format(4X,'gamma = ',f7.4,8X,'beta = ',f7.4)

end subroutine TIM_Newmark