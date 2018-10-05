implicit none
integer, parameter :: neq=4,nvar=4
real, dimension (neq,nvar) :: a
real,dimension(neq) :: b
integer :: i,j,ok
real,dimension(neq) :: pivot
real, parameter :: tol=1.0e-4

! opening input file
open(unit=10,file='sim_lin_eq_lapack.dat',status='old')
!reading the linear equation variables from input file
do i=1,neq
read(10,*)(a(i,j),j=1,nvar),b(i)
enddo
!printing the values just to cross check 
print*,'The given matrix is'
do i=1,neq
write(*,90) (a(i,j),j=1,nvar),b(i)
enddo

call SGESV(neq, 1, A, neq, pivot, b, neq, ok)

	do i=1, nvar
	   write(*,*) b(i)
	end do

90 format(1x,5(f8.2,2x))

stop
end

