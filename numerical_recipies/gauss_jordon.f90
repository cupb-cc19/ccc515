implicit none
integer, parameter :: neq=4,nvar=4
real, dimension (neq,nvar) :: a
real,dimension(neq) :: b
real,dimension(nvar) :: x
integer :: i,j,k,k2,m,row_count,out
real :: pivot
real, parameter :: tol=1.0e-4

! opening input file
open(unit=10,file='sim_lin_eq.dat',status='old')
!reading the linear equation variables from input file
do i=1,neq
read(10,*)(a(i,j),j=1,nvar),b(i)
enddo
!printing the values just to cross check 
print*,'The given matrix is'
do i=1,neq
write(*,90) (a(i,j),j=1,nvar),b(i)
enddo
! Start of the gauss jordon method
do k=1,neq-1
call sort(k,neq,nvar,a,b)  ! Sorting which is equivalent to pivoting
if(abs(a(k,k)).lt.tol)then
goto 98
else
do k2=k+1,neq
pivot=a(k2,k)/a(k,k)       ! calculating the pivot k+1
!print*,pivot 
do m=1,nvar
a(k2,m)=a(k2,m)-a(k,m)*pivot ! doing row operation on a for all columns. Hence a do loop.
enddo
b(k2)=b(k2)-b(k)*pivot       ! doing row operation on b
enddo
print*,'Row equivalent matrix after',k,'th row operations'
do i=1,neq                    ! printing the matrix A and B after row operation on k+1 row
write(*,90) (a(i,j),j=1,nvar),b(i)
enddo
endif
enddo

98 continue
print*,'Final row echilon matrix'
do i=1,neq
write(*,90) (a(i,j),j=1,nvar),b(i)
enddo

print*,'Calculating the no of zero rows in row echilon matrix'
call row_echilon(neq,nvar,a,b,row_count,out)
print*,'The no of rows with all A"s zero',  row_count
if(out==1)then
call unique(neq,nvar,a,b,x)
!printing the values just to cross check 
do i=1,neq
write(*,90) (a(i,j),j=1,nvar),b(i)
enddo
print*,'The solution for the system of linear equations is'
do i=1,nvar
write(*,97)x(i)
enddo
endif
if(out==2)then
call infinite(neq,nvar,row_count,a,b,x)
endif

90 format(1x,f8.3,1x,f8.3,1x,f8.3,1x,f8.3,1x,f8.3)
97 format(1x,f8.4)
stop
end

