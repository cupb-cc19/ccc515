implicit none
integer, parameter :: neq=3,nvar=3
real, dimension (neq,nvar) :: a
real,dimension(neq) :: b
integer :: i,j,k,k2,m
real :: pivot

! opening input file
open(unit=10,file='sim_lin_eq.dat',status='old')
!reading the linear equation variables from input file
do i=1,neq
read(10,*)(a(i,j),j=1,nvar),b(i)
enddo
!printing the values just to cross check 
do i=1,neq
write(*,90) (a(i,j),j=1,nvar),b(i)
enddo

! Start of the gauss jordon method
do k=1,neq-1
call sort(k,neq,nvar,a,b)  ! Sorting which is equivalent to pivoting
if(a(k,k)==0)then
goto 98
else
do k2=k+1,neq
pivot=a(k2,k)/a(k,k)       ! calculating the pivot k+1
print*,pivot 
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

call row_echilon(neq,nvar,a,b,out)

90 format(1x,f8.3,1x,f8.3,1x,f8.3,1x,f8.3)

stop
end

subroutine sort(p,neq,nvar,a,b)
implicit none
integer, intent(in) :: neq,nvar,p
real, dimension(neq,nvar), intent(inout) :: a
real,dimension(neq),intent(inout) :: b
real :: max,swapa,swapb
integer :: i, j, jmax, k

real, parameter :: tol=1.0e-5

do i=p,neq
max=a(i,i)
do j=i,neq
if (a(j,i).gt.max)then
max=a(j,i)
jmax=j
else
jmax=i
endif
enddo
if (jmax /= i) then
do k=1,nvar
swapa = a(i,k)
a(i,k)=a(jmax,k)
a(jmax,k)=swapa
enddo
swapb=b(i)
b(i)=b(jmax)
b(jmax)=swapb
endif
enddo

print*,'Sorting the matrix after',p,'th row'
do i=1,neq
write(*,90)(a(i,j),j=1,nvar),b(i)
enddo

90 format(1x,f8.3,1x,f8.3,1x,f8.3,1x,f8.3)

return
end

subroutine row_echilon(m,n,a,b,out)
implicit none
integer, intent(in) :: m,n
real,intent(in),dimension(m,n) :: a
real,intent(in),dimension(m) :: b
integer, intent(out) :: out
integer :: i, j, row_count,col_count,b_count
real,parameter :: tol=1.0e-4

row_count=0
do i=1,m
col_count=0
do j=1,n
if(abs(a(i,j)).le.tol) then
col_count=col_count+1
else
exit
endif
enddo
if(col_count==n)then
row_count=row_count+1
if(abs(b(i)).gt.tol)then
print*,'The system has no solutions'
out=0
goto 88
else
b_count=b_count+1
endif
enddo

if(row_count==b_count)then
if(m-(row_count)==n)then
print*,'The system has unique solution'
x=1
else
print*,'THe system has infinitely many solutions'
x=2
endif
endif

88 continue
return
end


