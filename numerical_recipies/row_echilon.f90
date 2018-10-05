subroutine row_echilon(m,n,a,b,row_count,out)
implicit none
integer, intent(in) :: m,n
real,intent(in),dimension(m,n) :: a
real,intent(in),dimension(m) :: b
integer, intent(out) :: out,row_count
integer :: i, j, col_count, b_count
real,parameter :: tol=1.0e-4

b_count=0
row_count=0
do i=1,m
col_count=0
do j=1,n
if(abs(a(i,j)).le.tol) then
col_count=col_count+1
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
endif
enddo

!print*,row_count,b_count
if(row_count==b_count)then
if(m-(row_count)==n)then
print*,'The system has unique solution'
out=1
else
print*,'THe system has infinitely many solutions'
out=2
endif
endif

88 continue
return
end


