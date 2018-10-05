subroutine unique(m,n,a,b,x)
implicit none
integer, intent(in) :: m, n
real, dimension(m,n), intent(inout) :: a
real,dimension(m), intent(inout) :: b
real,dimension(n), intent(out) :: x
integer :: i,k,k2,k3
real :: pivot

print*,'Proceeding to convert A into diagonal matrix'
do k=m,2,-1
do k2=k-1,1,-1
pivot=a(k2,k)/a(k,k)       ! calculating the pivot k+1
do k3=1,n
a(k2,k3)=a(k2,k3)-pivot*a(k,k3)
enddo
b(k2)=b(k2)-pivot*b(k)
enddo
enddo

do i=1,m
x(i)=b(i)/a(i,i)
enddo

return
end

