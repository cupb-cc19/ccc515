subroutine sort(p,neq,nvar,a,b)
implicit none
integer, intent(in) :: neq,nvar,p
real, dimension(neq,nvar), intent(inout) :: a
real,dimension(neq),intent(inout) :: b
real :: maxm,swapa,swapb
integer :: i, j, jmax, k

do i=p,neq
if(a(i,p).lt.0)then
do j=1,nvar
a(i,j)=-a(i,j)
enddo
b(i)=-b(i)
endif
enddo

do i=p,neq
maxm=a(i,p)
jmax=i
do j=i,neq
if (a(j,p).gt.maxm)then
maxm=a(j,p)
jmax=j
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

print*,'Sorting the matrix after',p,'th column'
do i=1,neq
write(*,90)(a(i,j),j=1,nvar),b(i)
enddo

90 format(1x,f8.3,1x,f8.3,1x,f8.3,1x,f8.3,1x,f8.3)

return
end



