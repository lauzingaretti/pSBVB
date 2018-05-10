program pp
use aux_sub_m
logical :: swapp=.false.
integer :: v(4)=[1,34,3,12],w(4)=[0,12,0,12],n
real :: x(10)
real(r8) :: x1(100), x2(100), rho
character :: parfile*30='sbvb.par', cmd*100, xc(20)*30
real(r8) :: z=3

do
   read*, rho
   call random_normal(x1)
   call random_normal(x2)
   print*, correlation(x1,x2)
   call sortcorr(x1,x2,rho)
   print*, correlation(x1,x2)
enddo
stop

do i=1, 22
   !print* , i, (i-1)/2+1, 2-mod(i,2), factorial(i), ppoisson(i,z)
enddo;stop

print*, wf('qq')

stop
 call get_command (cmd)
 print*, cmd
 call nums2 (cmd, n, xc=xc)
 do i=2, n
   select case (xc(i))
      case ('-h', '-help')
        call system('cat README.sbvb')
        STOP
      case ('-i')
        parfile = xc(i+1)
    end select
 enddo
print*, parfile
STOP
iu=10
inquire(iolength=length) v
open(iu,access='direct',recl=length)
do i=1, 10000
   write(iu,rec=i) v
enddo
do i=10000, 1,-1
   read(iu,rec=i) v
enddo
stop
where(v+w==0) v=-10
print*, count(v<3)
print*, v
do i=1, 10
   !swap=.not.swap
   v(1:2)=v(2:1:-1)
   !print*, swap, v
enddo

contains

subroutine printv(a)
integer :: a
print*, a
end subroutine


end program


