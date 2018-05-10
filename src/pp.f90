
module sub
contains

!-------------------
 real function  dodom (g,a,d,delta)
!-------------------
! specifies dominance deviation using delta function (0,ploidy-1,ploidy-2,..,1,0)
 integer :: g
 real    :: a, d, delta(0:)
 !--> allele 1 dominant (d=a)
 if(a == sign(a,d)) then
    dodom = delta(g) * d
 !--> allele 0 dominant d=-a, indices are inverted
 else
    dodom = delta(size(delta)-g-1) * d
 endif
!------------
 end function
!------------

end module

program pp
use aux_sub_m
use random
use sub
integer, parameter :: nh=4
logical :: swapp=.false.
integer :: v(4)=[1,34,3,12],w(4)=[2,12,0,-12],n=5,nbase=20,i,j, nblocks
real :: x(10), delta(0:nh), a, d
character :: parfile*30='sbvb.par', cmd*100, xc(20)*30
real(r8) :: z=3, eff8(10), eff9(10), xx, s
delta(:)=(/0,(i,i=nh-1,1,-1),0/)

! check polyploid meiosis
! returns a matrix with chr pairing according to may be unequal probs
a=.2
d=.02
do i=0, nh
   print*, i, (i-nh/2)*a, dodom(i,a,d,delta), (i-nh/2)*a + dodom(i,a,d,delta) 
enddo
stop
n=size(w)
w=w(n:1:-1)
x=0.8

stop


print*, wc('missfont.log')
stop

cmd='qw 23 23.58 22'
do j=1, 3
read(1,'(a)') cmd
read(cmd,*) xc(1),n,x(1), i
print*, xc(1), n,x(1), i
enddo
call initia_random()
print*, wm('q',1)
stop

call random_gamma_v(1.d0, 1.d0, eff8)
call random_gamma_v(5.d0, 1.d0, eff9)
print*, real(eff8)
print*, real(eff9)
eff9(1:5)=0
call sortcorr_minus0(eff8,eff9,1d0)
print*, real(eff8)
print*, real(eff9)
stop

read*, i
print*, wm('sbvb.ped',i)
stop
print*, wc('example/q')
stop

call cpu_time(x(1))

do i=nbase+1, 93
   j=i-(i/nbase)*nbase+1
   nblocks = nbase/n
   print*, i, i-((i)/nbase)*nbase+1, mod(j,nblocks)*n+1, mod(j,nblocks)*n+n
enddo
stop
x(1)=x(1)*100000;
write(cmd,*) x(1)
cmd=trim(cmd)//'.txt'
open(333,file=cmd)
write(333,*) x(1)
close(333,status='delete')
stop

print*, wc('qq')
stop
print*, wl('sbvb.ped',3)
stop

read*, xx, s
call random_gamma_v(xx, s, eff8)
print*, eff8
print*, mmean(eff8), sqrt(variance(eff8))
stop
do i=1, 22
   print* , i, (i-1)/2+1, 2-mod(i,2), factorial(i), ppoisson(i,real(z))
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




