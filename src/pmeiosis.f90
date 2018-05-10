program pp
use aux_sub_m
use random
integer, parameter :: ploidy=8
real(r8)    :: tau(ploidy,ploidy)=1
integer :: xpair(ploidy/2,2)=0
! check polyploid meiosis
! returns a matrix with chr pairing according to may be unequal probs
call initia_random
!tau = reshape(source=(/0,1,1,1, 1,0,1,1, 1,1,0,1, 1,1,1,0/), shape=(/ploidy,ploidy/))
do i=1, ploidy
   tau(i,i)=0
enddo
call do_pairs(xpair, tau, ploidy)

CONTAINS

!-------------------
 subroutine do_pairs (xpair,tau,ploidy)
!-------------------
 implicit none
 integer, intent(out) :: xpair(:,:)
 real(r8), intent(in) :: tau(:,:)
 integer, intent(in)  :: ploidy
 integer :: ic1, ic2, ix
 real(r8) :: ptemp(ploidy,ploidy)
 logical :: used(ploidy)
 ptemp = tau
 used = .false.
 ix=1
 do ic1=1, ploidy-1
    if(used(ic1)) CYCLE
    call random_loaded(ic2,ptemp(ic1,:)/sum(ptemp(ic1,:)))
    xpair(ix,1:2) = [ic1,ic2]
    used(ic2) = .true.
    ptemp(:,ic1) = 0
    ptemp(:,ic2) = 0
    print*, xpair(ix,:)
    ix=ix+1
 enddo
 print*, sum(xpair)
!--------------
 end subroutine
!--------------


end program


