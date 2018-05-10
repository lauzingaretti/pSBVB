! 2016-03-08 11:26:08 : slight change in wc to simplify it
! 2016-02-24 11:34:13 : trim(adjustl(infile)) to avoid newline characters
! 2016-02-18 11:50:15 : extracts first col in wc to avoid very wide file issues
!   add tab2space
! 2016-02-11 10:37:26 : sortcor_minus0 rm 0 values in sortcor
! 2016-01-25 16:41:52 : wm(file,col) max level in col of file
! change in wc to avoid equal file names
! 2015-12-11 10:20:41 : wc allows for existing but void file, returning 0
! 2015-11-06 12:12:21 : wl(file,col) count number of levels in col col of file 
! 2015-10-26 12:22:22 : subr sortcorr for reorder vectors such that corr is attained
! 2015-10-22 12:39:08 : optionally print seed in random_init
! 2015-10-21 11:41:22 : ppoisson and factorial functions
! 2015-10-07 16:33:25 : match_pos allows for missing posns
! call system(" nn=$(awk 'NF>1' | wc -l")
! call get_environment_variable("nn",wc) no funciona

! changed awk for rm last blank line in wc
! 2015-08-20 16:44:31 : optimized random permu with indexx
!                       sortmat with indexx
! 2015-08-19 11:36:58 : rm last line if blank for wc
! 2014-05-16 08:36:55 : inquire in wc function
! 2014-05-12 08:34:25 : function wc counts how many lines in file
! 2014/04/15 15:24:30 : init error in recodeped
! 10/04/2014 13:48:42 : function to recode pedigree
! 09/04/2014 15:55:41 : change in mpe05ehf_v
! 17/01/06 use in mkdprime
! 19/12/05 missing w in uper and lower case
! 02/12/05 read_line changed
! 06/10/05 diag --> diagm
! 16/05/05 NUMS: new optional variable FLAG
! 17/10/03 UPOIDEV: truncated Poisson
! ONLY DIFFERENCE IS THAT r4, etc are eliminated not to conflict with misztal
!! WARNING: for compiler other than intel remove calls to rxnd1 
!! and random_number1

!*****************
 MODULE AUX_DEFS_M
!*****************
   use kinds
   integer, parameter :: unknown  = 0, &
                         absent   = unknown, &
                         even     = 0,  &
                         odd      = 1,  &
                         failed   = 0,  &
                         censored = 1,  &
                         left     = -1,  &
                         right    = 1 ,&
                         male     = 1,&
                         female   = 2 !,&
                        ! r4    = SELECTED_REAL_KIND (6, 37),&
                        ! r8    = SELECTED_REAL_KIND (15, 307),&
                        ! r16   = SELECTED_REAL_KIND (18, 4931)

 !--------------
  type name_real
 !--------------
  character:: name*25
  real(r8) :: y, z
 !----------
  end type
 !----------

!*********************
 END MODULE AUX_DEFS_M
!*********************

!******************
 MODULE CONSTANTS_M
!******************
   use aux_defs_m
   real(r8), parameter :: pi = 3.1415926535897932384626433832795
!**********************
 END MODULE CONSTANTS_M
!**********************

!****************
 MODULE AUX_SUB_M
!****************
 use aux_defs_m
 implicit none

 INTERFACE upper_case
   module procedure upper_case_1,&
                    upper_case_n
 END INTERFACE

 INTERFACE lower_case
   module procedure lower_case_1,&
                    lower_case_n
 END INTERFACE

 INTERFACE convert
   module procedure convert_ci,&
                    convert_cl
 END INTERFACE

 INTERFACE enumerate
   module procedure enumerate_s,&
                    enumerate_v
 END INTERFACE

 INTERFACE sortmat2
   module procedure sortmat1_1,&
                    sortmat1_2,&
                    sortmat2_1,&
                    sortmat2_2
 END INTERFACE

 INTERFACE extract
   module procedure extract_1,&
                    extract_2
 END INTERFACE

 INTERFACE diagm
   module procedure diag_i,&
                    diag_r8
 END INTERFACE

 INTERFACE variance
   module procedure variance_1,&
                    variance_n
 END INTERFACE

 INTERFACE mmean
   module procedure mean_r,&
                    mean_r8
 END INTERFACE

 INTERFACE correlation
   module procedure correlation_1,&
                    correlation_n,&
                    autocorrelation
 END INTERFACE

 INTERFACE random_loaded
   module procedure random_loaded_1,&
                    random_loaded_n
 END INTERFACE

 INTERFACE random_permu
   module procedure random_permu_i,&
                    random_permu_r
 END INTERFACE

 INTERFACE random_normal
   module procedure random_normal_1,&
                    random_normal_v,&
                    random_normal_m
 END INTERFACE

 INTERFACE matmul0
   ! matrix multiplication allowing for missing values
   module procedure matmul0_1,&
                    matmul0_2,&
                    matmul0_3,&
                    matmul0_4
 END INTERFACE

 INTERFACE printmat_mpe
   !--- prints matrices & vectors
   module procedure printmat_1,&
                    printmat_2,&
                    printmat_3,&
                    printmat_4
 END INTERFACE

 INTERFACE OPERATOR (.approx.)
   module procedure approx
 END INTERFACE

 CONTAINS

!----------------
 INTEGER FUNCTION match_pos(pos,allpos) result(iindex)
!----------------
! matches pos and allpos, returning index of pos in allpos
! ULL: pos and allpos must be ordered
 integer(i8) :: pos(:), allpos(:), iall, ipos, nall, npos
 dimension   :: iindex(size(pos))
 npos = size(pos)
 nall = size(allpos)
 iindex(1:size(pos))=0
 ipos = 1
 iall = 1
 do 
    if(iall>nall .or. ipos>npos) EXIT
    if(allpos(iall) < pos(ipos)) then
       iall=iall+1
    elseif(pos(ipos)==allpos(iall)) then
       iindex(ipos)=iall
       ipos=ipos+1
       iall=iall+1
    elseif(allpos(iall) > pos(ipos)) then
       ipos=ipos+1
    endif
 enddo
 !if(any(iindex==0)) print*, 'WARNING: some positions undefined'
!------------
 END FUNCTION
!------------

!----------------
 INTEGER FUNCTION wm(infile,icol)
!----------------
! max value in icol of infile
 character :: infile*(*), ofile*20, fcol*3=''
 integer   :: ioin=313, ios, icol
 integer(8):: time(8)
 logical   :: ex
 call date_and_time(values=time)
 time(1)=10000*time(8)+sum(time)
 write(ofile,'(i12)') time(1)
 ofile=trim(adjustl(ofile))//'.tzy'

 wm = 0
 if(icol>999) STOP 'max no. col value is 999 in wm'
 write(fcol,'(i3)') icol
 inquire(file=infile,exist=ex)
 if(ex) then
    call system("awk '{print $" // adjustl(fcol) // "}' " // trim(adjustl(infile)) // " | sort -rn | head -n1 > "//ofile)
    open(ioin,file=ofile)
    read(ioin,*,iostat=ios) wm
    if(ios/=0) wm=0
    call system('rm '//ofile)
 endif
!------------
 END FUNCTION
!------------


!----------------
 INTEGER FUNCTION wl(infile,icol)
!----------------
! counts no. levels in icol of infile
 character :: infile*(*), ofile*20, fcol*3=''
 integer   :: ios, icol
 integer(8):: time(8)
 logical   :: ex
 call date_and_time(values=time)
 time(1)=10000*time(8)+sum(time)
 write(ofile,'(i12)') time(1)
 ofile=trim(adjustl(ofile))//'.tzy'

 wl = 0
 if(icol>999) STOP 'max no. col value is 999 in wl'
 write(fcol,'(i3)') icol
 inquire(file=infile,exist=ex)
 if(ex) then
    call system("awk '{print $" // adjustl(fcol) // "}' " // trim(adjustl(infile)) // " | sort | uniq > "//ofile)
    wl = wc(ofile)
    call system('rm '//ofile)
 endif
!------------
 END FUNCTION
!------------


!----------------
 INTEGER FUNCTION wf(infile)
!----------------
! counts no. fields in file
 character :: infile*(*), ofile*20
 integer   :: ioin=333, ios
 integer(8):: time(8)
 logical   :: ex
 call date_and_time(values=time)
 time(1)=10000*time(8)+sum(time)
 write(ofile,'(i12)') time(1)
 ofile=trim(adjustl(ofile))//'.tzw'

 wf = 0
 inquire(file=infile,exist=ex)
 if(ex) then
    call system('head -n1 ' // trim(adjustl(infile)) // "| awk '{print NF}'  >"//ofile)
    open(ioin,file=ofile)
    read(ioin,*,iostat=ios) wf
    if(ios/=0) wf=0
    close (ioin, status='delete')
 endif
!------------
 END FUNCTION
!------------

!----------------
 INTEGER FUNCTION wc(infile)
!----------------
! counts file lines; removes blank end line if it exists
 character :: infile*(*), ofile*20
 integer   :: ioin=333, ios
 integer(8):: time(8)
 logical   :: ex
 call date_and_time(values=time)
 time(1)=10000*time(8)+sum(time)
 write(ofile,'(i12)') time(1)
 ofile=trim(adjustl(ofile))//'.tzx'

 wc = 0
 inquire(file=infile,exist=ex)
 !--> all this fuss is to avoid problems in very wide files, rm trailing blanks and rm last empty line !!
 if(ex) then
    !call system("sed 's/^[ \t]*//' " // trim(adjustl(infile)) // " | cut -f1 | cut -d ' ' -f1 | " // &
    !            "awk 'NR > 1{print t} {t = $0}END{if (NF) print }' | wc -l >"//ofile)
    call system("awk '{print substr($1,1,10)}' " // trim(adjustl(infile)) //  &
                " | awk 'NR > 1{print t} {t = $0}END{if (NF) print }' | wc -l > "//ofile)
    open(ioin,file=ofile)
    read(ioin,*,iostat=ios) wc
    if(ios/=0) wc=0
    close (ioin, status='delete')
 endif
!------------
 END FUNCTION
!------------

!-----------------
 SUBROUTINE switch (i1, i2)
!-----------------
 integer :: i1, i2, i
 i=i1
 i1=i2
 i2=i
!--------------
 END SUBROUTINE
!--------------

!--------------------
 subroutine read_line (unit, xr, xi, xc, n, nohash)
!--------------------
! Reads a line from unit and decomposes it into numeric fields in x and
! alphanumeric fields in xc; the number of fields is stored in n.
! If optional variable nohash is absent, all characters after 
! character # are ignored.
! Ignacy Misztal
! slightly modified M_P-E

 optional :: xr, xi, xc, nohash
 integer  :: unit, xi(:), n, nohash, stat, i
 character:: xc(:)*(*), a*1000
 real     :: xr(:)
 real, allocatable :: x4(:)
 
 if(present(xi)) then
    allocate(x4(size(xi)))
 elseif(present(xc)) then
    allocate(x4(size(xc)))
 elseif(present(xr)) then
    allocate(x4(size(xr)))
 else
    STOP 'at least XR XC or XC must be present'
 endif
 if(present(xi)) xi(:)=0
 if(present(xr)) xr(:)=0.
 if(present(xc)) xc(:)=' ' 
 x4=0

 do
   read(unit,'(a)',iostat=stat) a
   if (stat /= 0) then
      n=-1
      return
   endif
   a=adjustl(a)               ! ignore leading spaces
     
   if (present(nohash)) then 
      exit
   else !ignore characters after #
      i=scan(a,'#')
      if (i == 0) exit
      if (i>1) then
         a=a(1:i-1)
         exit
      endif
   endif   
 enddo
  
 call nums (a,n,xr=x4,xc=xc)
 if(present(xr)) xr=x4
 if(present(xi)) xi=int(x4)
 deallocate(x4)
!--------------
 end subroutine
!--------------

!------------------
 function tab2space (a)
!------------------
! replace all TAB with SPACE (I. Aguilar)
 character*(*) :: a
 character(len=len(a)) tab2space
 integer :: i

 do i=len_trim(a),1,-1
    if (a(i:i)==achar(9)) then
       a(i:i) = " "
    endif
 enddo
 tab2space=a
!------------
 end function
!------------

!----------------
 subroutine nums2 (a, n, x, xc)
!----------------
! separates array a into items delimited by blanks. character elements are
! put into optional character vector xc, decoded numeric values
! into optional real vector x, and n contains the number of items. The
! dimension of x and xc can be lower than n.
! format changed slightly to read real numbers in scientific notation (mpe)
! 2/23/2000 IMisztal

 character (*)          :: a
 character (*),optional :: xc(:)
 real,optional          :: x(:)
 integer :: n, curr, first, last, lena, stat, i

 curr=1
 lena=len(a)
 n=0

 do
   !--> search for first nonspace
   first=0
   do i=curr,lena
     if (a(i:i) /= ' ') then
        first=i
        exit
     endif
   enddo
   if (first == 0) exit

   !--> search for first space
   curr=first+1
   last=0
   do i=curr,lena
      if (a(i:i) == ' ') then
        last=i
        exit
      endif
   enddo

   if (last == 0) last=lena

   n=n+1
   if (present(xc)) then
      if (size(xc) >= n) then
         xc(n)=a(first:last)
      else
         print*, 'NUMS2: Increase size of XC'
      endif
   endif

   if (present(x)) then
      if (size(x) >= n) then
        read(a(first:last),*,iostat=stat) x(n)    !NEW (mpe)
        if (stat .ne. 0) x(n)=0
      else
         print*, 'NUMS2: Increase size of X'
      endif
   endif

   curr=last+1
 enddo
!--------------
 end subroutine
!--------------

!---------------
 subroutine nums (a, n, xr, xc)
!---------------
! separates array a into items delimited by blanks. character elements are
! put into optional character vector xc, decoded numeric values 
! into optional real vector x, and n contains the number of items. The 
! dimension of x and xc can be lower than n.
! A modification of nums() from f77 to f90
! Now accepts real numbers
! format changed slightly to read real numbers in scientific notation (mpe)
! 2/23/2000 Ignacy Misztal

 character (*)          :: a
 character (*),optional :: xc(:)
 real,optional          :: xr(:)
 integer                :: flag
 integer :: n, curr, first, last, lena, stat, i, iflag

 curr=1
 lena=len(a)
 n=0
 iflag=0

 do 
   !--> search for first nonspace
   first=0
   do i=curr,lena
     if (a(i:i) /= ' ') then
        first=i
        exit
     endif
   enddo
   if (first == 0) exit
  
   !--> search for first space
   curr=first+1
   last=0
   do i=curr,lena
      if (a(i:i) == ' ') then
        last=i
        exit
      endif
   enddo
  
   if (last == 0) last=lena
  
   n=n+1
   if (present(xc)) then
      if (size(xc) >= n) then
         xc(n)=a(first:last)
      else
         iflag = -1
         print*, 'NUMS: Increase size of XC'
      endif
   endif   

   if (present(xr)) then
      if (size(xr) >= n) then
        read(a(first:last),*,iostat=stat) xr(n)    !NEW (mpe)
        if (stat /=0) xr(n)=0
      else
         iflag = -2
         print*, 'NUMS: Increase size of XR'       
      endif
   endif   
 
   curr=last+1
 enddo
 !if(present(flag)) flag=iflag
!--------------
 end subroutine
!--------------

!------------------------
 LOGICAL FUNCTION numeric (c)
!------------------------
! checks whether all characters are numeric
 character(*) :: c
 integer      :: i
 numeric = .true.
 do i=1, len(c)
    select case (c(i:i))
    case ('1','2','3','4','5','6','7','8','9','0','.',' ')
       continue
    case default
       numeric=.false.
    end select 
 enddo
!--------------
 END FUNCTION
!--------------

!-----------------------
 SUBROUTINE UPPER_case_n (c)
!-----------------------
! converts all characters in vector string C into UPPER case
 character(*) :: c(:)
 integer      :: i
 do i=1, size(c)
    call upper_case_1 (c(i))
 enddo
!--------------
 END SUBROUTINE
!--------------

!-----------------------
 SUBROUTINE lower_case_n (c)
!-----------------------
! converts all characters in vector string C into UPPER case
 character(*) :: c(:)
 integer      :: i
 do i=1, size(c)
    call lower_case_1 (c(i))
 enddo
!--------------
 END SUBROUTINE
!--------------

!-----------------------
 SUBROUTINE UPPER_case_1 (c)
!-----------------------
! converts all characters in string C into UPPER case
 character(*):: c
 integer     :: i
 
 do i=1, len(c)
    if (c(i:i) == 'a') then
       c(i:i) = 'A'
    elseif (c(i:i) == 'b') then
       c(i:i) = 'B'
    elseif (c(i:i) == 'c') then
       c(i:i) = 'C'
    elseif (c(i:i) == 'd') then
       c(i:i) = 'D'
    elseif (c(i:i) == 'e') then
       c(i:i) = 'E'
    elseif (c(i:i) == 'f') then
       c(i:i) = 'F'
    elseif (c(i:i) == 'g') then
       c(i:i) = 'G'
    elseif (c(i:i) == 'h') then
       c(i:i) = 'H'
    elseif (c(i:i) == 'i') then
       c(i:i) = 'I'
    elseif (c(i:i) == 'j') then
       c(i:i) = 'J'
    elseif (c(i:i) == 'k') then
       c(i:i) = 'K'
    elseif (c(i:i) == 'l') then
       c(i:i) = 'L'
    elseif (c(i:i) == 'm') then
       c(i:i) = 'M'
    elseif (c(i:i) == 'n') then
       c(i:i) = 'N'
    elseif (c(i:i) == 'o') then
       c(i:i) = 'O'
    elseif (c(i:i) == 'p') then
       c(i:i) = 'P'
    elseif (c(i:i) == 'q') then
       c(i:i) = 'Q'
    elseif (c(i:i) == 'r') then
       c(i:i) = 'R'
    elseif (c(i:i) == 's') then
       c(i:i) = 'S'
    elseif (c(i:i) == 't') then
       c(i:i) = 'T'
    elseif (c(i:i) == 'u') then
       c(i:i) = 'U'
    elseif (c(i:i) == 'v') then
       c(i:i) = 'V'
    elseif (c(i:i) == 'w') then
       c(i:i) = 'W'
    elseif (c(i:i) == 'x') then
       c(i:i) = 'X'
    elseif (c(i:i) == 'y') then
       c(i:i) = 'Y'
    elseif (c(i:i) == 'z') then
       c(i:i) = 'Z'
    endif
 enddo
!--------------
 END SUBROUTINE
!--------------

!-----------------------
 SUBROUTINE lower_case_1 (c)
!-----------------------
! converts all characters in string C into lower case
 character(*):: c
 integer     :: i
 
 do i=1, len(c)
    if (c(i:i) == 'A') then
       c(i:i) = 'a'
    elseif (c(i:i) == 'B') then
       c(i:i) = 'b'
    elseif (c(i:i) == 'C') then
       c(i:i) = 'c'
    elseif (c(i:i) == 'D') then
       c(i:i) = 'd'
    elseif (c(i:i) == 'E') then
       c(i:i) = 'e'
    elseif (c(i:i) == 'F') then
       c(i:i) = 'f'
    elseif (c(i:i) == 'G') then
       c(i:i) = 'g'
    elseif (c(i:i) == 'H') then
       c(i:i) = 'h'
    elseif (c(i:i) == 'I') then
       c(i:i) = 'i'
    elseif (c(i:i) == 'J') then
       c(i:i) = 'j'
    elseif (c(i:i) == 'K') then
       c(i:i) = 'k'
    elseif (c(i:i) == 'L') then
       c(i:i) = 'l'
    elseif (c(i:i) == 'M') then
       c(i:i) = 'm'
    elseif (c(i:i) == 'N') then
       c(i:i) = 'n'
    elseif (c(i:i) == 'O') then
       c(i:i) = 'o'
    elseif (c(i:i) == 'P') then
       c(i:i) = 'p'
    elseif (c(i:i) == 'Q') then
       c(i:i) = 'q'
    elseif (c(i:i) == 'R') then
       c(i:i) = 'r'
    elseif (c(i:i) == 'S') then
       c(i:i) = 's'
    elseif (c(i:i) == 'T') then
       c(i:i) = 't'
    elseif (c(i:i) == 'U') then
       c(i:i) = 'u'
    elseif (c(i:i) == 'V') then
       c(i:i) = 'v'
    elseif (c(i:i) == 'W') then
       c(i:i) = 'w'
    elseif (c(i:i) == 'X') then
       c(i:i) = 'x'
    elseif (c(i:i) == 'Y') then
       c(i:i) = 'y'
    elseif (c(i:i) == 'Z') then
       c(i:i) = 'z'
    endif
 enddo
!--------------
 END SUBROUTINE
!--------------

!---------------------
 SUBROUTINE convert_ci (c, iv)
!---------------------
! breaks up a character string C into an integer vector IV 
! with as many numbers as characters in C
 character(*):: c
 integer     :: i, iv(:)
 
 do i=1, len(c)
    read(c(i:i),*) iv(i)
 enddo
!--------------
 END SUBROUTINE
!--------------

!---------------------
 SUBROUTINE convert_cl (c, iv)
!---------------------
! breaks up a character string C into a logical vector IV 
! with as many numbers as characters in C
 character(*):: c
 logical     :: iv(:), l(0:1)
 integer     :: i, j
 data l/.false., .true./
 
 do i=1, len(c)
    read(c(i:i),*) j
    iv(i)=l(j)
 enddo
!--------------
 END SUBROUTINE
!--------------

!----------------------
 SUBROUTINE enumerate_s (matrix,n_cases,n_items,n_values)
!-----m_p-e_12/01------
!scalar version: n_values = constant
!This subroutine enumerates all possible combinations of n_items
!when each item can take any of 1 through n_values different values
!and stores the result in the matrix(n_cases x n_items)
!n_cases=n_values^n_items
!Example: three genotypes, two possible alleles(2), the matrix is 
! 111
! 112
! 121
! ...etc
! 
 implicit none
 integer, intent(in) :: n_items, n_values
 integer, intent(out):: n_cases, matrix(:,:) 
 integer :: i, j, v_values(n_values)

 n_cases=n_values**n_items
 v_values=(/(i,i=1,n_values)/)
 do j=1,n_items
    do i=1,n_cases
       matrix(i,j)=v_values(1)
       if(mod(i,n_values**(j-1))==0) v_values=cshift(v_values,1)
    enddo
 enddo
!--------------------------
 END SUBROUTINE ENUMERATE_S
!--------------------------

!----------------------
 SUBROUTINE ENUMERATE_V (matrix,n_cases,n_items,n_values)
!-----m_p-e_12/01------
! vector version: n_values = vector(n_items)
 implicit none
 integer :: i, j, n_cases, n_items, n_values(n_items) &
          , matrix(:,:), prod_n_values(n_items)
 integer, allocatable :: v_values(:)
 n_cases=product(n_values(1:n_items) )
 prod_n_values(1)=1
 do j=2, n_items
    prod_n_values(j)=prod_n_values(j-1)*n_values(j-1)
 enddo
 do j=1,n_items
    allocate (v_values(n_values(j)))
    v_values=(/(i,i=1,n_values(j))/)
    do i=1,n_cases
       matrix(i,j)=v_values(1)
       if(mod(i,prod_n_values(j))==0) v_values=cshift(v_values,1)
    enddo
    deallocate (v_values)
 enddo
!--------------
 END SUBROUTINE
!--------------

!----------------------------
 logical function informative (alleles)
!----------------------------
 integer :: alleles(:)
 if (alleles(1)/=alleles(2) .and. all(alleles/=unknown)) then
    informative=.true.
 else
    informative=.false.
 endif
!------------------------
 end function informative
!------------------------

!--------------------------
 integer function recodeped (ped) result(reped)
!--------------------------
! recode pedigree ids
 integer   :: ped(:,:), icode(size(ped,dim=1)), i, nind
 dimension :: reped(size(ped,1),size(ped,2))
 nind  = size(ped,1)
 icode = (/(i, i=1,nind)/)
 reped(:,1)   = icode(:)
 reped(:,2:3) = 0
 do i=1, nind
    if(ped(i,2)>0) reped(i:i,2) = pack(icode, ped(:,1)==ped(i,2))
    if(ped(i,3)>0) reped(i:i,3) = pack(icode, ped(:,1)==ped(i,3))
 enddo
 
!------------------------
 end function recodeped
!------------------------

!-----------------
 subroutine indexx (n,arrin,indx)
!-----------------
!     Use the Heapsort algorithm to index an array arrin of length n.
!     Output the array indx such that arrin(indx(j)) is in ascending
!     order for j = 1,2,...,n.  The input quantities n and arrin are
!     not changed.

!     This is a Numerical Recipes routine (Press et al), but modified by one
!     line to work if n equals 1, and adapted to f90 syntax.

 integer     :: i, n, indx(:), indxt, ir, j, l
 real(r8)    :: arrin(:), q
 indx(:) =(/(j,j=1,n)/)
 if (n .eq. 1) return
 l=n/2+1
 ir=n
 do
    if(l.gt.1)then
       l=l-1
       indxt=indx(l)
       q=arrin(indxt)
    else
       indxt=indx(ir)
       q=arrin(indxt)
       indx(ir)=indx(1)
       ir=ir-1
       if(ir.eq.1)then
          indx(1)=indxt
          return
       endif
    endif
    i=l
    j=l+l
    do while (j.le.ir) 
       if(j.lt.ir)then
          if(arrin(indx(j)).lt.arrin(indx(j+1)))j=j+1
       endif
       if(q.lt.arrin(indx(j)))then
          indx(i)=indx(j)
          i=j
          j=j+j
       else
          j=ir+1
       endif
    enddo
    indx(i)=indxt
 enddo
!--------------
 end subroutine
!--------------

!----------
 subroutine sortcorr_minus0 (v1,v2,rho)
!----------
! ignores <= zeros in v2, then uses sortcor 
! v1 is unchanged
 real(r8) :: v1(:), v2(:), v11(size(v1)), v22(size(v1)), rho
 integer  :: i, n0, ix1(size(v1))

 ix1 =[(i,i=1,size(v1))]
 n0 = count(v2>0.d0)
 ix1(1:n0) = pack(ix1,v2>0.d0)
 v11(1:n0) = pack(v1,v2>0.d0)
 v22(1:n0) = pack(v2,v2>0.d0)
 call sortcorr(v11(1:n0),v22(1:n0),rho)
 !--> retrieves v2 values with new order
 do i=1, n0
    v2(ix1(i)) = v22(i)
 enddo
!--------------
 end subroutine
!--------------


!----------
 subroutine sortcorr (v1,v2,rho)
!----------
! this subroutine returns a permuted v2 such that cor(v1,v2) is rho, approx
! this is done by adding a random variable on the sorted v1, and reordering
! sorted v2 following the v1 sorting vector
! v1 is unchanged
 real(r8) :: v1(:), v2(:), temp(size(v1)), rho, ve
 integer  :: i, n, ix1(size(v1)), ix2(size(v1)), xi(size(v1))
 if(rho==0) then
    call random_permu(v2)
 else
    n=size(v1)
    ve = variance(v1)*(1./(rho**2) - 1.)
    call indexx(n,v1,ix1)
    call indexx(n,v2,ix2)
    !--> I assume v2 order is not relevant
    v2 = v2(ix2)
    !--> add noise to v1 to create a pseudorandom var with desired cor
    call random_normal(temp,0.d0,ve)
    temp = temp + v1(ix1)
    if(rho<0) temp=-temp
    call indexx(n,temp,ix2)
    v2 = v2(ix2)
    !--> now i need to link back v2 to original v1 order
    do i=1, n
       xi(ix1(i)) = i
    enddo
    v2 = v2(xi)
 endif
!--------------
 end subroutine
!--------------



!------------------
 subroutine sortmat (a,col,n)
!------------------
! ignacy misztal 
! sorts matrix of integers in ascending order column by column.
! To sort x for column 1: call sortmat(x,(/1/))
! To sort x for columns 5 (first key), 2 (second key) and 3 
! (third key):  call sortmat(x,/(5,2,3/)) 
! Optionally, only first n columns are sorted
implicit none 
integer,optional::n 
integer :: i,j,s,l,r,j1,k,icol,ix 
integer :: a(:,:) 
integer,dimension(size(a,dim=1))::x,y 
integer :: stack(50,2),col(:) 
integer,parameter::is_lt=1, is_eq=2, is_gt=3
if (present(n)) then 
  ix=n 
else 
  ix=size(a,dim=2) 
endif
icol=size(col) 
s=1 
stack(1,1)=1; stack(1,2)=ix
do 
  l=stack(s,1); r=stack(s,2) 
  s=s-1
  do
    i=l; j=r 
    j1=(l+r)/2 !median element
    x(1:icol)=a(col,j1)
    do ! select larger than median starting from bottom 
      do
        k=compint(a(col,i),x(1:icol))
        select case (k) 
          case (is_lt)
            i=i+1
            cycle
          case (is_eq,is_gt)
            exit
          case default
            print*,'should not happen 123548'
            stop
        end select
      enddo 
      !select smaller than median starting from top
      do
        k=compint(a(col,j),x(1:icol))
        select case (k)
          case (is_gt)
            j=j-1
            cycle
          case (is_eq,is_lt)
            exit
          case default
            print*,'should not happen 123549' 
            stop
        end select
      enddo
    ! swap
    if (i.le.j) then
      y=a(:,i); a(:,i)=a(:,j); a(:,j)=y !swap 
      i=i+1; j=j-1
    endif
    if (i > j) exit
  enddo
    if (i.lt.r) then 
      s=s+1 
      if (s > 50) then 
        print*,'stack > 50' 
        stop 
      endif 
      stack(s,1)=i 
      stack(s,2)=r 
    endif
    r=j 
    if (l >= r) exit 
  enddo
  if (s == 0) exit 
enddo 
!----------------------
 end subroutine sortmat
!----------------------

!------------------------
 integer function compint (x,y) result (j)
!------------------------ 
! compares integer vectors sequentially 
! j = 1 if x < y 
! 2 if x = y 
! 3 if x > y 
integer::x(:),y(:),i
j=2 
do i=1,size(x) 
  if (x(i) < y(i)) then 
   j=1 
   exit 
  elseif (x(i) > y(i)) then 
   j=3 
   exit 
  endif 
enddo 
!--------------------
 end function compint
!--------------------

!---------------------
 subroutine sortmat1_1 (vector)
!---------------------
!indexx based
 implicit none
 real(r8)  :: vector(:)
 integer   :: ix(size(vector))
 call indexx(size(vector),vector,ix)
 vector=vector(ix) 
!-------------------------
 end subroutine sortmat1_1 
!-------------------------


!---------------------
 subroutine sortmat1_2 (vector)
!---------------------
 implicit none
 integer   :: vector(:), ix(size(vector))
 call indexx(size(vector),real(vector,r8),ix)
 vector=vector(ix)
!-------------------------
 end subroutine sortmat1_2 
!-------------------------


!---------------------
 subroutine sortmat2_1 (matrix,ipos)
!---------------------
!indexx based
 integer, intent(in) :: ipos
 real(r8)   :: matrix(:,:)
 integer    :: i, ncol, ix(size(matrix,dim=1))
 ncol = size(matrix,dim=2)
 call indexx(size(ix),matrix(:,ipos),ix)
 do i=1, ncol
    matrix(:,i) = matrix(ix,i)
 enddo
!-------------------------
 end subroutine sortmat2_1 
!-------------------------


!---------------------
 subroutine sortmat2_2 (matrix,ipos)
!---------------------
!indexx based
 integer, intent(in) :: ipos
 integer    :: matrix(:,:), i, ncol, ix(size(matrix,dim=1))
 ncol = size(matrix,dim=2)
 call indexx(size(ix),real(matrix(:,ipos),r8),ix)
 do i=1, ncol
    matrix(:,i) = matrix(ix,i)
 enddo
!-------------------------
 end subroutine sortmat2_2 
!-------------------------


!------------------------
 subroutine initia_random (seed0,printseed)
!------------------------
! initializes the random number generator 
! if seed0 present it uses that number as seed, 
! otherwise it ensures a different random seed per call.
 integer :: n, v(8)
 integer, allocatable :: seed(:)
 integer, optional    :: seed0
 logical, optional    :: printseed
 real                 :: x

 call random_seed(size=n)
 allocate (seed(n))

 if(present(seed0)) then
    seed=seed0
 else !-- random initialization
    call date_and_time(values=v)
    seed=sum(v)+1000*v(8)+100*v(7)*v(6)
 endif
 if(present(printseed)) print*, 'Seed = ',seed(1)
 call random_seed(put=seed(1:n))
 call random_number(x)
 if(allocated(seed)) deallocate(seed)
!----------------------------
 end subroutine initia_random
!----------------------------

!----------------------
 real(r8) function rxnd ()
!----------------------
 call random_number (rxnd)
!-----------------
 end function rxnd
!-----------------
 
!----------------------
 real(r8) function rxnd1 ()
!----------------------
! x1 es la semilla
! old function
  real(r8) :: divis, trans, divid, lsol, x1
  logical  :: first_call = .true.
  integer  :: seed(20)
  save :: x1, first_call
  if (first_call) then
     call initia_random ()
     call random_seed(get=seed)
     x1 = real(seed(1),r8) / 100000.d0
     first_call = .false.
  endif
  divis = 2.d0**31.d0-1.d0
  trans = 7.d0**5.d0
  divid = trans*x1
  lsol = int(divid/divis)
  x1 = divid - lsol*divis
  rxnd1 = x1/divis
!------------------
 end function rxnd1
!------------------

!------------------------
 SUBROUTINE RANDOM_NUMBER1 (v)
!------------------------
 real(r8) :: v(:)
 integer  :: i
 do i=1, size(v)
    v(i) = rxnd1()
 enddo
!----------------------------
 END SUBROUTINE RANDOM_NUMBER1
!----------------------------

!-------------------------
 integer function binomial (p)
!-------------------------
! generates a 1 with probability p, zero otherwise
 real(r8) :: p
 binomial = 0
 if (rxnd() <= p) binomial=1
!------------
 end function
!------------

!----------------------
 real function ppoisson (k,xm)
!----------------------
! computes prob of an event k from poisson(xm)
 integer :: k
 real    :: xm
 ppoisson = exp(-xm) * xm**k / factorial(k)
!------------
 end function
!------------

!------------------------------
 integer(i8) function factorial (n)
!------------------------------
! trick to product in one shot
 integer :: i, n
 if(n>12) STOP 'factorial undefined >12'
 factorial = nint(exp(sum(log([(real(i,r8),i=1,n)]))))
!------------
 end function
!------------

!------------------------
 integer function upoidev (type, xm)
!------------------------
! truncated Poisson random generator
! type can take values odd or even
 integer, parameter :: mx_iter=100
 integer :: iter, type
 real(r8):: xm

 if (type/=even .and. type/=odd) then
    upoidev = poidev(xm) !<-- regular call
    RETURN
 else
    do iter=1, mx_iter
       upoidev = poidev(xm)
       if (type==even .and. mod(upoidev,2)==0) RETURN
       if (type==odd  .and. mod(upoidev,2)/=0) RETURN
    enddo
    upoidev=type !<-- no sucess
 endif
!------------
 end function
!------------

!-----------------------
 integer function poidev (xm)
!-----------------------
! returns a random Poisson deviate of mean xm using
! Press et al.1989
 real(r8), parameter :: pi=3.14159265358979
 real(r8) :: alxm, em, g, oldm, sq, t, xm, y
 data oldm /-1.d0/
 save oldm, g, alxm, sq

 if (xm <= 0.d0) then
   poidev=0
   return
 elseif (xm.lt.12.d0) then
   if(xm.ne.oldm) then
     oldm=xm
     g=exp(-xm)
   endif
   em=-1.d0
   t=1.d0
 2  em=em+1.d0
   t=t*rxnd()
   if (t.gt.g) goto 2
 else
   if (xm.ne.oldm) then
      oldm=xm
      sq=dsqrt(2.d0*xm)
      alxm=log(xm)
      g=xm*alxm-gammln(xm+1.d0)
   endif
 1  y=tan(pi*rxnd())
   em=sq*y+xm
   if (em.lt.0.d0) goto 1
   em=int(em)
   t=0.9d0*(1.d0+y**2)*exp(em*alxm-gammln(em+1.d0)-g)
   if(rxnd().gt.t) goto 1
 endif
 poidev=int(em)
!--------------------
  end function poidev
!--------------------

!  -------------------------------------
       function gammln(xx)
!  -------------------------------------
!
implicit none
integer :: j
real (r8):: cof(6),stp,half,one,fpf,x,tmp,ser,gammln,xx
data cof,stp/76.18009173d0,-86.50532033d0,24.01409822d0 &
      ,-1.231739516d0,.120858003d-2,-.536382d-5,2.50662827465d0/
data half,one,fpf/0.5d0,1.0d0,5.5d0/
save cof,stp,half,one,fpf
x=xx-one
tmp=x+fpf
tmp=(x+half)*log(tmp)-tmp
ser=one
do j=1,6
  x=x+one
  ser=ser+cof(j)/x
enddo
gammln=tmp+log(stp*ser)
!  -------------------------------------
       end function gammln
!  -------------------------------------


      REAL (r8) FUNCTION STUDNT (T, DOFF, IFAULT)
!
!     ALGORITHM AS 27  APPL. STATIST. VOL.19, NO.1
!
!     Calculate the upper tail area under Student's t-distribution
!
!     Translated from Algol by Alan Miller
!
      INTEGER :: IFAULT
      REAL (r8) :: T, DOFF
 
!     Local variables

      REAL V, X, TT, TWO, FOUR, ONE, ZERO, HALF
      REAL A1, A2, A3, A4, A5, B1, B2,  &
           C1, C2, C3, C4, C5, D1, D2,  &
           E1, E2, E3, E4, E5, F1, F2,  &
           G1, G2, G3, G4, G5, H1, H2,  &
           I1, I2, I3, I4, I5, J1, J2
      LOGICAL POS
      DATA TWO /2.0/, FOUR /4.0/, ONE /1.0/, ZERO /0.0/, HALF /0.5/
      DATA A1, A2, A3, A4, A5 /0.09979441, -0.581821, 1.390993, &
           -1.222452, 2.151185/, B1, B2 /5.537409, 11.42343/
      DATA C1, C2, C3, C4, C5 /0.04431742, -0.2206018, -0.03317253, &
           5.679969, -12.96519/, D1, D2 /5.166733, 13.49862/
      DATA E1, E2, E3, E4, E5 /0.009694901, -0.1408854, 1.88993,&
           -12.75532, 25.77532/, F1, F2 /4.233736, 14.3963/
      DATA G1, G2, G3, G4, G5 /-9.187228E-5, 0.03789901, -1.280346,&
          9.249528, -19.08115/, H1, H2 /2.777816, 16.46132/
      DATA I1, I2, I3, I4, I5 /5.79602E-4, -0.02763334, 0.4517029, &
           -2.657697, 5.127212/, J1, J2 /0.5657187, 21.83269/

!     Check that number of degrees of freedom > 4.

      IF (DOFF .LT. TWO) THEN
      IFAULT = 1
      STUDNT = - ONE
      RETURN
      END IF

      IF (DOFF .LE. FOUR) THEN
      IFAULT = DOFF
      ELSE
      IFAULT = 0
      END IF

!     Evaluate series.

      V = ONE / DOFF
      POS = (T .GE. ZERO)
      TT = ABS(T)
      X = HALF * (ONE +   &
          TT * (((A1 + V * (A2 + V * (A3 + V * (A4 + V * A5)))) /   &
              (ONE - V * (B1 - V * B2))) +                          &
          TT * (((C1 + V * (C2 + V * (C3 + V * (C4 + V * C5)))) /   &
              (ONE - V * (D1 - V * D2))) +                          &
          TT * (((E1 + V * (E2 + V * (E3 + V * (E4 + V * E5)))) /   &
              (ONE - V * (F1 - V * F2))) +                          &
          TT * (((G1 + V * (G2 + V * (G3 + V * (G4 + V * G5)))) /   &
              (ONE - V * (H1 - V * H2))) +                          &
          TT * ((I1 + V * (I2 + V * (I3 + V * (I4 + V * I5)))) /    &
              (ONE - V * (J1 - V * J2))) ))))) ** (-8)
      IF (POS) THEN
      STUDNT = X
      ELSE
      STUDNT = ONE - X
      END IF

      RETURN
      END FUNCTION


!-------------------------
 integer function mpe05ehf (n1,n2,iflag)
!-------------------------
! returns an integer number taken at random from interval [n1,n2], n1<n2
! numbers with mask=.false. are excluded
! m_p-e Mon Nov 25, 1996 11:06:40 (f95: 30/11/2000)
 optional :: iflag
 integer  :: iflag, n1, n2
 if (n1 > n2) then
    if(present(iflag)) iflag = -9
    return
 endif
 mpe05ehf = int(rxnd()*(n2-n1+1)+1.d0) + n1 - 1
!------------
 end function
!------------

!---------------------------
 integer function mpe05ehf_v (n1,n2,n,mask) result(iv)
!---------------------------
! returns an integer vector with numbers taken at random from interval [n1,n2], n1<n2
! m_p-e Mon Nov 25, 1996 11:06:40 (f95: 30/11/2000)
 optional :: mask
 integer  :: n1, n2, i, j, n, iaux(n1:n2)
 logical  :: mask(n1:n2)
 dimension:: iv(n)

 if (n1 > n2) RETURN
 iaux=(/(i, i=n1,n2)/)
 if (present(mask)) then
    i = count(mask)
    iaux(1:i) = pack(iaux,mask)
 else
    i=size(iaux)
 endif
 do j=1, n
    call random_permu(iaux(1:i))
    iv(j) = iaux(1)
 enddo
!------------
 end function
!------------

!---------------------------
 integer function mpe05ehf_old (n1,n2,n,mask) result(iv)
!---------------------------
! returns an integer vector with numbers taken at random from interval [n1,n2], n1<n2
! numbers with mask=.false. are excluded
! m_p-e Mon Nov 25, 1996 11:06:40 (f95: 30/11/2000)
 optional :: mask
 integer  :: n1, n2, i, n, iaux(n1:n2)
 logical  :: mask(n1:n2)
 dimension:: iv(n)

 if (n1 > n2) RETURN
 iaux=(/(i, i=n1,n2)/)
 if (present(mask)) then
    i = count(mask)
    iaux(1:i) = pack(iaux,mask)
 else
    i=size(iaux)
 endif
 call random_permu(iaux(1:i))
 iv(1:n) = iaux(1:n)
!------------
 end function
!------------

!------------------------
 REAL (r8) FUNCTION P_LOG (p1,p2)
!--25/01/01--------------
 implicit none
 real (r8) :: p1, p2
 real (r8), parameter :: tol_log=200
 if(p1-p2 > tol_log) then
   p_log=1.d0-1.d0/exp(tol_log) 
 elseif(p2-p1 > tol_log) then
   p_log=1.d0/exp(tol_log)
 else 
   p_log=exp(p1-p2)/(1.d0+exp(p1-p2))
 endif
!------------------
 END FUNCTION P_LOG
!------------------

!-----------------------------------
 INTEGER FUNCTION LOG_RANDOM_INTEGER (p1,p2)
!--12/01/01-------------------------
 real (r8) :: p1, p2
 log_random_integer=1
 if (rxnd() > p_log(p1,p2)) log_random_integer=2
!-------------------------------
 END FUNCTION LOG_RANDOM_INTEGER
!-------------------------------

!----------------
 subroutine log_p (w,use0)
!----------------
 real (r8), intent(inout) :: w(:) 
 logical, optional, intent(in) :: use0(:)
 logical                       :: use(size(w))
 if (present(use0)) then
    use = use0
 else
   use = .true.
 endif
 w = w-maxval(w,mask=use)
 w = exp(w)
 w = w/sum(w,mask=use)
 where (.not.use) w=0.d0
!--------------------
 end subroutine log_p
!--------------------

!-----------------------------
 subroutine log_4random_integer (i,w,res)
!-----------------------------
!incorporate restrictions
 implicit none
 integer :: i, n, res(4)
 real (r8) ::  w(4)
 real (r8), parameter :: tol_log=200.d0  
 n=4
 w=max(w-maxval(w),-tol_log)
 w=exp(w)
 w=w*res
 w=w/sum(w)
 call random_loaded(i,w(1:n))
!--------------------
 end subroutine log_4random_integer
!----------------

!--------------------------
 subroutine random_loaded_1 (i, w)
!--------------------------
 implicit none
 intent(in)  :: w
 intent(out) :: i
 integer     :: i
 real (r8)   :: x, w(:), sum_w
 sum_w=w(1)
 x = rxnd()
 do i=1, size(w)-1
    if(x < sum_w) EXIT 
    sum_w=sum_w+w(i+1)
 enddo
!------------------------------
 end subroutine random_loaded_1
!------------------------------

!--------------------------
 subroutine random_loaded_n (i,w)
!--------------------------
! matrix version, size(i) random numbers are obtained
 intent(in)  :: w
 intent(out) :: i
 integer     :: i(:), ii, j
 real (r8)   :: x(size(i)), w(:), sum_w
 call random_number(x)
 do j=1, size(i)
    sum_w=w(1)
    do ii=1, size(w) !WARNING: note difference with scalar version
       i(j)=ii
       if(x(j) < sum_w) EXIT 
       sum_w = sum_w + w( i(j)+1 )
    enddo
 enddo
!------------------------------
 end subroutine random_loaded_n
!------------------------------

!-------------------------
 subroutine random_permu_i2 (iw)
!-------------------------
! deprecated
! returns a random permutation of integer vector iw in ipw
 implicit none
 integer   :: iw(:)
 real (r8) :: w(size(iw),2)
 call random_number (w(:,1))
 w(:,2) = real(iw)
 call sortmat2(w,1)
 iw = nint(w(:,2))
!-----------------------------
 end subroutine random_permu_i2
!-----------------------------

!-------------------------
 subroutine random_permu_i (iw)
!-------------------------
! returns a random permutation of integer vector iw in ipw
 implicit none
 integer   :: iw(:), ix(size(iw))
 real (r8) :: w(size(iw))
 call random_number (w(:))
 call indexx(size(iw),w,ix)
 iw = iw(ix)
!-----------------------------
 end subroutine 
!-----------------------------

!-------------------------
 subroutine random_permu_r (rw)
!-------------------------
! returns a random permutation of real vector w in ipw
 implicit none
 real (r8) :: rw(:), w(size(rw))
 integer   :: ix(size(rw))
 call random_number (w)
 call indexx(size(rw),rw,ix)
 rw = rw(ix)
!-----------------------------
 end subroutine random_permu_r
!-----------------------------

!---------------
 subroutine swap (m1,m2)
!---------------
 integer :: m, m1, m2
 m  = m1
 m1 = m2
 m2 = m
!-------------------
 END SUBROUTINE SWAP
!-------------------

!---------------------------
 RECURSIVE SUBROUTINE BOUNCE (x,bound_l,bound_r)
!-7/12/00-------------------
 implicit none
 intent(in):: bound_l, bound_r
 real (r8) :: x, bound_l, bound_r
 real (r8), parameter :: tol=1.d-5
 !if(x < bound_l) x=bound_l + tol 
 !if(x > bound_r) x=bound_r - tol 
 if (bound_l < x .and. x < bound_r ) then
   RETURN
 elseif (bound_l > x) then
   x=2.*bound_l - x
   call bounce (x,bound_l,bound_r) 
 elseif (bound_r < x) then
   x=2.*bound_r - x
   call bounce (x,bound_l,bound_r) 
 endif
!---------------------
 END SUBROUTINE BOUNCE
!---------------------

!--------------------------
 SUBROUTINE RANDOM_NORMAL_v (x, mu, s2)
!-7/12/00------------------
 real(r8), intent(in), optional :: mu, s2
 real(r8), intent(out) :: x(:)
 real(r8):: mu0, s20
 integer :: i
 mu0 = 1.d0; s20=1.d0
 if (present(mu)) mu0 = mu
 if (present(s2)) s20 = s2
 do i=1, size(x)
    call random_normal_1 (x(i),mu0,s20)
 enddo
!------------------------------
 END SUBROUTINE RANDOM_NORMAL_v
!------------------------------

!--------------------------
 SUBROUTINE RANDOM_NORMAL_m (x, mu, s2)
!-7/12/00------------------
 real(r8), intent(in), optional :: mu, s2
 real(r8), intent(out) :: x(:,:)
 real(r8):: mu0, s20
 integer :: i, j
 mu0 = 1.d0; s20=1.d0
 if (present(mu)) mu0 = mu
 if (present(s2)) s20 = s2
 do i=1, size(x,dim=1)
    do j=1, size(x,dim=2)   
       call random_normal_1 (x(i,j),mu0,s20)
    enddo
 enddo
!------------------------------
 END SUBROUTINE RANDOM_NORMAL_m
!------------------------------

!--------------------------
 SUBROUTINE RANDOM_NORMAL_1 (x,mu,s2)
!-7/12/00------------------
!by default N(0,1)
 real (r8), intent(in), optional :: mu, s2
 real (r8), intent(out) :: x
 real (r8) :: fac, gset,r,r1,r2,v1,v2
 integer :: iset
 data iset/0/
 save iset, gset
 if(iset.eq.0) then
 1 r1=rxnd()
   r2=rxnd()
   v1=(2.d0*r1-1.d0)
   v2=(2.d0*r2-1.d0)
   r=v1**2+v2**2
   if(r > 1.d0 .or. r == 0.d0) goto 1
   fac=sqrt(-2.d0*log(r)/r)
   gset=v1*fac
   x=v2*fac
   iset=1
 else
   x=gset
   iset=0
 endif
 if(present(mu) .and. present(s2)) x=x*sqrt(s2)+mu
!------------------------------
 END SUBROUTINE RANDOM_NORMAL_1
!------------------------------

!----------------------
 SUBROUTINE RANDOM_CHI2 (chi2,df)
!-7/12/00--------------
 implicit none
 integer :: i, df
 real (r8) :: chi2, x
 chi2 = 0.d0
 do i=1,df
    call random_normal (x,0.d0,1.d0)
    chi2 = chi2 + x*x
 enddo
!--------------------------
 END SUBROUTINE RANDOM_CHI2
!--------------------------

!----------------------
 REAL (r8) FUNCTION FHI (x,mu,s2)
!----------------------
 implicit none
 real (r8) :: x, mu, s2
 real (r8), parameter  :: c=0.39894228d0, big=150.d0
 fhi=min( (x-mu)**2/s2 * 0.5d0 , big)
 fhi=c*exp(-fhi)/sqrt(s2)
!----------------
 END FUNCTION FHI
!----------------

!--------------------------
 REAL (r8) FUNCTION LOG_FHI (x,mu,s2)
!--------------------------
 implicit none
 real (r8) :: x, mu, s2
 real (r8), parameter  :: c=0.918938533d0
 log_fhi=-(x-mu)**2/s2*0.5d0 - 0.5d0*log(s2) - c
!--------------------
 END FUNCTION LOG_FHI
!--------------------

!-------------------------
 REAL(r8) FUNCTION haldane (x)
!-------------------------
 real (r8) :: x
 haldane = -0.5*log(1.-2.*min(x,.49999999d0))
!--------------------
 END FUNCTION haldane
!--------------------

!---------------------------
 REAL(r8) FUNCTION inhaldane (d)
!---------------------------
 real (r8) :: d
 inhaldane = 0.5d0*(1.d0-exp(-2.d0*abs(d)))
!----------------------
 END FUNCTION inhaldane
!----------------------

!-------------------
 SUBROUTINE mkdprime (dprime, r2, gen_1, gen_2, use)
!-------------------
! Computes D prime and R2 linkage disequilibrium measures
! between loci gen_1 and gen_2
 real, parameter      :: tol=0.0001
 logical, optional    :: use(:)
 logical, allocatable :: use0(:)
 integer              :: i, j, n_ind, gen_1(:), gen_2(:)
 real(r8)             :: pi, pj, dij, dmax, dprime, r2

 if(size(gen_1) /= size(gen_2)) then
    print*, 'WARNING in MKDPRIME: unequal vector sizes'
    RETURN
 endif

 allocate(use0(size(gen_1)))
 if(present(use)) then
    use0 = use
 else
    use0 = .true.
 endif
 
 r2 = 0.
 dprime = 0.
 n_ind = count(use0)
 if(n_ind==0) RETURN

 do i = minval(gen_1(:)), maxval(gen_1(:))
    pi = count(gen_1(:)==i .and. use0)
    pi = pi/n_ind

    do j = minval(gen_2(:)), maxval(gen_2(:))
       pj = count(gen_2(:)==j .and. use0)
       pj = pj / n_ind
       if (pi>tol .and. pj>tol .and. (1-pi)>tol .and. (1-pj)>tol) then
           dij = count((gen_1(:)==i) .and. (gen_2(:)==j) .and. use0)
           dij = dij/n_ind - pi*pj   
           if (dij<0.0) then
              dmax = min (pi*pj,(1-pi)*(1-pj) )
           else 
              dmax = min (pi*(1-pj), pj*(1-pi) )
           endif  
           if (dmax>0.) dprime = dprime + pi*pj*abs(dij)/dmax 
           r2 = r2 + dij**2 / (pi*pj)
       endif      
    enddo
 enddo
 r2 = correlation(real(gen_1,r8), real(gen_2,r8), use)**2
 deallocate(use0)
!--------------
 END SUBROUTINE
!--------------

!---------------------------
 REAL (r8) FUNCTION EXTRACTR (mat0, q, mask) RESULT (mat)
!---------------------------
!=========================================================
! This function extracts according to a mask (rows, dim=1)
!=========================================================
 integer   :: i, q, n 
 real (r8) :: mat0(:,:), v(size(mat0))
 logical   :: mask(:), maskm(size(mat0,dim=1),size(mat0,dim=2))
 dimension :: mat(q,size(mat0,dim=2))

 n=size(mat0,dim=1)
 maskm=.false.
 do i=1,n
    if(mask(i)) maskm(i,:)=.true.
 enddo
 v(1:size(mat))=pack(mat0,maskm)
 mat=reshape(v(1:size(mat)), (/q,size(mat0,dim=2)/) )
!----------------------
 END FUNCTION EXTRACTR
!----------------------

!----------------------------
 REAL (r8) FUNCTION EXTRACT_1 (mat0, q, mask) RESULT (mat)
!----------------------------
!-----------------------------------------------------
! This function extracts according to a mask (columns)
!-----------------------------------------------------
 integer      :: i, q, n 
 real (r8)    :: mat0(:,:), v(size(mat0))
 logical      :: mask(:), maskm(size(mat0,dim=1),size(mat0,dim=2))
 dimension    :: mat(size(mat0,dim=1),q)

 n=size(mat0,dim=2)
 maskm=.false.
 do i=1,n; if(mask(i)) maskm(:,i)=.true.; enddo
 v(1:size(mat)) = pack(mat0,maskm)
 mat=reshape(v(1:size(mat0)), (/size(mat0,dim=1),q/))
!----------------------
 END FUNCTION EXTRACT_1
!----------------------

!--------------------------
 LOGICAL FUNCTION EXTRACT_2 (mat0, q, mask) RESULT (mat)
!--------------------------
!---------------------------------------------------------------
! This function extracts a logical matrix according to a mask (columns)
!---------------------------------------------------------------
 integer    :: i, q, n 
 logical    :: mat0(:,:), v(size(mat0)) &
             , mask(:), maskm(size(mat0,dim=1),size(mat0,dim=2))
 dimension  :: mat(size(mat0,dim=1),q)

 n=size(mat0,dim=2)
 maskm=.false.
 do i=1,n; if(mask(i)) maskm(:,i)=.true.; enddo
 v(1:size(mat)) = pack(mat0,maskm)
 mat=reshape(v,(/size(mat0,dim=1),q/))
!----------------------
 END FUNCTION EXTRACT_2
!----------------------


!---------------------------------
 REAL (r8) FUNCTION MATMUL0_1 (v1,v2,mask1,mask2) RESULT (mat)
!---------------------------------
!---------------------------------------------------------------
! This function is a generalization of intrinsic MATMUL to allow
! for missing values
! values where mask is false are eliminated
! MATRIX x MATRIX
!---------------------------------------------------------------
 real (r8)    :: v1(:,:), v2(:,:)
 logical, optional :: mask1(:,:), mask2(:,:)
 dimension         :: mat(size(v1,dim=1),size(v2,dim=2))
 real (r8)    :: cv1(size(v1,dim=1),size(v1,dim=2))
 real (r8)    :: cv2(size(v2,dim=1),size(v2,dim=2))

 if(.not.present(mask1) .and. .not.present(mask2)) then
   mat=matmul(v1,v2)
 else
   cv1=v1; cv2=v2
   if(present(mask1)) where(.not.mask1) cv1=0.d0
   if(present(mask2)) where(.not.mask2) cv2=0.d0
   mat=matmul(cv1,cv2)
 endif
!----------------------
 END FUNCTION MATMUL0_1
!----------------------

!---------------------------------
 REAL (r8) FUNCTION MATMUL0_2 (v1,v2,mask1,mask2) RESULT (mat)
!---------------------------------
!---------------------------------------------------------------
! This function is a generalization of intrinsic MATMUL to allow
! for missing values
! values where mask is false are eliminated
! MATRIX x VECTOR
!---------------------------------------------------------------
real (r8)    :: v1(:,:), v2(:)
logical, optional :: mask1(:,:), mask2(:)
dimension         :: mat(size(v1,dim=1))
real (r8)    :: cv1(size(v1,dim=1),size(v1,dim=2))
real (r8)    :: cv2(size(v2))

if(.not.present(mask1) .and. .not.present(mask2)) then
  mat=matmul(v1,v2)
else
  cv1=v1; cv2=v2
  if(present(mask1)) where(.not.mask1) cv1=0.d0
  if(present(mask2)) where(.not.mask2) cv2=0.d0
  mat=matmul(cv1,cv2)
endif
!----------------------
 END FUNCTION MATMUL0_2
!----------------------

!---------------------------------
 REAL (r8) FUNCTION MATMUL0_3 (v1,v2,mask1,mask2) RESULT (mat)
!---------------------------------
!---------------------------------------------------------------
! This function is a generalization of intrinsic MATMUL to allow
! for missing values
! values where mask is false are eliminated
! VECTOR x MATRIX
!---------------------------------------------------------------
real (r8)    :: v1(:), v2(:,:)
logical, optional :: mask1(:), mask2(:,:)
dimension         :: mat(size(v2,dim=2))
real (r8)    :: cv1(size(v1))
real (r8)    :: cv2(size(v2,dim=1),size(v2,dim=2))
 
if(.not.present(mask1) .and. .not.present(mask2)) then
  mat=matmul(v1,v2)
else
  cv1=v1; cv2=v2
  if(present(mask1)) where(.not.mask1) cv1=0.d0
  if(present(mask2)) where(.not.mask2) cv2=0.d0
  mat=matmul(cv1,cv2)
endif
!----------------------
 END FUNCTION MATMUL0_3
!----------------------

!----------------------------
 REAL (r8) FUNCTION MATMUL0_4 (v1,v2,mask1,mask2) RESULT (mat)
!----------------------------
!---------------------------------------------------------------
! This function is a generalization of intrinsic MATMUL to allow
! for missing values
! values where mask is false are eliminated
! VECTOR x VECTOR'
! WARNING: matmul0(v1,v2) /= matmul0(v2,v1)
!---------------------------------------------------------------
real (r8)    :: v1(:), v2(:)
logical, optional :: mask1(:), mask2(:)
dimension         :: mat(size(v1),size(v2))
real (r8)    :: cv1(size(v1))
real (r8)    :: cv2(size(v2))
integer           :: i, j
 
cv1=v1; cv2=v2
if(present(mask1)) where(.not.mask1) cv1=0.d0
if(present(mask2)) where(.not.mask2) cv2=0.d0
do i=1, size(v1)
   do j=1, size(v2)
      mat(i,j)=cv1(i)*cv2(j)
   enddo
enddo
!----------------------
 END FUNCTION MATMUL0_4
!----------------------

!--------------------------
 REAL (r8) FUNCTION MATMUL2 (v1,v2) RESULT (mat)
!--------------------------
!---------------------------------------------------------------
! This function is a generalization of intrinsic MATMUL to allow
! for the vector multiplication v v'
! WARNING: matmul2(v1,v2) /= matmul2(v2,v1)
!---------------------------------------------------------------
real (r8) :: v1(:), v2(:)
dimension      :: mat(size(v1),size(v2))
integer        :: i, j, m, n
m=size(v1)
n=size(v2)
do i=1, m
   do j=1, n
      mat(i,j)=v1(i)*v2(j)
   enddo
enddo
!--------------------
 END FUNCTION MATMUL2
!--------------------

!-------------------------------
 REAL (r8) FUNCTION dot_product0 (v1, v2, mask)
!-------------------------------
! values where mask is false are eliminated 
 real (r8) :: v1(:), v2(:)
 logical, optional :: mask(:)
 if (.not.present(mask)) then
    dot_product0 = dot_product(v1,v2)
 else
    dot_product0 = sum(v1*v2, mask=mask)
 endif
!-------------------------
 END FUNCTION dot_product0
!-------------------------

!--------------------------
 REAL (r8) FUNCTION diag_r8 (mat,mask) RESULT(d)
!--------------------------
!extract diagonal
!values where mask is false are eliminated
 optional  :: mask 
 real (r8) :: mat(:,:)
 dimension :: d(size(mat,dim=1))
 logical   :: mask(:)
 integer   :: i
 if (.not.present(mask)) then
    d=(/(mat(i,i), i=1,size(mat,dim=1))/)
 else
    STOP 'diag option not defined'
 endif
!--------------------
 END FUNCTION diag_r8
!--------------------

!-----------------------
 INTEGER FUNCTION diag_i (mat,mask) RESULT(d)
!-----------------------
!extract diagonal
!values where mask is false are eliminated
 optional  :: mask 
 integer   :: mat(:,:), i
 dimension :: d(size(mat,dim=1))
 logical   :: mask(:)

 if (.not.present(mask)) then
    d(:) = (/(mat(i,i), i=1,size(mat,dim=1))/)
 else
    STOP 'diag option not defined'
 endif
!-------------------
 END FUNCTION diag_i
!-------------------

!----------------------------
 REAL (r8) FUNCTION id_matrix (n) RESULT(d)
!----------------------------
!creates a diagonal matrix of dimension n
 integer   :: i, n
 dimension :: d(n,n)
 d(:,:)=0.d0
 d(:,1)=1.d0
 d(:,:)=cshift( array=d, shift=(/(-i,i=0,n-1)/), dim=2)
!----------------------
 END FUNCTION id_matrix
!----------------------

!--------------------
 REAL FUNCTION MEAN_r (v, mask)
!--------------------
! Computes the mean of a vector
! values where mask is false are eliminated 
  real    :: v(:)
  logical, optional :: mask(:)
  if(.not.present(mask)) then 
    mean_r = sum(v)/size(v)
  else
    if(count(mask)==0) RETURN
    mean_r = sum(v,mask=mask) / count(mask)
  endif
!-----------------
 END FUNCTION 
!-----------------

!--------------------------
 REAL (r8) FUNCTION MEAN_r8 (v, mask)
!--------------------------
! Computes the mean of a vector
! values where mask is false are eliminated 
  real (r8)    :: v(:)
  logical, optional :: mask(:)
  if(.not.present(mask)) then 
    mean_r8 = sum(v)/size(v)
  else
    if(count(mask)==0) RETURN
    mean_r8 = sum(v,mask=mask) / count(mask)
  endif
!-----------------
 END FUNCTION
!-----------------

!-----------------------------
 REAL (r8) FUNCTION VARIANCE_1 (v, mask)
!-----------------------------
! Computes the sampling variance of a vector, d.f.=n-1
! values where mask is false are eliminated 
  optional :: mask
  real(r8) :: v(:)
  logical  :: mask(:)
  integer  :: n
  if (.not.present(mask)) then
    n=size(v)
    variance_1 = (dot_product(v,v)-sum(v)**2/n)/(n-1.d0)
  else
    n=count(mask)
    if (n==0) RETURN
    variance_1 = (dot_product0(v,v,mask)-sum(v,mask=mask)**2/n)/(n-1.d0)
  endif
!-----------------------
 END FUNCTION VARIANCE_1
!-----------------------

!----------------------------
 REAL(r8) FUNCTION VARIANCE_n (v, mask) RESULT(var)
!----------------------------
! Computes the sampling variance of a vector, d.f.=n-1
! values where mask is false are eliminated 
! returns a variance-covariance matrix
! WARNING: VARIABLES ARE IN COLUMNS!
  optional  :: mask 
  real(r8)  :: v(:,:)
  logical   :: mask(:,:)
  integer   :: i, j, k
  dimension :: var(size(v,dim=2),size(v,dim=2))

  k = size(v,dim=2)
  if (.not.present(mask)) then
    do i=1, k
       var(i,i) = variance_1(v(:,i))
       do j=i+1, k
          var(i,j) = covariance(v(:,i),v(:,j))
          var(j,i) = var(i,j)
       enddo
    enddo
  else
    do i=1, k
       var(i,i) = variance_1(v(:,i),mask(:,i))
       do j=i+1, k
          var(i,j) = covariance (v(:,i), v(:,j), &
                                (mask(:,i).and.mask(:,j)) )
          var(j,i) = var(i,j)
       enddo
    enddo
  endif
!-----------------------
 END FUNCTION VARIANCE_n
!-----------------------

!-----------------------------
 REAL (r8) FUNCTION COVARIANCE (v1,v2,mask)
!-----------------------------
! Computes the covariance of vectors v1 & v2, d.f.=n-1
  real (r8)    :: v1(:), v2(:)
  logical, optional :: mask(:)
  integer :: n
  if(size(v1) /= size(v2)) STOP 'COVARIANCE: unequal size vectors'
  if (.not.present(mask)) then
    n=size(v1)
    covariance=(dot_product(v1,v2)-sum(v1)*sum(v2)/n)/(n-1.d0)
  else
    n=count(mask)
    if (n==0) RETURN
    covariance=(dot_product0(v1,v2,mask) -                  &
                sum(v1,mask=mask)*sum(v2,mask=mask)/n)/(n-1.d0)
  endif
!-----------------------
 END FUNCTION COVARIANCE
!-----------------------

!--------------------------------
 REAL (r8) FUNCTION CORRELATION_1 (v1, v2, mask)
!--------------------------------
! Computes correlation coefficient of vectors v1 & v2
  optional :: mask
  real(r8) :: v1(:), v2(:)
  logical  :: mask(:)
  if (.not.present(mask)) then
    correlation_1 = covariance(v1,v2)/(sqrt(variance(v1)*variance(v2)))
  else
    correlation_1 = covariance(v1,v2,mask)/ &
                   (sqrt(variance(v1,mask)*variance(v2,mask)))
  endif
!--------------------------
 END FUNCTION CORRELATION_1
!--------------------------

!--------------------------------
 REAL (r8) FUNCTION CORRELATION_n (v1, mask) RESULT (rho)
!--------------------------------
! Computes correlation matrix of variables in matrix v1
  optional  :: mask
  real (r8) :: v1(:,:), sd(size(v1,dim=2))
  logical   :: mask(:,:)
  integer   :: i, j, k
  dimension :: rho(size(v1,dim=2), size(v1,dim=2))

  k = size(v1,dim=2)
  if (.not.present(mask)) then
     rho = variance(v1)
  else
     rho = variance(v1, mask)
  endif
  sd = sqrt(diagm(rho))
  do i=1, k
     do j=1, k
        rho(i,j) = rho(i,j) / sd(i) / sd(j)
     enddo
  enddo
!--------------------------
 END FUNCTION CORRELATION_n
!--------------------------

!----------------------------------
 REAL (r8) FUNCTION AUTOCORRELATION (v, istep)
!----------------------------------
! Computes autocorrelation coefficient of vector v1 every 1 step
  optional :: istep
  integer  :: istep, n
  real(r8) :: v(:), v1(size(v)), v2(size(v)) 
  n = size(v)
  v1(1:n-1) = v(1:n-1)
  v2(1:n-1) = v(2:n)
  autocorrelation = correlation_1 (v1(1:n-1), v2(1:n-1))
!----------------------------
 END FUNCTION AUTOCORRELATION
!----------------------------

!---------------------
 SUBROUTINE PRINTMAT_1 (mat, iout)
!---------------------
 optional :: iout
 integer  :: i, iout
 real (r8):: mat(:,:)
 if (present(iout)) then
    do i=1, size(mat,dim=1)
       write(iout,'(i3,10000f10.4)') i, mat(i,:)
    enddo
 else
    do i=1, size(mat,dim=1)
       print*, ' '
       write(*,'(i3,10000f10.4)') i, mat(i,:)
    enddo
 endif
!------------------------
 END SUBROUTINE PRINTMAT_1
!------------------------

!---------------------
 SUBROUTINE PRINTMAT_2 (vec, iout)
!---------------------
 optional :: iout
 integer   :: i, iout
 real (r8) :: vec(:)
 if (present(iout)) then
    do i=1, size(vec)
      write(iout,'(i3,10000f10.4)') i, vec(i)
    enddo
 else
    print*, ' '
    do i=1, size(vec)
      write(*,'(i3,10000f10.4)') i, vec(i)
   enddo
 endif
!------------------------
 END SUBROUTINE PRINTMAT_2
!------------------------

!---------------------
 SUBROUTINE PRINTMAT_3 (mat, iout)
!---------------------
 optional :: iout
 integer  :: i, iout, mat(:,:)
 if (present(iout)) then
    write(iout,*) ' '
    do i=1, size(mat,dim=1)
      write(iout,'(i3,10000i5)') i, mat(i,:)
    enddo
 else
    print*, ' '
    do i=1, size(mat,dim=1)
      write(*,'(i3,1000i5)') i, mat(i,:)
    enddo
 endif 
!------------------------
 END SUBROUTINE PRINTMAT_3
!------------------------

!---------------------
 SUBROUTINE PRINTMAT_4 (mat, iout)
!---------------------
 optional :: iout
 integer  :: i, iout, mat(:)
 if (present(iout)) then
   write(iout,*) ' '
   do i=1, size(mat)
      write(iout,'(i3,10000i5)') i, mat(i)
   enddo
 else
   write(*,*) ' '
   do i=1, size(mat)
      write(*,'(i3,10000i5)') i, mat(i)
   enddo
 endif
!------------------------
 END SUBROUTINE PRINTMAT_4
!------------------------

!----------------------------
 LOGICAL FUNCTION ABOUT_EQUAL (v, v_old, new_tol)
!----------------------------
! tests whether the values in two vectors are similar
! used for testing convergence in iterative procedures
 real (r8), parameter :: default_tol=1.d-6
 real (r8), optional  :: new_tol
 real (r8) :: v(:), v_old(:), tol
 tol = default_tol
 if (present(new_tol)) tol = new_tol
 if (dot_product(v-v_old, v-v_old) / size(v) < tol) then
   about_equal = .true. 
 else
   about_equal = .false.
 endif
!------------------------
 END FUNCTION ABOUT_EQUAL
!------------------------

!-----------------------
 LOGICAL FUNCTION APPROX (v, w)
!-----------------------
! tests whether two values are similar
 real (r8), parameter :: default_tol=1.d-6
 real (r8), intent(in):: v, w
 if (abs(v-w) < default_tol) then
   approx = .true. 
 else
   approx = .false.
 endif
!-------------------
 END FUNCTION APPROX
!-------------------

!-------------------------
 LOGICAL FUNCTION APPROX_r4 (v, w)
!-------------------------
! tests whether two values are similar
 real (r4), parameter :: default_tol=1.e-6
 real (r4), intent(in):: v, w
 if (abs(v-w) < default_tol) then
   approx_r4 = .true. 
 else
   approx_r4 = .false.
 endif
!-------------------
 END FUNCTION APPROX_r4
!-------------------

!------------------------------
 SUBROUTINE PRINT_DATE_AND_TIME (iout, label, oneline)
!-12/09/02---------------------
 optional         :: iout, label, oneline
 integer          :: iout
 character(len=*) :: label
 logical          :: oneline
   
 character :: date*10, time*10
 call date_and_time (date,time)
 if (present(iout)) then
    if (present(label)) write(iout,*) label
    write(iout,*) 'Date yy/mm/dd ',date(1:4),'/',date(5:6),'/',date(7:8)
    write(iout,*) 'Time hh/mm/ss   ',time(1:2),'/',time(3:4),'/',time(5:6)
 else
    if(present(oneline) .and. oneline) then
        if (present(label)) then
           write(*,*) label,' ',date(1:4),'/',date(5:6),'/',date(7:8),' ',time(1:2),':',time(3:4),':',time(5:6)
        else
           write(*,*) date(1:4),'/',date(5:6),'/',date(7:8),' ',time(1:2),':',time(3:4),':',time(5:6)
        endif
     else
       if (present(label)) write(*,*) label
       write(*,*) ' Date yy/mm/dd ',date(1:4),'/',date(5:6),'/',date(7:8)
       write(*,*) ' Time hh/mm/ss   ',time(1:2),'/',time(3:4),'/',time(5:6)
    endif
 endif
!----------------------------------
 END SUBROUTINE PRINT_DATE_AND_TIME
!----------------------------------


!---------------------
 SUBROUTINE METROPOLIS (l_old,l_new,accepted)
!--7/12/00------------
!function assumed to be in log scale
implicit none
real (r8) :: l_old,l_new,l_ratio, p, x
real (r8), parameter :: tol_log=30
logical :: accepted
l_ratio=l_new-l_old
if(l_new > l_old) then
  accepted=.true. 
else 
  call random_number(x)
  if(l_ratio > tol_log) then
    p=1.d0/exp(tol_log)
  else 
    p=exp(l_ratio)
  endif
  if(p >= x) then
    accepted=.true.
  else
    accepted=.false.
  endif
endif
!-------------------------
 END SUBROUTINE METROPOLIS
!-------------------------

!---------------
 SUBROUTINE ginv (a,irank)
!---------------
! returns generalized inverse of matrix x of size n x n declared
! as m x m. tol is working zero (real*8) and irank returns the rank of
! the matrix. w is a work vector of size m,
! by rohan fernando, slightly structured by i. misztal 05/05/87
! translated brute force to f90 by M Perez-Enciso 2/2003
 real(r8), parameter :: tol = 1.d-12
 real(r8) a(:,:), w(size(a,dim=1)), re, sum
 integer i, j, ii, iim1, iii, n, irank

 n = size(a,dim=1)
 irank=n
 do i=1,n
   do j=1,i-1
      re=a(i,j)
      do ii=i,n
         a(ii,i) = a(ii,i)-re*a(ii,j)
      enddo
   enddo
   if (a(i,i) < tol) then
      a(i,i) = 0.0
      a(i+1:n,i) = 0.
      irank = irank-1
   else
      a(i,i)=sqrt(a(i,i))
      do ii=i+1,n
         a(ii,i)=a(ii,i)/a(i,i)
      enddo
   endif
 enddo
 
 do i=1,n
   if (a(i,i)==0.) then
      a(i+1:n,i)=0.
   else
      a(i,i) = 1.d0/a(i,i)
      w(i+1:n) = 0.
      do ii=i+1,n
         iim1=ii-1
         re=a(iim1,i)
         do iii=ii,n
            w(iii)=w(iii)-a(iii,iim1)*re
         enddo
         if (a(ii,ii)==0.) then
            a(ii,i)=0.
         else
            a(ii,i)=w(ii)/a(ii,ii)
         endif
      enddo
   endif
 enddo
 
 do j=1,n
    do i=j,n
       a(i,j)=dot_product(a(i:n,j),a(i:n,i))
    enddo
 enddo

 do i=1,n
    a(i,i+1:n)=a(i+1:n,i)
 enddo
!--------------
 END SUBROUTINE
!--------------

!------------------
 SUBROUTINE irecode (vec)
!------------------
! recode in consecutive ascending integer order a vector values
 intent (inout) :: vec
 integer :: i, idim(2), j, vec(:)
 integer, allocatable :: temp(:)

 idim(1) = minval(vec)
 idim(2) = maxval(vec)
 allocate (temp(idim(1):idim(2)))
 temp = 0
 temp(vec) = 1
 j = 0
 do i=idim(1), idim(2)
    if (temp(i) /= 0) then
       j = j + 1
       temp(i) = j
    endif
 enddo 
 vec = temp(vec)
 deallocate (temp)
!----------------------
 END SUBROUTINE irecode
!----------------------

!----------------------
 INTEGER FUNCTION ijmap (i, j, start)
!----------------------
! Finds position in a symmetric low triangular stored matrix
! It requires previous computation of Sum(n(i-1)), done in
! FILL_START subroutine
  integer :: i, j, start(:)
  ijmap = start(max(i,j)) + min(i,j)
!------------------
 END FUNCTION ijmap
!------------------

!---------------------
 SUBROUTINE fill_start (start, n)
!---------------------
 intent(in) :: n
 intent(out):: start
 integer :: i, j, n, start(:)
 j = 0
 do i=1, n
    start(i) = j
    j = j + i
 enddo
!-------------------------
 END SUBROUTINE fill_start
!-------------------------
 
!*********************************************************************
!        STATISTICS
!*********************************************************************

!-----------------
 subroutine T_TEST (t, df, x, y)
!-----------------
 intent(in) :: x, y
 intent(out):: t, df
 real(r8)   :: t, x(:), y(:), s2, n, m
 integer    :: df

 t  = 0.; df = 0
 n  = size(x)
 m  = size(y)
 s2 =  dot_product(x-mmean(x), x-mmean(x)) + &
       dot_product(y-mmean(y), y-mmean(y))
 s2 = s2/(n+m-2)
 if(n>2 .and. m>2) then
    t  = (mmean(x)-mmean(y)) / sqrt(s2*(1./n + 1./m))
    df = n+m-2
 endif
!--------------
 end subroutine
!--------------
 
!*********************************************************************
!        BEGIN OF NAG INTERFACE SUBROUTINES
!*********************************************************************

!------------------
 subroutine NAGNOVA (it, y, F, p_value)
!------------------
! interface to NAG G04BBF ANOVA completely randomized design
! IT: treatments, integer
! Y: records, real8
! F: F value
! P_Value of the corresponding F

 integer, parameter :: ldt=10, ldc=ldt
 integer :: n, iblock, nt, it(:), irep, irdf, ifail
 real(r8):: y(:), F, p_value, gmean, bmean, tmean, table(ldt,5) &
          , c, r(size(y)), ef(size(y)), tol, wk
 logical :: unassigned(size(it))
 allocatable :: bmean(:), tmean(:), c(:,:), irep(:), wk(:) 

 n=size(y)
 iblock=1
 ifail = -1
 irdf = 0
 tol=0.d0

 if (any(it==0)) STOP 'NAGNOVA: Treatment codes must be 1 at least'

 !-- count # treatments
 unassigned = .true.
 nt = 0
 do while (any(unassigned))
   if( minval(it,mask=unassigned) > nt) then
     nt = nt+1
     where (it == minval(it,mask=unassigned))
       it=nt
       unassigned=.false.
     endwhere
   endif
 enddo
 iblock = 1
 allocate (bmean(abs(iblock)))
 allocate (tmean(nt))
 allocate (c(ldc,nt))
 allocate (irep(nt))
 allocate (wk(nt*nt+nt))

 !call G04BBF (n, y, iblock, nt, it, gmean, bmean, tmean, table, ldt &
 !           , c, ldc, irep, r, ef, tol, irdf, wk, ifail)
  STOP 'wrongcall'
 F       = table(2,4)
 p_value = table(2,5)

 deallocate (bmean, tmean, c, irep, wk) 

!----------------------
 end subroutine nagnova
!----------------------

!********************
 END MODULE AUX_SUB_M
!********************
