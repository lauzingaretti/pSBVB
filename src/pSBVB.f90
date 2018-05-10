!ULL correct print iom line 1833 aprox
! 20171025 option MIMIC_DIPLOID to treat polyploids as diploid
! dogrm, var is n*p*(1-p)
! think of estimates when allele dosages are not known
! 20170623 ALLOPLOIDY
! 20170621 additive modeling generalized as a-ploidy/2
!          dominance modeled via generalized delta
! 20170613 print_indiv subroutine
! 20170610 SBVB diverges to allow for polyploids
! add NPAR ?
! gamma  S = SHAPE PARAMETER OF DISTRIBUTION (0 < REAL).
!        B = Scale parameter
! 2016/08/09 15:12:30 : check ve>0 can be met
!                       check pedfile, qtnfile, nbase specified
! 2016/08/08 14:54:35 : detects nf=13 in epifile for ntrait=1
!                       bug in read_pos
! 2016/03/22 13:09:31 : allows controlling first indiv to write G, mkr and y
! 2016/03/16 16:41:40 : controls max no of xovers MXOVER section
! 2016/03/09 12:44:08 : GZIP compresses gwas and mkr files
! 2016/03/08 09:15:28 : GWASFILE: prints regression coeff & b/se
!                       checks H2BYCOL requires FRQBYCOL
! 2016/03/03 12:41:15 : option OLDHAP reads old hap files where no dom and epi were written
!                       correction in computing s2e con h2 y h2g
! 2016-03-02 16:59:59 : bug in getting ve
! 2016/03/01 17:03:38 : bug in init_g for sexchr
!                       ybv file is y, tbv, tbv+dom, tbv+dom+epi
!                       hap file is y, tbv, dom, epi
! 2016/02/29 16:20:05 : modify dogenotypes to save memory
!                       error in init gf%h2 / h2g
! 2016/02/26 10:53:06 : epistasis modeled via epifile
!                       ULL: tbv does not include the epistatic component
!                       ybv file is y, tbv+epi, tbv ; tbv includes add+dom
! 2016/02/24 12:20:21 : multiple traits
!                       ULL hapfile fomat includes no. of traits in header
!                       check if sex missing
! 2016/01/27 15:51:33: sex chrs
!                      requires id field sex, takes first allele in X male chrs
!                      check sortcor and rm zeros
! 2016/01/25 18:07:00: small init bug when not restartql, hapfile format changed slightly
! 2016/01/19 16:00:07: small bug in reading hap when h2bycol
!                      no overdomininance by default
! ok read complete path files with '(a)' format
! ok expand_nbase and qtlrestart
! allows for base population expansion
! v6 allow for constant h2 across breeds by diff ve
! bug in computing vg, divide by nind instead of nbase
! print qtl freqs for whole pedigree and by class in pedigree file
! error in reading hap when restartqtl
! small rearrangement in computing missing snps
! RESTARTQTL simulate qtl conditional on pedigree (disregard hap information)
!    several parts restructured
! allow for correlation btw freq and effect RHOQA
! print seed
! set recs btw 1 and 3: Mercier et al 2015. Ann Rev Plant Biol
! allow for sex specific rec rates
! include blas
! output in tped & tfam plink format for GWAS (option OUTPLINK)
! optimizes computing GRM
! v2: allows for several chip files (up to 9), decide on options useseq, etc
! check qtl and snp files
! remove fixed snps when reading vcf
! ok outhap and inhap files are now equal
! ok ULL error in gt sum as declared integer 1
! ok optimize memory in reading snps from vcf
! ok optimizes memory with integer(kind=1) in genotype arrays
! ok guess max no. SNPs in parfile! allows for undefined snp or qtl positions
! ok program print output: mkr: check. dochip, replace printchip by printmkr
! ok check binary traits
! ok allow for larger than last snp position in defining rec maps
! ok print grm
! ok haplotype reading and writing, check if y already generated, read y, tbv
! ok read previous haplotypes, this should allow running sbvb several times
! ok allocate memory dynamically when reading genotypes
! ok check meaining of sign qtl
! ok check local and general variables
! OK allow for unequal rec rates, move map into %
! ok check dogenotype, program qtl effects, read qtlpos
! ok generate qtl effects (see ms2gs)
! ok new type for gfounder
! ok get qtl origin in mpeiosic
! ok check meiosis, do we need ipos? 
! ok modify mpeiosic for chr%ipos

!----------------------------------------------------------------------------------------!
!                            Sequence Based Virtual Breeding                             !
!------                                                                           -------!
!                      (c) Miguel Perez-Enciso (miguel.perez@uab.es)                     !
!----------------------------------------------------------------------------------------!
! compile
!   make:    gfortran -fcheck=all kind.f90 ALliball.f90 aux_sub11.f90 sbvb.f90 -o sbvb -lblas
!   make o3: gfortran -O3 kind.f90 ALliball.f90 aux_sub11.f90 sbvb.f90 -o sbvb -lblas
! usage
!  zcat file.vcf.gz | perl vcf2tped2.pl -hap | awk '{$2=$3=""}1' | ./sbvb -i sbvb.par 
!  cat file.vcf | perl vcf2tped2.pl -hap | ./sbvb -i sbvb.par 
!  cat file.vcf | perl vcf2tped2.pl -hap | ./sbvb -i sbvb.par | ./doselection | ./sbvb -i sbvb.par ...
!
! the procedure for gs would be
!  (1) run sbvb < par,m,ped > y,m,grm,hap
!  (2) run gs   < y,m,grm   > par,ped
!  (3) run sbvb < par,m,ped,hap > y,m,grm,hap
!  (4) run gs ...
!
! keeps track internally only of recombinant events and recover needed genotypes 
! using a fast algorithm
!----------------------------------------------------------------------------------------!

!-------------------
 MODULE sbvb_m
!-------------------
 use kinds
 use aux_sub_m

!---------------
 type t_qdensity ! qtl distribution
!---------------
! f can be U, N, G, or C for uniform, normal, gamma
 character :: f*1=' '
 real(r8)  :: x=5, s=2
!-------------------
 end type t_qdensity
!------------------- 

!-----------------
 type t_chip
!-----------------
 integer               :: nsnp=0
 integer, pointer      :: ipos(:)=>null()    !snp order of chip snp
 integer(i8), pointer  :: chipos(:)=>null()  !position of chip snp (used only temporarily)
!-----------------
 end type t_chip
!-----------------

!-----------------
 type t_fchr    
!-----------------
! contains chr features
 integer               :: nqtl=0, nsnp=0, xploidy=2 !--> ploidy 1 for  male X
 real                  :: lengthM(2)          ! chr length in Morgans, per sex
 integer(i8), pointer  :: pos(:)=>null()      ! snp positions
 integer(i8), pointer  :: qtlpos(:)=>null()   ! qtl positions
 integer,     pointer  :: iposqtl(:)=>null()  ! snp order of qtl
 real,        pointer  :: qtladd(:,:)=>null() ! add effect i-th qtl and j-th trait
 real,        pointer  :: qtldom(:,:)=>null() ! dom effect i-th qtl
 integer,     pointer  :: qtlepi(:,:)=>null() ! epistatic ipair and ilocus in pair (1,2), 0 if none
 real,        pointer  :: freq(:)=>null()     ! snp freq in founder pop
 integer(i8), pointer  :: mapbp(:)=>null()    ! bp position of recombination chunk
 real(r8),    pointer  :: mapper(:,:)=>null() ! chunk % length relative to total length lengthM
 real(r8),    pointer  :: pxover(:,:)=>null() ! prob of having nx xovers per sex
 type(t_chip),pointer  :: chip(:)=>null()
!---------
 end type
!---------

!----------------
 type t_fgenome      
!----------------
! contains genome features, including ploidy, delta 
 integer                  :: nchr=23, ntrait=1, nchip=0, nepi=0, sexchr=0, mxover=3, ploidy=2
 real                     :: cm2mb=1, sign_qtl=0.5
 real, pointer            :: se(:,:)=>null(), &  ! allows for ve heterogeneity btw breeds
                             va(:,:)=>null(), &
                             vg(:,:)=>null(), &
                             vepi(:)=>null(), &
                             h2(:)=>null(),   &
                             h2g(:)=>null(),  &
                             p(:)=>null(),    &
                             threshold(:)=>null(), &
                             delta(:)=>null()
 real(r8), pointer        :: rhoqa(:)=>null(), &  ! cor btw qtl eff and freq
                             tau(:,:)=>null()     ! for polyploids, controls P of pairing across homeol.
 logical                  :: init_add=.false., init_dom=.false., &
                             overdominance=.false., mimic=.false.
 logical, pointer         :: binary(:)=>null()
 type(t_fchr), pointer    :: fchr(:)=>null()
 type(t_epipair),pointer  :: epi(:)=>null()
 type(t_qdensity),pointer :: qdista(:)=>null(), &
                             qdistd(:)=>null()
!----------------
 end type
!----------------

!--------------
 type t_epipair
!--------------
 integer :: trait, chr(2), iqtl(2)
 real    :: eff(3,3)
!--------------
 end type
!--------------

!----------------
 type t_chunk_chr    ! contains chr structure as chr chunks
!----------------
! gt filled in only for founder individuals
 integer                  :: nx=0, nqtl=0, xploidy=2
 integer(kind=1), pointer :: gt(:)=>null()      ! founder's genotype 
 integer(i8), pointer     :: pos(:)=>null()     ! bp position of chr chunk
 integer,     pointer     :: ori(:)=>null()     ! chunk origin
 integer,     pointer     :: qtlori(:)=>null()  ! origin of i-th qtl
!----------------
 end type 
!----------------

!----------------
 type t_genome      
!----------------
! contains genomes of population individuals, can be chip, or gfeatures
! depending on fields filled-in in t_chunk_chr 
 integer                    :: nchr=0
 type(t_chunk_chr), pointer :: chr(:,:)=>null()
!----------------
 end type
!----------------

!-------------
 type t_indiv
!-------------
 real(r8), pointer :: y(:)=>null(), tbv(:)=>null(), dom(:)=>null(), epi(:)=>null()
 integer           :: id, parents(2)=0, sex=0, class=1
 logical           :: hapknown=.false.
 type(t_genome)    :: g
!-------------
 end type
!-------------

 CONTAINS
 
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

!-------------------
 END MODULE
!-------------------


!---------------------------------------------------------------------------------------!
!                                  MAIN PROGRAM                                         !
!---------------------------------------------------------------------------------------!

!------------
 PROGRAM sbvb
!------------
 use sbvb_m
 use random
 implicit none
 
 !----------
 type t_info
 !----------
  character*90 :: qtlfile =' ', &
                  epifile =' ', &
                  pedfile =' ', &
                  hapfile ='sbvb.hap', &
                  outhfile='sbvb.hap', &
                  outgfile=' ', &
                  outyfile='sbvb.ybv', &
                  mapfile =' ', &
                  outmfile='sbvb.mkr', &
                  outqfile=' ', &
                  gwasfile=' '
  character*90, &
   allocatable :: snpfile(:)
  integer      :: nbase=0, nind=0, ncol, frqcol=0, maxclass=0, expand_t=0, &
                  expand_nfam=0, indfirst=1
  logical      :: printgrm=.false., printmkr=.false., useseq=.true., &
                  printqtl=.false., printhap=.false., plink=.false., &
                  restart=.false.,  restartqtl=.false., h2bycol=.false., &
                  oldhap=.false.,   gwas=.false., gzip=.false.
 !-------                 
 end type
 !-------

 type(t_info)                :: info           !--> handle for general info
 type(t_fgenome)             :: gfeatures      !--> contains main genome and qtl features
 type(t_indiv), allocatable  :: inds(:), &     !--> population genomes, in chunk format
                                inds0(:),&     !--> random pedigree to expand base pop
                                gfounders(:)   !--> founder genotypes, stored in g%gt, as haploid
 !--> aux variables
 integer   :: ioped=1, ioy=33, i, ii, ichip, ichr, ind, iorigin, j, h, n, nind, nbase, nchr,&
              isire, idam, seed, icol(10)
 character :: parfile*30='', cmd*100, xc(20)*30, path*30
 real      :: x(size(xc)), t1, t2
 logical   :: seed_defined=.false., ex
 integer, allocatable :: iped(:,:), idcode(:)

 call cpu_time(t1)

 !--> read options
 call get_command (cmd)
 print*, cmd
 call nums2(tab2space(cmd), n=n, x=x, xc=xc)
 do i=2, n
   select case (xc(i))
      case ('-h', '-help')
        !--> trick to extract last instance of '/' and thence the path to ms2gs
        path = xc(1)(1:index(xc(1),'/',back=.true.))
        call system('cat '//trim(path)//'README.sbvb')
        STOP
      case ('-i')
        parfile = xc(i+1)
      case ('-seed')
        seed = x(i+1)
        seed_defined = .true.
    end select
 enddo

 !--> random seed, it can be initialized with -seed in parfile
 if(.not.seed_defined) then
    call initia_random(printseed=.true.)
 else
    call initia_random(seed); print*, 'seed:', seed
 endif
 
 !--> read info
 call read_par(parfile,info,gfeatures)

 nbase = info%nbase
 nind  = info%nind
 allocate(gfounders(gfeatures%ploidy*nbase), &
          inds(nind) )

 !--> read snp pos & founder genotypes from STDIN
 call read_vcf(gfounders, gfeatures, info%nbase)
 call cpu_time(t2)
 print*, 'VCF file read in secs', t2-t1
 
 !--> read snpfiles if any
 do ichip=1, gfeatures%nchip
    call read_snp(gfeatures, info%snpfile(ichip), ichip)
 enddo

 !--> read mapfile or init otherwise
 call read_map(gfeatures, info)

 !--> read qtlfile, epifile and info, only if no restart, and no restartqtl
 if(.not.(info%restart.and.wc(info%hapfile)>0) .or. info%restartqtl) then
    call read_qtl(gfeatures, info)
    call init_qtl_effects(gfounders,gfeatures,info%h2bycol,info%frqcol,&
                          info%pedfile)
 endif

 !--> read haplotype structure if restart, hapknown=.true.
 if(info%restart) call read_hap(inds,gfeatures,info%hapfile,info%restartqtl)
 call cpu_time(t1)
 print*, 'Rest of files read in secs', t1-t2
 
 !--> generate recomb chrs following pedigree
 open(ioped,file=info%pedfile)
 iorigin=0
 do i=1, nind
    read(ioped,'(a)') cmd
    call nums2(tab2space(cmd), n=n, x=x)
    ind=x(1); isire=x(2); idam=x(3)
    inds(ind)%id      = ind
    inds(ind)%parents = [isire, idam]
    if(n>3) inds(ind)%sex = x(4)
    
    if(gfeatures%sexchr>0 .and. inds(ind)%sex.ne.1 .and. inds(ind)%sex.ne.2) &
       STOP 'sex must be 1 or 2 if sex chr defined' 
    !--> if qtl freqs by class (say pop)
    if(info%frqcol>0) inds(ind)%class = x(info%frqcol)

    !--> base individual
    if(ind<=nbase) then
       if(isire/=0 .or. idam/=0) then
          print*, 'In Ind ',ind, ' parents set to unknown'
          print*, 'Base individuals must have both parents unknown'
       endif
       !--> init g, and init qtl
       if(.not.inds(ind)%hapknown .or. info%restartqtl)  then
          call init_g(inds(ind),gfeatures,iorigin,info%restartqtl)
       endif
       !--> origin id updated
       iorigin = iorigin + gfeatures%ploidy
       
    !--> recombinant individual
    else
       !--> this means an expanded population
       if(isire==0 .or. idam==0) then
          if(info%expand_t==0 .and. .not.info%restartqtl) then
              print*, 'Ind ',ind
             STOP 'Both parents must be known in non base individuals or expand pedigree'
          elseif(.not.inds(ind)%hapknown) then
             call expand_ind(inds(ind), inds(1:nbase), info%expand_t, info%expand_nfam, gfeatures)
          elseif(info%restartqtl) then
             call get_qtlori(inds(ind),gfeatures)
          endif
       !--> this is for gene-dropping indivs
       else
          inds(isire)%sex = 1
          inds(idam)%sex  = 2
          if(.not.inds(ind)%hapknown) then
             call init_g(inds(ind),gfeatures)
             call mate(inds(ind),inds(isire),inds(idam),gfeatures)
          endif
          
          !--> ascertain qtlorigins
          if(.not.inds(ind)%hapknown .or. info%restartqtl) &
             call get_qtlori(inds(ind),gfeatures)
          do ichr=1, gfeatures%nchr; do h=1, gfeatures%ploidy
           print*, 'ind',ind, ichr, h, inds(ind)%g%chr(ichr,h)%pos, inds(ind)%g%chr(ichr,h)%ori
          enddo  ; enddo
       endif
    endif
    
    !--> generate true breeding value and phenotype if not read from hapfile, or restartqtl
    if(.not.inds(ind)%hapknown .or. info%restartqtl) then
       call get_tbv(inds(i),gfeatures,gfounders)
       call get_y(inds(i),gfeatures)
    endif
    !call print_indiv(inds(ind),gfeatures,gfounders)
 enddo
 close(ioped)
 call cpu_time(t2)
 print*, 'Pedigree generated in secs', t2-t1
 
 !--> print data + tbv file
 if(info%plink) info%outyfile = trim(adjustl(info%outyfile))//'.tfam'
 open(ioy,file=info%outyfile)
 do ind=info%indfirst, nind
    if(info%plink) then
       write(ioy,'(i2,3i6,i2,100f10.3)') 1, ind, inds(ind)%parents(:), inds(ind)%sex, &
                                            inds(ind)%y, inds(ind)%tbv,  &
                                            inds(ind)%tbv+inds(ind)%dom, &
                                            inds(ind)%tbv+inds(ind)%dom+inds(ind)%epi
    else
       write(ioy,'(i6,100f10.3)') ind, inds(ind)%y, inds(ind)%tbv, &
                                                    inds(ind)%tbv+inds(ind)%dom, &
                                                    inds(ind)%tbv+inds(ind)%dom+inds(ind)%epi
    endif
 enddo
 if(.not.info%plink) print*, 'FORMAT in Y file ',trim(info%outyfile),&
   ' is: ind, (y[i], i=1,ntrait) , (add[i], i=1,ntrait), (add[i]+dom[i], i=1,ntrait), (add[i]+dom[i]+epi[i], i=1,ntrait)'
 close(ioy)
 
 if(info%printqtl) call print_qtl(inds,gfeatures,gfounders,info%outqfile,info%maxclass)
 
 if(info%printhap) call print_hap(inds,gfeatures,info%outhfile)
 
 call cpu_time(t1)
 print*, 'Files written in secs', t1-t2

 !--> output GRM & GWAS, whole or chip marker information if desired
 do ichip=1, gfeatures%nchip
    call dogenotypes(inds,gfeatures,gfounders,info,ichip)
    call cpu_time(t2)
    print'(3a,i1,a,f12.6)', 'chip GRM in file ',trim(info%outgfile),'.',ichip,' generated in secs', t2-t1
    t1=t2
 enddo
 
 if(info%useseq) then
    call dogenotypes(inds,gfeatures,gfounders,info,0)
    call cpu_time(t2)
    print*, 'seq GRM in file ',trim(info%outgfile),'.0 generated in secs', t2-t1
 endif

 CONTAINS

!-------------------
 subroutine read_par (pfile,info,gf)
!-------------------
 type(t_info)    :: info
 type(t_fgenome) :: gf
 integer         :: iopar=87, ios, ichr, n, isnp, ichip, i
 character       :: pfile*(*), temp*3='zxw', c*50, line*100, xc(3)*100
 logical         :: ex, noprinthap=.false.

 inquire(file=pfile,exist=ex)
 if(.not.ex .or. wc(pfile)==0) STOP 'Parameter file does not seem to exist; for help: sbvb -h '
 !--> creates a temp parameter file w/o comments (# or !) nor blank lines
 call system ('grep -v ^\# ' // pfile // '| grep -v ^$ | grep -v ^! >' // temp)
 
 !--> counts no. of chips
 call system ('grep SNPFILE ' // temp // '> zxv'); gf%nchip=wc('zxv'); call system ('rm zxv')
 if(gf%nchip>0) then
    allocate(info%snpfile(gf%nchip))
 endif
 
 open(iopar,file=temp)
 ichip=0
 do
    read(iopar,*,iostat=ios) c
    if(ios.ne.0) EXIT
    select case (c)
    case('NTRAIT')
      read(iopar,*) gf%ntrait
    case('MAXNCHR','NCHR')
      read(iopar,*) gf%nchr
    case('MXOVER')
      read(iopar,*) gf%mxover
    case('SEXCHR')
      read(iopar,*) gf%sexchr
    case('PLOIDY')  !--> by default, autoploidy (all homeologs pair randomly)
      read(iopar,*) gf%ploidy
      allocate (gf%tau(gf%ploidy,gf%ploidy))
      gf%tau = 1.
      forall(i = 1:gf%ploidy) gf%tau(i,i)=0.
    case('ALLOPLOIDY')  !-->  homeologs do not pair 
      if(.not.associated(gf%tau)) STOP 'PLOIDY section must appear before ALLOPLOIDY'
      gf%tau = 0.
      forall(i = 1:gf%ploidy-1:2) 
         gf%tau(i,i+1)=1. ; gf%tau(i+1,i)=1.
      end forall
    case('RHOPLOIDY') !--> P of xover across homeologs, diag=0
      if(.not.associated(gf%tau)) STOP 'PLOIDY section must appear before RHOPLOIDY'
      read(iopar,*) gf%tau(:,:)
      forall(i = 1:gf%ploidy) gf%tau(i,i)=0.
    case('MIMIC_DIPLOID')    !--> treats ployploid genotypes as diploids
      if(gf%ploidy == 2) STOP 'MIMIC_DIPLOID section must appear after PLOIDY'
      gf%mimic=.true.
    case('RESTART')    !--> prepares files for new run of sbvb
      info%restart=.true.
      info%printhap=.true.
    case('RESTARTQTL','RESTARTQTN') !--> reads hap but generate new qtl and y with qtlfile
      info%restartqtl=.true.
    case('EXPAND_BASEPOP') !--> writes expanded base population
      read(iopar,*) info%expand_t, info%expand_nfam
    case('NOPRINTHAP') !--> does not print hap (eg if last iterate)
      noprinthap=.true.
    case('NOSEQUENCE') !--> does not print seq
      info%useseq=.false.
    case('QTLFILE','QTNFILE')    !--> add & dom effects can be defined in cols 3 & 4
      read(iopar,'(a)') line
      call nums2(line,xc=xc,n=n)
      info%qtlfile=trim(adjustl(xc(1)))
      if(wf(info%qtlfile)>2) gf%init_add = .true.
      if(wf(info%qtlfile)>3) gf%init_dom = .true.
      if(gf%ntrait>1 .and. gf%init_add .and. wf(info%qtlfile)/=2+2*gf%ntrait) &
      STOP 'Either none or both add and dom effects for every trait must be specified in QTLFILE'
    case('EPIFILE') !--> add & dom effects can be defined in cols 3 & 4
      read(iopar,'(a)') line
      call nums2(line,xc=xc,n=n)
      info%epifile=trim(adjustl(xc(1)))
      if(wc(info%epifile)==0)  print*, 'WARNING: epistatic file seems to be empty'
    case('PEDFILE')
      read(iopar,'(a)') line
      call nums2(tab2space(line),xc=xc,n=n)
      info%pedfile=trim(adjustl(xc(1)))
      info%nind=wc(info%pedfile)   !--> n inds is lines in pedfile
      info%ncol=wf(info%pedfile)   !--> ncols just in case
      if(wm(info%pedfile,1).ne.info%nind) STOP 'Inds in pedigree must be consecutive ids 1,2..n'
    case('SNPFILE') !--> file with genotyped snps, can be repeated
      ichip = ichip + 1
      read(iopar,'(a)') line
      call nums2(line,xc=xc,n=n)
      info%snpfile(ichip)=trim(adjustl(xc(1))) !need to be a vector
    case('MAPFILE') !--> recomb map file: chr, basepos, cm2Mb [, cm2Mb_sex2]
      read(iopar,'(a)') line
      call nums2(line,xc=xc,n=n)
      info%mapfile=trim(adjustl(xc(1)))
    case('HAPFILE') !--> contains hap structure so program can be restarted with RESTART: INPUT/OUTPUT file
      read(iopar,'(a)') line
      call nums2(line,xc=xc,n=n)
      info%hapfile=trim(adjustl(xc(1)))
      info%outhfile = info%hapfile
      info%printhap = .true.
    case('OLDHAP') !--> old hap format, no epistasis read
      info%oldhap = .true.
    case('OUTPLINK')
      info%plink = .true.
    case('OUTQFILE')
      read(iopar,*) info%outqfile
      info%printqtl=.true.
    case('OUTGFILE') !--> outfile with GRM, either chip or sequence
      read(iopar,*) info%outgfile
      info%printgrm=.true.
    case('OUTYFILE')
      read(iopar,*) info%outyfile 
    case('OUTMFILE') !--> outfile with mkr data, either chip or sequence
      read(iopar,*) info%outmfile
      info%printmkr=.true.
    case('GWASFILE') !--> outfile with mkr data, either chip or sequence
      read(iopar,*) info%gwasfile
      info%gwas=.true.
    case ('NBASE')   !--> nind which genotypes are read from STDIN (nori=ploidy*nbase)
      read(iopar,*) info%nbase
    case ('INDFIRST') !--> first ind to compute G, etc
      read(iopar,*) info%indfirst
    case ('H2')
      allocate(gf%h2(gf%ntrait))
      do i=1, gf%ntrait
         read(iopar,*) gf%h2(i)
      enddo
    case ('H2G')
      allocate(gf%h2g(gf%ntrait))
      do i=1, gf%ntrait
         read(iopar,*) gf%h2g(i)
      enddo
    case ('SIGNQTL','PSIGNQTN')    !--> P of derived allele being deleterious (only with gamma)
      read(iopar,*) gf%sign_qtl
    case ('OVERDOMINANCE') !--> P of derived allele being deleterious (only with gamma)
      gf%overdominance = .true.
    case ('RHOQA')         !--> correlation between abs qtl effect and frequency
      allocate(gf%rhoqa(gf%ntrait))
      do i=1, gf%ntrait
         read(iopar,*) gf%rhoqa(i)
         if(gf%rhoqa(i)<-1. .or. gf%rhoqa(i) >1.) STOP 'rho qtl effect freq must be between 0 and 1'
      enddo
    case ('BINARY')
      allocate(gf%binary(gf%ntrait), gf%p(gf%ntrait), gf%threshold(gf%ntrait))
      do i=1, gf%ntrait
         read(iopar,*) gf%p(i)
         gf%binary(i) = .true.
         if(gf%p(i)<=0. .or. gf%p(i) >=1.) gf%binary(i) = .false.
      enddo 
    case ('QTLDISTA','QTNDISTA')
      allocate(gf%qdista(gf%ntrait))
      do i=1, gf%ntrait
         read(iopar,*) gf%qdista(i)%f, gf%qdista(i)%x, gf%qdista(i)%s
      enddo
    case ('QTLDISTD','QTNDISTD')
      allocate(gf%qdistd(gf%ntrait))
      do i=1, gf%ntrait
         read(iopar,*) gf%qdistd(i)%f, gf%qdistd(i)%x, gf%qdistd(i)%s
      enddo
    case ('CM2MB') !--> default cm2mb=1
      read(iopar,*) gf%cm2mb
    case ('FRQBYCOL')
      read(iopar,*) info%frqcol
    case ('H2BYCOL')
      info%h2bycol = .true.
    case ('GZIP')
      info%gzip = .true.
    end select
 enddo
 close (iopar, status='delete')
 
 !--> init if NTRAIT=1
 if(.not.associated(gf%rhoqa)) then
    allocate(gf%rhoqa(gf%ntrait)); gf%rhoqa=0
 endif
 if(.not.associated(gf%binary)) then
    allocate(gf%binary(gf%ntrait)); gf%binary=.false.
 endif
 if(.not.associated(gf%qdistd)) then
    allocate(gf%qdistd(gf%ntrait)); gf%qdistd(:)%f=' '
 endif

 if(wc(info%qtlfile)==0) STOP 'QTNFILE not specified or empty'

 if(info%nind==0) STOP 'NIND=0, likely pedfile not specified or empty'

 if(info%nbase==0) STOP 'NBASE must be specified (no. inds in vcf file)'

 if(associated(gf%h2) .and. .not. associated(gf%h2g) ) then
    allocate(gf%h2g(gf%ntrait))
    gf%h2g=0.
 elseif(associated(gf%h2g) .and. .not. associated(gf%h2) ) then
    allocate(gf%h2(gf%ntrait))
    gf%h2=0.
 else
    STOP 'Only either H2 or H2G must be present in par file'
 endif

 if(gf%ploidy>2 .and. gf%sexchr>0) STOP 'SEXCHR and POLYPLOIDS are not jointly allowed'

 if(gf%ploidy>2 .and. wc(info%epifile)>0) STOP 'Epistasis not allowed so far with polyploids'
 
 !--> specifies generic delta for dominance (check dodom function)
 allocate(gf%delta(gf%ploidy))
 gf%delta = (/0,(i,i=gf%ploidy-1,1,-1),0/)
 
 !--> make sure is not printed
 if(noprinthap) info%printhap=.false.
 if(info%restartqtl .and. .not.info%restart) then
    info%restartqtl=.false.
    print*, 'WARNING: RESTARTQTL must be used with RESTART, as assumes an hap file to be read'
 endif
 
 !--> no effect if hapfile does not exist
 if(info%restartqtl .and. wc(info%hapfile)==0) info%restartqtl=.false.
 
 !--> if qtl freqs by class
 if(info%frqcol>0) then
    if(wf(info%pedfile)<info%frqcol) STOP 'pedfile has less columns than specified in FRQBYCOL'
    info%maxclass = wl(info%pedfile,info%frqcol)
    print*, info%maxclass, ' levels found in PEDFILE '
 endif
 if(info%h2bycol .and. info%frqcol==0) STOP 'FRQBYCOL must be specified with H2BYCOL'
 
 if(info%expand_nfam > info%nbase)   STOP 'In EXPAND_BASEPOP, nbase must be > n founders in sub-pedigree'
  
 if(gf%sexchr>0) then
    if(wf(info%pedfile)<4)   STOP 'If sex chr specified, sex must be in column 4 of pedfile'
    if(wm(info%pedfile,4)>2) STOP 'If sex chr specified, sex must be coded 1,2 in column 4 of pedfile'
 endif
!--------------
 end subroutine
!--------------

!-------------------
 subroutine read_vcf (ff, gf, nbase)
!-------------------
! ok VERIFY PLOIDYYYYYYYYYY
! optimizes memory
! rm fixed snps
! reads founder genotypes
 intent(in)      :: nbase
 type(t_indiv)   :: ff(:)
 type(t_fgenome) :: gf
 integer(i8)     :: bpos
 integer(1)      :: gt(gf%ploidy*nbase)
 integer(i8), allocatable      :: temp_pos(:), tp(:)
 integer(kind=1), allocatable  :: temp(:,:), t(:,:)
 real, allocatable             :: f(:), tf(:)
 integer                       :: maxnchr, maxn=3000000, step=1000000, maxn0, nbase, ios, &
                                  i, ind, ichr, nchr, n, nsnp, new_chr, ntot
 
 !--> nchr is initially set to gf%nchr
 maxnchr = gf%nchr
 allocate(temp_pos(maxn), temp(maxn,gf%ploidy*nbase), f(maxn))
 allocate(gf%fchr(maxnchr))
 do ind=1, gf%ploidy*nbase  !--> ploidy
    allocate(ff(ind)%g%chr(maxnchr,1) )
 enddo
 ichr=1
 i=0
 ntot=0
 do
    read(*,*,iostat=ios) new_chr, bpos, gt(1:gf%ploidy*nbase)
    !--> assign if new chr or last snp
    if(ios/=0 .or. new_chr.ne.ichr) then
       if(new_chr > maxnchr) STOP 'chr id found larger than specified'
       nsnp = i
       ntot = ntot + nsnp
       print*, i,' SNPs in chr',ichr
       allocate( gf%fchr(ichr)%pos(nsnp) , &
                 gf%fchr(ichr)%freq(nsnp)  )
       gf%fchr(ichr)%nsnp   = nsnp
       gf%fchr(ichr)%pos(:) = temp_pos(1:nsnp)
       gf%fchr(ichr)%freq(:)= f(1:nsnp)
       !--> each haplotype is a new founder individual
       do ind=1, gf%ploidy*nbase
          allocate(ff(ind)%g%chr(ichr,1)%gt(nsnp))
          ff(ind)%g%chr(ichr,1)%gt(:) = temp(1:nsnp,ind)
       enddo
       if(ios/=0) EXIT
       deallocate(temp_pos, temp, f)
       ichr=new_chr
       i=0
       allocate(temp_pos(maxn), temp(maxn,gf%ploidy*nbase), f(maxn))
    endif
    
    !--> skip fixed SNPs
    if(all(gt==gt(1))) CYCLE 
    i=i+1
    temp_pos(i)=bpos
    temp(i,1:gf%ploidy*nbase)=gt(:)
    f(i)=count(gt==1)/real(gf%ploidy*nbase)
    if(mod(i,1000000)==0) print*, i,'markers read'
    !--> increases memory
    if(i==maxn) then
       maxn0 = maxn
       maxn  = maxn + step
       allocate(tp(maxn), t(maxn,gf%ploidy*nbase), tf(maxn))
       tp(1:maxn0)=temp_pos; t(1:maxn0,:)=temp; tf(1:maxn0)=f
       deallocate(temp_pos,temp,f)
       allocate(temp_pos(maxn), temp(maxn,gf%ploidy*nbase), f(maxn))
       temp_pos(1:maxn0)=tp(1:maxn0)
       temp(1:maxn0,:)=t(1:maxn0,:)
       f(1:maxn0)=tf(1:maxn)
       deallocate(tp,t,tf)
    endif
 enddo

 print*, ntot,' SNPs read in total'
 gf%nchr = ichr
 if(ichr.ne.maxnchr) print*, 'WARNING: specified and found n chrs differ'
 do ichr=1, gf%nchr
    allocate(gf%fchr(ichr)%chip(gf%nchip))
 enddo

 deallocate(temp_pos, temp, f)
!--------------
 end subroutine
!--------------

!-------------------
 subroutine read_qtl (gf,info)
!-------------------
! read qtlfile and assigns nqtl per chr
 type(t_fgenome) :: gf
 type(t_info)    :: info
 integer         :: ichr, ioin=11, iqtl, i, j, nqtl, ntot, ntrait
 character       :: a*4, temp*4='temp'
 real, allocatable        :: qa(:,:), qd(:,:)
 integer, allocatable     :: ipos(:)
 integer(i8), allocatable :: qpos(:)
 ntrait = gf%ntrait

 if(wc(info%qtlfile)==0) STOP 'Empty or non existent qtl file'

 !--> ascertain  no. qtls per chr with linux trick
 do ichr=1, gf%nchr
    write(a,'(i3)') ichr
    call system("awk '$1=="//trim(a)//"' "//info%qtlfile//'>'//temp)
    nqtl = wc(temp)
    gf%fchr(ichr)%nqtl = nqtl
    allocate( gf%fchr(ichr)%qtlpos(nqtl),  &
              gf%fchr(ichr)%iposqtl(nqtl), &
              gf%fchr(ichr)%qtlepi(nqtl,2),  &
              gf%fchr(ichr)%qtladd(nqtl,ntrait),  &
              gf%fchr(ichr)%qtldom(nqtl,ntrait)   )
    gf%fchr(ichr)%qtladd = 0
    gf%fchr(ichr)%qtldom = 0
 enddo
 call system('rm '//temp)

 !--> reads qtl pos, and effects if defined
 open(ioin,file=info%qtlfile)
 do ichr=1, gf%nchr
    do iqtl=1, gf%fchr(ichr)%nqtl
       if(gf%init_dom) then
          read(ioin,*) i, gf%fchr(ichr)%qtlpos(iqtl),    &
                          (gf%fchr(ichr)%qtladd(iqtl,j), &
                           gf%fchr(ichr)%qtldom(iqtl,j), j=1, ntrait)
       elseif(gf%init_add) then
          read(ioin,*) i, gf%fchr(ichr)%qtlpos(iqtl),    &
                          gf%fchr(ichr)%qtladd(iqtl,1)
       else
          read(ioin,*) i, gf%fchr(ichr)%qtlpos(iqtl)
       endif
       if(i.ne.ichr) STOP 'Qtl positions unordered in qtl file'
    enddo
 enddo
 close(ioin)

 !--> match qtl positions
 ntot=0
 do ichr=1, gf%nchr
    if(gf%fchr(ichr)%nqtl>0) then
       gf%fchr(ichr)%iposqtl(:) = match_pos(gf%fchr(ichr)%qtlpos, gf%fchr(ichr)%pos)
       !--> rm unknown positions
       if(any(gf%fchr(ichr)%iposqtl==0)) then
          print*, 'WARNING:', count(gf%fchr(ichr)%iposqtl(:)==0), &
                  ' Undefined QTL positions removed in chr', ichr
          nqtl = count(gf%fchr(ichr)%iposqtl(:)>0)
          gf%fchr(ichr)%nqtl = nqtl
          allocate(ipos(nqtl), qpos(nqtl), qa(nqtl,ntrait), qd(nqtl,ntrait))
          ipos = pack(gf%fchr(ichr)%iposqtl,gf%fchr(ichr)%iposqtl(:)>0)
          qpos = pack(gf%fchr(ichr)%qtlpos,gf%fchr(ichr)%iposqtl(:)>0)
          do i=1, ntrait
             qa(:,i) = pack(gf%fchr(ichr)%qtladd(:,i),gf%fchr(ichr)%iposqtl(:)>0)
             qd(:,i) = pack(gf%fchr(ichr)%qtldom(:,i),gf%fchr(ichr)%iposqtl(:)>0)
          enddo
          deallocate(gf%fchr(ichr)%iposqtl, gf%fchr(ichr)%qtlpos, gf%fchr(ichr)%qtladd, &
                     gf%fchr(ichr)%qtldom, gf%fchr(ichr)%qtlepi )
          allocate(gf%fchr(ichr)%iposqtl(nqtl), &
                   gf%fchr(ichr)%qtlpos(nqtl),  &
                   gf%fchr(ichr)%qtlepi(nqtl,2), &
                   gf%fchr(ichr)%qtladd(nqtl,ntrait),  &
                   gf%fchr(ichr)%qtldom(nqtl,ntrait)   )
          gf%fchr(ichr)%iposqtl = ipos
          gf%fchr(ichr)%qtlpos  = qpos
          gf%fchr(ichr)%qtladd  = qa
          gf%fchr(ichr)%qtldom  = qd
          deallocate(ipos, qpos, qa, qd)
       endif
       !--> init no epistatic interactions
       gf%fchr(ichr)%qtlepi(:,:)=0
       ntot = ntot + gf%fchr(ichr)%nqtl
    endif  
 enddo
 print*, 'Total no of qtl found:', ntot
 
 if(wc(info%epifile)>0) call read_epi(gf,info)

!--------------
 end subroutine
!--------------

!-------------------
 subroutine read_epi (gf,info)
!-------------------
! read epistasis file and assigns epistatic pair structure
 type(t_fgenome) :: gf
 type(t_info)    :: info
 integer         :: nf, nepi0, iq, ic1, ic2, i, iqtl(1), ioe=372
 integer(i8)     :: bp1, bp2
 integer(i8), allocatable :: tempi(:,:)
 real, allocatable        :: tempe(:,:,:)
 
 nepi0 = wc(info%epifile)
 nf = wf(info%epifile)
 if(nf/=14 .and. gf%ntrait>1) then
    STOP 'Epistatic file must contain 14 columns if ntrait>1'
 elseif ((nf/=13 .and. nf/=14) .and. gf%ntrait==1) then
    STOP 'Epistatic file must contain 13 or 14 columns if ntrait=1'
 endif
 
 allocate(tempi(nepi0,6), tempe(nepi0,3,3))
 tempi=0

 !--> epifile fmt is: trait, chr1, bp1, chr2, bp2, effects(3,3) 
 open(ioe,file=info%epifile)
 do iq=1, nepi0
    if(nf==14) then
       read(ioe,*) tempi(iq,1:5), (tempe(iq,i,1:3),i=1,3)
       if(tempi(iq,1)>gf%ntrait) STOP 'Trait undefined in epistatic file'
    else
       read(ioe,*) tempi(iq,2:5), (tempe(iq,i,1:3),i=1,3)
       tempi(iq,1)=1
    endif
    ic1=tempi(iq,2); ic2=tempi(iq,4)
    bp1=tempi(iq,3); bp2=tempi(iq,5)
    !--> both qtl found
    if(any(gf%fchr(ic1)%qtlpos==bp1)  .and. any(gf%fchr(ic2)%qtlpos==bp2)) then  
       iqtl(:) = pack( [(i,i=1,gf%fchr(ic1)%nqtl)], gf%fchr(ic1)%qtlpos==bp1)
       tempi(iq,3) = iqtl(1)      
       iqtl(:) = pack( [(i,i=1,gf%fchr(ic2)%nqtl)], gf%fchr(ic2)%qtlpos==bp2)
       tempi(iq,5) = iqtl(1)      
       tempi(iq,6) = 1
    endif
 enddo
 close(ioe)
 
 !--> actual epi pairs found
 gf%nepi = count(tempi(:,6)>0)
 allocate(gf%epi(gf%nepi))
 iq=0
 do i=1, nepi0
    if(tempi(i,6)>0) then
       iq=iq+1
       ic1=tempi(i,2)
       ic2=tempi(i,4)
       gf%epi(iq)%trait = tempi(i,1)
       gf%epi(iq)%chr(:)= [tempi(i,2),tempi(i,4)]
       gf%epi(iq)%iqtl  = [tempi(i,3),tempi(i,5)]
       gf%epi(iq)%eff   = tempe(i,:,:)
       !--> keeps track of these qtl pertaining to iq epistatic pair
       if(gf%fchr(ic1)%qtlepi(gf%epi(iq)%iqtl(1),1)>0 .or.  &
          gf%fchr(ic2)%qtlepi(gf%epi(iq)%iqtl(2),1)>0 )     &
          STOP 'Each QTN can be in only one epistatic pair'
       gf%fchr(ic1)%qtlepi(gf%epi(iq)%iqtl(1),1) = iq      
       gf%fchr(ic1)%qtlepi(gf%epi(iq)%iqtl(1),2) = 1      
       !--> each qtl is first or second in pair
       gf%fchr(ic2)%qtlepi(gf%epi(iq)%iqtl(2),1) = iq      
       gf%fchr(ic2)%qtlepi(gf%epi(iq)%iqtl(2),2) = 2      
    endif
 enddo
 print*, 'Total no of epi pairs found:', gf%nepi
 deallocate(tempi, tempe)
!--------------
 end subroutine
!--------------

!--------------------
 subroutine print_qtl (pop, gf, ff, qfile, maxclass)
!--------------------
 type(t_fgenome) :: gf
 type(t_indiv)   :: pop(:), ff(:)
 integer         :: maxclass, nind, ichr, ioin=11, h, iqtl, ipos, ind, ori, n, ntrait, it
 character       :: qfile*(*)
 integer(i8)     :: bpos
 real, allocatable     :: frq(:), alpha(:,:)
 integer, allocatable  :: ichunk(:,:), class(:), gt(:)
 nind=size(pop)
 ntrait=gf%ntrait
 allocate (ichunk(nind,gf%ploidy),  class(nind), gt(nind), frq(maxclass+1), alpha(maxclass+1,ntrait) )
 do ind=1,nind
    class(ind)=pop(ind)%class
 enddo

 open(ioin,file=qfile)
 do ichr=1, gf%nchr
    ichunk(:,:)=1
    do iqtl=1, gf%fchr(ichr)%nqtl
       ipos = gf%fchr(ichr)%iposqtl(iqtl)
       bpos = gf%fchr(ichr)%pos(ipos)
       gt   = 0
       frq  = 0.
       alpha = 0.
       do ind=1, nind
          do h=1, gf%ploidy
             if (bpos > pop(ind)%g%chr(ichr,h)%pos(ichunk(ind,h)) ) then
                ichunk(ind,h)=count( pop(ind)%g%chr(ichr,h)%pos(:) < bpos )+1
             endif
             ori     = pop(ind)%g%chr(ichr,h)%ori(ichunk(ind,h))
             gt(ind) = gt(ind) + ff(ori)%g%chr(ichr,1)%gt(ipos)
          enddo
       enddo
       frq(1) = sum(gt)/real(gf%ploidy*nind)
       !--> all this is defined only if ploidy=2
       do it=1, ntrait
          alpha(1,it) = gf%fchr(ichr)%qtladd(iqtl,it) + gf%fchr(ichr)%qtldom(iqtl,it)*(1.-2*frq(1))
          alpha(1,it) = 2*frq(1)*(1.-frq(1))*alpha(1,it)**2
          do i=1, maxclass         
             n = count(class==i)
             if(n>0) then
                frq(1+i) = sum(pack(gt,class==i))/real(n*gf%ploidy)
                alpha(1+i,it) = gf%fchr(ichr)%qtladd(iqtl,it) + gf%fchr(ichr)%qtldom(iqtl,it)*(1.-2*frq(i+1))
                alpha(1+i,it) = 2*frq(1+i)*(1.-frq(1+i))*alpha(1+i,it)**2
             endif
          enddo
       enddo
       write(ioin,'(i3,i10,500f9.3)') ichr, &
                   gf%fchr(ichr)%qtlpos(iqtl), gf%fchr(ichr)%freq(ipos), frq(:), &
                   (gf%fchr(ichr)%qtladd(iqtl,it), gf%fchr(ichr)%qtldom(iqtl,it), alpha(:,it), it=1,ntrait)
    enddo
 enddo
 print*, 'FORMAT in QTN file ',trim(qfile),' is: ichr, pos, freq_base, freq, (add[i],dom[i],va[i]; i=1,ntrait)'
 close(ioin)
!--------------
 end subroutine
!--------------


!---------------------------
 subroutine init_qtl_effects (ff, gf, docol, col, pedfile)
!---------------------------
! sample qtl effects following distributions and ve given h2
! for ploidy > 2, I would require to read them from a file
 type(t_indiv)         :: ff(:)
 type(t_fgenome)       :: gf
 logical               :: docol
 character             :: pedfile*(*), aa
 integer               :: col, nclass=1, eg(2)
 integer, allocatable  :: class(:)
 integer               :: i, iclass, ind, ichr, iqtl, nqtl, ntot, g, h, n, nbase, isnp, &
                          ipair, iori, ntrait, iepi, it, iope=333
 real(r8), allocatable :: tbv(:), tba(:), tbp(:), v(:,:)
 real(kind(1.d0))      :: x
 ntrait = gf%ntrait
 nbase = size(ff)/gf%ploidy
  
 !--> reads class if pertinent
 allocate(class(nbase))
 class=1
 if(docol) then
    open(ioped,file=pedfile)
    do i=1, nbase
       read(ioped,*) (x,n=1,col-1), class(i)
    enddo
    close(ioped)
 endif

 !--> allocate  ve et al
 nclass=maxval(class)
 allocate(gf%se(nclass,ntrait), gf%va(nclass,ntrait), gf%vg(nclass,ntrait), &
          gf%vepi(ntrait), tba(nbase), tbv(nbase), tbp(nbase))

 !--> sample qtl effects by chr
 do it=1, ntrait
    ntot = 0
    do ichr=1, gf%nchr
       nqtl = gf%fchr(ichr)%nqtl
       ntot = ntot + nqtl
       if(nqtl>0) then
          if (.not.gf%init_add) then
             call sample_effect(gf%fchr(ichr)%qtladd(:,it), gf%qdista(it), gf%sign_qtl)
          endif
          if (.not.gf%init_dom .and. gf%qdistd(it)%f .ne. ' ') then
             call sample_effect(gf%fchr(ichr)%qtldom(:,it), gf%qdistd(it), gf%sign_qtl)
          elseif(.not.gf%init_dom) then
             gf%fchr(ichr)%qtldom(:,it) = 0
          endif
          if(.not.gf%overdominance .and. any(abs(gf%fchr(ichr)%qtldom)>0.)) then
             do iqtl=1, nqtl
                if(abs(gf%fchr(ichr)%qtldom(iqtl,it))>abs(gf%fchr(ichr)%qtladd(iqtl,it))) &
                       gf%fchr(ichr)%qtldom(iqtl,it) = sign(gf%fchr(ichr)%qtladd(iqtl,it) ,   &
                                                            gf%fchr(ichr)%qtldom(iqtl,it))
             enddo
          endif
       endif
    enddo
 enddo

 !--> reorder qtl effects such that rho btw effect and freq is as requested
 !    I need to remove effects=0 as these do not contribute
 do it=1, ntrait
    if(gf%rhoqa(it) .ne. 0.d0) then
       allocate(v(ntot,3))
       v(:,3)=1 !--> save sign frequency but not exact match as effects reordered
       i=0
       do ichr=1, gf%nchr
          do iqtl=1, gf%fchr(ichr)%nqtl
             i=i+1
             v(i,1) = gf%fchr(ichr)%freq( gf%fchr(ichr)%iposqtl(iqtl) )
             v(i,2) = gf%fchr(ichr)%qtladd(iqtl,it)
          enddo
       enddo
       where(v(:,2)<0) v(:,3)=-1
       v(:,2)=abs(v(:,2))
       !--> permutes v2 such that cor(v1,v2)=rhoqa, rm <=0 v2 values
       call sortcorr_minus0(v(:,1),v(:,2),gf%rhoqa(it))
       i=0
       do ichr=1, gf%nchr
          do iqtl=1, gf%fchr(ichr)%nqtl
             i=i+1               !--> keep p of sign but not the match
             gf%fchr(ichr)%qtladd(iqtl,it) = v(i,2) * v(i,3)
          enddo
       enddo
       deallocate(v)
    endif
 enddo

 !--> using founder genotypes to obtain var_a, var_g and var_epi
 do it=1, ntrait
    tba=0
    tbv=0
    do ind=1, nbase
       iclass = class(ind)
       do ichr=1, gf%nchr
          do iqtl=1, gf%fchr(ichr)%nqtl
             g = 0
             isnp = gf%fchr(ichr)%iposqtl(iqtl)
             do h=1, gf%ploidy  !--> ploidy
                iori = gf%ploidy*(ind-1)+h
                g    = g + ff(iori)%g%chr(ichr,1)%gt(isnp)
             enddo
             !.--> this should work with any ploidy
             tbv(ind) = tbv(ind) + (g-gf%ploidy/2) * gf%fchr(ichr)%qtladd(iqtl,it)
             tbv(ind) = tbv(ind) + dodom(g, gf%fchr(ichr)%qtladd(iqtl,it), gf%fchr(ichr)%qtldom(iqtl,it) , gf%delta)
             tba(ind) = tba(ind) + (g-gf%ploidy/2) * gf%fchr(ichr)%qtladd(iqtl,it)
          enddo
       enddo
     enddo
     
    !--> add epi
    tbp=0
    do iepi=1, gf%nepi
       if(gf%epi(iepi)%trait==it) then
          do ind=1, nbase
             eg=1  !--> genotypes coded 1,2,3
             do ipair=1, 2
                ichr = gf%epi(iepi)%chr(ipair)
                iqtl = gf%epi(iepi)%iqtl(ipair)
                isnp = gf%fchr(ichr)%iposqtl(iqtl)
                do h=1, gf%ploidy  !--> ploidy
                   iori = gf%ploidy*(ind-1)+h
                   eg(ipair) = eg(ipair) + ff(iori)%g%chr(ichr,1)%gt(isnp)
                enddo
             enddo
             tbp(ind) = tbp(ind) + gf%epi(iepi)%eff(eg(1),eg(2))
          enddo
       endif
    enddo

    !--> adjust ve by h2 or H2
    do iclass=1, nclass
       gf%va(iclass,it) = variance(pack(tba,class(:)==iclass))
       gf%vg(iclass,it) = variance(pack(tbv,class(:)==iclass))
    enddo
    gf%vepi(it) = variance(tbp)
    
    if(gf%h2(it)>0) then
       if( all( gf%va(:,it) / gf%h2(it) > (gf%vg(:,it) + gf%vepi(it)) ) ) then
          gf%se(:,it) = sqrt(gf%va(:,it) / gf%h2(it)  - (gf%vg(:,it) + gf%vepi(it)) )
          print*, gf%va(:,it) / gf%h2(it), (gf%vg(:,it) + gf%vepi(it))
       else
          STOP 'H2 requested cannot be met with current files, try fixing H2G'
       endif
    elseif(gf%h2g(it)>0) then
       gf%se(:,it) = sqrt( (gf%vg(:,it) + gf%vepi(it)) / gf%h2g(it) - (gf%vg(:,it) + gf%vepi(it)) )
    endif

    print*, 'trait va vg epi sde (by pop if appropriate)', it, gf%va(:,it), gf%vg(:,it), gf%vepi(it), gf%se(:,it)

    !--> for threshold traits I need to compute the threshold, given sva and mean
    !    only defined for first class
    if(gf%binary(it)) then
       call ppnd16(real(1.-gf%p(it),r8),x,i)
       gf%threshold(it) = x*sqrt(gf%va(1,it)+gf%se(1,it)**2) + mean(tba)
    endif
 enddo
 
 deallocate(tbv,tba,tbp)
!--------------
 end subroutine
!--------------


!------------------------
 subroutine sample_effect (eff, qd, sign, effa)
!------------------------
 type(t_qdensity) :: qd
 real             :: eff(:), sign
 real(r8)         :: eff8(size(eff))
 integer          :: neff,i
 real, optional   :: effa(:)
 select case (qd%f(1:1))
   case('U','u') !--> uniform effect
     call random_number(eff8)
     eff = qd%x + eff8 * (qd%s-qd%x)
   case('N','n') !--> normal effect
     call random_normal (eff8, real(qd%x,r8), real(qd%s,r8))
     eff = eff8
   case('G','g') !--> gamma effect
     call random_gamma_v(real(qd%x,r8), real(qd%s,r8), eff8)
     eff = eff8
     !--> inverts with probability sign (P of alternative allele being deleterious)
     do i=1, size(eff)
        if(rxnd() < sign) eff(i) = -eff(i)
     enddo
   case('C','c') !--> caballero-keightley 1994 [0 - exp-Ks]
     call random_number(eff8)
     eff = eff8 * exp(-qd%x * effa)
   case default
     print*, 'Unknown QTL distribution: ', qd%f
     STOP
 end select
!--------------
 end subroutine
!--------------

!-------------------
 subroutine read_map (gf,info)
!-------------------
! allows for separate sex maps
! read mapfile and assigns rec chunks by chr
 type(t_fgenome)  :: gf
 type(t_info)     :: info
 real,allocatable :: local_cm2mb(:)
 real(r8)         :: ratio
 integer          :: ichr, ioin=9, i, h, j, nsnp, nnew, nsex
 integer(i8)      :: last_pos
 character        :: a*4, temp*4='wdrt'
 integer(i8), allocatable :: nmap(:), mapbp(:)
 real(r8),    allocatable :: xlength(:,:)

 !--> default values if no map file
 if(wc(info%mapfile)==0) then
    do ichr=1, gf%nchr
       allocate( gf%fchr(ichr)%mapbp(1), &
                 gf%fchr(ichr)%mapper(2,1)   )
       nsnp = gf%fchr(ichr)%nsnp
       gf%fchr(ichr)%lengthM   = gf%fchr(ichr)%pos(nsnp) * 1e-8 * gf%cm2mb
       gf%fchr(ichr)%mapbp(1)  = gf%fchr(ichr)%pos(nsnp)
       gf%fchr(ichr)%mapper(:,1) = 1.
    enddo

 !--> mapfile exists
 else
    allocate(nmap(gf%nchr))
    nmap=0
    !--> determines if sex specific maps
    nsex = wf(info%mapfile)-2
    allocate(local_cm2mb(nsex))
    
    !--> counts no. chunks per chr
    do ichr=1, gf%nchr
       write(a,'(i3)') ichr
       call system("awk '$1=="//trim(a)//"' "//info%mapfile//'>'//temp)
       nmap(ichr) = wc(temp)
       allocate( gf%fchr(ichr)%mapbp(nmap(ichr)), &
                 gf%fchr(ichr)%mapper(2,nmap(ichr))   )
       gf%fchr(ichr)%mapper(2,nmap(ichr))=0
       !--> sex chromosome
       if(gf%sexchr==ichr) gf%fchr(ichr)%xploidy=1
    enddo
    call system('rm '//temp)

    !--> reads map file
    open(ioin,file=info%mapfile)
    do ichr=1, gf%nchr
       last_pos = 1
       do j=1, nmap(ichr)
          read(ioin,*) i, gf%fchr(ichr)%mapbp(j), local_cm2mb
          !--> converts length into Morgans
          do h=1, nsex
             gf%fchr(ichr)%mapper(h,j) = (gf%fchr(ichr)%mapbp(j) - last_pos) * 1e-8 * local_cm2mb(h)
          enddo
          last_pos = gf%fchr(ichr)%mapbp(j)
       enddo
       !--> copies onto sex 2 if single sex map
       if(nsex==1) gf%fchr(ichr)%mapper(2,:) = gf%fchr(ichr)%mapper(1,:)
    enddo
    close(ioin)

    !--> finish up in case last chunk is not last pos or no map in that chr
    do ichr=1, gf%nchr
       nsnp = gf%fchr(ichr)%nsnp
       if(nmap(ichr)==0) then
          allocate( gf%fchr(ichr)%mapbp(1), &
                    gf%fchr(ichr)%mapper(2,1)   )
          gf%fchr(ichr)%mapbp(1)    = gf%fchr(ichr)%pos(nsnp)
          gf%fchr(ichr)%mapper(:,1) = gf%fchr(ichr)%pos(nsnp) * 1e-8 * gf%cm2mb

       !--> complete telomere chunk with default cm2mb
       elseif( gf%fchr(ichr)%mapbp(nmap(ichr)) < gf%fchr(ichr)%pos(nsnp) ) then
          allocate(xlength(2,nmap(ichr)+1), mapbp(nmap(ichr)+1) )
          mapbp(1:nmap(ichr))     = gf%fchr(ichr)%mapbp(:)
          mapbp(size(mapbp))      = gf%fchr(ichr)%pos(nsnp)
          xlength(:,1:nmap(ichr)) = gf%fchr(ichr)%mapper(:,:)
          xlength(:,size(mapbp))  = (gf%fchr(ichr)%pos(nsnp) - mapbp(size(mapbp)-1)) * 1e-8 * gf%cm2mb
          deallocate(gf%fchr(ichr)%mapbp, gf%fchr(ichr)%mapper)
          allocate( gf%fchr(ichr)%mapbp(nmap(ichr)+1), &
                    gf%fchr(ichr)%mapper(2,nmap(ichr)+1)   )
          gf%fchr(ichr)%mapbp  = mapbp
          gf%fchr(ichr)%mapper = xlength
          deallocate(mapbp, xlength)
          
       !--> remove chunks with pos > max snp pos
       elseif( gf%fchr(ichr)%mapbp(nmap(ichr)) > gf%fchr(ichr)%pos(nsnp) ) then
          nnew = count(gf%fchr(ichr)%mapbp(:) < gf%fchr(ichr)%pos(nsnp))
          allocate(xlength(2,nnew+1), mapbp(nnew+1) )
          mapbp(1:nnew)        = gf%fchr(ichr)%mapbp(1:nnew)
          mapbp(size(mapbp))   = gf%fchr(ichr)%pos(nsnp)
          xlength(:,1:nnew)      = gf%fchr(ichr)%mapper(:,1:nnew)
          ! I need to scale, trick for real
          ratio = 1.d0*(gf%fchr(ichr)%pos(nsnp) - gf%fchr(ichr)%mapbp(nnew)) / &
                  (gf%fchr(ichr)%mapbp(nnew+1) - gf%fchr(ichr)%mapbp(nnew))
          xlength(:,size(mapbp)) = gf%fchr(ichr)%mapper(:,nnew+1)*ratio
          deallocate(gf%fchr(ichr)%mapbp, gf%fchr(ichr)%mapper)
          allocate( gf%fchr(ichr)%mapbp(nnew+1), &
                    gf%fchr(ichr)%mapper(2,nnew+1) )
          gf%fchr(ichr)%mapbp  = mapbp
          gf%fchr(ichr)%mapper = xlength
          deallocate(mapbp, xlength)         
       endif

       !--> stored as percentage of length
       do h=1,2
          gf%fchr(ichr)%lengthM(h)  = sum(gf%fchr(ichr)%mapper(h,:))
          gf%fchr(ichr)%mapper(h,:) = gf%fchr(ichr)%mapper(h,:) / gf%fchr(ichr)%lengthM(h)
       enddo
    enddo
    deallocate(nmap)
 endif
 
 !--> assign xover probs
 do ichr=1, gf%nchr
    allocate(gf%fchr(ichr)%pxover(2,gf%mxover+1))
    do h=1, 2
       do i=0, gf%mxover
          gf%fchr(ichr)%pxover(h,i+1) = ppoisson(i,gf%fchr(ichr)%lengthM(h))
       enddo
       gf%fchr(ichr)%pxover(h,:) = gf%fchr(ichr)%pxover(h,:) / sum(gf%fchr(ichr)%pxover(h,:))
    enddo
 enddo
!--------------
 end subroutine
!--------------

!-------------------
 subroutine read_snp (gf,snpfile,ichip)
!-------------------
! read snp i-th chip file and assigns nsnp per chr
 integer, intent(in) :: ichip
 type(t_fgenome)     :: gf
 integer             :: ichr, ioin=11, i, isnp, nsnp, ntot
 character           :: a*4, temp*4='temp', snpfile*(*)
 integer(i8), allocatable :: chipos(:)
 integer, allocatable     :: ipos(:)

 do ichr=1, gf%nchr
    write(a,'(i3)') ichr
    call system("awk '$1=="//trim(a)//"' "//snpfile//'>'//temp)
    nsnp = wc(temp)
    gf%fchr(ichr)%chip(ichip)%nsnp = nsnp
    allocate( gf%fchr(ichr)%chip(ichip)%ipos(nsnp),  &
              gf%fchr(ichr)%chip(ichip)%chipos(nsnp) )
 enddo
 call system('rm '//temp)
 
 open(ioin,file=snpfile)
 do ichr=1, gf%nchr
    do isnp=1, gf%fchr(ichr)%chip(ichip)%nsnp
       read(ioin,*) i, gf%fchr(ichr)%chip(ichip)%chipos(isnp)
       if(i.ne.ichr) STOP 'Possible unordered SNP positions in snp file'
    enddo
 enddo
 close(ioin)

 !--> match snp chip positions
 ntot = 0
 do ichr=1, gf%nchr
    if(gf%fchr(ichr)%chip(ichip)%nsnp>0)  then
       gf%fchr(ichr)%chip(ichip)%ipos(:) = &
          match_pos(gf%fchr(ichr)%chip(ichip)%chipos, gf%fchr(ichr)%pos)
       !--> rm unknown positions
       if(any(gf%fchr(ichr)%chip(ichip)%ipos(:)==0)) then
          print*, 'WARNING:', count(gf%fchr(ichr)%chip(ichip)%ipos(:)==0), &
                  'undefined SNP positions removed from chip', ichip, ' in chr', ichr
          nsnp = count(gf%fchr(ichr)%chip(ichip)%ipos(:)>0)
          gf%fchr(ichr)%chip(ichip)%nsnp = nsnp
          allocate(ipos(nsnp))
          ipos = pack(gf%fchr(ichr)%chip(ichip)%ipos, gf%fchr(ichr)%chip(ichip)%ipos(:)>0)
          deallocate(gf%fchr(ichr)%chip(ichip)%ipos)
          allocate(gf%fchr(ichr)%chip(ichip)%ipos(nsnp))
          gf%fchr(ichr)%chip(ichip)%ipos = ipos
          deallocate(ipos)
       endif
       ntot = ntot + gf%fchr(ichr)%chip(ichip)%nsnp
       !--> memory unneeded
       deallocate(gf%fchr(ichr)%chip(ichip)%chipos)
    endif
 enddo
 print*, 'Total no of chip snps found:', ntot
!--------------
 end subroutine
!--------------


!-------------------
 subroutine copy_ind (ind, inew, gf)
!-------------------
! ULL: I do not copy id, class and parents as causes error
 type(t_indiv)   :: ind, inew
 type(t_fgenome) :: gf
 integer :: ichr, nqtl, nx
 if(associated(ind%tbv)) then
    allocate(inew%tbv(gf%ntrait), inew%dom(gf%ntrait), inew%epi(gf%ntrait))
    inew%tbv = ind%tbv
    inew%dom = ind%dom
    inew%epi = ind%epi
 endif
 if(associated(ind%y)) then
    allocate(inew%y(gf%ntrait))
    inew%y = ind%y
 endif
 inew%g%nchr = ind%g%nchr
 if(.not.associated(inew%g%chr)) allocate(inew%g%chr(inew%g%nchr,gf%ploidy))
 do ichr=1, inew%g%nchr
    do h=1, gf%ploidy  !--> ploidy
       nqtl=gf%fchr(ichr)%nqtl
       nx = ind%g%chr(ichr,h)%nx
       inew%g%chr(ichr,h)%nx = nx
       if(associated(inew%g%chr(ichr,h)%ori))    deallocate(inew%g%chr(ichr,h)%ori)
       if(associated(inew%g%chr(ichr,h)%pos))    deallocate(inew%g%chr(ichr,h)%pos)
       if(associated(inew%g%chr(ichr,h)%qtlori)) deallocate(inew%g%chr(ichr,h)%qtlori)
       allocate(inew%g%chr(ichr,h)%ori(nx), &
                inew%g%chr(ichr,h)%pos(nx), &
                inew%g%chr(ichr,h)%qtlori(nqtl)  )
       inew%g%chr(ichr,h)%ori(:) = ind%g%chr(ichr,h)%ori(:)
       inew%g%chr(ichr,h)%pos(:) = ind%g%chr(ichr,h)%pos(:) 
       if(nqtl>0) inew%g%chr(ichr,h)%qtlori(:) = ind%g%chr(ichr,h)%qtlori(:)
    enddo
 enddo  
!--------------
 end subroutine
!--------------

!--------------------
 subroutine clean_ind (ind)
!--------------------
 type(t_indiv) :: ind
 integer       :: ichr
 if (associated(ind%tbv)) deallocate(ind%tbv, ind%epi, ind%dom)
 if (associated(ind%y))   deallocate(ind%y)
 ind%id  = 0
 if(associated(ind%g%chr)) then
    do ichr=1, ind%g%nchr
       do h=1, size(ind%g%chr(ichr,:)) !--> ploidy
          if(associated(ind%g%chr(ichr,h)%ori))    deallocate(ind%g%chr(ichr,h)%ori)
          if(associated(ind%g%chr(ichr,h)%pos))    deallocate(ind%g%chr(ichr,h)%pos)
          if(associated(ind%g%chr(ichr,h)%qtlori)) deallocate(ind%g%chr(ichr,h)%qtlori)     
          if(associated(ind%g%chr(ichr,h)%gt))     deallocate(ind%g%chr(ichr,h)%gt)     
       enddo
    enddo
    deallocate(ind%g%chr)
 endif
 ind%g%nchr = 0
!--------------
 end subroutine
!--------------

!-----------------
 subroutine init_g (ind, gf, iorig, restartqtl)
!-----------------
! initializes chromosome type
 type(t_indiv)     :: ind
 type(t_fgenome)   :: gf
 integer,optional  :: iorig
 logical,optional  :: restartqtl
 integer           :: ichr, nchr, nqtl, nx, nsnp
 
 nchr = gf%nchr
 nx   = 1
 ind%g%nchr = nchr
 if(.not.associated(ind%g%chr)) allocate(ind%g%chr(nchr,gf%ploidy))
 
 if(present(iorig)) then
  !--> all is initialized
  if(.not. restartqtl) then
     do ichr=1, nchr
        nqtl=gf%fchr(ichr)%nqtl
        nsnp=gf%fchr(ichr)%nsnp
        do h=1, gf%ploidy  !--> ploidy
           if(associated(ind%g%chr(ichr,h)%ori))    deallocate(ind%g%chr(ichr,h)%ori)
           if(associated(ind%g%chr(ichr,h)%pos))    deallocate(ind%g%chr(ichr,h)%pos)
           if(associated(ind%g%chr(ichr,h)%qtlori)) deallocate(ind%g%chr(ichr,h)%qtlori)
           allocate(ind%g%chr(ichr,h)%ori(nx), &
                    ind%g%chr(ichr,h)%pos(nx), &
                    ind%g%chr(ichr,h)%qtlori(nqtl)  )
           ind%g%chr(ichr,h)%nx = nx
           ind%g%chr(ichr,h)%ori(:) = iorig+h
           ind%g%chr(ichr,h)%pos(:) = gf%fchr(ichr)%pos(nsnp)  !--> last pos
           if(nqtl>0) ind%g%chr(ichr,h)%qtlori(:) = iorig+h
        enddo
     enddo
     
  else !--> only qtl structure
     do ichr=1, nchr
        nqtl=gf%fchr(ichr)%nqtl
        do h=1, gf%ploidy  !--> ploidy
           if(associated(ind%g%chr(ichr,h)%qtlori)) deallocate(ind%g%chr(ichr,h)%qtlori)
           allocate( ind%g%chr(ichr,h)%qtlori(nqtl)  )
           if(nqtl>0) ind%g%chr(ichr,h)%qtlori(:) = iorig+h
        enddo
     enddo
  endif
 endif
 
 !--> if any sex chr, copies onto dummy male X (xploidy=1)
 if(gf%sexchr>0 .and. ind%sex==1) then
    ichr = gf%sexchr
    if(present(iorig)) ind%g%chr(ichr,1)%ori(:) = ind%g%chr(ichr,2)%ori(:)
 endif
!--------------
 end subroutine
!--------------

!---------------
 subroutine mate (ind_son, ind_sire, ind_dam, gf)
!---------------
 type(t_indiv)   :: ind_son, ind_sire, ind_dam
 type(t_fgenome) :: gf
 integer         :: ichr, ih, hpair(gf%ploidy/2,2)
 
 if(gf%ploidy==2) then
 
  do ichr=1, gf%nchr
    !--> dam's chromosome 
    call mpeiosic(ind_son, ind_dam, ichr, 2, [1,2], 2, gf)
    !--> copy dam's X chr (actually makes it two identical X from dam's meiosis)
    if(gf%sexchr==ichr .and. ind_son%sex==1) then
       call cloneChr(ind_son,ichr,h=2)
    else !--> usual male gamete
       call mpeiosic(ind_son, ind_sire,  ichr, 1, [1,2], 1, gf)
    endif
  enddo
 
 else !--> polyploids
 
  !--> chr indices ih in homeolog pairs (1,2), (3,4)... 
  do ichr=1, gf%nchr
    !--> dam's chromosome
    hpair = do_pairs(gf%tau, gf%ploidy)
    do ih=1, gf%ploidy/2  !--> each of the homeologous pairs
       call mpeiosic(ind_son, ind_dam, ichr, 2*ih, hpair(ih,:), 2, gf)
    enddo
    !--> male gamete
    hpair = do_pairs(gf%tau, gf%ploidy)
    do ih=1, gf%ploidy/2
       call mpeiosic(ind_son, ind_sire, ichr, 2*ih-1, hpair(ih,:), 1, gf)
    enddo
  enddo

 endif
!--------------
 end subroutine
!--------------

!-------------------------
 integer function do_pairs (tau,ploidy) result(xpair)
!-------------------------
 dimension :: xpair(ploidy/2,2)
 real(r8), intent(in) :: tau(:,:)
 integer,  intent(in) :: ploidy
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
    ix=ix+1
 enddo
!------------
 end function
!------------

!-------------------
 subroutine mpeiosic (offspring, parent, ichr, h, ipair, isex, gf)
!-------------------
! allow for polyploid via hpair argument
! generates a recombinant gamete, genome is used to get ipos of new recombinant events
 integer, parameter       :: maxx = 3
 integer, intent(in)      :: ichr, h, isex, ipair(:)
 type(t_indiv)            :: offspring, parent
 type(t_fgenome)          :: gf
 integer(i8), allocatable :: temp(:,:), temp2(:,:), xcpos(:)
 integer, allocatable     :: iindex(:)
 integer  :: i, nx1, nx2, nxc, nx, nsize, ix, orig(2), ihap, old, ichunk, nqtl, iqtl
 real(r8) :: xlength
 logical  :: doswap
 
 nx1=parent%g%chr(ichr,ipair(1))%nx
 nx2=parent%g%chr(ichr,ipair(2))%nx
 !--> length in Morgans, for that sex (not needed as xover probs tabulated)
 ! xlength = gf%fchr(ichr)%lengthM(h)
 !--> no. of xovers
 !nxc = min(poidev(xlength), maxx)

 if(gf%fchr(ichr)%lengthM(isex)>0.0) then !--> btw 0 and 3 xovers
    call random_loaded(nxc, gf%fchr(ichr)%pxover(isex,:))
    nxc = nxc-1
 else !--> in the special case of no recombination (map=0)
    nxc=0
 endif

 !--> return xcover positions, weighted by rec rate
 allocate(xcpos(nxc))
 call get_xcpos(xcpos, gf%fchr(ichr)%mapper(isex,:), gf%fchr(ichr)%mapbp)

 !--> merge both chrs & xover, special treatment for last pos (must be the same)
 nsize = nx1+nx2+nxc-1 !--> max space needed
 allocate(temp(nsize,3), iindex(nsize) )
 temp=0
 temp(1:nx1,1)           = parent%g%chr(ichr,ipair(1))%pos(1:nx1)
 temp(1:nx1,2)           = parent%g%chr(ichr,ipair(1))%ori(1:nx1)
 temp(nx1+1:nx1+nx2-1,1) = parent%g%chr(ichr,ipair(2))%pos(1:nx2-1)
 temp(nx1+1:nx1+nx2-1,3) = parent%g%chr(ichr,ipair(2))%ori(1:nx2-1)
 temp(nx1,3)             = parent%g%chr(ichr,ipair(2))%ori(nx2) !--> last pos shared chunk
 temp(nx1+nx2:nsize,1)   = xcpos(:)                      !--> xc positions
 deallocate(xcpos)

 !--> sorts temp(:1) that contains positions (chunk & xover)
 call indexx(nsize, real(temp(:,1),r8), iindex)
 allocate(temp2(nsize,3))
 do i=1, nsize
    temp2(i,:)=temp(iindex(i),:)
 enddo
 temp=temp2
 deallocate(temp2)

 !--> marks xover positions
 where((temp(:,2)+temp(:,3))==0) temp(:,1)=-temp(:,1)
 
 !--> fill in backwards
 do ix=nsize-1, 1, -1
    if(temp(ix,2)==0) temp(ix,2)=temp(ix+1,2)
    if(temp(ix,3)==0) temp(ix,3)=temp(ix+1,3)
 enddo 

 !--> create recombinant gametes
 doswap = .false.
 do ix=1, nsize
    if(temp(ix,1)<0) then !-> xover
       doswap = .not. doswap
    else
       if (doswap) temp(ix,2:3) = temp(ix,3:2:-1)
    endif     
 enddo

 !--> pick up haplotype, clean and pack
 ihap=mpe05ehf(1,2)+1
 allocate(temp2(nsize,2))
 temp2=0
 temp2(1,1:2)=abs(temp(1,(/1,ihap/)))
 old=temp(1,ihap)
 ix=1
 do i=2, nsize
    !--> new chunk if new origin
    if(old .ne. temp(i,ihap)) then
       ix=ix+1
    endif
    temp2(ix,1)=abs(temp(i,1))
    temp2(ix,2)=temp(i,ihap)
    old=temp(i,ihap)
 enddo
 
 !--> clean up
 nx = count(temp2(:,2)>0)
 offspring%g%chr(ichr,h)%nx = nx
 allocate(offspring%g%chr(ichr,h)%pos(nx), &
          offspring%g%chr(ichr,h)%ori(nx))
 offspring%g%chr(ichr,h)%pos = temp2(1:nx,1)
 offspring%g%chr(ichr,h)%ori = temp2(1:nx,2)
 if(any(offspring%g%chr(ichr,h)%ori==0)) STOP 'Origin cannot be 0'
 
 deallocate(temp, temp2, iindex)
!--------------
 end subroutine
!--------------

!-------------------
 subroutine cloneChr (offspring,ichr,h)
!------------------- 
! used for sex chrs in males, NA for polyploids
 type(t_indiv)       :: offspring
 integer, intent(in) :: ichr, h
 integer :: nx, ih(2)=[2,1]
 nx = offspring%g%chr(ichr,h)%nx
 offspring%g%chr(ichr,ih(h))%nx = nx
 allocate(offspring%g%chr(ichr,ih(h))%pos(nx), &
          offspring%g%chr(ichr,ih(h))%ori(nx))
 offspring%g%chr(ichr,ih(h))%pos = offspring%g%chr(ichr,h)%pos
 offspring%g%chr(ichr,ih(h))%ori = offspring%g%chr(ichr,h)%ori
!--------------
 end subroutine
!--------------

!---------------------
 subroutine get_qtlori (offspring,gf)
!--------------------- 
 type(t_indiv)     :: offspring
 type(t_fgenome)   :: gf
 integer           :: ichr, h, ichunk, iqtl, nqtl
 do ichr=1, gf%nchr
    if (gf%fchr(ichr)%nqtl>0) then
       nqtl = gf%fchr(ichr)%nqtl
       do h=1, gf%ploidy  !--> ploidy
          if (associated(offspring%g%chr(ichr,h)%qtlori) ) deallocate(offspring%g%chr(ichr,h)%qtlori)
          allocate(offspring%g%chr(ichr,h)%qtlori(nqtl))
          do iqtl=1, nqtl
             ichunk = count(offspring%g%chr(ichr,h)%pos < gf%fchr(ichr)%qtlpos(iqtl)) + 1
             offspring%g%chr(ichr,h)%qtlori(iqtl) = offspring%g%chr(ichr,h)%ori(ichunk)
          enddo
       enddo
    endif
 enddo
!--------------
 end subroutine
!--------------

!--------------------
 subroutine get_xcpos (xcpos,map,bpmap)
!--------------------
! get recombination points with unequal xrates (xcpos)
! ixpos contains # snps below the xover point
 integer     :: nx, i, ix
 integer(i8) :: bpmap(:), xcpos(:)
 real(r8)    :: map(:)
 nx = size(xcpos)
 do i=1, nx
    !--> returns interval id, weighted according to cM length
    call random_loaded(ix,map)
    !--> now a random pos within the interval is chosen
    if(ix==1) then
       xcpos(i) = rxnd()*bpmap(ix)
    else
       xcpos(i) = bpmap(ix-1) + rxnd()*(bpmap(ix)-bpmap(ix-1))
    endif
 enddo
!--------------
 end subroutine
!--------------

!------------------
 subroutine get_tbv (indiv,gf,ff)
!------------------
! ULL: think model for polyploids
! generate genotypic value for individual, ff are founder genotypes
! allocate epistasis genotypes
! integer, parameter :: delta(0:2)=(/0,1,0/) !--> NA for polyploids
 type(t_indiv)      :: indiv, ff(:)
 type(t_fgenome)    :: gf
 integer            :: ntrait, ichr, iqtl, isnp, iori, g, h, it, itrait, &
                       iepi, eg(2), ipair
 ntrait = gf%ntrait
 if(associated(indiv%tbv)) deallocate(indiv%tbv)
 if(associated(indiv%y))   deallocate(indiv%y)
 if(associated(indiv%epi)) deallocate(indiv%epi)
 allocate(indiv%tbv(ntrait), indiv%y(ntrait), indiv%epi(ntrait), indiv%dom(ntrait))
 
 indiv%tbv(:) = 0
 indiv%dom(:) = 0
 do ichr=1, gf%nchr
    do iqtl=1, gf%fchr(ichr)%nqtl
       g = 0
       isnp = gf%fchr(ichr)%iposqtl(iqtl)
       do h=1, gf%ploidy   !--> ploidy
          iori = indiv%g%chr(ichr,h)%qtlori(iqtl)
          g    = g + ff(iori)%g%chr(ichr,1)%gt(isnp)
       enddo
       !--> general expression for n-ploidy
       indiv%tbv = indiv%tbv + (g-gf%ploidy/2) * gf%fchr(ichr)%qtladd(iqtl,:)
       do it=1, ntrait
          indiv%dom(it) = indiv%dom(it) + dodom(g, gf%fchr(ichr)%qtladd(iqtl,it), gf%fchr(ichr)%qtldom(iqtl,it) , gf%delta)
       enddo
        !indiv%dom = indiv%dom + delta(g) * gf%fchr(ichr)%qtldom(iqtl,:)
    enddo
 enddo
 
 !--> add epistasis if needed
 indiv%epi=0.
 do iepi=1, gf%nepi
    itrait = gf%epi(iepi)%trait
    eg=1
    do ipair=1, 2
       ichr = gf%epi(iepi)%chr(ipair)
       iqtl = gf%epi(iepi)%iqtl(ipair)
       isnp = gf%fchr(ichr)%iposqtl(iqtl)
       do h=1, 2   !--> ploidy
          iori  = indiv%g%chr(ichr,h)%qtlori(iqtl)
          eg(ipair) = eg(ipair) + ff(iori)%g%chr(ichr,1)%gt(isnp)
       enddo
    enddo
    indiv%epi(itrait) = indiv%epi(itrait) + gf%epi(iepi)%eff(eg(1),eg(2))
 enddo
!--------------
 end subroutine
!--------------

!----------------
 subroutine get_y (indiv,gf)
!----------------
! generate phenotypic value for individual
 type(t_indiv)   :: indiv
 type(t_fgenome) :: gf
 real(r8)        :: z
 integer         :: iclass, it
 
 !--> determine class if defined for h2
 iclass = 1
 if(size(gf%se,dim=1)>1) iclass=indiv%class

 !--> add random residual (class dependent if pertinent)
 call random_normal(z)
 indiv%y = indiv%tbv + indiv%dom + indiv%epi + z * gf%se(iclass,:)

 !--> in case binary traits
 do it=1, gf%ntrait
    if (gf%binary(it)) then
       if(indiv%y(it) > gf%threshold(it)) then
          indiv%y(it)=1
       else
          indiv%y(it)=0
       endif
    endif
 enddo
!--------------
 end subroutine
!--------------

!----------------------
 subroutine print_indiv (ind,gf,ff,printx)
!----------------------
! prints indiv haplotypes, optionally also xpos and origins
 type(t_indiv)   :: ind, ff(:)
 type(t_fgenome) :: gf
 logical, optional :: printx
 integer         :: ichr, h, ichunk, ori, isnp
 integer(i8)     :: bpos
 character :: a*20 = '---------------------'

 write(*,'(a)') a
 print'(a,i6)', 'ind: ', ind%id
 do ichr=1, gf%nchr
    !print'(a,i3)', ' chr: ', ichr
    do h=1, gf%ploidy
       ichunk=1
       do isnp=1, gf%fchr(ichr)%nsnp
          bpos = gf%fchr(ichr)%pos(isnp)
          if (bpos > ind%g%chr(ichr,h)%pos(ichunk) ) then
             ichunk = count( ind%g%chr(ichr,h)%pos(:) < bpos ) + 1
          endif
          !--> ori is founder haplotype
          ori  = ind%g%chr(ichr,h)%ori(ichunk)
          write(*,'(i1)',advance='no') ff(ori)%g%chr(ichr,1)%gt(isnp)
       enddo
       !write(*,*)
    enddo
 enddo
 !write(*,'(a)') a
!--------------
 end subroutine
!--------------

!----------------------
 subroutine dogenotypes (pop, gf, ff, datos, ichip)
!----------------------
! ULL: correct format in wriitng large nsnp!!!!!!!!!!!!!!!!!
! mimic option for polyploids
! new v controls memory
! allows for first ind /= 1
! minimizes doloop
! retrieve genotypes from a set of individuals stored as chunk_chr
! ULL: unrealistic to provide those for very large datsets, better output on stdout or GRM
! chip contains markers to be genotyped, all genotyped if absent
 implicit none
 integer, parameter     :: nsnp_block=100000
 integer, intent(in)    :: ichip
 type(t_indiv)          :: pop(:),&    !--> genomes in chr_chunk format
                           ff(:)       !--> founder genotypes in haploid format
 type(t_fgenome)        :: gf  
 type(t_info)           :: datos  
 integer                :: nind, nsnp, ichr, isnp, icsnp, ipos, ind, h, ori, iom=21, iogrm=22, &
                           i1, i2, nhtrg, nthr, i, iogwas=652, maxclass, ksnp, ifirst, vploidy
 integer(i8)            :: bpos
 real                   :: freq, t1, t2
 real(r8)               :: grm_d, one=1.d0, vx, cxy
 integer, allocatable   :: ichunk(:,:), gt(:,:), class(:), nclass(:)
 real(r8), allocatable  :: grm(:,:), g(:,:), y(:), b(:), sb(:), f(:), vy(:)
 character              :: mfile*30, gfile*30, wfile*30, a*1

 vploidy = gf%ploidy
 if(gf%mimic) vploidy=2
 if(ichip>9) STOP 'max no of chips allowed is 9'
 write(a,'(i1)') ichip
 mfile=trim(adjustl(datos%outmfile)) // '.' // a
 gfile=trim(adjustl(datos%outgfile)) // '.' // a
 wfile=trim(adjustl(datos%gwasfile)) // '.' // a
 if(datos%plink) mfile = trim(adjustl(mfile)) // '.tped'

 ifirst = info%indfirst
 nind=size(pop)-ifirst+1
 if(datos%printgrm) then
    allocate(grm(nind,nind))
    grm=0.; grm_d=0.
 endif
 
 if(datos%printmkr) open(iom,file=mfile)
 
 if(datos%gwas) then
    allocate(y(nind), class(nind))
    do ind=1, nind
       y(ind) = pop(ind+ifirst-1)%y(1)
       class(ind) = pop(ind+ifirst-1)%class
    enddo
    maxclass=maxval(class)
    allocate(b(maxclass), sb(maxclass), f(maxclass), vy(maxclass), nclass(maxclass))
    do i=1, maxclass
       vy(i) = variance(pack(y,class==i))
       nclass(i) = count(class==i)
    enddo
    open(iogwas,file=wfile)
 endif

 call cpu_time(t1)

 !--> ULL: must distinguish between snp order in chip and snp order in whole snp dataset (sequence)
 ksnp = 0
 allocate(ichunk(nind,gf%ploidy), gt(nind,gf%ploidy))
 do ichr=1, gf%nchr
 
    !--> only snps in chip are genotyped
    if(ichip>0) then
       nsnp=gf%fchr(ichr)%chip(ichip)%nsnp
    !--> whole sequence (ichip=0)
    else
       nsnp=gf%fchr(ichr)%nsnp
    endif
    allocate(g(nind,min(nsnp,nsnp_block)))
    g=0.
    
    !--> ichunk is chunk containing current genotyped marker for each indiv
    ichunk(:,:)=1
    isnp=0
    do icsnp=1, nsnp
       !--> isnp block counter
       isnp = isnp + 1
       if(ichip>0) then
          ipos=gf%fchr(ichr)%chip(ichip)%ipos(icsnp)
       else
          ipos=icsnp
       endif

       !--> snp bp position
       bpos = gf%fchr(ichr)%pos(ipos)
       do ind=1, nind
          !--> update ichunk if position is higher than current chunk limit
          do h=1, gf%ploidy   !--> ploidy
             if (bpos > pop(ind+ifirst-1)%g%chr(ichr,h)%pos(ichunk(ind,h)) ) then
                ichunk(ind,h)=count( pop(ind+ifirst-1)%g%chr(ichr,h)%pos(:) < bpos )+1
             endif
             !--> ori is founder haplotype
             ori         = pop(ind+ifirst-1)%g%chr(ichr,h)%ori(ichunk(ind,h))
             gt(ind,h)   = ff(ori)%g%chr(ichr,1)%gt(ipos)
             g(ind,isnp) = g(ind,isnp) + gt(ind,h)
          enddo
       enddo

       !--> computes freq according to whether mimic diploid is in action
       if(gf%mimic .and. maxval(g(:,isnp)) > 2 ) then
          where (g(:,isnp) > 0) g(:,isnp)=2
       endif
       freq = sum(g(:,isnp)) / real(vploidy*nind)

       !--> write marker data
       if(datos%printmkr) then
          if(datos%plink) then  !--> tped file
             ksnp = ksnp+1
             write(iom,'(i3,i9,i2,i12,5000i2)') ichr,ksnp,0,bpos,(gt(ind,:)+1,ind=1,nind)
          else
             write(iom,'(i3,i12,5000i2)')  ichr,bpos,int(g(:,isnp))
          endif
       endif
       
       if(datos%gwas) then
          do i=1, maxclass
             vx  = variance(pack(g(:,isnp),class==i))
             cxy = covariance(pack(y,class==i), pack(g(:,isnp),class==i))
             b(i)  = cxy / vx
             sb(i) = sqrt((vy(i) - b(i)*cxy) / (nclass(i)-2.) / vx)
             f(i) = 1./real(gf%ploidy) * sum(pack(g(:,isnp),class==i)) / count(class==i)
          enddo
          write(iogwas,'(i3,i12,100f10.4)') ichr, bpos, real(f(:)), real(b(:)), real(b(:)/sb(:))
       endif
 
       !--> ULL: modified for ploidy>2
       if(datos%printgrm) then
          grm_d = grm_d + vploidy * freq*(1.-freq)
          g(:,isnp) = g(:,isnp) - sum(g(:,isnp)) / (vploidy*nind)
       endif

       if(mod(isnp,nsnp_block)==0 .or. icsnp==nsnp) then
          !--> computes XX' and adds to GRM
          if(datos%printgrm) &
             call dgemm('n','t',nind,nind,isnp,one, g(:,1:isnp), nind, g(:,1:isnp),nind,one, GRM,nind)
          isnp=0
          g(:,:)=0  !--> unneeded really
          call cpu_time(t2)
          print*, 'snps processed',ichr,icsnp,t2-t1
          t2=t1
       endif
    enddo

    deallocate(g)
 enddo
 deallocate(ichunk, gt)

 if(datos%printmkr) then
    close(iom)
    if(datos%gzip) then
       call system('gzip -c ' // trim(mfile) // ' > ' // trim(mfile) // '.gz')
       call system('rm '//mfile)
    endif
 endif

 if(datos%gwas) then
    close(iogwas)
    if(datos%gzip) then
       call system('gzip -c ' // trim(wfile) // ' > ' // trim(wfile) // '.gz')
       call system('rm '//wfile)
    endif
    deallocate(y,vy,class,nclass,b,sb,f)
 endif
 
 !--> finish up GRM
 if(datos%printgrm) then
    grm=grm/grm_d
    open(iogrm,file=gfile)
    do i=1, nind
       write(iogrm,'(1000000f7.3)') grm(i,:)
    enddo
    close(iogrm)
    deallocate(grm)
 endif
!--------------
 end subroutine
!--------------

!---------------------
 subroutine expand_ind (ind, pop, t, n, gf)
!---------------------
! breed a new individual using base pop inds
 type(t_indiv)        :: ind, pop(:)
 type(t_fgenome)      :: gf
 integer, intent(in)  :: t, n
 type(t_indiv), allocatable :: inds0(:)
 integer, allocatable       :: iped(:,:)
 integer :: i0, ii, nbase, nblocks
 nbase=size(pop)
 
 allocate(iped((t-1)*n+1,3), &
          inds0((t-1)*n+1),  &
          idcode(nbase) )
 idcode=[(i0,i0=1,nbase)]
 
 !--> every how often nbase inds shuffled
 nblocks = nbase / n 
 !--> define counter for idcode, ids sampled such that Ne=2*N
 i0 = ind%id-ind%id/nbase*nbase
 i0 = mod(i0,nblocks)*n
 if(i0==1) call random_permu(idcode)
 
 !--> generate a new pedigree
 call doped(iped, idcode(i0+1:i0+n), t)
 
 !--> generates new haplotypes for pedigree
 !- first generation has nbase parents
 do ii=info%expand_nfam+1, 2*info%expand_nfam
    call init_g(inds0(ii),gfeatures)
    call mate(inds0(ii),pop(iped(ii,2)),pop(iped(ii,3)),gf)
    call get_qtlori(inds0(ii),gfeatures)
 enddo

 !- rest of generations
 do ii=2*info%expand_nfam+1, size(iped,dim=1)
    call init_g(inds0(ii),gfeatures)
    call mate(inds0(ii),inds0(iped(ii,2)),inds0(iped(ii,3)),gf)
    call get_qtlori(inds0(ii),gfeatures)
 enddo
 
 !--> copy last indiv into general pedigree
 call copy_ind(inds0(size(iped,dim=1)),ind,gf)
 
 !--> cleanup
 do ii=1, size(iped,dim=1)
    call clean_ind(inds0(ii))
 enddo
 deallocate(inds0, iped, idcode)
!--------------
 end subroutine
!--------------

!----------------
 subroutine doped (ped,id0,t)
!----------------
 integer, intent(in) :: id0(:), t
 integer :: i, ii, it, n, ped(:,:), &
            id1(size(id0)), id2(size(id0))
 n=size(id0)
 ped=0
 ped(1:n,1)=id0
 ii=n
 do it=2, t
    id1=ped((it-2)*n+1:(it-1)*n,1) 
    id2=id1 
    call random_permu(id1)
    call random_permu(id2)
    do i=1, n
       ii=ii+1
       ped(ii,1)=ii
       ped(ii,2)=id1(i)
       ped(ii,3)=id2(i)
       !print*, 'wwxw',it,ped(ii,:)
       ! only one individual needs to be generated in last t
       if(t==it) EXIT
    enddo
 enddo
!--------------
 end subroutine
!--------------

!--------------------
 subroutine print_hap (pop,gf,hapfile)
!--------------------
! prints both qtl and hap structure for further reading
 type(t_indiv)   :: pop(:)
 type(t_fgenome) :: gf
 character       :: hapfile*(*)
 integer         :: ind, ichr, ix, iq, nx, h, ioh=12
 open(ioh,file=hapfile)
 
 write(ioh,*) gf%nchr, size(gf%se,dim=1), gf%ntrait, gf%nepi
 write(ioh,*) gf%se
 
 do ichr=1, gf%nchr
    write(ioh,*) gf%fchr(ichr)%nqtl
    do iq=1, gf%fchr(ichr)%nqtl
       write(ioh,*) gf%fchr(ichr)%qtlpos(iq), gf%fchr(ichr)%iposqtl(iq), &
                    gf%fchr(ichr)%qtladd(iq,:), gf%fchr(ichr)%qtldom(iq,:), &
                    gf%fchr(ichr)%qtlepi(iq,:)
    enddo
 enddo
 
 do iq=1, gf%nepi
    write(ioh,*) gf%epi(iq)%trait, gf%epi(iq)%chr(:), gf%epi(iq)%iqtl(:), gf%epi(iq)%eff
 enddo
 
 do ind=1, size(pop)
    write(ioh,*) pop(ind)%id, real(pop(ind)%y), real(pop(ind)%tbv), real(pop(ind)%dom), real(pop(ind)%epi)
    do ichr=1, pop(ind)%g%nchr
       do h=1, gf%ploidy   !--> ploidy
          nx = pop(ind)%g%chr(ichr,h)%nx
          write(ioh,*) nx
          do ix=1, nx
             write(ioh,*) pop(ind)%g%chr(ichr,h)%pos(ix), &
                          pop(ind)%g%chr(ichr,h)%ori(ix) 
          enddo
       enddo
       do iq=1, size(pop(ind)%g%chr(ichr,1)%qtlori)
          write(ioh,*) (pop(ind)%g%chr(ichr,h)%qtlori(iq), h=1,gf%ploidy)   !--> ploidy
       enddo
    enddo
 enddo
 close(ioh)
!--------------
 end subroutine
!--------------

!-------------------
 subroutine read_hap (pop,gf,hapfile,restartqtl)
!-------------------
! reads haplotypes and allocates memory for each ind
 type(t_indiv)   :: pop(:)
 type(t_fgenome) :: gf
 character       :: hapfile*(*)
 integer         :: ind, nchr, nclass, ntrait, nepi, ichr, ix, iq, nx, h, ioh=12, ios
 logical         :: restartqtl
 integer, allocatable :: nqtl(:)
 real, allocatable    :: se(:,:), tbv(:), tbd(:), tbp(:), y(:)
 if(wc(hapfile)==0) RETURN
 open(ioh,file=hapfile)

 !--> basic test: se can be a vector
 print*, 'ULL: nepi in hapfile'
 if(info%oldhap) then
    read(ioh,*) nchr, nclass, ntrait
    nepi=0
 else
    read(ioh,*) nchr, nclass, ntrait, nepi
 endif
 if(nchr .ne. gf%nchr) STOP 'nchr does not match in hap file'
 gf%ntrait = ntrait
 allocate(nqtl(nchr), se(nclass,ntrait), y(ntrait), tbv(ntrait), tbd(ntrait), tbp(ntrait))
 read(ioh,*) se(:,:)
 
 !--> only used if qtl are not reinitialized
 if(.not.restartqtl) then
    if(gf%ntrait .ne. ntrait) &
       STOP 'NTRAIT must coincide in par and hap file unless RESTARTQTL'
    allocate(gf%se(nclass,ntrait))
    gf%se = se 
 endif

 !--> read qtl data (irrelevant if RESTARTQTL)
 nqtl(:)=0 
 do ichr=1, gf%nchr
    read(ioh,*) nqtl(ichr)
    if(.not.restartqtl) then
       gf%fchr(ichr)%nqtl = nqtl(ichr)
       allocate( gf%fchr(ichr)%qtlpos(nqtl(ichr)),  &
                 gf%fchr(ichr)%iposqtl(nqtl(ichr)), &
                 gf%fchr(ichr)%qtlepi(nqtl(ichr),2),  &
                 gf%fchr(ichr)%qtladd(nqtl(ichr),ntrait),  &
                 gf%fchr(ichr)%qtldom(nqtl(ichr),ntrait)   )
       do iq=1, gf%fchr(ichr)%nqtl
          read(ioh,*) gf%fchr(ichr)%qtlpos(iq),   gf%fchr(ichr)%iposqtl(iq),  &
                      gf%fchr(ichr)%qtladd(iq,:), gf%fchr(ichr)%qtldom(iq,:), &
                      gf%fchr(ichr)%qtlepi(iq,:)
       enddo
    else !--> skip if restart qtl
       do iq=1, nqtl(ichr); read(ioh,*); enddo
    endif
 enddo
 
 !--> read epistatic pairs info
 if(.not.restartqtl .and. nepi>0) then
    gf%nepi = nepi
    allocate(gf%epi(nepi))
    do iq=1, nepi
       read(ioh,*) gf%epi(iq)%trait, gf%epi(iq)%chr(1:2), gf%epi(iq)%iqtl(1:2), &
                   gf%epi(iq)%eff
    enddo
 elseif(restartqtl) then
    do iq=1, nepi; read(ioh,*); enddo
 endif
 
 !--> read haps
 do
    if(info%oldhap) then
       read(ioh,*,iostat=ios) ind, y, tbv
       tbd=0
       tbp=0
    else
       read(ioh,*,iostat=ios) ind, y, tbv, tbd, tbp
    endif
    if(ios/=0) EXIT
    pop(ind)%id       = ind
    if(.not.restartqtl) then
       allocate(pop(ind)%tbv(ntrait), pop(ind)%dom(ntrait), pop(ind)%epi(ntrait), pop(ind)%y(ntrait))
       pop(ind)%tbv      = tbv
       pop(ind)%dom      = tbd
       pop(ind)%epi      = tbp
       pop(ind)%y        = y
    else
       allocate(pop(ind)%tbv(gf%ntrait), pop(ind)%dom(gf%ntrait), pop(ind)%y(gf%ntrait))
    endif
    pop(ind)%g%nchr   = gf%nchr
    pop(ind)%hapknown = .true.
    !--> check if assigned and error or reallocate
    allocate(pop(ind)%g%chr(gf%nchr,2))
    do ichr=1, gf%nchr
       do h=1, gf%ploidy   !--> ploidy
          read(ioh,*) nx
          pop(ind)%g%chr(ichr,h)%nx = nx
          allocate (pop(ind)%g%chr(ichr,h)%pos(nx), &
                    pop(ind)%g%chr(ichr,h)%ori(nx), &
                    pop(ind)%g%chr(ichr,h)%qtlori( gf%fchr(ichr)%nqtl ))
          do ix=1, nx
             read(ioh,*) pop(ind)%g%chr(ichr,h)%pos(ix), &
                         pop(ind)%g%chr(ichr,h)%ori(ix) 
          enddo
       enddo
       if(.not.restartqtl) then
          do iq=1, nqtl(ichr)
             read(ioh,*) (pop(ind)%g%chr(ichr,h)%qtlori(iq), h=1,gf%ploidy)  !--> ploidy
          enddo
       else
          do iq=1, nqtl(ichr)
             read(ioh,*) 
          enddo
       endif
    enddo   
 enddo
 print*, count(pop(:)%hapknown), 'ind haplotypes read in hapfile'
 close(ioh)
 deallocate(nqtl,y,tbv,tbd,tbp,se)
!--------------
 end subroutine
!--------------


!------------
 END PROGRAM
!------------
