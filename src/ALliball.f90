!ULLL: wrong random initialization
!module kinds
!  integer, parameter :: single = SELECTED_REAL_KIND( 6, 37 )
!  integer, parameter :: double = SELECTED_REAL_KIND( 15, 307 )
!  integer, parameter :: extended = SELECTED_REAL_KIND( 18, 4931 )
! integer, parameter :: r4 = SELECTED_REAL_KIND( 6, 37 )
!  integer, parameter :: r8 = SELECTED_REAL_KIND( 15, 307 )
!  integer, parameter :: r16 = SELECTED_REAL_KIND( 18, 4931 )

  ! current precison for hash storage
!  integer, parameter :: rh=r8
!end module kinds

!LAPACK90R -reduced >the set of modules from the followinig files:
!l_auxmod.f90, dblas1.f90, dblas2.f90, dblas3.f90, dla.f90, 
!dor.f90, dge.f90, lapack90.f90, la_dgesv.f90, dsy.f90, la_dsyev.f90


MODULE LA_PRECISION

!  -- LAPACK90 interface driver routine (version 1.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     May 31, 1997

!  DEFINES SINGLE AND DOUBLE PRECISION PARAMETERS, SP AND DP.
!  THESE VALUES ARE COMPILER DEPENDENT.

!  N.B. With some compilers, it may be necessary to split this file into two
!       separate files so that the first module is available for use by the
!       second.

IMPLICIT NONE
INTEGER, PARAMETER :: sp = KIND(1.0), dp = KIND(1.0D0)
!
END MODULE LA_PRECISION


MODULE LA_AUXMOD

!  -- LAPACK90 interface driver routine (version 1.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     May 31, 1997

! ILAENV, DLAMCH and DLASRT have been added to this module.
! For a single-precision version, change wp below to point to sp.

! ELF90 translation by Alan Miller    03-Sep-1997
! Latest revision - 3 November 1997

USE la_precision, ONLY: wp => dp
IMPLICIT NONE

CONTAINS


FUNCTION LSAME( CA, CB ) RESULT(fn_val)
!
!  -- LAPACK auxiliary routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994
!
!  Purpose
!  =======
!
!  LSAME  tests if CA is the same letter as CB regardless of case.
!
!  Parameters
!  ==========
!
!  CA      (input) CHARACTER*1
!  CB      (input) CHARACTER*1
!           Characters to be compared.
!
!  .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: CA, CB
LOGICAL                       :: fn_val

!  .. Parameters ..
!  INTEGER, PARAMETER :: IOFF=32       ! Not used!
!  .. Local Scalars ..
   INTEGER :: INTA, INTB, ZCODE
!  .. Intrinsic Functions ..
!  INTRINSIC             ICHAR
!
!  .. Executable Statements ..
!
!  Test if the characters are equal
!
   fn_val = CA == CB
!
!  Now test for equivalence
!
   IF ( .NOT. fn_val ) THEN
!
!     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
!     machines, on which ICHAR returns a value with bit 8 set.
!     ICHAR('A') on Prime machines returns 193 which is the same as
!     ICHAR('A') on an EBCDIC machine.
!
      ZCODE = ICHAR( 'Z' )
!
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
!
      IF( ZCODE == 90 .OR. ZCODE == 122 ) THEN
!
!        ASCII is assumed - ZCODE is the ASCII code of either lower or
!        upper case 'Z'.
!
         IF( INTA >= 97 .AND. INTA <= 122 ) INTA = INTA - 32
         IF( INTB >= 97 .AND. INTB <= 122 ) INTB = INTB - 32
!
      ELSE IF( ZCODE == 233 .OR. ZCODE == 169 ) THEN
!
!        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
!        upper case 'Z'.
!
         IF( INTA >= 129 .AND. INTA <= 137 .OR. &
!            INTA >= 145 .AND. INTA <= 153 .OR. &
             INTA >= 162 .AND. INTA <= 169 ) INTA = INTA + 64
         IF( INTB >= 129 .AND. INTB <= 137 .OR. &
             INTB >= 145 .AND. INTB <= 153 .OR. &
             INTB >= 162 .AND. INTB <= 169 ) INTB = INTB + 64
!
      ELSE IF( ZCODE == 218 .OR. ZCODE == 250 ) THEN
!
!        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
!        plus 128 of either lower or upper case 'Z'.
!
         IF( INTA >= 225 .AND. INTA <= 250 ) INTA = INTA - 32
         IF( INTB >= 225 .AND. INTB <= 250 ) INTB = INTB - 32
      END IF
      fn_val = INTA == INTB
   END IF

RETURN
END FUNCTION LSAME


SUBROUTINE ERINFO(LINFO, SRNAME, INFO, ISTAT)

!  -- LAPACK90 interface driver routine (version 1.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     May 31, 1997

!  .. Scalar Arguments ..
CHARACTER( LEN = * ), INTENT(IN)               :: SRNAME
INTEGER             , INTENT(IN)               :: LINFO
INTEGER             , INTENT(IN OUT), OPTIONAL :: INFO
INTEGER             , INTENT(IN), OPTIONAL     :: ISTAT
!
!  .. Executable Statements ..
!
IF ( ( ( LINFO < 0 .AND. LINFO > -200 ) .OR. LINFO > 0 )           &
     &            .AND. .NOT.PRESENT(INFO) ) THEN
   WRITE (*,*) 'Program terminated in LAPACK_90 subroutine ',SRNAME
   WRITE (*,*) 'Error indicator, INFO = ',LINFO
   IF ( PRESENT(ISTAT) )THEN
     IF ( ISTAT /= 0 ) THEN
       IF ( LINFO == -100 ) THEN
         WRITE (*,*) 'The statement ALLOCATE causes STATUS = ', ISTAT
       ELSE
         WRITE (*,*) 'LINFO = ', LINFO, ' not expected'
       END IF
     END IF
   END IF
   STOP
    ELSE IF ( LINFO <= -200 ) THEN
      WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++'
      WRITE(*,*) '*** WARNING, INFO = ', LINFO, ' WARNING ***'
      IF ( LINFO == -200 ) THEN
        WRITE(*,*) 'Could not allocate sufficient workspace for the optimum'
        WRITE(*,*) 'blocksize, hence the routine may not have performed as'
        WRITE(*,*) 'efficiently as possible'
    ELSE
      WRITE(*,*) 'Unexpected warning'
    END IF
      WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++'
   END IF
   IF ( PRESENT(INFO) ) THEN
     INFO = LINFO
   END IF

RETURN
END SUBROUTINE ERINFO


FUNCTION ilaenv( ispec, NAME, opts, n1, n2, n3, n4 ) RESULT(fn_val)

!  -- LAPACK auxiliary routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994

! This version, which is compatible with ELF90, is by Alan Miller
! N.B. Arguments opts and n3 were not used in the F77 version.
! Latest revision - 2 September 1997

!     .. Scalar Arguments ..
CHARACTER (LEN = *), INTENT(IN) :: NAME, opts
INTEGER, INTENT(IN)             :: ispec, n1, n2, n3, n4
INTEGER                         :: fn_val
!     ..

!  Purpose
!  =======

!  ILAENV is called from the LAPACK routines to choose problem-dependent
!  parameters for the local environment.  See ISPEC for a description of
!  the parameters.

!  This version provides a set of parameters which should give good,
!  but not optimal, performance on many of the currently available
!  computers.  Users are encouraged to modify this subroutine to set
!  the tuning parameters for their particular machine using the option
!  and problem size information in the arguments.

!  This routine will not function correctly if it is converted to all
!  lower case.  Converting it to all upper case is allowed.

!  Arguments
!  =========

!  ISPEC   (input) INTEGER
!          Specifies the parameter to be returned as the value of ILAENV.
!          = 1: the optimal blocksize; if this value is 1, an unblocked
!               algorithm will give the best performance.
!          = 2: the minimum block size for which the block routine
!               should be used; if the usable block size is less than
!               this value, an unblocked routine should be used.
!          = 3: the crossover point (in a block routine, for N less
!               than this value, an unblocked routine should be used)
!          = 4: the number of shifts, used in the nonsymmetric
!               eigenvalue routines
!          = 5: the minimum column dimension for blocking to be used;
!               rectangular blocks must have dimension at least k by m,
!               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
!          = 6: the crossover point for the SVD (when reducing an m by n
!               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
!               this value, a QR factorization is used first to reduce
!               the matrix to a triangular form.)
!          = 7: the number of processors
!          = 8: the crossover point for the multishift QR and QZ methods
!               for nonsymmetric eigenvalue problems.

!  NAME    (input) CHARACTER*(*)
!          The name of the calling subroutine, in either upper or lower case.

!  OPTS    (input) CHARACTER*(*)
!          The character options to the subroutine NAME, concatenated
!          into a single character string.  For example, UPLO = 'U',
!          TRANS = 'T', and DIAG = 'N' for a triangular routine would
!          be specified as OPTS = 'UTN'.

!  N1      (input) INTEGER
!  N2      (input) INTEGER
!  N3      (input) INTEGER
!  N4      (input) INTEGER
!          Problem dimensions for the subroutine NAME; these may not all
!          be required.

! (ILAENV) (output) INTEGER
!          >= 0: the value of the parameter specified by ISPEC
!          < 0:  if ILAENV = -k, the k-th argument had an illegal value.

!  Further Details
!  ===============

!  The following conventions have been used when calling ILAENV from the
!  LAPACK routines:
!  1)  OPTS is a concatenation of all of the character options to
!      subroutine NAME, in the same order that they appear in the
!      argument list for NAME, even if they are not used in determining
!      the value of the parameter specified by ISPEC.
!  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
!      that they appear in the argument list for NAME.  N1 is used
!      first, N2 second, and so on, and unused problem dimensions are
!      passed a value of -1.
!  3)  The parameter value returned by ILAENV is checked for validity in
!      the calling subroutine.  For example, ILAENV is used to retrieve
!      the optimal blocksize for STRTRI as follows:

!      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
!      IF( NB.LE.1 ) NB = MAX( 1, N )

!  =====================================================================

!     .. Local Scalars ..
LOGICAL           :: cname, sname
CHARACTER (LEN=1) :: c1
CHARACTER (LEN=2) :: c2, c4
CHARACTER (LEN=3) :: c3
CHARACTER (LEN=6) :: subnam
INTEGER           :: i, ic, iz, nb, nbmin, nx
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
!     ..
!     .. Executable Statements ..

SELECT CASE (ispec)
  CASE (1:3)
    GO TO 100
  CASE (4)
    GO TO 400
  CASE (5)
    GO TO 500
  CASE (6)
    GO TO 600
  CASE (7)
    GO TO 700
  CASE (8)
    GO TO 800
  CASE DEFAULT
    fn_val = -1              !     Invalid value for ISPEC
    RETURN
END SELECT

!     Convert NAME to upper case if the first character is lower case.

100 fn_val = 1
subnam = NAME
ic = ICHAR( subnam( 1:1 ) )
iz = ICHAR( 'Z' )
IF( iz == 90 .OR. iz == 122 ) THEN
  
!        ASCII character set
  
  IF( ic >= 97 .AND. ic <= 122 ) THEN
    subnam( 1:1 ) = CHAR( ic-32 )
    DO i = 2, 6
      ic = ICHAR( subnam( i:i ) )
      IF( ic >= 97 .AND. ic <= 122 ) subnam( i:i ) = CHAR( ic-32 )
    END DO
  END IF
  
ELSE IF( iz == 233 .OR. iz == 169 ) THEN
  
!        EBCDIC character set
  
  IF( ( ic >= 129 .AND. ic <= 137 ) .OR.  &
      ( ic >= 145 .AND. ic <= 153 ) .OR.  &
      ( ic >= 162 .AND. ic <= 169 ) ) THEN
    subnam( 1:1 ) = CHAR( ic+64 )
    DO i = 2, 6
      ic = ICHAR( subnam( i:i ) )
      IF( ( ic >= 129 .AND. ic <= 137 ) .OR.  &
          ( ic >= 145 .AND. ic <= 153 ) .OR.  &
          ( ic >= 162 .AND. ic <= 169 ) ) subnam( i:i ) = CHAR( ic+64 )
    END DO
  END IF
  
ELSE IF( iz == 218 .OR. iz == 250 ) THEN
  
!        Prime machines:  ASCII+128
  
  IF( ic >= 225 .AND. ic <= 250 ) THEN
    subnam( 1:1 ) = CHAR( ic-32 )
    DO i = 2, 6
      ic = ICHAR( subnam( i:i ) )
      IF( ic >= 225 .AND. ic <= 250 ) subnam( i:i ) = CHAR( ic-32 )
    END DO
  END IF
END IF

c1 = subnam( 1:1 )
sname = c1 == 'S' .OR. c1 == 'D'
cname = c1 == 'C' .OR. c1 == 'Z'
IF( .NOT.( cname .OR. sname ) ) RETURN
c2 = subnam( 2:3 )
c3 = subnam( 4:6 )
c4 = c3( 2:3 )

IF (ispec == 2) THEN
  GO TO 200
ELSE IF (ispec == 3) THEN
  GO TO 300
END IF

!     ISPEC = 1:  block size

!     In these examples, separate code is provided for setting NB for
!     real and complex.  We assume that NB will take the same value in
!     single or double precision.

nb = 1

IF( c2 == 'GE' ) THEN
  IF( c3 == 'TRF' ) THEN
    IF( sname ) THEN
      nb = 64
    ELSE
      nb = 64
    END IF
  ELSE IF( c3 == 'QRF' .OR. c3 == 'RQF' .OR. c3 == 'LQF' .OR.  &
    c3 == 'QLF' ) THEN
    IF( sname ) THEN
      nb = 32
    ELSE
      nb = 32
    END IF
  ELSE IF( c3 == 'HRD' ) THEN
    IF( sname ) THEN
      nb = 32
    ELSE
      nb = 32
    END IF
  ELSE IF( c3 == 'BRD' ) THEN
    IF( sname ) THEN
      nb = 32
    ELSE
      nb = 32
    END IF
  ELSE IF( c3 == 'TRI' ) THEN
    IF( sname ) THEN
      nb = 64
    ELSE
      nb = 64
    END IF
  END IF
ELSE IF( c2 == 'PO' ) THEN
  IF( c3 == 'TRF' ) THEN
    IF( sname ) THEN
      nb = 64
    ELSE
      nb = 64
    END IF
  END IF
ELSE IF( c2 == 'SY' ) THEN
  IF( c3 == 'TRF' ) THEN
    IF( sname ) THEN
      nb = 64
    ELSE
      nb = 64
    END IF
  ELSE IF( sname .AND. c3 == 'TRD' ) THEN
    nb = 1
  ELSE IF( sname .AND. c3 == 'GST' ) THEN
    nb = 64
  END IF
ELSE IF( cname .AND. c2 == 'HE' ) THEN
  IF( c3 == 'TRF' ) THEN
    nb = 64
  ELSE IF( c3 == 'TRD' ) THEN
    nb = 1
  ELSE IF( c3 == 'GST' ) THEN
    nb = 64
  END IF
ELSE IF( sname .AND. c2 == 'OR' ) THEN
  IF( c3( 1:1 ) == 'G' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nb = 32
    END IF
  ELSE IF( c3( 1:1 ) == 'M' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nb = 32
    END IF
  END IF
ELSE IF( cname .AND. c2 == 'UN' ) THEN
  IF( c3( 1:1 ) == 'G' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nb = 32
    END IF
  ELSE IF( c3( 1:1 ) == 'M' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nb = 32
    END IF
  END IF
ELSE IF( c2 == 'GB' ) THEN
  IF( c3 == 'TRF' ) THEN
    IF( sname ) THEN
      IF( n4 <= 64 ) THEN
        nb = 1
      ELSE
        nb = 32
      END IF
    ELSE
      IF( n4 <= 64 ) THEN
        nb = 1
      ELSE
        nb = 32
      END IF
    END IF
  END IF
ELSE IF( c2 == 'PB' ) THEN
  IF( c3 == 'TRF' ) THEN
    IF( sname ) THEN
      IF( n2 <= 64 ) THEN
        nb = 1
      ELSE
        nb = 32
      END IF
    ELSE
      IF( n2 <= 64 ) THEN
        nb = 1
      ELSE
        nb = 32
      END IF
    END IF
  END IF
ELSE IF( c2 == 'TR' ) THEN
  IF( c3 == 'TRI' ) THEN
    IF( sname ) THEN
      nb = 64
    ELSE
      nb = 64
    END IF
  END IF
ELSE IF( c2 == 'LA' ) THEN
  IF( c3 == 'UUM' ) THEN
    IF( sname ) THEN
      nb = 64
    ELSE
      nb = 64
    END IF
  END IF
ELSE IF( sname .AND. c2 == 'ST' ) THEN
  IF( c3 == 'EBZ' ) THEN
    nb = 1
  END IF
END IF
fn_val = nb
RETURN

!     ISPEC = 2:  minimum block size

200 nbmin = 2
IF( c2 == 'GE' ) THEN
  IF( c3 == 'QRF' .OR. c3 == 'RQF' .OR. c3 == 'LQF' .OR. c3 == 'QLF' ) THEN
    IF( sname ) THEN
      nbmin = 2
    ELSE
      nbmin = 2
    END IF
  ELSE IF( c3 == 'HRD' ) THEN
    IF( sname ) THEN
      nbmin = 2
    ELSE
      nbmin = 2
    END IF
  ELSE IF( c3 == 'BRD' ) THEN
    IF( sname ) THEN
      nbmin = 2
    ELSE
      nbmin = 2
    END IF
  ELSE IF( c3 == 'TRI' ) THEN
    IF( sname ) THEN
      nbmin = 2
    ELSE
      nbmin = 2
    END IF
  END IF
ELSE IF( c2 == 'SY' ) THEN
  IF( c3 == 'TRF' ) THEN
    IF( sname ) THEN
      nbmin = 8
    ELSE
      nbmin = 8
    END IF
  ELSE IF( sname .AND. c3 == 'TRD' ) THEN
    nbmin = 2
  END IF
ELSE IF( cname .AND. c2 == 'HE' ) THEN
  IF( c3 == 'TRD' ) THEN
    nbmin = 2
  END IF
ELSE IF( sname .AND. c2 == 'OR' ) THEN
  IF( c3( 1:1 ) == 'G' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nbmin = 2
    END IF
  ELSE IF( c3( 1:1 ) == 'M' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nbmin = 2
    END IF
  END IF
ELSE IF( cname .AND. c2 == 'UN' ) THEN
  IF( c3( 1:1 ) == 'G' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nbmin = 2
    END IF
  ELSE IF( c3( 1:1 ) == 'M' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nbmin = 2
    END IF
  END IF
END IF
fn_val = nbmin
RETURN

!     ISPEC = 3:  crossover point

300 nx = 0
IF( c2 == 'GE' ) THEN
  IF( c3 == 'QRF' .OR. c3 == 'RQF' .OR. c3 == 'LQF' .OR. c3 == 'QLF' ) THEN
    IF( sname ) THEN
      nx = 128
    ELSE
      nx = 128
    END IF
  ELSE IF( c3 == 'HRD' ) THEN
    IF( sname ) THEN
      nx = 128
    ELSE
      nx = 128
    END IF
  ELSE IF( c3 == 'BRD' ) THEN
    IF( sname ) THEN
      nx = 128
    ELSE
      nx = 128
    END IF
  END IF
ELSE IF( c2 == 'SY' ) THEN
  IF( sname .AND. c3 == 'TRD' ) THEN
    nx = 1
  END IF
ELSE IF( cname .AND. c2 == 'HE' ) THEN
  IF( c3 == 'TRD' ) THEN
    nx = 1
  END IF
ELSE IF( sname .AND. c2 == 'OR' ) THEN
  IF( c3( 1:1 ) == 'G' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nx = 128
    END IF
  END IF
ELSE IF( cname .AND. c2 == 'UN' ) THEN
  IF( c3( 1:1 ) == 'G' ) THEN
    IF( c4 == 'QR' .OR. c4 == 'RQ' .OR. c4 == 'LQ' .OR.  &
      c4 == 'QL' .OR. c4 == 'HR' .OR. c4 == 'TR' .OR.c4 == 'BR' ) THEN
      nx = 128
    END IF
  END IF
END IF
fn_val = nx
RETURN

!     ISPEC = 4:  number of shifts (used by xHSEQR)

400 fn_val = 6
RETURN

!     ISPEC = 5:  minimum column dimension (not used)

500 fn_val = 2

IF (opts == '$#&?') fn_val = 0         ! These two lines are here to make
IF (n3 > 999) fn_val = n3              ! sure that opts and n3 are referenced.
RETURN

!     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)

600 fn_val = INT( REAL( MIN( n1, n2 ) )*1.6E0 )
RETURN

!     ISPEC = 7:  number of processors (not used)

700 fn_val = 1
RETURN

!     ISPEC = 8:  crossover point for multishift (used by xHSEQR)

800 fn_val = 50
RETURN

!     End of ILAENV

END FUNCTION ilaenv


FUNCTION dlamch( cmach ) RESULT(rmach)

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller    02-Sep-1997
! The version of DLAMCH distributed with LAPACK90 causes Lahey's LF90
! compiler (version 3.5) to hang.   This drastically cut down version
! does not, and gives the same results as ELF90 on the original.

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: cmach
REAL (wp)                     :: rmach
!     ..

!  Purpose
!  =======

!  DLAMCH determines double precision machine parameters.

!  Arguments
!  =========

!  CMACH   (input) CHARACTER*1
!          Specifies the value to be returned by DLAMCH:
!          = 'E' or 'e',   DLAMCH := eps
!          = 'S' or 's ,   DLAMCH := sfmin
!          = 'B' or 'b',   DLAMCH := base
!          = 'P' or 'p',   DLAMCH := eps*base
!          = 'N' or 'n',   DLAMCH := t
!          = 'R' or 'r',   DLAMCH := rnd
!          = 'M' or 'm',   DLAMCH := emin
!          = 'U' or 'u',   DLAMCH := rmin
!          = 'L' or 'l',   DLAMCH := emax
!          = 'O' or 'o',   DLAMCH := rmax

!          where

!          eps   = relative machine precision
!          sfmin = safe minimum, such that 1/sfmin does not overflow
!          base  = base of the machine
!          prec  = eps*base
!          t     = number of (base) digits in the mantissa
!          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
!          emin  = minimum exponent before (gradual) underflow
!          rmin  = underflow threshold - base**(emin-1)
!          emax  = largest exponent before overflow
!          rmax  = overflow threshold  - (base**emax)*(1-eps)

!     .. Local Scalars ..
LOGICAL, SAVE   :: first = .TRUE.
REAL (wp), SAVE :: base, emax, emin, eps, prec, rmax, rmin, rnd, sfmin, t
REAL (wp)       :: small
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlamc2
!     ..
!     .. Save statement ..
! SAVE   first, eps, sfmin, base, t, rnd, emin, rmin, emax, rmax, prec
!     ..
!     .. Data statements ..
! DATA               first / .true. /
!     ..
!     .. Executable Statements ..

IF( first ) THEN
  first = .false.
!  CALL dlamc2( beta, it, lrnd, eps, imin, rmin, imax, rmax )
  base = RADIX(1.0_wp)
  t = DIGITS(1.0_wp)
  rnd = 1.0_wp
  eps = EPSILON(1.0_wp)
  prec = eps*base
  emin = MINEXPONENT(1.0_wp)
  emax = MAXEXPONENT(1.0_wp)
  rmin = TINY(1.0_wp)
  rmax = HUGE(1.0_wp)
  sfmin = rmin
  small =1.0_wp / rmax
  IF( small >= sfmin ) THEN

!           Use SMALL plus a bit, to avoid the possibility of rounding
!           causing overflow when computing  1/sfmin.

    sfmin = small*(1.0_wp + eps )
  END IF
END IF

IF( lsame( cmach, 'E' ) ) THEN
  rmach = eps
ELSE IF( lsame( cmach, 'S' ) ) THEN
  rmach = sfmin
ELSE IF( lsame( cmach, 'B' ) ) THEN
  rmach = base
ELSE IF( lsame( cmach, 'P' ) ) THEN
  rmach = prec
ELSE IF( lsame( cmach, 'N' ) ) THEN
  rmach = t
ELSE IF( lsame( cmach, 'R' ) ) THEN
  rmach = rnd
ELSE IF( lsame( cmach, 'M' ) ) THEN
  rmach = emin
ELSE IF( lsame( cmach, 'U' ) ) THEN
  rmach = rmin
ELSE IF( lsame( cmach, 'L' ) ) THEN
  rmach = emax
ELSE IF( lsame( cmach, 'O' ) ) THEN
  rmach = rmax
END IF

RETURN

!     End of DLAMCH

END FUNCTION dlamch



SUBROUTINE dlasrt( id, n, d, info )

!  -- LAPACK routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994

! ELF90 translation by Alan Miller    11-Sep-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: id
INTEGER, INTENT(IN)           :: n
INTEGER, INTENT(OUT)          :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT)     :: d(:)
!     ..

!  Purpose
!  =======

!  Sort the numbers in D in increasing order (if ID = 'I') or
!  in decreasing order (if ID = 'D' ).

!  Use Quick Sort, reverting to Insertion sort on arrays of
!  size <= 20. Dimension of STACK limits N to about 2**32.

!  Arguments
!  =========

!  ID      (input) CHARACTER*1
!          = 'I': sort D in increasing order;
!          = 'D': sort D in decreasing order.

!  N       (input) INTEGER
!          The length of the array D.

!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the array to be sorted.
!          On exit, D has been sorted into increasing order
!          (D(1) <= ... <= D(N) ) or into decreasing order
!          (D(1) >= ... >= D(N) ), depending on ID.

!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value

!  =====================================================================

!     .. Parameters ..
INTEGER, PARAMETER :: select = 20
!     ..
!     .. Local Scalars ..
INTEGER   :: dir, endd, i, j, start, stkpnt
REAL (wp) :: d1, d2, d3, dmnmx, tmp
!     ..
!     .. Local Arrays ..
INTEGER   :: stack( 2, 32 )
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. External Subroutines ..
! EXTERNAL           xerbla
!     ..
!     .. Executable Statements ..

!     Test the input paramters.

info = 0
dir = -1
IF( lsame( id, 'D' ) ) THEN
  dir = 0
ELSE IF( lsame( id, 'I' ) ) THEN
  dir = 1
END IF
IF( dir == -1 ) THEN
  info = -1
ELSE IF( n < 0 ) THEN
  info = -2
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DLASRT')
  RETURN
END IF

!     Quick return if possible

IF( n <= 1 ) RETURN

stkpnt = 1
stack( 1, 1 ) = 1
stack( 2, 1 ) = n
10 start = stack( 1, stkpnt )
endd = stack( 2, stkpnt )
stkpnt = stkpnt - 1
IF( endd-start <= select .AND. endd-start > 0 ) THEN

!        Do Insertion sort on D( START:ENDD )

  IF( dir == 0 ) THEN

!           Sort into decreasing order

    DO i = start + 1, endd
      DO j = i, start + 1, -1
        IF( d( j ) > d( j-1 ) ) THEN
          dmnmx = d( j )
          d( j ) = d( j-1 )
          d( j-1 ) = dmnmx
        ELSE
          CYCLE
        END IF
      END DO
    END DO

  ELSE

!           Sort into increasing order

    DO i = start + 1, endd
      DO j = i, start + 1, -1
        IF( d( j ) < d( j-1 ) ) THEN
          dmnmx = d( j )
          d( j ) = d( j-1 )
          d( j-1 ) = dmnmx
        ELSE
          CYCLE
        END IF
      END DO
    END DO

  END IF

ELSE IF( endd-start > select ) THEN

!        Partition D( START:ENDD ) and stack parts, largest one first

!        Choose partition entry as median of 3

  d1 = d( start )
  d2 = d( endd )
  i = ( start + endd ) / 2
  d3 = d( i )
  IF( d1 < d2 ) THEN
    IF( d3 < d1 ) THEN
      dmnmx = d1
    ELSE IF( d3 < d2 ) THEN
      dmnmx = d3
    ELSE
      dmnmx = d2
    END IF
  ELSE
    IF( d3 < d2 ) THEN
      dmnmx = d2
    ELSE IF( d3 < d1 ) THEN
      dmnmx = d3
    ELSE
      dmnmx = d1
    END IF
  END IF

  IF( dir == 0 ) THEN

!           Sort into decreasing order

    i = start - 1
    j = endd + 1

    60 j = j - 1
    IF( d( j ) < dmnmx ) GO TO 60

    80 i = i + 1
    IF( d( i ) > dmnmx ) GO TO 80
    IF( i < j ) THEN
      tmp = d( i )
      d( i ) = d( j )
      d( j ) = tmp
      GO TO 60
    END IF
    IF( j-start > endd-j-1 ) THEN
      stkpnt = stkpnt + 1
      stack( 1, stkpnt ) = start
      stack( 2, stkpnt ) = j
      stkpnt = stkpnt + 1
      stack( 1, stkpnt ) = j + 1
      stack( 2, stkpnt ) = endd
    ELSE
      stkpnt = stkpnt + 1
      stack( 1, stkpnt ) = j + 1
      stack( 2, stkpnt ) = endd
      stkpnt = stkpnt + 1
      stack( 1, stkpnt ) = start
      stack( 2, stkpnt ) = j
    END IF
  ELSE

!           Sort into increasing order

    i = start - 1
    j = endd + 1

    90 j = j - 1
    IF( d( j ) > dmnmx ) GO TO 90

    110 i = i + 1
    IF( d( i ) < dmnmx ) GO TO 110
    IF( i < j ) THEN
      tmp = d( i )
      d( i ) = d( j )
      d( j ) = tmp
      GO TO 90
    END IF
    IF( j-start > endd-j-1 ) THEN
      stkpnt = stkpnt + 1
      stack( 1, stkpnt ) = start
      stack( 2, stkpnt ) = j
      stkpnt = stkpnt + 1
      stack( 1, stkpnt ) = j + 1
      stack( 2, stkpnt ) = endd
    ELSE
      stkpnt = stkpnt + 1
      stack( 1, stkpnt ) = j + 1
      stack( 2, stkpnt ) = endd
      stkpnt = stkpnt + 1
      stack( 1, stkpnt ) = start
      stack( 2, stkpnt ) = j
    END IF
  END IF
END IF
IF( stkpnt > 0 ) GO TO 10

RETURN

!     End of DLASRT

END SUBROUTINE dlasrt


END MODULE LA_AUXMOD

MODULE dblas
! Contains only the INTEGER, REAL (wp) & COMPLEX (wp)
! parts of BLAS level 1.
! For single precision, change wp to point to sp instead of dp
! This very much simplified BLAS module is by Alan Miller
! alan @ vic.cmis.csiro.au    URL: www.ozemail.com.au/~milleraj

! Latest revision - 25 August 1998

USE la_precision, ONLY: wp => dp             ! From file l_auxmod.f90
IMPLICIT NONE

REAL (wp), PARAMETER :: zero = 0._wp, one = 1._wp

CONTAINS

! ============= dscal.f ==============
SUBROUTINE dscal(n, a, x, incx)

!     scales a vector by a constant.

INTEGER, INTENT(IN)       :: n, incx
REAL (wp), INTENT(IN)     :: a
REAL (wp), INTENT(IN OUT) :: x(:)

IF( n <= 0 .OR. incx <= 0 ) RETURN
IF(incx == 1) THEN
  x(:n) = a * x(:n)
  RETURN
END IF
x(:n*incx:incx) = a * x(:n*incx:incx)

RETURN
END SUBROUTINE dscal

! ============= ddot.f ==============
FUNCTION ddot(n, x, incx, y, incy) RESULT(fn_val)

!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.

INTEGER, INTENT(IN)   :: n, incx, incy
REAL (wp), INTENT(IN) :: x(:), y(:)
REAL (wp)             :: fn_val

fn_val = zero
IF(n <= 0) RETURN
IF(incx == 1 .AND. incy == 1) THEN
  fn_val = DOT_PRODUCT( x(:n), y(:n) )
  RETURN
END IF
fn_val = DOT_PRODUCT( x(:n*incx:incx), y(:n*incy:incy) )

RETURN
END FUNCTION ddot
! ============= daxpy.f ==============
SUBROUTINE daxpy(n, a, x, incx, y, incy)

!     constant times a vector plus a vector.
!     new y() = old y() + a.x()

INTEGER, INTENT(IN)       :: n, incx, incy
REAL (wp), INTENT(IN)     :: x(:), a
REAL (wp), INTENT(IN OUT) :: y(:)

IF (n <= 0) RETURN
IF (a == zero) RETURN
IF (incx == 1 .AND. incy == 1) THEN
  y(:n) = y(:n) + a * x(:n)
  RETURN
END IF
y(:n*incy:incy) = y(:n*incy:incy) + a * x(:n*incx:incx)

RETURN
END SUBROUTINE daxpy

! ============= idamax.f ==============
FUNCTION idamax(n, x, incx) RESULT(fn_val)

!     finds the index of element having max. absolute value.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)

INTEGER, INTENT(IN)   :: n, incx
REAL (wp), INTENT(IN) :: x(:)
INTEGER               :: fn_val

INTEGER :: imax(1)

fn_val = 0
IF( n < 1 .OR. incx <= 0 ) RETURN
fn_val = 1
IF(n == 1) RETURN
IF(incx == 1) THEN
  imax = MAXLOC( ABS(x(:n)) )
ELSE
  imax = MAXLOC( ABS(x(:n*incx:incx)) )
END IF
fn_val = imax(1)

RETURN
END FUNCTION idamax
! ============= dswap.f ==============
SUBROUTINE dswap (n, x, incx, y, incy)

!     interchanges two vectors.

INTEGER, INTENT(IN)       :: n, incx, incy
REAL (wp), INTENT(IN OUT) :: x(:), y(:)

! Local variables
REAL (wp) :: temp(n)

IF(n <= 0) RETURN
IF(incx == 1 .AND. incy == 1) THEN
  temp = x(:n)
  x(:n) = y(:n)
  y(:n) = temp
  RETURN
END IF

temp = x(:n*incx:incx)
x(:n*incx:incx) = y(:n*incy:incy)
y(:n*incy:incy) = temp

RETURN
END SUBROUTINE dswap

! ============= dnrm2.f ==============
FUNCTION dnrm2 ( n, x, incx) RESULT( norm )
!     .. Scalar Arguments ..

INTEGER, INTENT(IN)   :: n
REAL (wp), INTENT(IN) :: x(:)
INTEGER, INTENT(IN)   :: incx
REAL (wp)             :: norm

!  DNRM2 returns the Euclidean norm of a vector via the function
!  name, so that

!     DNRM2 := sqrt( x'*x )


!  -- This version written on 25-October-1982.
!     Modified on 14-October-1993 to inline the call to DLASSQ.
!     Sven Hammarling, Nag Ltd.

!     .. Local Scalars ..
INTEGER   :: ix
REAL (wp) :: absxi, scale, ssq
!     ..
!     .. Executable Statements ..
IF( n < 1 .OR. incx < 1 ) THEN
  norm  = zero
ELSE IF( n == 1 ) THEN
  norm  = ABS( x( 1 ) )
ELSE
  scale = zero
  ssq   = one
!        The following loop is equivalent to this call to the LAPACK
!        auxiliary routine:
!        CALL DLASSQ( N, X, INCX, SCALE, SSQ )

  DO  ix = 1, 1 + ( n - 1 )*incx, incx
    IF( x( ix ) /= zero ) THEN
      absxi = ABS( x( ix ) )
      IF( scale < absxi ) THEN
        ssq   = one   + ssq*( scale/absxi )**2
        scale = absxi
      ELSE
        ssq   = ssq   +     ( absxi/scale )**2
      END IF
    END IF
  END DO
  norm  = scale * SQRT( ssq )
END IF

RETURN

!     End of DNRM2.

END FUNCTION dnrm2



END MODULE dblas

MODULE dblas2
! For single precision, change wp to point to sp instead of dp
! This very much simplified BLAS2 module is by Alan Miller
! alan @ vic.cmis.csiro.au    URL: www.ozemail.com.au/~milleraj

! Latest revision - 23 October 1997

USE la_precision, ONLY: wp => dp             ! From file l_auxmod.f90
USE la_auxmod
USE dblas
IMPLICIT NONE

CONTAINS


SUBROUTINE dger  ( m, n, alpha, x, incx, y, incy, a, lda )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
REAL (wp), INTENT(IN)     :: alpha
INTEGER, INTENT(IN)       :: incx, incy, lda, m, n
!     .. Array Arguments ..
REAL (wp), INTENT(IN)     :: x(:), y(:)
REAL (wp), INTENT(IN OUT) :: a(:,:)
!     ..

!  Purpose
!  =======

!  DGER   performs the rank 1 operation

!     A := alpha*x*y' + A,

!  where alpha is a scalar, x is an m element vector, y is an n element
!  vector and A is an m by n matrix.

!  Parameters
!  ==========

!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.

!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.

!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.

!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( m - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the m
!           element vector x.
!           Unchanged on exit.

!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.

!  Y      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.

!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients. On exit, A is
!           overwritten by the updated matrix.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.


!  Level 2 Blas routine.

!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.


!     .. Local Scalars ..
REAL (wp) :: temp
INTEGER   ::  i, info, ix, j, jy, kx
!     .. External Subroutines ..
! EXTERNAL           erinfo
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
IF     ( m < 0 ) THEN
  info = 1
ELSE IF( n < 0 ) THEN
  info = 2
ELSE IF( incx == 0 ) THEN
  info = 5
ELSE IF( incy == 0 ) THEN
  info = 7
ELSE IF( lda < MAX( 1, m ) ) THEN
  info = 9
END IF
IF( info /= 0 ) THEN
  CALL erinfo(info,  'DGER  ')
  RETURN
END IF

!     Quick return if possible.

IF( ( m == 0 ).OR.( n == 0 ).OR.( alpha == zero ) ) RETURN

!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.

IF( incy > 0 ) THEN
  jy = 1
ELSE
  jy = 1 - ( n - 1 )*incy
END IF
IF( incx == 1 ) THEN
  DO j = 1, n
    IF( y( jy ) /= zero ) THEN
      temp = alpha*y( jy )
      a( 1:m, j ) = a( 1:m, j ) + x( 1:m )*temp
    END IF
    jy = jy + incy
  END DO
ELSE
  IF( incx > 0 ) THEN
    kx = 1
  ELSE
    kx = 1 - ( m - 1 )*incx
  END IF
  DO j = 1, n
    IF( y( jy ) /= zero ) THEN
      temp = alpha*y( jy )
      ix   = kx
      DO i = 1, m
        a( i, j ) = a( i, j ) + x( ix )*temp
        ix        = ix        + incx
      END DO
    END IF
    jy = jy + incy
  END DO
END IF

RETURN

!     End of DGER  .

END SUBROUTINE dger


SUBROUTINE dsymv ( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
REAL (wp), INTENT(IN)         :: alpha, beta
INTEGER, INTENT(IN)           :: incx, incy, lda, n
CHARACTER (LEN=1), INTENT(IN) :: uplo
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: a(:,:), x(:)
REAL (wp), INTENT(IN OUT)     :: y(:)
!     ..

!  Purpose
!  =======

!  DSYMV  performs the matrix-vector  operation

!     y := alpha*A*x + beta*y,

!  where alpha and beta are scalars, x and y are n element vectors and
!  A is an n by n symmetric matrix.

!  Parameters
!  ==========

!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the upper or lower
!           triangular part of the array A is to be referenced as
!           follows:

!              UPLO = 'U' or 'u'   Only the upper triangular part of A
!                                  is to be referenced.

!              UPLO = 'L' or 'l'   Only the lower triangular part of A
!                                  is to be referenced.

!           Unchanged on exit.

!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.

!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular part of the symmetric matrix and the strictly
!           lower triangular part of A is not referenced.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular part of the symmetric matrix and the strictly
!           upper triangular part of A is not referenced.
!           Unchanged on exit.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.

!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x.
!           Unchanged on exit.

!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.

!  BETA   - DOUBLE PRECISION.
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.

!  Y      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y. On exit, Y is overwritten by the updated
!           vector y.

!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.


!  Level 2 Blas routine.

!  -- Written on 20-July-1986.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.

!     .. Local Scalars ..
REAL (wp) :: temp1, temp2
INTEGER   ::  i, info, ix, iy, j, jx, jy, kx, ky
!     .. External Functions ..
! LOGICAL ::  lsame
! EXTERNAL           lsame
!     .. External Subroutines ..
! EXTERNAL           erinfo
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
IF      ( .NOT.lsame( uplo, 'U' ).AND. .NOT.lsame( uplo, 'L' )      ) THEN
  info = 1
ELSE IF ( n < 0 ) THEN
  info = 2
ELSE IF ( lda < MAX(1,n) ) THEN
  info = 5
ELSE IF ( incx == 0 ) THEN
  info = 7
ELSE IF ( incy == 0 ) THEN
  info = 10
END IF
IF( info /= 0 ) THEN
  CALL erinfo(info, 'DSYMV ')
  RETURN
END IF

!     Quick return if possible.

IF( ( n == 0 ).OR.( ( alpha == zero ).AND.( beta == one ) ) ) RETURN

!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through the triangular part
!     of A.

!     First form  y := beta*y  and set up the start points in X and Y if
!     the increments are not both unity.

IF( ( incx == 1 ).AND.( incy == 1 ) ) THEN
  IF( beta /= one ) THEN
    IF( beta == zero ) THEN
      DO i = 1, n
        y( i ) = zero
      END DO
    ELSE
      DO i = 1, n
        y( i ) = beta*y( i )
      END DO
    END IF
  END IF
ELSE
  IF( incx > 0 ) THEN
    kx = 1
  ELSE
    kx = 1 - ( n - 1 )*incx
  END IF
  IF( incy > 0 ) THEN
    ky = 1
  ELSE
    ky = 1 - ( n - 1 )*incy
  END IF
  IF( beta /= one ) THEN
    iy = ky
    IF( beta == zero ) THEN
      DO i = 1, n
        y( iy ) = zero
        iy      = iy   + incy
      END DO
    ELSE
      DO i = 1, n
        y( iy ) = beta*y( iy )
        iy      = iy           + incy
      END DO
    END IF
  END IF
END IF
IF( alpha == zero ) RETURN
IF( lsame( uplo, 'U' ) ) THEN

!        Form  y  when A is stored in upper triangle.

  IF( ( incx == 1 ).AND.( incy == 1 ) ) THEN
    DO j = 1, n
      temp1 = alpha*x( j )
      temp2 = zero
      DO i = 1, j - 1
        y( i ) = y( i ) + temp1*a( i, j )
        temp2  = temp2  + a( i, j )*x( i )
      END DO
      y( j ) = y( j ) + temp1*a( j, j ) + alpha*temp2
    END DO
  ELSE
    ix = kx - incx
    DO j = 1, n
      temp1 = alpha*x( ix + incx )
      temp2 = zero
      ix    = kx
      iy    = ky
      DO i = 1, j - 1
        y( iy ) = y( iy ) + temp1*a( i, j )
        temp2   = temp2   + a( i, j )*x( ix )
        ix      = ix      + incx
        iy      = iy      + incy
      END DO
      y( iy ) = y( iy ) + temp1*a( j, j ) + alpha*temp2
    END DO
  END IF
ELSE

!        Form  y  when A is stored in lower triangle.

  IF( ( incx == 1 ).AND.( incy == 1 ) ) THEN
    DO j = 1, n
      temp1  = alpha*x( j )
      temp2  = zero
      y( j ) = y( j )       + temp1*a( j, j )
      DO i = j + 1, n
        y( i ) = y( i ) + temp1*a( i, j )
        temp2  = temp2  + a( i, j )*x( i )
      END DO
      y( j ) = y( j ) + alpha*temp2
    END DO
  ELSE
    jx = kx
    jy = ky
    DO j = 1, n
      temp1   = alpha*x( jx )
      temp2   = zero
      y( jy ) = y( jy )       + temp1*a( j, j )
      ix      = jx
      iy      = jy
      DO i = j + 1, n
        ix      = ix      + incx
        iy      = iy      + incy
        y( iy ) = y( iy ) + temp1*a( i, j )
        temp2   = temp2   + a( i, j )*x( ix )
      END DO
      y( jy ) = y( jy ) + alpha*temp2
      jx      = jx      + incx
      jy      = jy      + incy
    END DO
  END IF
END IF

RETURN

!     End of DSYMV .

END SUBROUTINE dsymv


SUBROUTINE dsyr2 ( uplo, n, alpha, x, incx, y, incy, a, lda )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
REAL (wp), INTENT(IN)         :: alpha
INTEGER, INTENT(IN)           :: incx, incy, lda, n
CHARACTER (LEN=1), INTENT(IN) :: uplo
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: x(:), y(:)
REAL (wp), INTENT(IN OUT)     :: a(:,:)
!     ..

!  Purpose
!  =======

!  DSYR2  performs the symmetric rank 2 operation

!     A := alpha*x*y' + alpha*y*x' + A,

!  where alpha is a scalar, x and y are n element vectors and A is an n
!  by n symmetric matrix.

!  Parameters
!  ==========

!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the upper or lower
!           triangular part of the array A is to be referenced as
!           follows:

!              UPLO = 'U' or 'u'   Only the upper triangular part of A
!                                  is to be referenced.

!              UPLO = 'L' or 'l'   Only the lower triangular part of A
!                                  is to be referenced.

!           Unchanged on exit.

!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.

!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.

!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x.
!           Unchanged on exit.

!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.

!  Y      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.

!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular part of the symmetric matrix and the strictly
!           lower triangular part of A is not referenced. On exit, the
!           upper triangular part of the array A is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular part of the symmetric matrix and the strictly
!           upper triangular part of A is not referenced. On exit, the
!           lower triangular part of the array A is overwritten by the
!           lower triangular part of the updated matrix.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.


!  Level 2 Blas routine.

!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.


!     .. Local Scalars ..
REAL (wp) :: temp1, temp2
INTEGER ::  i, info, ix, iy, j, jx, jy, kx, ky
!     .. External Functions ..
! LOGICAL ::  lsame
! EXTERNAL           lsame
!     .. External Subroutines ..
! EXTERNAL           erinfo
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
IF     ( .NOT.lsame( uplo, 'U' ).AND. .NOT.lsame( uplo, 'L' )      ) THEN
  info = 1
ELSE IF( n < 0 ) THEN
  info = 2
ELSE IF( incx == 0 ) THEN
  info = 5
ELSE IF( incy == 0 ) THEN
  info = 7
ELSE IF( lda < MAX( 1, n ) ) THEN
  info = 9
END IF
IF( info /= 0 ) THEN
  CALL erinfo(info, 'DSYR2 ')
  RETURN
END IF

!     Quick return if possible.

IF( ( n == 0 ).OR.( alpha == zero ) ) RETURN

!     Set up the start points in X and Y if the increments are not both
!     unity.

IF( ( incx /= 1 ).OR.( incy /= 1 ) ) THEN
  IF( incx > 0 ) THEN
    kx = 1
  ELSE
    kx = 1 - ( n - 1 )*incx
  END IF
  IF( incy > 0 ) THEN
    ky = 1
  ELSE
    ky = 1 - ( n - 1 )*incy
  END IF
  jx = kx
  jy = ky
END IF

!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through the triangular part
!     of A.

IF( lsame( uplo, 'U' ) ) THEN

!        Form  A  when A is stored in the upper triangle.

  IF( ( incx == 1 ).AND.( incy == 1 ) ) THEN
    DO j = 1, n
      IF( ( x( j ) /= zero ).OR.( y( j ) /= zero ) ) THEN
        temp1 = alpha*y( j )
        temp2 = alpha*x( j )
        a( 1:j, j ) = a( 1:j, j ) + x( 1:j )*temp1 + y( 1:j )*temp2
      END IF
    END DO
  ELSE
    DO j = 1, n
      IF( ( x( jx ) /= zero ).OR.( y( jy ) /= zero ) ) THEN
        temp1 = alpha*y( jy )
        temp2 = alpha*x( jx )
        ix    = kx
        iy    = ky
        DO i = 1, j
          a( i, j ) = a( i, j ) + x( ix )*temp1+ y( iy )*temp2
          ix        = ix        + incx
          iy        = iy        + incy
        END DO
      END IF
      jx = jx + incx
      jy = jy + incy
    END DO
  END IF
ELSE

!        Form  A  when A is stored in the lower triangle.

  IF( ( incx == 1 ).AND.( incy == 1 ) ) THEN
    DO j = 1, n
      IF( ( x( j ) /= zero ).OR.( y( j ) /= zero ) ) THEN
        temp1 = alpha*y( j )
        temp2 = alpha*x( j )
        a( j:n, j ) = a( j:n, j ) + x( j:n )*temp1 + y( j:n )*temp2
      END IF
    END DO
  ELSE
    DO j = 1, n
      IF( ( x( jx ) /= zero ).OR.( y( jy ) /= zero ) ) THEN
        temp1 = alpha*y( jy )
        temp2 = alpha*x( jx )
        ix    = jx
        iy    = jy
        DO i = j, n
          a( i, j ) = a( i, j ) + x( ix )*temp1+ y( iy )*temp2
          ix        = ix        + incx
          iy        = iy        + incy
        END DO
      END IF
      jx = jx + incx
      jy = jy + incy
    END DO
  END IF
END IF

RETURN

!     End of DSYR2 .

END SUBROUTINE dsyr2


SUBROUTINE dgemv ( trans, m, n, alpha, a, lda, x, incx, beta, y, incy )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
REAL (wp), INTENT(IN)         :: alpha, beta
INTEGER, INTENT(IN)           :: incx, incy, lda, m, n
CHARACTER (LEN=1), INTENT(IN) :: trans
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: a(:,:), x(:)
REAL (wp), INTENT(IN OUT)     :: y(:)
!     ..

!  Purpose
!  =======

!  DGEMV  performs one of the matrix-vector operations

!     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,

!  where alpha and beta are scalars, x and y are vectors and A is an
!  m by n matrix.

!  Parameters
!  ==========

!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:

!              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.

!              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.

!              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.

!           Unchanged on exit.

!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.

!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.

!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients.
!           Unchanged on exit.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least max( 1, m ).
!           Unchanged on exit.

!  X      - DOUBLE PRECISION array of DIMENSION at least
!           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
!           Before entry, the incremented array X must contain the
!           vector x.
!           Unchanged on exit.

!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.

!  BETA   - DOUBLE PRECISION.
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.

!  Y      - DOUBLE PRECISION array of DIMENSION at least
!           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
!           Before entry with BETA non-zero, the incremented array Y
!           must contain the vector y. On exit, Y is overwritten by the
!           updated vector y.

!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.


!  Level 2 Blas routine.

!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.


!     .. Local Scalars ..
REAL (wp) :: temp
INTEGER   ::  i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
!     .. External Functions ..
! LOGICAL ::  lsame
! EXTERNAL           lsame
!     .. External Subroutines ..
! EXTERNAL           erinfo
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
IF     ( .NOT.lsame( trans, 'N' ).AND. .NOT.lsame( trans, 'T' ).AND.  &
  .NOT.lsame( trans, 'C' )      ) THEN
  info = 1
ELSE IF( m < 0 ) THEN
  info = 2
ELSE IF( n < 0 ) THEN
  info = 3
ELSE IF( lda < MAX( 1, m ) ) THEN
  info = 6
ELSE IF( incx == 0 ) THEN
  info = 8
ELSE IF( incy == 0 ) THEN
  info = 11
END IF
IF( info /= 0 ) THEN
  CALL erinfo(info,  'DGEMV ')
  RETURN
END IF

!     Quick return if possible.

IF( ( m == 0 ).OR.( n == 0 ).OR.  &
( ( alpha == zero ).AND.( beta == one ) ) )RETURN

!     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!     up the start points in  X  and  Y.

IF( lsame( trans, 'N' ) ) THEN
  lenx = n
  leny = m
ELSE
  lenx = m
  leny = n
END IF
IF( incx > 0 ) THEN
  kx = 1
ELSE
  kx = 1 - ( lenx - 1 )*incx
END IF
IF( incy > 0 ) THEN
  ky = 1
ELSE
  ky = 1 - ( leny - 1 )*incy
END IF

!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.

!     First form  y := beta*y.

IF( beta /= one ) THEN
  IF( incy == 1 ) THEN
    IF( beta == zero ) THEN
      y( :leny ) = zero
    ELSE
      y( :leny ) = beta*y( :leny )
    END IF
  ELSE
    iy = ky
    IF( beta == zero ) THEN
      DO i = 1, leny
        y( iy ) = zero
        iy      = iy   + incy
      END DO
    ELSE
      DO i = 1, leny
        y( iy ) = beta*y( iy )
        iy      = iy           + incy
      END DO
    END IF
  END IF
END IF
IF( alpha == zero ) RETURN
IF( lsame( trans, 'N' ) ) THEN

!        Form  y := alpha*A*x + y.

  jx = kx
  IF( incy == 1 ) THEN
    DO j = 1, n
      IF( x( jx ) /= zero ) THEN
        temp = alpha*x( jx )
        y( 1:m ) = y( 1:m ) + temp*a( 1:m, j )
      END IF
      jx = jx + incx
    END DO
  ELSE
    DO j = 1, n
      IF( x( jx ) /= zero ) THEN
        temp = alpha*x( jx )
        iy   = ky
        DO i = 1, m
          y( iy ) = y( iy ) + temp*a( i, j )
          iy      = iy      + incy
        END DO
      END IF
      jx = jx + incx
    END DO
  END IF
ELSE

!        Form  y := alpha*A'*x + y.

  jy = ky
  IF( incx == 1 ) THEN
    DO j = 1, n
      temp = DOT_PRODUCT( a(1:m,j), x(1:m) )
      y( jy ) = y( jy ) + alpha*temp
      jy      = jy      + incy
    END DO
  ELSE
    DO j = 1, n
      temp = zero
      ix   = kx
      DO i = 1, m
        temp = temp + a( i, j )*x( ix )
        ix   = ix   + incx
      END DO
      y( jy ) = y( jy ) + alpha*temp
      jy      = jy      + incy
    END DO
  END IF
END IF

RETURN

!     End of DGEMV .

END SUBROUTINE dgemv


SUBROUTINE dtrmv ( uplo, trans, diag, n, a, lda, x, incx )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)           :: incx, lda, n
CHARACTER (LEN=1), INTENT(IN) :: diag, trans, uplo
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: a(:,:)
REAL (wp), INTENT(IN OUT)     :: x(:)
!     ..

!  Purpose
!  =======

!  DTRMV  performs one of the matrix-vector operations

!     x := A*x,   or   x := A'*x,

!  where x is n element vector and A is an n by n unit, or non-unit,
!  upper or lower triangular matrix.

!  Parameters
!  ==========

!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix is an upper or
!           lower triangular matrix as follows:

!              UPLO = 'U' or 'u'   A is an upper triangular matrix.

!              UPLO = 'L' or 'l'   A is a lower triangular matrix.

!           Unchanged on exit.

!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:

!              TRANS = 'N' or 'n'   x := A*x.

!              TRANS = 'T' or 't'   x := A'*x.

!              TRANS = 'C' or 'c'   x := A'*x.

!           Unchanged on exit.

!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit
!           triangular as follows:

!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.

!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.

!           Unchanged on exit.

!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular matrix and the strictly lower triangular part of
!           A is not referenced.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular matrix and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of
!           A are not referenced either, but are assumed to be unity.
!           Unchanged on exit.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.

!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x. On exit, X is overwritten with the
!           tranformed vector x.

!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.


!  Level 2 Blas routine.

!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.


!     .. Local Scalars ..
REAL (wp) :: temp
INTEGER ::  i, info, ix, j, jx, kx
LOGICAL ::  nounit
!     .. External Functions ..
! LOGICAL ::  lsame
! EXTERNAL           lsame
!     .. External Subroutines ..
! EXTERNAL           erinfo
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
IF     ( .NOT.lsame( uplo , 'U' ).AND. .NOT.lsame( uplo , 'L' )      ) THEN
  info = 1
ELSE IF( .NOT.lsame( trans, 'N' ).AND.  &
  .NOT.lsame( trans, 'T' ).AND..NOT.lsame( trans, 'C' )      ) THEN
  info = 2
ELSE IF( .NOT.lsame( diag , 'U' ).AND.  &
  .NOT.lsame( diag , 'N' )      ) THEN
  info = 3
ELSE IF( n < 0 ) THEN
  info = 4
ELSE IF( lda < MAX( 1, n ) ) THEN
  info = 6
ELSE IF( incx == 0 ) THEN
  info = 8
END IF
IF( info /= 0 ) THEN
  CALL erinfo(info, 'DTRMV ')
  RETURN
END IF

!     Quick return if possible.

IF( n == 0 ) RETURN

nounit = lsame( diag, 'N' )

!     Set up the start point in X if the increment is not unity. This
!     will be  ( N - 1 )*INCX  too small for descending loops.

IF( incx <= 0 ) THEN
  kx = 1 - ( n - 1 )*incx
ELSE IF( incx /= 1 ) THEN
  kx = 1
END IF

!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.

IF( lsame( trans, 'N' ) ) THEN

!        Form  x := A*x.

  IF( lsame( uplo, 'U' ) ) THEN
    IF( incx == 1 ) THEN
      DO j = 1, n
        IF( x( j ) /= zero ) THEN
          temp = x( j )
          x( 1:j-1 ) = x( 1:j-1 ) + temp*a( 1:j-1, j )
          IF( nounit ) x( j ) = x( j )*a( j, j )
        END IF
      END DO
    ELSE
      jx = kx
      DO j = 1, n
        IF( x( jx ) /= zero ) THEN
          temp = x( jx )
          ix   = kx
          DO i = 1, j - 1
            x( ix ) = x( ix ) + temp*a( i, j )
            ix      = ix      + incx
          END DO
          IF( nounit ) x( jx ) = x( jx )*a( j, j )
        END IF
        jx = jx + incx
      END DO
    END IF
  ELSE
    IF( incx == 1 ) THEN
      DO j = n, 1, -1
        IF( x( j ) /= zero ) THEN
          temp = x( j )
          x( n:j+1:-1 ) = x( n:j+1:-1 ) + temp*a( n:j+1:-1, j )
          IF( nounit ) x( j ) = x( j )*a( j, j )
        END IF
      END DO
    ELSE
      kx = kx + ( n - 1 )*incx
      jx = kx
      DO j = n, 1, -1
        IF( x( jx ) /= zero ) THEN
          temp = x( jx )
          ix   = kx
          DO i = n, j + 1, -1
            x( ix ) = x( ix ) + temp*a( i, j )
            ix      = ix      - incx
          END DO
          IF( nounit ) x( jx ) = x( jx )*a( j, j )
        END IF
        jx = jx - incx
      END DO
    END IF
  END IF
ELSE

!        Form  x := A'*x.

  IF( lsame( uplo, 'U' ) ) THEN
    IF( incx == 1 ) THEN
      DO j = n, 1, -1
        temp = x( j )
        IF( nounit ) temp = temp*a( j, j )
        temp = temp + DOT_PRODUCT( a(j-1:1:-1,j), x(j-1:1:-1) )
        x( j ) = temp
      END DO
    ELSE
      jx = kx + ( n - 1 )*incx
      DO j = n, 1, -1
        temp = x( jx )
        ix   = jx
        IF( nounit ) temp = temp*a( j, j )
        DO i = j - 1, 1, -1
          ix   = ix   - incx
          temp = temp + a( i, j )*x( ix )
        END DO
        x( jx ) = temp
        jx      = jx   - incx
      END DO
    END IF
  ELSE
    IF( incx == 1 ) THEN
      DO j = 1, n
        temp = x( j )
        IF( nounit ) temp = temp*a( j, j )
        DO i = j + 1, n
          temp = temp + a( i, j )*x( i )
        END DO
        x( j ) = temp
      END DO
    ELSE
      jx = kx
      DO j = 1, n
        temp = x( jx )
        ix   = jx
        IF( nounit ) temp = temp*a( j, j )
        DO i = j + 1, n
          ix   = ix   + incx
          temp = temp + a( i, j )*x( ix )
        END DO
        x( jx ) = temp
        jx      = jx   + incx
      END DO
    END IF
  END IF
END IF

RETURN

!     End of DTRMV .

END SUBROUTINE dtrmv

SUBROUTINE dtrsv ( uplo, trans, diag, n, a, lda, x, incx )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)           :: incx, lda, n
CHARACTER (LEN=1), INTENT(IN) :: diag, trans, uplo
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: a(:,:)
REAL (wp), INTENT(IN OUT)     :: x(:)
!     ..

!  Purpose
!  =======

!  DTRSV  solves one of the systems of equations

!     A*x = b,   or   A'*x = b,

!  where b and x are n element vectors and A is an n by n unit, or
!  non-unit, upper or lower triangular matrix.

!  No test for singularity or near-singularity is included in this
!  routine. Such tests must be performed before calling this routine.

!  Parameters
!  ==========

!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix is an upper or
!           lower triangular matrix as follows:

!              UPLO = 'U' or 'u'   A is an upper triangular matrix.

!              UPLO = 'L' or 'l'   A is a lower triangular matrix.

!           Unchanged on exit.

!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the equations to be solved as
!           follows:

!              TRANS = 'N' or 'n'   A*x = b.

!              TRANS = 'T' or 't'   A'*x = b.

!              TRANS = 'C' or 'c'   A'*x = b.

!           Unchanged on exit.

!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit
!           triangular as follows:

!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.

!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.

!           Unchanged on exit.

!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular matrix and the strictly lower triangular part of
!           A is not referenced.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular matrix and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of
!           A are not referenced either, but are assumed to be unity.
!           Unchanged on exit.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.

!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element right-hand side vector b. On exit, X is overwritten
!           with the solution vector x.

!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.


!  Level 2 Blas routine.

!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.


!     .. Local Scalars ..
REAL (wp) :: temp
INTEGER   :: i, info, ix, j, jx, kx
LOGICAL   :: nounit
!     .. External Functions ..
! LOGICAL :: lsame
! EXTERNAL           lsame
!     .. External Subroutines ..
! EXTERNAL           erinfo
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
IF     ( .NOT.lsame( uplo , 'U' ).AND. .NOT.lsame( uplo , 'L' )      ) THEN
  info = 1
ELSE IF( .NOT.lsame( trans, 'N' ).AND.  &
  .NOT.lsame( trans, 'T' ).AND..NOT.lsame( trans, 'C' )      ) THEN
  info = 2
ELSE IF( .NOT.lsame( diag , 'U' ).AND.  &
  .NOT.lsame( diag , 'N' )      ) THEN
  info = 3
ELSE IF( n < 0 ) THEN
  info = 4
ELSE IF( lda < MAX( 1, n ) ) THEN
  info = 6
ELSE IF( incx == 0 ) THEN
  info = 8
END IF
IF( info /= 0 ) THEN
  CALL erinfo(info, 'DTRSV ')
  RETURN
END IF

!     Quick return if possible.

IF( n == 0 ) RETURN

nounit = lsame( diag, 'N' )

!     Set up the start point in X if the increment is not unity. This
!     will be  ( N - 1 )*INCX  too small for descending loops.

IF( incx <= 0 ) THEN
  kx = 1 - ( n - 1 )*incx
ELSE IF( incx /= 1 ) THEN
  kx = 1
END IF

!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.

IF( lsame( trans, 'N' ) ) THEN

!        Form  x := inv( A )*x.

  IF( lsame( uplo, 'U' ) ) THEN
    IF( incx == 1 ) THEN
      DO j = n, 1, -1
        IF( x( j ) /= zero ) THEN
          IF( nounit ) x( j ) = x( j )/a( j, j )
          temp = x( j )
          x( j-1:1:-1 ) = x( j-1:1:-1 ) - temp*a( j-1:1:-1, j )
        END IF
      END DO
    ELSE
      jx = kx + ( n - 1 )*incx
      DO j = n, 1, -1
        IF( x( jx ) /= zero ) THEN
          IF( nounit ) x( jx ) = x( jx )/a( j, j )
          temp = x( jx )
          ix   = jx
          DO i = j - 1, 1, -1
            ix      = ix      - incx
            x( ix ) = x( ix ) - temp*a( i, j )
          END DO
        END IF
        jx = jx - incx
      END DO
    END IF
  ELSE
    IF( incx == 1 ) THEN
      DO j = 1, n
        IF( x( j ) /= zero ) THEN
          IF( nounit ) x( j ) = x( j )/a( j, j )
          temp = x( j )
          x( j+1:n ) = x( j+1:n ) - temp*a( j+1:n, j )
        END IF
      END DO
    ELSE
      jx = kx
      DO j = 1, n
        IF( x( jx ) /= zero ) THEN
          IF( nounit ) x( jx ) = x( jx )/a( j, j )
          temp = x( jx )
          ix   = jx
          DO i = j + 1, n
            ix      = ix      + incx
            x( ix ) = x( ix ) - temp*a( i, j )
          END DO
        END IF
        jx = jx + incx
      END DO
    END IF
  END IF
ELSE

!        Form  x := inv( A' )*x.

  IF( lsame( uplo, 'U' ) ) THEN
    IF( incx == 1 ) THEN
      DO j = 1, n
        temp = x( j ) - DOT_PRODUCT( a( 1:j-1, j ), x( 1:j-1 ) )
        IF( nounit ) temp = temp/a( j, j )
        x( j ) = temp
      END DO
    ELSE
      jx = kx
      DO j = 1, n
        temp = x( jx )
        ix   = kx
        DO i = 1, j - 1
          temp = temp - a( i, j )*x( ix )
          ix   = ix   + incx
        END DO
        IF( nounit ) temp = temp/a( j, j )
        x( jx ) = temp
        jx      = jx   + incx
      END DO
    END IF
  ELSE
    IF( incx == 1 ) THEN
      DO j = n, 1, -1
        temp = x( j ) - DOT_PRODUCT( a( n:j+1:-1, j), x( n:j+1:-1 ) )
        IF( nounit ) temp = temp/a( j, j )
        x( j ) = temp
      END DO
    ELSE
      kx = kx + ( n - 1 )*incx
      jx = kx
      DO j = n, 1, -1
        temp = x( jx )
        ix   = kx
        DO i = n, j + 1, -1
          temp = temp - a( i, j )*x( ix )
          ix   = ix   - incx
        END DO
        IF( nounit ) temp = temp/a( j, j )
        x( jx ) = temp
        jx      = jx   - incx
      END DO
    END IF
  END IF

END IF

RETURN

!     End of DTRSV .

END SUBROUTINE dtrsv

END MODULE dblas2


MODULE dblas3
! For single precision, change wp to point to sp instead of dp
! This very much simplified BLAS2 module is by Alan Miller
! alan @ vic.cmis.csiro.au    URL: www.ozemail.com.au/~milleraj

! Latest revision - 3 September 1997

USE la_precision, ONLY: wp => dp             ! From file l_auxmod.f90
USE la_auxmod
USE dblas
IMPLICIT NONE

CONTAINS




SUBROUTINE dsyr2k( uplo, trans, n, k, alpha, a, lda, b, ldb,beta, c, ldc )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: uplo, trans
INTEGER, INTENT(IN)           :: n, k, lda, ldb, ldc
REAL (wp), INTENT(IN)         :: alpha, beta
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: a(:,:), b(:,:)
REAL (wp), INTENT(IN OUT)     :: c(:,:)
!     ..

!  Purpose
!  =======

!  DSYR2K  performs one of the symmetric rank 2k operations

!     C := alpha*A*B' + alpha*B*A' + beta*C,

!  or

!     C := alpha*A'*B + alpha*B'*A + beta*C,

!  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
!  and  A and B  are  n by k  matrices  in the  first  case  and  k by n
!  matrices in the second case.

!  Parameters
!  ==========

!  UPLO   - CHARACTER*1.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:

!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.

!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.

!           Unchanged on exit.

!  TRANS  - CHARACTER*1.
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:

!              TRANS = 'N' or 'n'   C := alpha*A*B' + alpha*B*A' +
!                                        beta*C.

!              TRANS = 'T' or 't'   C := alpha*A'*B + alpha*B'*A +
!                                        beta*C.

!              TRANS = 'C' or 'c'   C := alpha*A'*B + alpha*B'*A +
!                                        beta*C.

!           Unchanged on exit.

!  N      - INTEGER.
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.
!           Unchanged on exit.

!  K      - INTEGER.
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns  of the  matrices  A and B,  and on  entry  with
!           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
!           of rows of the matrices  A and B.  K must be at least  zero.
!           Unchanged on exit.

!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).
!           Unchanged on exit.

!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  k by n  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.

!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDB must be at least  max( 1, n ), otherwise  LDB must
!           be at least  max( 1, k ).
!           Unchanged on exit.

!  BETA   - DOUBLE PRECISION.
!           On entry, BETA specifies the scalar beta.
!           Unchanged on exit.

!  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!           upper triangular part of the array C must contain the upper
!           triangular part  of the  symmetric matrix  and the strictly
!           lower triangular part of C is not referenced.  On exit, the
!           upper triangular part of the array  C is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!           lower triangular part of the array C must contain the lower
!           triangular part  of the  symmetric matrix  and the strictly
!           upper triangular part of C is not referenced.  On exit, the
!           lower triangular part of the array  C is overwritten by the
!           lower triangular part of the updated matrix.

!  LDC    - INTEGER.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).
!           Unchanged on exit.


!  Level 3 Blas routine.


!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.


!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     .. External Subroutines ..
! EXTERNAL           xerbla
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     .. Local Scalars ..
LOGICAL   :: upper
INTEGER   :: i, info, j, l, nrowa
REAL (wp) :: temp1, temp2
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

IF( lsame( trans, 'N' ) )THEN
  nrowa = n
ELSE
  nrowa = k
END IF
upper = lsame( uplo, 'U' )

info = 0
IF(      ( .NOT.upper               ).AND.  &
  ( .NOT.lsame( uplo , 'L' ) )      )THEN
  info = 1
ELSE IF( ( .NOT.lsame( trans, 'N' ) ).AND.  &
  ( .NOT.lsame( trans, 'T' ) ).AND.( .NOT.lsame( trans, 'C' ) )      )THEN
  info = 2
ELSE IF( n  < 0               )THEN
  info = 3
ELSE IF( k  < 0               )THEN
  info = 4
ELSE IF( lda < MAX( 1, nrowa ) )THEN
  info = 7
ELSE IF( ldb < MAX( 1, nrowa ) )THEN
  info = 9
ELSE IF( ldc < MAX( 1, n     ) )THEN
  info = 12
END IF
IF( info /= 0 )THEN
  CALL erinfo(info, 'DSYR2K')
  RETURN
END IF

!     Quick return if possible.

IF( ( n == 0 ).OR.  &
( ( ( alpha == zero ).OR.( k == 0 ) ).AND.( beta == one ) ) )RETURN

!     And when  alpha.eq.zero.

IF( alpha == zero )THEN
  IF( upper )THEN
    IF( beta == zero )THEN
      DO j = 1, n
        c( 1:j, j ) = zero
      END DO
    ELSE
      DO j = 1, n
        c( 1:j, j ) = beta*c( 1:j, j )
      END DO
    END IF
  ELSE
    IF( beta == zero )THEN
      DO j = 1, n
        c( j:n, j ) = zero
      END DO
    ELSE
      DO j = 1, n
        c( j:n, j ) = beta*c( j:n, j )
      END DO
    END IF
  END IF
  RETURN
END IF

!     Start the operations.

IF( lsame( trans, 'N' ) )THEN

!        Form  C := alpha*A*B' + alpha*B*A' + C.

  IF( upper )THEN
    DO j = 1, n
      IF( beta == zero )THEN
        c( 1:j, j ) = zero
      ELSE IF( beta /= one )THEN
        c( 1:j, j ) = beta*c( 1:j, j )
      END IF
      DO l = 1, k
        IF( ( a( j, l ) /= zero ).OR. ( b( j, l ) /= zero )     )THEN
          temp1 = alpha*b( j, l )
          temp2 = alpha*a( j, l )
          c( 1:j, j ) = c( 1:j, j ) + a( 1:j, l )*temp1 + b( 1:j, l )*temp2
        END IF
      END DO
    END DO
  ELSE
    DO j = 1, n
      IF( beta == zero )THEN
        c( j:n, j ) = zero
      ELSE IF( beta /= one )THEN
        c( j:n, j ) = beta*c( j:n, j )
      END IF
      DO l = 1, k
        IF( ( a( j, l ) /= zero ).OR. ( b( j, l ) /= zero )     )THEN
          temp1 = alpha*b( j, l )
          temp2 = alpha*a( j, l )
          c( j:n, j ) = c( j:n, j ) + a( j:n, l )*temp1 + b( j:n, l )*temp2
        END IF
      END DO
    END DO
  END IF
ELSE

!        Form  C := alpha*A'*B + alpha*B'*A + C.

  IF( upper )THEN
    DO j = 1, n
      DO i = 1, j
        temp1 = zero
        temp2 = zero
        DO l = 1, k
          temp1 = temp1 + a( l, i )*b( l, j )
          temp2 = temp2 + b( l, i )*a( l, j )
        END DO
        IF( beta == zero )THEN
          c( i, j ) = alpha*temp1 + alpha*temp2
        ELSE
          c( i, j ) = beta *c( i, j ) +alpha*temp1 + alpha*temp2
        END IF
      END DO
    END DO
  ELSE
    DO j = 1, n
      DO i = j, n
        temp1 = zero
        temp2 = zero
        DO l = 1, k
          temp1 = temp1 + a( l, i )*b( l, j )
          temp2 = temp2 + b( l, i )*a( l, j )
        END DO
        IF( beta == zero )THEN
          c( i, j ) = alpha*temp1 + alpha*temp2
        ELSE
          c( i, j ) = beta *c( i, j ) +alpha*temp1 + alpha*temp2
        END IF
      END DO
    END DO
  END IF
END IF

RETURN

!     End of DSYR2K.

END SUBROUTINE dsyr2k


SUBROUTINE dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: side, uplo, transa, diag
INTEGER, INTENT(IN)           :: m, n, lda, ldb
REAL (wp), INTENT(IN)         :: alpha
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: a(:,:)
REAL (wp), INTENT(IN OUT)     :: b(:,:)
!     ..

!  Purpose
!  =======

!  DTRSM  solves one of the matrix equations

!     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,

!  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of

!     op( A ) = A   or   op( A ) = A'.

!  The matrix X is overwritten on B.

!  Parameters
!  ==========

!  SIDE   - CHARACTER*1.
!           On entry, SIDE specifies whether op( A ) appears on the left
!           or right of X as follows:

!              SIDE = 'L' or 'l'   op( A )*X = alpha*B.

!              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.

!           Unchanged on exit.

!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:

!              UPLO = 'U' or 'u'   A is an upper triangular matrix.

!              UPLO = 'L' or 'l'   A is a lower triangular matrix.

!           Unchanged on exit.

!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:

!              TRANSA = 'N' or 'n'   op( A ) = A.

!              TRANSA = 'T' or 't'   op( A ) = A'.

!              TRANSA = 'C' or 'c'   op( A ) = A'.

!           Unchanged on exit.

!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:

!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.

!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.

!           Unchanged on exit.

!  M      - INTEGER.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.

!  N      - INTEGER.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.

!  ALPHA  - DOUBLE PRECISION.
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.

!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain  the  right-hand  side  matrix  B,  and  on exit  is
!           overwritten by the solution matrix  X.

!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.


!  Level 3 Blas routine.


!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.


!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     .. External Subroutines ..
! EXTERNAL           xerbla
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     .. Local Scalars ..
LOGICAL   :: lside, nounit, upper
INTEGER   :: i, info, j, k, nrowa
REAL (wp) :: temp
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

lside  = lsame( side  , 'L' )
IF( lside )THEN
  nrowa = m
ELSE
  nrowa = n
END IF
nounit = lsame( diag  , 'N' )
upper  = lsame( uplo  , 'U' )

info   = 0
IF(      ( .NOT.lside                ).AND.  &
  ( .NOT.lsame( side  , 'R' ) )      )THEN
  info = 1
ELSE IF( ( .NOT.upper                ).AND.  &
  ( .NOT.lsame( uplo  , 'L' ) )      )THEN
  info = 2
ELSE IF( ( .NOT.lsame( transa, 'N' ) ).AND.  &
  ( .NOT.lsame( transa, 'T' ) ).AND.( .NOT.lsame( transa, 'C' ) )      )THEN
  info = 3
ELSE IF( ( .NOT.lsame( diag  , 'U' ) ).AND.  &
  ( .NOT.lsame( diag  , 'N' ) )      )THEN
  info = 4
ELSE IF( m  < 0               )THEN
  info = 5
ELSE IF( n  < 0               )THEN
  info = 6
ELSE IF( lda < MAX( 1, nrowa ) )THEN
  info = 9
ELSE IF( ldb < MAX( 1, m     ) )THEN
  info = 11
END IF
IF( info /= 0 )THEN
  CALL erinfo(info, 'DTRSM ')
  RETURN
END IF

!     Quick return if possible.

IF( n == 0 ) RETURN

!     And when  alpha.eq.zero.

IF( alpha == zero )THEN
  b( 1:m, 1:n ) = zero
  RETURN
END IF

!     Start the operations.

IF( lside )THEN
  IF( lsame( transa, 'N' ) )THEN

!           Form  B := alpha*inv( A )*B.

    IF( upper )THEN
      DO j = 1, n
        IF( alpha /= one )THEN
          b( 1:m, j ) = alpha*b( 1:m, j )
        END IF
        DO k = m, 1, -1
          IF( b( k, j ) /= zero )THEN
            IF( nounit ) b( k, j ) = b( k, j )/a( k, k )
            b( 1:k-1, j ) = b( 1:k-1, j ) - b( k, j )*a( 1:k-1, k )
          END IF
        END DO
      END DO
    ELSE
      DO j = 1, n
        IF( alpha /= one )THEN
          b( 1:m, j ) = alpha*b( 1:m, j )
        END IF
        DO k = 1, m
          IF( b( k, j ) /= zero )THEN
            IF( nounit ) b( k, j ) = b( k, j )/a( k, k )
            b( k+1:m, j ) = b( k+1:m, j ) - b( k, j )*a( k+1:m, k )
          END IF
        END DO
      END DO
    END IF
  ELSE

!           Form  B := alpha*inv( A' )*B.

    IF( upper )THEN
      DO j = 1, n
        DO i = 1, m
          temp = alpha*b( i, j ) - DOT_PRODUCT( a(:i-1,i), b(:i-1,j) )
          IF( nounit ) temp = temp/a( i, i )
          b( i, j ) = temp
        END DO
      END DO
    ELSE
      DO j = 1, n
        DO i = m, 1, -1
          temp = alpha*b( i, j ) - DOT_PRODUCT( a(i+1:m,i), b(i+1:m,j) )
          IF( nounit ) temp = temp/a( i, i )
          b( i, j ) = temp
        END DO
      END DO
    END IF
  END IF
ELSE
  IF( lsame( transa, 'N' ) )THEN

!           Form  B := alpha*B*inv( A ).

    IF( upper )THEN
      DO j = 1, n
        IF( alpha /= one )THEN
          b( 1:m, j ) = alpha*b( 1:m, j )
        END IF
        DO k = 1, j - 1
          IF( a( k, j ) /= zero )THEN
            b( 1:m, j ) = b( 1:m, j ) - a( k, j )*b( 1:m, k )
          END IF
        END DO
        IF( nounit )THEN
          temp = one/a( j, j )
          b( 1:m, j ) = temp*b( 1:m, j )
        END IF
      END DO
    ELSE
      DO j = n, 1, -1
        IF( alpha /= one )THEN
          b( 1:m, j ) = alpha*b( 1:m, j )
        END IF
        DO k = j + 1, n
          IF( a( k, j ) /= zero )THEN
            b( 1:m, j ) = b( 1:m, j ) - a( k, j )*b( 1:m, k )
          END IF
        END DO
        IF( nounit )THEN
          temp = one/a( j, j )
          b( 1:m, j ) = temp*b( 1:m, j )
        END IF
      END DO
    END IF
  ELSE

!           Form  B := alpha*B*inv( A' ).

    IF( upper )THEN
      DO k = n, 1, -1
        IF( nounit )THEN
          temp = one/a( k, k )
          b( 1:m, k ) = temp*b( 1:m, k )
        END IF
        DO j = 1, k - 1
          IF( a( j, k ) /= zero )THEN
            temp = a( j, k )
            b( 1:m, j ) = b( 1:m, j ) - temp*b( 1:m, k )
          END IF
        END DO
        IF( alpha /= one )THEN
          b( 1:m, k ) = alpha*b( 1:m, k )
        END IF
      END DO
    ELSE
      DO k = 1, n
        IF( nounit )THEN
          temp = one/a( k, k )
          b( 1:m, k ) = temp*b( 1:m, k )
        END IF
        DO j = k + 1, n
          IF( a( j, k ) /= zero )THEN
            temp = a( j, k )
            b( 1:m, j ) = b( 1:m, j ) - temp*b( 1:m, k )
          END IF
        END DO
        IF( alpha /= one )THEN
          b( 1:m, k ) = alpha*b( 1:m, k )
        END IF
      END DO
    END IF
  END IF
END IF

RETURN

!     End of DTRSM .

END SUBROUTINE dtrsm


SUBROUTINE dtrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: side, uplo, transa, diag
INTEGER, INTENT(IN)           :: m, n, lda, ldb
REAL (wp), INTENT(IN)         :: alpha
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: a(:,:)
REAL (wp), INTENT(IN OUT)     :: b(:,:)
!     ..

!  Purpose
!  =======

!  DTRMM  performs one of the matrix-matrix operations

!     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),

!  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of

!     op( A ) = A   or   op( A ) = A'.

!  Parameters
!  ==========

!  SIDE   - CHARACTER*1.
!           On entry,  SIDE specifies whether  op( A ) multiplies B from
!           the left or right as follows:

!              SIDE = 'L' or 'l'   B := alpha*op( A )*B.

!              SIDE = 'R' or 'r'   B := alpha*B*op( A ).

!           Unchanged on exit.

!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:

!              UPLO = 'U' or 'u'   A is an upper triangular matrix.

!              UPLO = 'L' or 'l'   A is a lower triangular matrix.

!           Unchanged on exit.

!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:

!              TRANSA = 'N' or 'n'   op( A ) = A.

!              TRANSA = 'T' or 't'   op( A ) = A'.

!              TRANSA = 'C' or 'c'   op( A ) = A'.

!           Unchanged on exit.

!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:

!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.

!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.

!           Unchanged on exit.

!  M      - INTEGER.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.

!  N      - INTEGER.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.

!  ALPHA  - DOUBLE PRECISION.
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.

!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain the matrix  B,  and  on exit  is overwritten  by the
!           transformed matrix.

!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.


!  Level 3 Blas routine.

!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.


!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     .. External Subroutines ..
! EXTERNAL           xerbla
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     .. Local Scalars ..
LOGICAL ::            lside, nounit, upper
INTEGER ::            i, info, j, k, nrowa
REAL (wp) ::   temp
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

lside  = lsame( side  , 'L' )
IF( lside )THEN
  nrowa = m
ELSE
  nrowa = n
END IF
nounit = lsame( diag  , 'N' )
upper  = lsame( uplo  , 'U' )

info   = 0
IF(      ( .NOT.lside                ).AND.  &
  ( .NOT.lsame( side  , 'R' ) )      )THEN
  info = 1
ELSE IF( ( .NOT.upper                ).AND.  &
  ( .NOT.lsame( uplo  , 'L' ) )      )THEN
  info = 2
ELSE IF( ( .NOT.lsame( transa, 'N' ) ).AND.  &
  ( .NOT.lsame( transa, 'T' ) ).AND.( .NOT.lsame( transa, 'C' ) )      )THEN
  info = 3
ELSE IF( ( .NOT.lsame( diag  , 'U' ) ).AND.  &
  ( .NOT.lsame( diag  , 'N' ) )      )THEN
  info = 4
ELSE IF( m  < 0               )THEN
  info = 5
ELSE IF( n  < 0               )THEN
  info = 6
ELSE IF( lda < MAX( 1, nrowa ) )THEN
  info = 9
ELSE IF( ldb < MAX( 1, m     ) )THEN
  info = 11
END IF
IF( info /= 0 )THEN
  CALL erinfo(info, 'DTRMM ')
  RETURN
END IF

!     Quick return if possible.

IF( n == 0 ) RETURN

!     And when  alpha = zero.

IF( alpha == zero )THEN
  b( 1:m, 1:n ) = zero
  RETURN
END IF

!     Start the operations.

IF( lside )THEN
  IF( lsame( transa, 'N' ) )THEN

!           Form  B := alpha*A*B.

    IF( upper )THEN
      DO j = 1, n
        DO k = 1, m
          IF( b( k, j ) /= zero )THEN
            temp = alpha*b( k, j )
            b( 1:k-1, j ) = b( 1:k-1, j ) + temp*a( 1:k-1, k )
            IF( nounit ) temp = temp*a( k, k )
            b( k, j ) = temp
          END IF
        END DO
      END DO
    ELSE
      DO j = 1, n
        DO k = m, 1, -1
          IF( b( k, j ) /= zero )THEN
            temp      = alpha*b( k, j )
            b( k, j ) = temp
            IF( nounit ) b( k, j ) = b( k, j )*a( k, k )
            b( k+1:m, j ) = b( k+1:m, j ) + temp*a( k+1:m, k )
          END IF
        END DO
      END DO
    END IF
  ELSE

!           Form  B := alpha*B*A'.

    IF( upper )THEN
      DO j = 1, n
        DO i = m, 1, -1
          temp = b( i, j )
          IF( nounit ) temp = temp*a( i, i )
          temp = temp + DOT_PRODUCT( a( 1:i-1, i ), b( 1:i-1, j ) )
          b( i, j ) = alpha*temp
        END DO
      END DO
    ELSE
      DO j = 1, n
        DO i = 1, m
          temp = b( i, j )
          IF( nounit ) temp = temp*a( i, i )
          temp = temp + DOT_PRODUCT( a( i+1:m, i ), b( i+1:m, j ) )
          b( i, j ) = alpha*temp
        END DO
      END DO
    END IF
  END IF
ELSE
  IF( lsame( transa, 'N' ) )THEN

!           Form  B := alpha*B*A.

    IF( upper )THEN
      DO j = n, 1, -1
        temp = alpha
        IF( nounit ) temp = temp*a( j, j )
        b( 1:m, j ) = temp*b( 1:m, j )
        DO k = 1, j - 1
          IF( a( k, j ) /= zero )THEN
            temp = alpha*a( k, j )
            b( 1:m, j ) = b( 1:m, j ) + temp*b( 1:m, k )
          END IF
        END DO
      END DO
    ELSE
      DO j = 1, n
        temp = alpha
        IF( nounit ) temp = temp*a( j, j )
        b( 1:m, j ) = temp*b( 1:m, j )
        DO k = j + 1, n
          IF( a( k, j ) /= zero )THEN
            temp = alpha*a( k, j )
            b( 1:m, j ) = b( 1:m, j ) + temp*b( 1:m, k )
          END IF
        END DO
      END DO
    END IF
  ELSE

!           Form  B := alpha*B*A'.

    IF( upper )THEN
      DO k = 1, n
        DO j = 1, k - 1
          IF( a( j, k ) /= zero )THEN
            temp = alpha*a( j, k )
            b( 1:m, j ) = b( 1:m, j ) + temp*b( 1:m, k )
          END IF
        END DO
        temp = alpha
        IF( nounit ) temp = temp*a( k, k )
        IF( temp /= one )THEN
          b( 1:m, k ) = temp*b( 1:m, k )
        END IF
      END DO
    ELSE
      DO k = n, 1, -1
        DO j = k + 1, n
          IF( a( j, k ) /= zero )THEN
            temp = alpha*a( j, k )
            b( 1:m, j ) = b( 1:m, j ) + temp*b( 1:m, k )
          END IF
        END DO
        temp = alpha
        IF( nounit ) temp = temp*a( k, k )
        IF( temp /= one )THEN
          b( 1:m, k ) = temp*b( 1:m, k )
        END IF
      END DO
    END IF
  END IF
END IF

RETURN

!     End of DTRMM .

END SUBROUTINE dtrmm


SUBROUTINE dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb,  &
                   beta, c, ldc )
! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: transa, transb
INTEGER, INTENT(IN)           :: m, n, k, lda, ldb, ldc
REAL (wp), INTENT(IN)         :: alpha, beta
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: a(:,:), b(:,:)
REAL (wp), INTENT(IN OUT)     :: c(:,:)
!     ..

!  Purpose
!  =======

!  DGEMM  performs one of the matrix-matrix operations

!     C := alpha*op( A )*op( B ) + beta*C,

!  where  op( X ) is one of

!     op( X ) = X   or   op( X ) = X',

!  alpha and beta are scalars, and A, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.

!  Parameters
!  ==========

!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:

!              TRANSA = 'N' or 'n',  op( A ) = A.

!              TRANSA = 'T' or 't',  op( A ) = A'.

!              TRANSA = 'C' or 'c',  op( A ) = A'.

!           Unchanged on exit.

!  TRANSB - CHARACTER*1.
!           On entry, TRANSB specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:

!              TRANSB = 'N' or 'n',  op( B ) = B.

!              TRANSB = 'T' or 't',  op( B ) = B'.

!              TRANSB = 'C' or 'c',  op( B ) = B'.

!           Unchanged on exit.

!  M      - INTEGER.
!           On entry,  M  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix  C.  M  must  be at least  zero.
!           Unchanged on exit.

!  N      - INTEGER.
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.
!           Unchanged on exit.

!  K      - INTEGER.
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.
!           Unchanged on exit.

!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.

!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
!           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.

!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, k ).
!           Unchanged on exit.

!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
!           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
!           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.

!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
!           LDB must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).
!           Unchanged on exit.

!  BETA   - DOUBLE PRECISION.
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.

!  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n  matrix
!           ( alpha*op( A )*op( B ) + beta*C ).

!  LDC    - INTEGER.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.


!  Level 3 Blas routine.

!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.


!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     .. External Subroutines ..
! EXTERNAL           xerbla
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     .. Local Scalars ..
LOGICAL ::            nota, notb
INTEGER ::            i, info, j, l, nrowa, nrowb
REAL (wp) ::   temp
!     ..
!     .. Executable Statements ..

!     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
!     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
!     and  columns of  A  and the  number of  rows  of  B  respectively.

nota  = lsame( transa, 'N' )
notb  = lsame( transb, 'N' )
IF( nota )THEN
  nrowa = m
ELSE
  nrowa = k
END IF
IF( notb )THEN
  nrowb = k
ELSE
  nrowb = n
END IF

!     Test the input parameters.

info = 0
IF(      ( .NOT.nota                 ).AND.  &
  ( .NOT.lsame( transa, 'C' ) ).AND.( .NOT.lsame( transa, 'T' ) )      )THEN
  info = 1
ELSE IF( ( .NOT.notb                 ).AND.  &
  ( .NOT.lsame( transb, 'C' ) ).AND.( .NOT.lsame( transb, 'T' ) )      )THEN
  info = 2
ELSE IF( m  < 0               )THEN
  info = 3
ELSE IF( n  < 0               )THEN
  info = 4
ELSE IF( k  < 0               )THEN
  info = 5
ELSE IF( lda < MAX( 1, nrowa ) )THEN
  info = 8
ELSE IF( ldb < MAX( 1, nrowb ) )THEN
  info = 10
ELSE IF( ldc < MAX( 1, m     ) )THEN
  info = 13
END IF
IF( info /= 0 )THEN
  CALL erinfo(info, 'DGEMM ')
  RETURN
END IF

!     Quick return if possible.

IF( ( m == 0 ).OR.( n == 0 ).OR.  &
( ( ( alpha == zero ).OR.( k == 0 ) ).AND.( beta == one ) ) )RETURN

!     And if  alpha.eq.zero.

IF( alpha == zero ) THEN
  IF( beta == zero ) THEN
    c(1:m,1:n) = zero
  ELSE
    c(1:m,1:n) = beta * c(1:m,1:n)
  END IF
  RETURN
END IF

!     Start the operations.

IF( notb )THEN
  IF( nota )THEN

!           Form  C := alpha*A*B + beta*C.

    DO j = 1, n
      IF( beta == zero )THEN
        DO i = 1, m
          c( i, j ) = zero
        END DO
      ELSE IF( beta /= one )THEN
        c( 1:m, j ) = beta*c( 1:m, j )
      END IF
      DO l = 1, k
        IF( b( l, j ) /= zero )THEN
          temp = alpha*b( l, j )
          c( 1:m, j ) = c( 1:m, j ) + temp*a( 1:m, l )
        END IF
      END DO
    END DO
  ELSE

!           Form  C := alpha*A'*B + beta*C

    DO j = 1, n
      DO i = 1, m
        temp = DOT_PRODUCT( a(1:k,i), b(1:k,j) )
        IF( beta == zero )THEN
          c( i, j ) = alpha*temp
        ELSE
          c( i, j ) = alpha*temp + beta*c( i, j )
        END IF
      END DO
    END DO
  END IF
ELSE
  IF( nota )THEN

!           Form  C := alpha*A*B' + beta*C

    DO j = 1, n
      IF( beta == zero )THEN
        c( 1:m, j ) = zero
      ELSE IF( beta /= one )THEN
        c( 1:m, j ) = beta*c( 1:m, j )
      END IF
      DO l = 1, k
        IF( b( j, l ) /= zero )THEN
          temp = alpha*b( j, l )
          c( 1:m, j ) = c( 1:m, j ) + temp*a( 1:m, l )
        END IF
      END DO
    END DO
  ELSE

!           Form  C := alpha*A'*B' + beta*C

    DO j = 1, n
      DO i = 1, m
        temp = DOT_PRODUCT( a(1:k,i), b(j,1:k) )
        IF( beta == zero )THEN
          c( i, j ) = alpha*temp
        ELSE
          c( i, j ) = alpha*temp + beta*c( i, j )
        END IF
      END DO
    END DO
  END IF
END IF

RETURN

!     End of DGEMM .

END SUBROUTINE dgemm



END MODULE dblas3


MODULE dla
! LAPACK auxiliary routines

! Translated by Alan Miller
! Alan.Miller @ vic.cmis.csiro.au    URL: www.ozemail.com.au/~milleraj

! Latest revision - 17 August 1998

USE la_precision, ONLY: wp => dp
USE la_auxmod
USE dblas
USE dblas2, ONLY: dgemv, dsymv, dtrmv, dtrsv, dger
USE dblas3, ONLY: dgemm, dtrmm
IMPLICIT NONE

CONTAINS



SUBROUTINE dlarfg( n, alpha, x, incx, tau )

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: incx, n
REAL (wp), INTENT(IN OUT) :: alpha
REAL (wp), INTENT(OUT)    :: tau
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT) :: x(:)
!     ..

!  Purpose
!  =======

!  DLARFG generates a real elementary reflector H of order n, such
!  that

!        H * ( alpha ) = ( beta ),   H' * H = I.
!            (   x   )   (   0  )

!  where alpha and beta are scalars, and x is an (n-1)-element real
!  vector. H is represented in the form

!        H = I - tau * ( 1 ) * ( 1 v' ) ,
!                      ( v )

!  where tau is a real scalar and v is a real (n-1)-element
!  vector.

!  If the elements of x are all zero, then tau = 0 and H is taken to be
!  the unit matrix.

!  Otherwise  1 <= tau <= 2.

!  Arguments
!  =========

!  N       (input) INTEGER
!          The order of the elementary reflector.

!  ALPHA   (input/output) DOUBLE PRECISION
!          On entry, the value alpha.
!          On exit, it is overwritten with the value beta.

!  X       (input/output) DOUBLE PRECISION array, dimension
!                         (1+(N-2)*abs(INCX))
!          On entry, the vector x.
!          On exit, it is overwritten with the vector v.

!  INCX    (input) INTEGER
!          The increment between elements of X. INCX <> 0.

!  TAU     (output) DOUBLE PRECISION
!          The value tau.

!  =====================================================================

!     .. Local Scalars ..
INTEGER   :: knt
REAL (wp) :: beta, rsafmn, safmin, xnorm
!     ..
!     .. External Functions ..
! DOUBLE PRECISION ::   dlamch, dlapy2, dnrm2
! EXTERNAL           dlamch, dlapy2, dnrm2
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS, SIGN
!     ..
!     .. External Subroutines ..
! EXTERNAL           dscal
!     ..
!     .. Executable Statements ..

IF( n <= 1 ) THEN
  tau = zero
  RETURN
END IF

xnorm = dnrm2( n-1, x, incx )

IF( xnorm == zero ) THEN

!        H  =  I

  tau = zero
ELSE

!        general case

  beta = -SIGN( dlapy2( alpha, xnorm ), alpha )
  safmin = dlamch( 'S' )
  IF( ABS( beta ) < safmin ) THEN

!           XNORM, BETA may be inaccurate; scale X and recompute them

    rsafmn = one / safmin
    knt = 0

    10 knt = knt + 1
    CALL dscal( n-1, rsafmn, x, incx )
    beta = beta*rsafmn
    alpha = alpha*rsafmn
    IF( ABS( beta ) < safmin ) GO TO 10

!           New BETA is at most 1, at least SAFMIN

    xnorm = dnrm2( n-1, x, incx )
    beta = -SIGN( dlapy2( alpha, xnorm ), alpha )
    tau = ( beta-alpha ) / beta
    CALL dscal( n-1, one / ( alpha-beta ), x, incx )

!           If ALPHA is subnormal, it may lose relative accuracy

    alpha = beta * safmin**knt
  ELSE
    tau = ( beta-alpha ) / beta
    CALL dscal( n-1, one / ( alpha-beta ), x, incx )
    alpha = beta
  END IF
END IF

RETURN

!     End of DLARFG

END SUBROUTINE dlarfg



SUBROUTINE dlartg( f, g, cs, sn, r )

!  -- LAPACK auxiliary routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994

! ELF90 translation by Alan Miller    30-Nov-1997

!     .. Scalar Arguments ..
REAL (wp), INTENT(IN)  :: f, g
REAL (wp), INTENT(OUT) :: cs, r, sn
!     ..

!  Purpose
!  =======

!  DLARTG generate a plane rotation so that

!     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
!     [ -SN  CS  ]     [ G ]     [ 0 ]

!  This is a slower, more accurate version of the BLAS1 routine DROTG,
!  with the following other differences:
!     F and G are unchanged on return.
!     If G=0, then CS=1 and SN=0.
!     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
!        floating point operations (saves work in DBDSQR when
!        there are zeros on the diagonal).

!  If F exceeds G in magnitude, CS will be positive.

!  Arguments
!  =========

!  F       (input) DOUBLE PRECISION
!          The first component of vector to be rotated.

!  G       (input) DOUBLE PRECISION
!          The second component of vector to be rotated.

!  CS      (output) DOUBLE PRECISION
!          The cosine of the rotation.

!  SN      (output) DOUBLE PRECISION
!          The sine of the rotation.

!  R       (output) DOUBLE PRECISION
!          The nonzero component of the rotated vector.

!  =====================================================================

!     .. Local Scalars ..
LOGICAL, SAVE        :: first = .TRUE.
INTEGER              :: count, i
REAL (wp)            :: eps, f1, g1, scale
REAL (wp), SAVE      :: safmin, safmn2, safmx2
REAL (wp), PARAMETER :: two = 2.0_wp
!     ..
!     .. External Functions ..
! DOUBLE PRECISION ::   dlamch
! EXTERNAL           dlamch
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS, INT, LOG, MAX, SQRT
!     ..
!     .. Save statement ..
! SAVE               first, safmx2, safmin, safmn2
!     ..
!     .. Data statements ..
! DATA               first / .true. /
!     ..
!     .. Executable Statements ..

IF( first ) THEN
  first = .false.
  safmin = dlamch( 'S' )
  eps = dlamch( 'E' )
  safmn2 = dlamch( 'B' )**INT( LOG( safmin / eps ) /  &
  LOG( dlamch( 'B' ) ) / two )
  safmx2 = one / safmn2
END IF
IF( g == zero ) THEN
  cs = one
  sn = zero
  r = f
ELSE IF( f == zero ) THEN
  cs = zero
  sn = one
  r = g
ELSE
  f1 = f
  g1 = g
  scale = MAX( ABS( f1 ), ABS( g1 ) )
  IF( scale >= safmx2 ) THEN
    count = 0

    10 count = count + 1
    f1 = f1*safmn2
    g1 = g1*safmn2
    scale = MAX( ABS( f1 ), ABS( g1 ) )
    IF( scale >= safmx2 ) GO TO 10
    r = SQRT( f1**2 + g1**2 )
    cs = f1 / r
    sn = g1 / r
    DO i = 1, count
      r = r*safmx2
    END DO
  ELSE IF( scale <= safmn2 ) THEN
    count = 0

    30 count = count + 1
    f1 = f1*safmx2
    g1 = g1*safmx2
    scale = MAX( ABS( f1 ), ABS( g1 ) )
    IF( scale <= safmn2 ) GO TO 30
    r = SQRT( f1**2 + g1**2 )
    cs = f1 / r
    sn = g1 / r
    DO i = 1, count
      r = r*safmn2
    END DO
  ELSE
    r = SQRT( f1**2 + g1**2 )
    cs = f1 / r
    sn = g1 / r
  END IF
  IF( ABS( f ) > ABS( g ) .AND. cs < zero ) THEN
    cs = -cs
    sn = -sn
    r = -r
  END IF
END IF
RETURN

!     End of DLARTG

END SUBROUTINE dlartg



SUBROUTINE dlae2( a, b, c, rt1, rt2 )

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
REAL (wp), INTENT(IN)  :: a, b, c
REAL (wp), INTENT(OUT) :: rt1, rt2
!     ..

!  Purpose
!  =======

!  DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix
!     [  A   B  ]
!     [  B   C  ].
!  On return, RT1 is the eigenvalue of larger absolute value, and RT2
!  is the eigenvalue of smaller absolute value.

!  Arguments
!  =========

!  A       (input) DOUBLE PRECISION
!          The (1,1) entry of the 2-by-2 matrix.

!  B       (input) DOUBLE PRECISION
!          The (1,2) and (2,1) entries of the 2-by-2 matrix.

!  C       (input) DOUBLE PRECISION
!          The (2,2) entry of the 2-by-2 matrix.

!  RT1     (output) DOUBLE PRECISION
!          The eigenvalue of larger absolute value.

!  RT2     (output) DOUBLE PRECISION
!          The eigenvalue of smaller absolute value.

!  Further Details
!  ===============

!  RT1 is accurate to a few ulps barring over/underflow.

!  RT2 may be inaccurate if there is massive cancellation in the
!  determinant A*C-B*B; higher precision or correctly rounded or
!  correctly truncated arithmetic would be needed to compute RT2
!  accurately in all cases.

!  Overflow is possible only if RT1 is within a factor of 5 of overflow.
!  Underflow is harmless if the input data is 0 or exceeds
!     underflow_threshold / macheps.


!     .. Parameters ..
REAL (wp), PARAMETER :: two = 2._wp, half = 0.5_wp
!     ..
!     .. Local Scalars ..
REAL (wp) :: ab, acmn, acmx, adf, df, rt, sm, tb
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS, SQRT
!     ..
!     .. Executable Statements ..

!     Compute the eigenvalues

sm = a + c
df = a - c
adf = ABS( df )
tb = b + b
ab = ABS( tb )
IF( ABS( a ) > ABS( c ) ) THEN
  acmx = a
  acmn = c
ELSE
  acmx = c
  acmn = a
END IF
IF( adf > ab ) THEN
  rt = adf*SQRT( one+( ab / adf )**2 )
ELSE IF( adf < ab ) THEN
  rt = ab*SQRT( one+( adf / ab )**2 )
ELSE

!        Includes case AB=ADF=0

  rt = ab*SQRT( two )
END IF
IF( sm < zero ) THEN
  rt1 = half*( sm-rt )

!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.

  rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
ELSE IF( sm > zero ) THEN
  rt1 = half*( sm+rt )

!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.

  rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
ELSE

!        Includes case RT1 = RT2 = 0

  rt1 = half*rt
  rt2 = -half*rt
END IF
RETURN

!     End of DLAE2

END SUBROUTINE dlae2




SUBROUTINE dlaev2( a, b, c, rt1, rt2, cs1, sn1 )

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
REAL (wp), INTENT(IN)  :: a, b, c
REAL (wp), INTENT(OUT) :: cs1, rt1, rt2, sn1
!     ..

!  Purpose
!  =======

!  DLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix
!     [  A   B  ]
!     [  B   C  ].
!  On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
!  eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
!  eigenvector for RT1, giving the decomposition

!     [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
!     [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].

!  Arguments
!  =========

!  A       (input) DOUBLE PRECISION
!          The (1,1) entry of the 2-by-2 matrix.

!  B       (input) DOUBLE PRECISION
!          The (1,2) entry and the conjugate of the (2,1) entry of the
!          2-by-2 matrix.

!  C       (input) DOUBLE PRECISION
!          The (2,2) entry of the 2-by-2 matrix.

!  RT1     (output) DOUBLE PRECISION
!          The eigenvalue of larger absolute value.

!  RT2     (output) DOUBLE PRECISION
!          The eigenvalue of smaller absolute value.

!  CS1     (output) DOUBLE PRECISION
!  SN1     (output) DOUBLE PRECISION
!          The vector (CS1, SN1) is a unit right eigenvector for RT1.

!  Further Details
!  ===============

!  RT1 is accurate to a few ulps barring over/underflow.

!  RT2 may be inaccurate if there is massive cancellation in the
!  determinant A*C-B*B; higher precision or correctly rounded or
!  correctly truncated arithmetic would be needed to compute RT2
!  accurately in all cases.

!  CS1 and SN1 are accurate to a few ulps barring over/underflow.

!  Overflow is possible only if RT1 is within a factor of 5 of overflow.
!  Underflow is harmless if the input data is 0 or exceeds
!     underflow_threshold / macheps.


!     .. Parameters ..
REAL (wp), PARAMETER :: two = 2.0_wp, half = 0.5_wp
!     ..
!     .. Local Scalars ..
INTEGER   :: sgn1, sgn2
REAL (wp) :: ab, acmn, acmx, acs, adf, cs, ct, df, rt, sm,tb, tn
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS, SQRT
!     ..
!     .. Executable Statements ..

!     Compute the eigenvalues

sm = a + c
df = a - c
adf = ABS( df )
tb = b + b
ab = ABS( tb )
IF( ABS( a ) > ABS( c ) ) THEN
  acmx = a
  acmn = c
ELSE
  acmx = c
  acmn = a
END IF
IF( adf > ab ) THEN
  rt = adf*SQRT( one+( ab / adf )**2 )
ELSE IF( adf < ab ) THEN
  rt = ab*SQRT( one+( adf / ab )**2 )
ELSE

!        Includes case AB=ADF=0

  rt = ab*SQRT( two )
END IF
IF( sm < zero ) THEN
  rt1 = half*( sm-rt )
  sgn1 = -1

!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.

  rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
ELSE IF( sm > zero ) THEN
  rt1 = half*( sm+rt )
  sgn1 = 1

!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.

  rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
ELSE

!        Includes case RT1 = RT2 = 0

  rt1 = half*rt
  rt2 = -half*rt
  sgn1 = 1
END IF

!     Compute the eigenvector

IF( df >= zero ) THEN
  cs = df + rt
  sgn2 = 1
ELSE
  cs = df - rt
  sgn2 = -1
END IF
acs = ABS( cs )
IF( acs > ab ) THEN
  ct = -tb / cs
  sn1 = one / SQRT( one+ct*ct )
  cs1 = ct*sn1
ELSE
  IF( ab == zero ) THEN
    cs1 = one
    sn1 = zero
  ELSE
    tn = -cs / tb
    cs1 = one / SQRT( one+tn*tn )
    sn1 = tn*cs1
  END IF
END IF
IF( sgn1 == sgn2 ) THEN
  tn = cs1
  cs1 = -sn1
  sn1 = tn
END IF
RETURN

!     End of DLAEV2

END SUBROUTINE dlaev2




SUBROUTINE dlarf( side, m, n, v, incv, tau, c, ldc )

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. Argument WORK has been removed.

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: side
INTEGER, INTENT(IN)           :: incv, ldc, m, n
REAL (wp), INTENT(IN)         :: tau
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: v(:)
REAL (wp), INTENT(IN OUT)     :: c(:,:)
!     ..

!  Purpose
!  =======

!  DLARF applies a real elementary reflector H to a real m by n matrix
!  C, from either the left or the right. H is represented in the form

!        H = I - tau * v * v'

!  where tau is a real scalar and v is a real vector.

!  If tau = 0, then H is taken to be the unit matrix.

!  Arguments
!  =========

!  SIDE    (input) CHARACTER*1
!          = 'L': form  H * C
!          = 'R': form  C * H

!  M       (input) INTEGER
!          The number of rows of the matrix C.

!  N       (input) INTEGER
!          The number of columns of the matrix C.

!  V       (input) DOUBLE PRECISION array, dimension
!                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
!                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
!          The vector v in the representation of H. V is not used if
!          TAU = 0.

!  INCV    (input) INTEGER
!          The increment between elements of v. INCV <> 0.

!  TAU     (input) DOUBLE PRECISION
!          The value tau in the representation of H.

!  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
!          On entry, the m by n matrix C.
!          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
!          or C * H if SIDE = 'R'.

!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).

!  =====================================================================

REAL (wp), ALLOCATABLE :: work(:)

!     .. External Subroutines ..
! EXTERNAL           dgemv, dger
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. Executable Statements ..

IF( lsame( side, 'L' ) ) THEN
  ALLOCATE( work(n) )

!        Form  H * C

  IF( tau /= zero ) THEN

!           w := C' * v

    CALL dgemv( 'Transpose', m, n, one, c, ldc, v, incv, zero, work, 1 )

!           C := C - v * w'

    CALL dger( m, n, -tau, v, incv, work, 1, c, ldc )
  END IF
ELSE
  ALLOCATE( work(m) )

!        Form  C * H

  IF( tau /= zero ) THEN

!           w := C * v

    CALL dgemv( 'No transpose', m, n, one, c, ldc, v, incv, zero, work, 1 )

!           C := C - w * v'

    CALL dger( m, n, -tau, work, 1, v, incv, c, ldc )
  END IF
END IF

DEALLOCATE( work )
RETURN

!     End of DLARF

END SUBROUTINE dlarf



SUBROUTINE dlarfb( side, trans, DIRECT, storev, m, n, k, v, ldv,  &
                   t, ldt, c, ldc )

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. Arguments WORK and LDWORK have been removed.

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: side, trans, direct, storev
INTEGER, INTENT(IN)           :: k, ldc, ldt, ldv, m, n
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: t(:,:), v(:,:)
REAL (wp), INTENT(IN OUT)     :: c(:,:)
!     ..

!  Purpose
!  =======

!  DLARFB applies a real block reflector H or its transpose H' to a
!  real m by n matrix C, from either the left or the right.

!  Arguments
!  =========

!  SIDE    (input) CHARACTER*1
!          = 'L': apply H or H' from the Left
!          = 'R': apply H or H' from the Right

!  TRANS   (input) CHARACTER*1
!          = 'N': apply H (No transpose)
!          = 'T': apply H' (Transpose)

!  DIRECT  (input) CHARACTER*1
!          Indicates how H is formed from a product of elementary
!          reflectors
!          = 'F': H = H(1) H(2) . . . H(k) (Forward)
!          = 'B': H = H(k) . . . H(2) H(1) (Backward)

!  STOREV  (input) CHARACTER*1
!          Indicates how the vectors which define the elementary
!          reflectors are stored:
!          = 'C': Columnwise
!          = 'R': Rowwise

!  M       (input) INTEGER
!          The number of rows of the matrix C.

!  N       (input) INTEGER
!          The number of columns of the matrix C.

!  K       (input) INTEGER
!          The order of the matrix T (= the number of elementary
!          reflectors whose product defines the block reflector).

!  V       (input) DOUBLE PRECISION array, dimension
!                                (LDV,K) if STOREV = 'C'
!                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
!                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
!          The matrix V. See further details.

!  LDV     (input) INTEGER
!          The leading dimension of the array V.
!          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
!          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
!          if STOREV = 'R', LDV >= K.

!  T       (input) DOUBLE PRECISION array, dimension (LDT,K)
!          The triangular k by k matrix T in the representation of the
!          block reflector.

!  LDT     (input) INTEGER
!          The leading dimension of the array T. LDT >= K.

!  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
!          On entry, the m by n matrix C.
!          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.

!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDA >= max(1,M).

!  =====================================================================

!     .. Local Scalars ..
INTEGER                :: j, ldwork
CHARACTER (LEN=1)      :: transt
REAL (wp), ALLOCATABLE :: work(:,:)
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. External Subroutines ..
! EXTERNAL           dgemm, dtrmm
!     ..
!     .. Executable Statements ..

!     Quick return if possible

IF( m <= 0 .OR. n <= 0 ) RETURN

!     Allocate workspace
IF( lsame( side, 'L') ) THEN
  ldwork = MAX(1, n)
ELSE
  ldwork = MAX(1, m)
END IF
ALLOCATE( work(ldwork, k) )

IF( lsame( trans, 'N' ) ) THEN
  transt = 'T'
ELSE
  transt = 'N'
END IF

IF( lsame( storev, 'C' ) ) THEN

  IF( lsame( DIRECT, 'F' ) ) THEN

!           Let  V =  ( V1 )    (first K rows)
!                     ( V2 )
!           where  V1  is unit lower triangular.

    IF( lsame( side, 'L' ) ) THEN

!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )

!              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)

!              W := C1'

      DO j = 1, k
        work(1:n, j) = c(j, 1:n)
      END DO

!              W := W * V1

      CALL dtrmm( 'Right', 'Lower', 'No transpose', 'Unit', n,  &
                  k, one, v, ldv, work, ldwork )
      IF( m > k ) THEN

!                 W := W + C2'*V2

        CALL dgemm( 'Transpose', 'No transpose', n, k, m-k, one,  &
                    c( k+1:, : ), ldc, v( k+1:, : ), ldv, one, work, ldwork )
      END IF

!              W := W * T'  or  W * T

      CALL dtrmm( 'Right', 'Upper', transt, 'Non-unit', n, k,  &
                  one, t, ldt, work, ldwork )

!              C := C - V * W'

      IF( m > k ) THEN

!                 C2 := C2 - V2 * W'

        CALL dgemm( 'No transpose', 'Transpose', m-k, n, k, -one,  &
                    v( k+1:, : ), ldv, work, ldwork, one, c( k+1:, : ), ldc )
      END IF

!              W := W * V1'

      CALL dtrmm( 'Right', 'Lower', 'Transpose', 'Unit', n, k,  &
                  one, v, ldv, work, ldwork )

!              C1 := C1 - W'

      DO j = 1, k
        c( j, 1:n ) = c( j, 1:n ) - work( 1:n, j )
      END DO

    ELSE IF( lsame( side, 'R' ) ) THEN

!              Form  C * H  or  C * H'  where  C = ( C1  C2 )

!              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)

!              W := C1

      work(1:m, 1:k) = c(1:m, 1:k)

!              W := W * V1

      CALL dtrmm( 'Right', 'Lower', 'No transpose', 'Unit', m,  &
                  k, one, v, ldv, work, ldwork )
      IF( n > k ) THEN

!                 W := W + C2 * V2

        CALL dgemm( 'No transpose', 'No transpose', m, k, n-k, one,  &
                    c( :, k+1: ), ldc, v( k+1:, : ), ldv, one, work, ldwork )
      END IF

!              W := W * T  or  W * T'

      CALL dtrmm( 'Right', 'Upper', trans, 'Non-unit', m, k,  &
                  one, t, ldt, work, ldwork )

!              C := C - W * V'

      IF( n > k ) THEN

!                 C2 := C2 - W * V2'

        CALL dgemm( 'No transpose', 'Transpose', m, n-k, k, -one,  &
                    work, ldwork, v( k+1:, : ), ldv, one, c( :, k+1: ), ldc )
      END IF

!              W := W * V1'

      CALL dtrmm( 'Right', 'Lower', 'Transpose', 'Unit', m, k,  &
                  one, v, ldv, work, ldwork )

!              C1 := C1 - W

      DO j = 1, k
        c( 1:m, j ) = c( 1:m, j ) - work( 1:m, j )
      END DO
    END IF

  ELSE

!           Let  V =  ( V1 )
!                     ( V2 )    (last K rows)
!           where  V2  is unit upper triangular.

    IF( lsame( side, 'L' ) ) THEN

!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )

!              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)

!              W := C2'

      DO j = 1, k
        work(1:n, j) = c(m-k+j, 1:n)
      END DO

!              W := W * V2

      CALL dtrmm( 'Right', 'Upper', 'No transpose', 'Unit', n,  &
                  k, one, v( m-k+1:, : ), ldv, work, ldwork )
      IF( m > k ) THEN

!                 W := W + C1'*V1

        CALL dgemm( 'Transpose', 'No transpose', n, k, m-k,  &
                    one, c, ldc, v, ldv, one, work, ldwork )
      END IF

!              W := W * T'  or  W * T

      CALL dtrmm( 'Right', 'Lower', transt, 'Non-unit', n, k,  &
                  one, t, ldt, work, ldwork )

!              C := C - V * W'

      IF( m > k ) THEN

!                 C1 := C1 - V1 * W'

        CALL dgemm( 'No transpose', 'Transpose', m-k, n, k, -one, v,  &
                    ldv, work, ldwork, one, c, ldc )
      END IF

!              W := W * V2'

      CALL dtrmm( 'Right', 'Upper', 'Transpose', 'Unit', n, k,  &
                  one, v( m-k+1:, : ), ldv, work, ldwork )

!              C2 := C2 - W'

      DO j = 1, k
        c( m-k+j, 1:n ) = c( m-k+j, 1:n ) - work( 1:n, j )
      END DO

    ELSE IF( lsame( side, 'R' ) ) THEN

!              Form  C * H  or  C * H'  where  C = ( C1  C2 )

!              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)

!              W := C2

      work(1:m, 1:k) = c(1:m, n-k+1:n)

!              W := W * V2

      CALL dtrmm( 'Right', 'Upper', 'No transpose', 'Unit', m,  &
                  k, one, v( n-k+1:, : ), ldv, work, ldwork )
      IF( n > k ) THEN

!                 W := W + C1 * V1

        CALL dgemm( 'No transpose', 'No transpose', m, k, n-k, one, c,  &
                    ldc, v, ldv, one, work, ldwork )
      END IF

!              W := W * T  or  W * T'

      CALL dtrmm( 'Right', 'Lower', trans, 'Non-unit', m, k,  &
                  one, t, ldt, work, ldwork )

!              C := C - W * V'

      IF( n > k ) THEN

!                 C1 := C1 - W * V1'

        CALL dgemm( 'No transpose', 'Transpose', m, n-k, k,  &
                    -one, work, ldwork, v, ldv, one, c, ldc )
      END IF

!              W := W * V2'

      CALL dtrmm( 'Right', 'Upper', 'Transpose', 'Unit', m, k,  &
                  one, v( n-k+1:, : ), ldv, work, ldwork )

!              C2 := C2 - W

      DO j = 1, k
        c( 1:m, n-k+j ) = c( 1:m, n-k+j ) - work( 1:m, j )
      END DO
    END IF
  END IF

ELSE IF( lsame( storev, 'R' ) ) THEN

  IF( lsame( DIRECT, 'F' ) ) THEN

!           Let  V =  ( V1  V2 )    (V1: first K columns)
!           where  V1  is unit upper triangular.

    IF( lsame( side, 'L' ) ) THEN

!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )

!              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)

!              W := C1'

      DO j = 1, k
        work(1:n, j) = c(j, 1:n)
      END DO

!              W := W * V1'

      CALL dtrmm( 'Right', 'Upper', 'Transpose', 'Unit', n, k,  &
                  one, v, ldv, work, ldwork )
      IF( m > k ) THEN

!                 W := W + C2'*V2'

        CALL dgemm( 'Transpose', 'Transpose', n, k, m-k, one,  &
                    c( k+1:, : ), ldc, v( :, k+1: ), ldv, one, work, ldwork )
      END IF

!              W := W * T'  or  W * T

      CALL dtrmm( 'Right', 'Upper', transt, 'Non-unit', n, k,  &
                  one, t, ldt, work, ldwork )

!              C := C - V' * W'

      IF( m > k ) THEN

!                 C2 := C2 - V2' * W'

        CALL dgemm( 'Transpose', 'Transpose', m-k, n, k, -one,  &
                    v( :, k+1: ), ldv, work, ldwork, one, c( k+1:, : ), ldc )
      END IF

!              W := W * V1

      CALL dtrmm( 'Right', 'Upper', 'No transpose', 'Unit', n,  &
                  k, one, v, ldv, work, ldwork )

!              C1 := C1 - W'

      DO j = 1, k
        c( j, 1:n ) = c( j, 1:n ) - work( 1:n, j )
      END DO

    ELSE IF( lsame( side, 'R' ) ) THEN

!              Form  C * H  or  C * H'  where  C = ( C1  C2 )

!              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)

!              W := C1

      work(1:m, 1:k) = c(1:m, 1:k)

!              W := W * V1'

      CALL dtrmm( 'Right', 'Upper', 'Transpose', 'Unit', m, k,  &
                  one, v, ldv, work, ldwork )
      IF( n > k ) THEN

!                 W := W + C2 * V2'

        CALL dgemm( 'No transpose', 'Transpose', m, k, n-k, one,  &
                    c( :, k+1: ), ldc, v( :, k+1: ), ldv, one, work, ldwork )
      END IF

!              W := W * T  or  W * T'

      CALL dtrmm( 'Right', 'Upper', trans, 'Non-unit', m, k,  &
                  one, t, ldt, work, ldwork )

!              C := C - W * V

      IF( n > k ) THEN

!                 C2 := C2 - W * V2

        CALL dgemm( 'No transpose', 'No transpose', m, n-k, k, -one,  &
                    work, ldwork, v( :, k+1: ), ldv, one, c( :, k+1: ), ldc )
      END IF

!              W := W * V1

      CALL dtrmm( 'Right', 'Upper', 'No transpose', 'Unit', m,  &
                  k, one, v, ldv, work, ldwork )

!              C1 := C1 - W

      c( 1:m, 1:k ) = c( 1:m, 1:k ) - work( 1:m, 1:k )

    END IF

  ELSE

!           Let  V =  ( V1  V2 )    (V2: last K columns)
!           where  V2  is unit lower triangular.

    IF( lsame( side, 'L' ) ) THEN

!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )

!              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)

!              W := C2'

      DO j = 1, k
        work(1:n, j) = c(m-k+j, 1:n)
      END DO

!              W := W * V2'

      CALL dtrmm( 'Right', 'Lower', 'Transpose', 'Unit', n, k,  &
                  one, v( :, m-k+1: ), ldv, work, ldwork )
      IF( m > k ) THEN

!                 W := W + C1'*V1'

        CALL dgemm( 'Transpose', 'Transpose', n, k, m-k, one,  &
                    c, ldc, v, ldv, one, work, ldwork )
      END IF

!              W := W * T'  or  W * T

      CALL dtrmm( 'Right', 'Lower', transt, 'Non-unit', n, k,  &
                  one, t, ldt, work, ldwork )

!              C := C - V' * W'

      IF( m > k ) THEN

!                 C1 := C1 - V1' * W'

        CALL dgemm( 'Transpose', 'Transpose', m-k, n, k, -one,  &
                    v, ldv, work, ldwork, one, c, ldc )
      END IF

!              W := W * V2

      CALL dtrmm( 'Right', 'Lower', 'No transpose', 'Unit', n,  &
                  k, one, v( :, m-k+1: ), ldv, work, ldwork )

!              C2 := C2 - W'

      DO j = 1, k
        c( m-k+j, 1:n ) = c( m-k+j, 1:n ) - work( 1:n, j )
      END DO

    ELSE IF( lsame( side, 'R' ) ) THEN

!              Form  C * H  or  C * H'  where  C = ( C1  C2 )

!              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)

!              W := C2

      work(1:m, 1:k) = c(1:m, n-k+1:n)

!              W := W * V2'

      CALL dtrmm( 'Right', 'Lower', 'Transpose', 'Unit', m, k,  &
                  one, v( :, n-k+1: ), ldv, work, ldwork )
      IF( n > k ) THEN

!                 W := W + C1 * V1'

        CALL dgemm( 'No transpose', 'Transpose', m, k, n-k,  &
                    one, c, ldc, v, ldv, one, work, ldwork )
      END IF

!              W := W * T  or  W * T'

      CALL dtrmm( 'Right', 'Lower', trans, 'Non-unit', m, k,  &
                  one, t, ldt, work, ldwork )

!              C := C - W * V

      IF( n > k ) THEN

!                 C1 := C1 - W * V1

        CALL dgemm( 'No transpose', 'No transpose', m, n-k, k,  &
                    -one, work, ldwork, v, ldv, one, c, ldc )
      END IF

!              W := W * V2

      CALL dtrmm( 'Right', 'Lower', 'No transpose', 'Unit', m,  &
                  k, one, v( :, n-k+1: ), ldv, work, ldwork )

!              C1 := C1 - W

      DO j = 1, k
        c( 1:m, n-k+j ) = c( 1:m, n-k+j ) - work( 1:m, j )
      END DO

    END IF

  END IF
END IF

DEALLOCATE( work )
RETURN

!     End of DLARFB

END SUBROUTINE dlarfb



SUBROUTINE dlarft( DIRECT, storev, n, k, v, ldv, tau, t, ldt )

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: direct, storev
INTEGER, INTENT(IN)           :: k, ldt, ldv, n
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)     :: tau(:)
REAL (wp), INTENT(IN OUT) :: v(:,:)
REAL (wp), INTENT(OUT)    :: t(:,:)
!     ..

!  Purpose
!  =======

!  DLARFT forms the triangular factor T of a real block reflector H
!  of order n, which is defined as a product of k elementary reflectors.

!  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;

!  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.

!  If STOREV = 'C', the vector which defines the elementary reflector
!  H(i) is stored in the i-th column of the array V, and

!     H  =  I - V * T * V'

!  If STOREV = 'R', the vector which defines the elementary reflector
!  H(i) is stored in the i-th row of the array V, and

!     H  =  I - V' * T * V

!  Arguments
!  =========

!  DIRECT  (input) CHARACTER*1
!          Specifies the order in which the elementary reflectors are
!          multiplied to form the block reflector:
!          = 'F': H = H(1) H(2) . . . H(k) (Forward)
!          = 'B': H = H(k) . . . H(2) H(1) (Backward)

!  STOREV  (input) CHARACTER*1
!          Specifies how the vectors which define the elementary
!          reflectors are stored (see also Further Details):
!          = 'C': columnwise
!          = 'R': rowwise

!  N       (input) INTEGER
!          The order of the block reflector H. N >= 0.

!  K       (input) INTEGER
!          The order of the triangular factor T (= the number of
!          elementary reflectors). K >= 1.

!  V       (input/output) DOUBLE PRECISION array, dimension
!                               (LDV,K) if STOREV = 'C'
!                               (LDV,N) if STOREV = 'R'
!          The matrix V. See further details.

!  LDV     (input) INTEGER
!          The leading dimension of the array V.
!          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.

!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i).

!  T       (output) DOUBLE PRECISION array, dimension (LDT,K)
!          The k by k triangular factor T of the block reflector.
!          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
!          lower triangular. The rest of the array is not used.

!  LDT     (input) INTEGER
!          The leading dimension of the array T. LDT >= K.

!  Further Details
!  ===============

!  The shape of the matrix V and the storage of the vectors which define
!  the H(i) is best illustrated by the following example with n = 5 and
!  k = 3. The elements equal to 1 are not stored; the corresponding
!  array elements are modified but restored on exit. The rest of the
!  array is not used.

!  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':

!               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
!                   ( v1  1    )                     (     1 v2 v2 v2 )
!                   ( v1 v2  1 )                     (        1 v3 v3 )
!                   ( v1 v2 v3 )
!                   ( v1 v2 v3 )

!  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':

!               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
!                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
!                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
!                   (     1 v3 )
!                   (        1 )

!  =====================================================================

!     .. Local Scalars ..
INTEGER   :: i
REAL (wp) :: vii
!     ..
!     .. External Subroutines ..
! EXTERNAL           dgemv, dtrmv
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. Executable Statements ..

!     Quick return if possible

IF( n == 0 ) RETURN

IF( lsame( DIRECT, 'F' ) ) THEN
  DO i = 1, k
    IF( tau( i ) == zero ) THEN

!              H(i)  =  I

      t( 1:i, i ) = zero
    ELSE

!              general case

      vii = v( i, i )
      v( i, i ) = one
      IF( lsame( storev, 'C' ) ) THEN

!                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i)

        CALL dgemv( 'Transpose', n-i+1, i-1, -tau( i ), v( i:, : ), ldv,  &
                    v( i:, i ), 1, zero, t( :, i ), 1 )
      ELSE

!                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)'

        CALL dgemv( 'No transpose', i-1, n-i+1, -tau( i ), v( :, i: ), ldv, &
                    v( i, i: ), 1, zero, t( :, i ), 1 )
      END IF
      v( i, i ) = vii

!              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)

      CALL dtrmv( 'Upper', 'No transpose', 'Non-unit', i-1, t,  &
                  ldt, t( :, i ), 1 )
      t( i, i ) = tau( i )
    END IF
  END DO
ELSE
  DO i = k, 1, -1
    IF( tau( i ) == zero ) THEN

!              H(i)  =  I

      t( i:k, i ) = zero
    ELSE

!              general case

      IF( i < k ) THEN
        IF( lsame( storev, 'C' ) ) THEN
          vii = v( n-k+i, i )
          v( n-k+i, i ) = one

!                    T(i+1:k,i) :=
!                            - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i)

          CALL dgemv( 'Transpose', n-k+i, k-i, -tau( i ), v( :, i+1: ),  &
                      ldv, v( :, i ), 1, zero, t( i+1:, i ), 1 )
          v( n-k+i, i ) = vii
        ELSE
          vii = v( i, n-k+i )
          v( i, n-k+i ) = one

!                    T(i+1:k,i) :=
!                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)'

          CALL dgemv( 'No transpose', k-i, n-k+i, -tau( i ), v( i+1:, : ),  &
                      ldv, v( i, : ), 1, zero, t( i+1:, i ), 1 )
          v( i, n-k+i ) = vii
        END IF

!                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)

        CALL dtrmv( 'Lower', 'No transpose', 'Non-unit', k-i,  &
                    t( i+1:, i+1: ), ldt, t( i+1:, i ), 1 )
      END IF
      t( i, i ) = tau( i )
    END IF
  END DO
END IF
RETURN

!     End of DLARFT

END SUBROUTINE dlarft



SUBROUTINE dlasr( side, pivot, DIRECT, m, n, c, s, a, lda )

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: side, pivot, direct
INTEGER, INTENT(IN)           :: lda, m, n
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: c(:), s(:)
REAL (wp), INTENT(IN OUT)     :: a(:,:)
!     ..

!  Purpose
!  =======

!  DLASR   performs the transformation

!     A := P*A,   when SIDE = 'L' or 'l'  (  Left-hand side )

!     A := A*P',  when SIDE = 'R' or 'r'  ( Right-hand side )

!  where A is an m by n real matrix and P is an orthogonal matrix,
!  consisting of a sequence of plane rotations determined by the
!  parameters PIVOT and DIRECT as follows ( z = m when SIDE = 'L' or 'l'
!  and z = n when SIDE = 'R' or 'r' ):

!  When  DIRECT = 'F' or 'f'  ( Forward sequence ) then

!     P = P( z - 1 )*...*P( 2 )*P( 1 ),

!  and when DIRECT = 'B' or 'b'  ( Backward sequence ) then

!     P = P( 1 )*P( 2 )*...*P( z - 1 ),

!  where  P( k ) is a plane rotation matrix for the following planes:

!     when  PIVOT = 'V' or 'v'  ( Variable pivot ),
!        the plane ( k, k + 1 )

!     when  PIVOT = 'T' or 't'  ( Top pivot ),
!        the plane ( 1, k + 1 )

!     when  PIVOT = 'B' or 'b'  ( Bottom pivot ),
!        the plane ( k, z )

!  c( k ) and s( k )  must contain the  cosine and sine that define the
!  matrix  P( k ).  The two by two plane rotation part of the matrix
!  P( k ), R( k ), is assumed to be of the form

!     R( k ) = (  c( k )  s( k ) ).
!              ( -s( k )  c( k ) )

!  This version vectorises across rows of the array A when SIDE = 'L'.

!  Arguments
!  =========

!  SIDE    (input) CHARACTER*1
!          Specifies whether the plane rotation matrix P is applied to
!          A on the left or the right.
!          = 'L':  Left, compute A := P*A
!          = 'R':  Right, compute A:= A*P'

!  DIRECT  (input) CHARACTER*1
!          Specifies whether P is a forward or backward sequence of
!          plane rotations.
!          = 'F':  Forward, P = P( z - 1 )*...*P( 2 )*P( 1 )
!          = 'B':  Backward, P = P( 1 )*P( 2 )*...*P( z - 1 )

!  PIVOT   (input) CHARACTER*1
!          Specifies the plane for which P(k) is a plane rotation
!          matrix.
!          = 'V':  Variable pivot, the plane (k,k+1)
!          = 'T':  Top pivot, the plane (1,k+1)
!          = 'B':  Bottom pivot, the plane (k,z)

!  M       (input) INTEGER
!          The number of rows of the matrix A.  If m <= 1, an immediate
!          return is effected.

!  N       (input) INTEGER
!          The number of columns of the matrix A.  If n <= 1, an
!          immediate return is effected.

!  C, S    (input) DOUBLE PRECISION arrays, dimension
!                  (M-1) if SIDE = 'L'
!                  (N-1) if SIDE = 'R'
!          c(k) and s(k) contain the cosine and sine that define the
!          matrix P(k).  The two by two plane rotation part of the
!          matrix P(k), R(k), is assumed to be of the form
!          R( k ) = (  c( k )  s( k ) ).
!                   ( -s( k )  c( k ) )

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          The m by n matrix A.  On exit, A is overwritten by P*A if
!          SIDE = 'R' or by A*P' if SIDE = 'L'.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).

!     .. Local Scalars ..
INTEGER ::            i, info, j
REAL (wp) ::   ctemp, stemp, temp
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. External Subroutines ..
! EXTERNAL           xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input parameters

info = 0
IF( .NOT.( lsame( side, 'L' ) .OR. lsame( side, 'R' ) ) ) THEN
  info = 1
ELSE IF( .NOT.( lsame( pivot, 'V' ) .OR. lsame( pivot,  &
  'T' ) .OR. lsame( pivot, 'B' ) ) ) THEN
  info = 2
ELSE IF( .NOT.( lsame( DIRECT, 'F' ) .OR. lsame( DIRECT, 'B' ) ) )  &
  THEN
  info = 3
ELSE IF( m < 0 ) THEN
  info = 4
ELSE IF( n < 0 ) THEN
  info = 5
ELSE IF( lda < MAX( 1, m ) ) THEN
  info = 9
END IF
IF( info /= 0 ) THEN
  CALL erinfo(info, 'DLASR ')
  RETURN
END IF

!     Quick return if possible

IF( ( m == 0 ) .OR. ( n == 0 ) ) RETURN
IF( lsame( side, 'L' ) ) THEN

!        Form  P * A

  IF( lsame( pivot, 'V' ) ) THEN
    IF( lsame( DIRECT, 'F' ) ) THEN
      DO j = 1, m - 1
        ctemp = c( j )
        stemp = s( j )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, n
            temp = a( j+1, i )
            a( j+1, i ) = ctemp*temp - stemp*a( j, i )
            a( j, i ) = stemp*temp + ctemp*a( j, i )
          END DO
        END IF
      END DO
    ELSE IF( lsame( DIRECT, 'B' ) ) THEN
      DO j = m - 1, 1, -1
        ctemp = c( j )
        stemp = s( j )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, n
            temp = a( j+1, i )
            a( j+1, i ) = ctemp*temp - stemp*a( j, i )
            a( j, i ) = stemp*temp + ctemp*a( j, i )
          END DO
        END IF
      END DO
    END IF
  ELSE IF( lsame( pivot, 'T' ) ) THEN
    IF( lsame( DIRECT, 'F' ) ) THEN
      DO j = 2, m
        ctemp = c( j-1 )
        stemp = s( j-1 )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, n
            temp = a( j, i )
            a( j, i ) = ctemp*temp - stemp*a( 1, i )
            a( 1, i ) = stemp*temp + ctemp*a( 1, i )
          END DO
        END IF
      END DO
    ELSE IF( lsame( DIRECT, 'B' ) ) THEN
      DO j = m, 2, -1
        ctemp = c( j-1 )
        stemp = s( j-1 )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, n
            temp = a( j, i )
            a( j, i ) = ctemp*temp - stemp*a( 1, i )
            a( 1, i ) = stemp*temp + ctemp*a( 1, i )
          END DO
        END IF
      END DO
    END IF
  ELSE IF( lsame( pivot, 'B' ) ) THEN
    IF( lsame( DIRECT, 'F' ) ) THEN
      DO j = 1, m - 1
        ctemp = c( j )
        stemp = s( j )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, n
            temp = a( j, i )
            a( j, i ) = stemp*a( m, i ) + ctemp*temp
            a( m, i ) = ctemp*a( m, i ) - stemp*temp
          END DO
        END IF
      END DO
    ELSE IF( lsame( DIRECT, 'B' ) ) THEN
      DO j = m - 1, 1, -1
        ctemp = c( j )
        stemp = s( j )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, n
            temp = a( j, i )
            a( j, i ) = stemp*a( m, i ) + ctemp*temp
            a( m, i ) = ctemp*a( m, i ) - stemp*temp
          END DO
        END IF
      END DO
    END IF
  END IF
ELSE IF( lsame( side, 'R' ) ) THEN

!        Form A * P'

  IF( lsame( pivot, 'V' ) ) THEN
    IF( lsame( DIRECT, 'F' ) ) THEN
      DO j = 1, n - 1
        ctemp = c( j )
        stemp = s( j )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, m
            temp = a( i, j+1 )
            a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
            a( i, j ) = stemp*temp + ctemp*a( i, j )
          END DO
        END IF
      END DO
    ELSE IF( lsame( DIRECT, 'B' ) ) THEN
      DO j = n - 1, 1, -1
        ctemp = c( j )
        stemp = s( j )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, m
            temp = a( i, j+1 )
            a( i, j+1 ) = ctemp*temp - stemp*a( i, j )
            a( i, j ) = stemp*temp + ctemp*a( i, j )
          END DO
        END IF
      END DO
    END IF
  ELSE IF( lsame( pivot, 'T' ) ) THEN
    IF( lsame( DIRECT, 'F' ) ) THEN
      DO j = 2, n
        ctemp = c( j-1 )
        stemp = s( j-1 )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, m
            temp = a( i, j )
            a( i, j ) = ctemp*temp - stemp*a( i, 1 )
            a( i, 1 ) = stemp*temp + ctemp*a( i, 1 )
          END DO
        END IF
      END DO
    ELSE IF( lsame( DIRECT, 'B' ) ) THEN
      DO j = n, 2, -1
        ctemp = c( j-1 )
        stemp = s( j-1 )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, m
            temp = a( i, j )
            a( i, j ) = ctemp*temp - stemp*a( i, 1 )
            a( i, 1 ) = stemp*temp + ctemp*a( i, 1 )
          END DO
        END IF
      END DO
    END IF
  ELSE IF( lsame( pivot, 'B' ) ) THEN
    IF( lsame( DIRECT, 'F' ) ) THEN
      DO j = 1, n - 1
        ctemp = c( j )
        stemp = s( j )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, m
            temp = a( i, j )
            a( i, j ) = stemp*a( i, n ) + ctemp*temp
            a( i, n ) = ctemp*a( i, n ) - stemp*temp
          END DO
        END IF
      END DO
    ELSE IF( lsame( DIRECT, 'B' ) ) THEN
      DO j = n - 1, 1, -1
        ctemp = c( j )
        stemp = s( j )
        IF( ( ctemp /= one ) .OR. ( stemp /= zero ) ) THEN
          DO i = 1, m
            temp = a( i, j )
            a( i, j ) = stemp*a( i, n ) + ctemp*temp
            a( i, n ) = ctemp*a( i, n ) - stemp*temp
          END DO
        END IF
      END DO
    END IF
  END IF
END IF

RETURN

!     End of DLASR

END SUBROUTINE dlasr




SUBROUTINE dlaswp( n, a, k1, k2, ipiv, incx )

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! Argument LDA has been removed.

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: incx, k1, k2, n
!     ..
!     .. Array Arguments ..
INTEGER, INTENT(IN)       :: ipiv(:)
REAL (wp), INTENT(IN OUT) :: a(:,:)
!     ..

!  Purpose
!  =======

!  DLASWP performs a series of row interchanges on the matrix A.
!  One row interchange is initiated for each of rows K1 through K2 of A.

!  Arguments
!  =========

!  N       (input) INTEGER
!          The number of columns of the matrix A.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the matrix of column dimension N to which the row
!          interchanges will be applied.
!          On exit, the permuted matrix.

!  K1      (input) INTEGER
!          The first element of IPIV for which a row interchange will
!          be done.

!  K2      (input) INTEGER
!          The last element of IPIV for which a row interchange will
!          be done.

!  IPIV    (input) INTEGER array, dimension (M*abs(INCX))
!          The vector of pivot indices.  Only the elements in positions
!          K1 through K2 of IPIV are accessed.
!          IPIV(K) = L implies rows K and L are to be interchanged.

!  INCX    (input) INTEGER
!          The increment between successive values of IPIV.  If IPIV
!          is negative, the pivots are applied in reverse order.


!     .. Local Scalars ..
INTEGER ::            i, ip, ix
!     ..
!     .. External Subroutines ..
! EXTERNAL           dswap
!     ..
!     .. Executable Statements ..

!     Interchange row I with row IPIV(I) for each of rows K1 through K2.

IF( incx == 0 ) RETURN
IF( incx > 0 ) THEN
  ix = k1
ELSE
  ix = 1 + ( 1-k2 )*incx
END IF
IF( incx == 1 ) THEN
  DO i = k1, k2
    ip = ipiv( i )
    IF( ip /= i ) CALL dswap( n, a( i, : ), 1, a( ip, : ), 1 )
  END DO
ELSE IF( incx > 1 ) THEN
  DO i = k1, k2
    ip = ipiv( ix )
    IF( ip /= i ) CALL dswap( n, a( i, : ), 1, a( ip, : ), 1 )
    ix = ix + incx
  END DO
ELSE IF( incx < 0 ) THEN
  DO i = k2, k1, -1
    ip = ipiv( ix )
    IF( ip /= i ) CALL dswap( n, a( i, : ), 1, a( ip, : ), 1 )
    ix = ix + incx
  END DO
END IF

RETURN

!     End of DLASWP

END SUBROUTINE dlaswp


SUBROUTINE dlatrd( uplo, n, nb, a, lda, e, tau, w, ldw )

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: uplo
INTEGER, INTENT(IN)           :: lda, ldw, n, nb
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT) :: a(:,:)
REAL (wp), INTENT(OUT)    :: e(:), tau(:), w(:,:)
!     ..

!  Purpose
!  =======

!  DLATRD reduces NB rows and columns of a real symmetric matrix A to
!  symmetric tridiagonal form by an orthogonal similarity
!  transformation Q' * A * Q, and returns the matrices V and W which are
!  needed to apply the transformation to the unreduced part of A.

!  If UPLO = 'U', DLATRD reduces the last NB rows and columns of a
!  matrix, of which the upper triangle is supplied;
!  if UPLO = 'L', DLATRD reduces the first NB rows and columns of a
!  matrix, of which the lower triangle is supplied.

!  This is an auxiliary routine called by DSYTRD.

!  Arguments
!  =========

!  UPLO    (input) CHARACTER
!          Specifies whether the upper or lower triangular part of the
!          symmetric matrix A is stored:
!          = 'U': Upper triangular
!          = 'L': Lower triangular

!  N       (input) INTEGER
!          The order of the matrix A.

!  NB      (input) INTEGER
!          The number of rows and columns to be reduced.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
!          n-by-n upper triangular part of A contains the upper
!          triangular part of the matrix A, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading n-by-n lower triangular part of A contains the lower
!          triangular part of the matrix A, and the strictly upper
!          triangular part of A is not referenced.
!          On exit:
!          if UPLO = 'U', the last NB columns have been reduced to
!            tridiagonal form, with the diagonal elements overwriting
!            the diagonal elements of A; the elements above the diagonal
!            with the array TAU, represent the orthogonal matrix Q as a
!            product of elementary reflectors;
!          if UPLO = 'L', the first NB columns have been reduced to
!            tridiagonal form, with the diagonal elements overwriting
!            the diagonal elements of A; the elements below the diagonal
!            with the array TAU, represent the  orthogonal matrix Q as a
!            product of elementary reflectors.
!          See Further Details.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= (1,N).

!  E       (output) DOUBLE PRECISION array, dimension (N-1)
!          If UPLO = 'U', E(n-nb:n-1) contains the superdiagonal
!          elements of the last NB columns of the reduced matrix;
!          if UPLO = 'L', E(1:nb) contains the subdiagonal elements of
!          the first NB columns of the reduced matrix.

!  TAU     (output) DOUBLE PRECISION array, dimension (N)
!          The scalar factors of the elementary reflectors, stored in
!          TAU(n-nb:n-1) if UPLO = 'U', and in TAU(1:nb) if UPLO = 'L'.
!          See Further Details.

!  W       (output) DOUBLE PRECISION array, dimension (LDW,NB)
!          The n-by-nb matrix W required to update the unreduced part
!          of A.

!  LDW     (input) INTEGER
!          The leading dimension of the array W. LDW >= max(1,N).

!  Further Details
!  ===============

!  If UPLO = 'U', the matrix Q is represented as a product of elementary
!  reflectors

!     Q = H(n) H(n-1) . . . H(n-nb+1).

!  Each H(i) has the form

!     H(i) = I - tau * v * v'

!  where tau is a real scalar, and v is a real vector with
!  v(i:n) = 0 and v(i-1) = 1; v(1:i-1) is stored on exit in A(1:i-1,i),
!  and tau in TAU(i-1).

!  If UPLO = 'L', the matrix Q is represented as a product of elementary
!  reflectors

!     Q = H(1) H(2) . . . H(nb).

!  Each H(i) has the form

!     H(i) = I - tau * v * v'

!  where tau is a real scalar, and v is a real vector with
!  v(1:i) = 0 and v(i+1) = 1; v(i+1:n) is stored on exit in A(i+1:n,i),
!  and tau in TAU(i).

!  The elements of the vectors v together form the n-by-nb matrix V
!  which is needed, with W, to apply the transformation to the unreduced
!  part of the matrix, using a symmetric rank-2k update of the form:
!  A := A - V*W' - W*V'.

!  The contents of A on exit are illustrated by the following examples
!  with n = 5 and nb = 2:

!  if UPLO = 'U':                       if UPLO = 'L':

!    (  a   a   a   v4  v5 )              (  d                  )
!    (      a   a   v4  v5 )              (  1   d              )
!    (          a   1   v5 )              (  v1  1   a          )
!    (              d   1  )              (  v1  v2  a   a      )
!    (                  d  )              (  v1  v2  a   a   a  )

!  where d denotes a diagonal element of the reduced matrix, a denotes
!  an element of the original matrix that is unchanged, and vi denotes
!  an element of the vector defining H(i).

!  =====================================================================

!     .. Parameters ..
REAL (wp), PARAMETER :: half = 0.5_wp
!     ..
!     .. Local Scalars ..
INTEGER   :: i, iw
REAL (wp) :: alpha
!     ..
!     .. External Subroutines ..
! EXTERNAL           daxpy, dgemv, dlarfg, dscal, dsymv
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! DOUBLE PRECISION ::   ddot
! EXTERNAL           lsame, ddot
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MIN
!     ..
!     .. Executable Statements ..

!     Quick return if possible

IF( n <= 0 ) RETURN

IF( lsame( uplo, 'U' ) ) THEN

!        Reduce last NB columns of upper triangle

  DO i = n, n - nb + 1, -1
    iw = i - n + nb
    IF( i < n ) THEN

!              Update A(1:i,i)

      CALL dgemv( 'No transpose', i, n-i, -one, a( :, i+1: ),  &
                  lda, w( i, iw+1: ), 1, one, a( :, i ), 1 )
      CALL dgemv( 'No transpose', i, n-i, -one, w( :, iw+1: ),  &
                  ldw, a( i, i+1: ), 1, one, a( :, i ), 1 )
    END IF
    IF( i > 1 ) THEN

!              Generate elementary reflector H(i) to annihilate
!              A(1:i-2,i)

      CALL dlarfg( i-1, a( i-1, i ), a( :, i ), 1, tau( i-1 ) )
      e( i-1 ) = a( i-1, i )
      a( i-1, i ) = one

!              Compute W(1:i-1,i)

      CALL dsymv( 'Upper', i-1, one, a, lda, a( :, i ), 1, zero, w( :, iw ), &
                  1 )
      IF( i < n ) THEN
        CALL dgemv( 'Transpose', i-1, n-i, one, w( :, iw+1: ),  &
                    ldw, a( :, i ), 1, zero, w( i+1:, iw ), 1 )
        CALL dgemv( 'No transpose', i-1, n-i, -one, a( :, i+1: ),  &
                    lda, w( i+1:, iw ), 1, one, w( :, iw ), 1 )
        CALL dgemv( 'Transpose', i-1, n-i, one, a( :, i+1: ),  &
                    lda, a( :, i ), 1, zero, w( i+1:, iw ), 1 )
        CALL dgemv( 'No transpose', i-1, n-i, -one, w( :, iw+1: ),  &
                    ldw, w( i+1:, iw ), 1, one, w( :, iw ), 1 )
      END IF
      w(1:i-1, iw) = tau(i-1) * w(1:i-1, iw)
      alpha = -half*tau( i-1 ) * ddot( i-1, w( :, iw ), 1, a( :, i ), 1 )
      CALL daxpy( i-1, alpha, a( :, i ), 1, w( :, iw ), 1 )
    END IF

  END DO
ELSE

!        Reduce first NB columns of lower triangle

  DO i = 1, nb

!           Update A(i:n,i)

    CALL dgemv( 'No transpose', n-i+1, i-1, -one, a( i:, : ),  &
                lda, w( i, : ), 1, one, a( i:, i ), 1 )
    CALL dgemv( 'No transpose', n-i+1, i-1, -one, w( i:, : ),  &
                ldw, a( i, : ), 1, one, a( i:, i ), 1 )
    IF( i < n ) THEN

!              Generate elementary reflector H(i) to annihilate
!              A(i+2:n,i)

      CALL dlarfg( n-i, a( i+1, i ), a( MIN( i+2, n ):, i ), 1, tau( i ) )
      e( i ) = a( i+1, i )
      a( i+1, i ) = one

!              Compute W(i+1:n,i)

      CALL dsymv( 'Lower', n-i, one, a( i+1:, i+1: ), lda,  &
                  a( i+1:, i ), 1, zero, w( i+1:, i ), 1 )
      CALL dgemv( 'Transpose', n-i, i-1, one, w( i+1:, : ), ldw,  &
                  a( i+1:, i ), 1, zero, w( :, i ), 1 )
      CALL dgemv( 'No transpose', n-i, i-1, -one, a( i+1:, : ),  &
                  lda, w( :, i ), 1, one, w( i+1:, i ), 1 )
      CALL dgemv( 'Transpose', n-i, i-1, one, a( i+1:, : ), lda,  &
                  a( i+1:, i ), 1, zero, w( :, i ), 1 )
      CALL dgemv( 'No transpose', n-i, i-1, -one, w( i+1:, : ),  &
                  ldw, w( :, i ), 1, one, w( i+1:, i ), 1 )
      w(i+1:n, i) = tau(i) * w(i+1:n, i)
      alpha = -half*tau( i ) * ddot( n-i, w( i+1:, i ), 1, a( i+1:, i ), 1 )
      CALL daxpy( n-i, alpha, a( i+1:, i ), 1, w( i+1:, i ), 1 )
    END IF

  END DO
END IF

RETURN

!     End of DLATRD

END SUBROUTINE dlatrd



SUBROUTINE dlaset( uplo, m, n, alpha, beta, a)

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! Last argument, LDA, has been deleted.

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: uplo
INTEGER, INTENT(IN)           :: m, n
REAL (wp), INTENT(IN)         :: alpha, beta
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT)     :: a(:,:)
!     ..

!  Purpose
!  =======

!  DLASET initializes an m-by-n matrix A to BETA on the diagonal and
!  ALPHA on the offdiagonals.

!  Arguments
!  =========

!  UPLO    (input) CHARACTER*1
!          Specifies the part of the matrix A to be set.
!          = 'U':      Upper triangular part is set; the strictly lower
!                      triangular part of A is not changed.
!          = 'L':      Lower triangular part is set; the strictly upper
!                      triangular part of A is not changed.
!          Otherwise:  All of the matrix A is set.

!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.

!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.

!  ALPHA   (input) DOUBLE PRECISION
!          The constant to which the offdiagonal elements are to be set.

!  BETA    (input) DOUBLE PRECISION
!          The constant to which the diagonal elements are to be set.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On exit, the leading m-by-n submatrix of A is set as follows:

!          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
!          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
!          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,

!          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).


!     .. Local Scalars ..
INTEGER ::            i, j
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MIN
!     ..
!     .. Executable Statements ..

IF( lsame( uplo, 'U' ) ) THEN

!        Set the strictly upper triangular or trapezoidal part of the
!        array to ALPHA.

  DO j = 2, n
    DO i = 1, MIN( j-1, m )
      a( i, j ) = alpha
    END DO
  END DO

ELSE IF( lsame( uplo, 'L' ) ) THEN

!        Set the strictly lower triangular or trapezoidal part of the
!        array to ALPHA.

  DO j = 1, MIN( m, n )
    a( j+1:m, j ) = alpha
  END DO

ELSE

!        Set the leading m-by-n submatrix to ALPHA.

  a( 1:m, 1:n ) = alpha
END IF

!     Set the first min(M,N) diagonal elements to BETA.

DO i = 1, MIN( m, n )
  a( i, i ) = beta
END DO

RETURN

!     End of DLASET

END SUBROUTINE dlaset


SUBROUTINE dlassq( n, x, incx, scale, sumsq )


!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: incx, n
REAL (wp), INTENT(IN OUT) :: scale, sumsq
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)     ::   x(:)
!     ..

!  Purpose
!  =======

!  DLASSQ  returns the values  scl  and  smsq  such that

!     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,

!  where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
!  assumed to be non-negative and  scl  returns the value

!     scl = max( scale, abs( x( i ) ) ).

!  scale and sumsq must be supplied in SCALE and SUMSQ and
!  scl and smsq are overwritten on SCALE and SUMSQ respectively.

!  The routine makes only one pass through the vector x.

!  Arguments
!  =========

!  N       (input) INTEGER
!          The number of elements to be used from the vector X.

!  X       (input) DOUBLE PRECISION
!          The vector for which a scaled sum of squares is computed.
!             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.

!  INCX    (input) INTEGER
!          The increment between successive values of the vector X.
!          INCX > 0.

!  SCALE   (input/output) DOUBLE PRECISION
!          On entry, the value  scale  in the equation above.
!          On exit, SCALE is overwritten with  scl , the scaling factor
!          for the sum of squares.

!  SUMSQ   (input/output) DOUBLE PRECISION
!          On entry, the value  sumsq  in the equation above.
!          On exit, SUMSQ is overwritten with  smsq , the basic sum of
!          squares from which  scl  has been factored out.


!     .. Local Scalars ..
INTEGER   :: ix
REAL (wp) :: absxi
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS
!     ..
!     .. Executable Statements ..

IF( n > 0 ) THEN
  DO ix = 1, 1 + ( n-1 )*incx, incx
    IF( x( ix ) /= zero ) THEN
      absxi = ABS( x( ix ) )
      IF( scale < absxi ) THEN
        sumsq = 1 + sumsq*( scale / absxi )**2
        scale = absxi
      ELSE
        sumsq = sumsq + ( absxi / scale )**2
      END IF
    END IF
  END DO
END IF
RETURN

!     End of DLASSQ

END SUBROUTINE dlassq



FUNCTION dlapy2( x, y ) RESULT(fn_val)

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
REAL (wp)             :: fn_val
REAL (wp), INTENT(IN) :: x, y
!     ..

!  Purpose
!  =======

!  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
!  overflow.

!  Arguments
!  =========

!  X       (input) DOUBLE PRECISION
!  Y       (input) DOUBLE PRECISION
!          X and Y specify the values x and y.

!     .. Local Scalars ..
REAL (wp) :: w, xabs, yabs, z
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS, MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..

xabs = ABS( x )
yabs = ABS( y )
w = MAX( xabs, yabs )
z = MIN( xabs, yabs )
IF( z == zero ) THEN
  fn_val = w
ELSE
  fn_val = w*SQRT( one+( z / w )**2 )
END IF
RETURN

!     End of DLAPY2

END FUNCTION dlapy2


FUNCTION dlanst( norm, n, d, e ) RESULT(fn_val)

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: norm
REAL (wp)                     :: fn_val
INTEGER, INTENT(IN)           :: n
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: d(:), e(:)
!     ..

!  Purpose
!  =======

!  DLANST  returns the value of the one norm,  or the Frobenius norm, or
!  the  infinity norm,  or the  element of  largest absolute value  of a
!  real symmetric tridiagonal matrix A.

!  Description
!  ===========

!  DLANST returns the value

!     DLANST = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!              (
!              ( norm1(A),         NORM = '1', 'O' or 'o'
!              (
!              ( normI(A),         NORM = 'I' or 'i'
!              (
!              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'

!  where  norm1  denotes the  one norm of a matrix (maximum column sum),
!  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!  normF  denotes the  Frobenius norm of a matrix (square root of sum of
!  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.

!  Arguments
!  =========

!  NORM    (input) CHARACTER*1
!          Specifies the value to be returned in DLANST as described
!          above.

!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.  When N = 0, DLANST is
!          set to zero.

!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The diagonal elements of A.

!  E       (input) DOUBLE PRECISION array, dimension (N-1)
!          The (n-1) sub-diagonal or super-diagonal elements of A.

!  =====================================================================

!     .. Local Scalars ..
INTEGER   :: i
REAL (wp) :: anorm, scale, sum
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlassq
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS, MAX, SQRT
!     ..
!     .. Executable Statements ..

IF( n <= 0 ) THEN
  anorm = zero
ELSE IF( lsame( norm, 'M' ) ) THEN

!        Find max(abs(A(i,j))).

  anorm = ABS( d( n ) )
  DO i = 1, n - 1
    anorm = MAX( anorm, ABS( d( i ) ) )
    anorm = MAX( anorm, ABS( e( i ) ) )
  END DO
ELSE IF( lsame( norm, 'O' ) .OR. norm == '1' .OR.  &
  lsame( norm, 'I' ) ) THEN

!        Find norm1(A).

  IF( n == 1 ) THEN
    anorm = ABS( d( 1 ) )
  ELSE
    anorm = MAX( ABS( d( 1 ) )+ABS( e( 1 ) ),ABS( e( n-1 ) )+ABS( d( n ) ) )
    DO i = 2, n - 1
      anorm = MAX( anorm, ABS( d( i ) )+ABS( e( i ) )+ABS( e( i-1 ) ) )
    END DO
  END IF
ELSE IF( ( lsame( norm, 'F' ) ) .OR. ( lsame( norm, 'E' ) ) ) THEN

!        Find normF(A).

  scale = zero
  sum = one
  IF( n > 1 ) THEN
    CALL dlassq( n-1, e, 1, scale, sum )
    sum = 2*sum
  END IF
  CALL dlassq( n, d, 1, scale, sum )
  anorm = scale*SQRT( sum )
END IF

fn_val = anorm
RETURN

!     End of DLANST

END FUNCTION dlanst


FUNCTION dlansy( norm, uplo, n, a, lda ) RESULT(value)

!  -- LAPACK auxiliary routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. The last argument, WORK, has been removed.

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: norm, uplo
REAL (wp)                     :: value
INTEGER, INTENT(IN)           :: lda, n
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: a(:,:)
!     ..

!  Purpose
!  =======

!  DLANSY  returns the value of the one norm,  or the Frobenius norm, or
!  the  infinity norm,  or the  element of  largest absolute value  of a
!  real symmetric matrix A.

!  Description
!  ===========

!  DLANSY returns the value

!     DLANSY = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!              (
!              ( norm1(A),         NORM = '1', 'O' or 'o'
!              (
!              ( normI(A),         NORM = 'I' or 'i'
!              (
!              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'

!  where  norm1  denotes the  one norm of a matrix (maximum column sum),
!  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!  normF  denotes the  Frobenius norm of a matrix (square root of sum of
!  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.

!  Arguments
!  =========

!  NORM    (input) CHARACTER*1
!          Specifies the value to be returned in DLANSY as described
!          above.

!  UPLO    (input) CHARACTER*1
!          Specifies whether the upper or lower triangular part of the
!          symmetric matrix A is to be referenced.
!          = 'U':  Upper triangular part of A is referenced
!          = 'L':  Lower triangular part of A is referenced

!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.  When N = 0, DLANSY is
!          set to zero.

!  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
!          The symmetric matrix A.  If UPLO = 'U', the leading n by n
!          upper triangular part of A contains the upper triangular part
!          of the matrix A, and the strictly lower triangular part of A
!          is not referenced.  If UPLO = 'L', the leading n by n lower
!          triangular part of A contains the lower triangular part of
!          the matrix A, and the strictly upper triangular part of A is
!          not referenced.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(N,1).

!     .. Local Scalars ..
INTEGER   :: i, j
REAL (wp) :: absa, scale, sum
REAL (wp) :: work(n)
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlassq
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS, MAX, SQRT
!     ..
!     .. Executable Statements ..

IF( n == 0 ) THEN
  value = zero
ELSE IF( lsame( norm, 'M' ) ) THEN

!        Find max(abs(A(i,j))).

  value = zero
  IF( lsame( uplo, 'U' ) ) THEN
    DO j = 1, n
      DO i = 1, j
        value = MAX( value, ABS( a( i, j ) ) )
      END DO
    END DO
  ELSE
    DO j = 1, n
      DO i = j, n
        value = MAX( value, ABS( a( i, j ) ) )
      END DO
    END DO
  END IF
ELSE IF( ( lsame( norm, 'I' ) ) .OR. ( lsame( norm, 'O' ) ) .OR.  &
  ( norm == '1' ) ) THEN

!        Find normI(A) ( = norm1(A), since A is symmetric).

  value = zero
  IF( lsame( uplo, 'U' ) ) THEN
    DO j = 1, n
      sum = zero
      DO i = 1, j - 1
        absa = ABS( a( i, j ) )
        sum = sum + absa
        work( i ) = work( i ) + absa
      END DO
      work( j ) = sum + ABS( a( j, j ) )
    END DO
    value = MAXVAL( work(1:n) )
  ELSE
    work( 1:n ) = zero
    DO j = 1, n
      sum = work( j ) + ABS( a( j, j ) )
      DO i = j + 1, n
        absa = ABS( a( i, j ) )
        sum = sum + absa
        work( i ) = work( i ) + absa
      END DO
      value = MAX( value, sum )
    END DO
  END IF
ELSE IF( ( lsame( norm, 'F' ) ) .OR. ( lsame( norm, 'E' ) ) ) THEN

!        Find normF(A).

  scale = zero
  sum = one
  IF( lsame( uplo, 'U' ) ) THEN
    DO j = 2, n
      CALL dlassq( j-1, a( :, j ), 1, scale, sum )
    END DO
  ELSE
    DO j = 1, n - 1
      CALL dlassq( n-j, a( j+1:, j ), 1, scale, sum )
    END DO
  END IF
  sum = 2*sum
  CALL dlassq( n, a(:,1), lda+1, scale, sum )
  value = scale*SQRT( sum )
END IF

RETURN

!     End of DLANSY

END FUNCTION dlansy



END MODULE dla

MODULE dor
! This module contains translations to ELF90 compatability of the
! LAPACK Orthogonal Real routines

! Translated by Alan Miller
! alan @ vic.cmis.csiro.au    URL: www.ozemail.com.au/~milleraj

! Latest revision - 30 October 1997

USE la_precision, ONLY: wp => dp
USE la_auxmod
USE dblas
USE dla, ONLY: dlarf, dlarft, dlarfb
IMPLICIT NONE

CONTAINS



SUBROUTINE dorg2l( m, n, k, a, lda, tau, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. The workspace has been removed from the list of arguments.

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: k, lda, m, n
INTEGER, INTENT(OUT)      :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT) :: a(:,:)
REAL (wp), INTENT(IN)     :: tau(:)
!     ..

!  Purpose
!  =======

!  DORG2L generates an m by n real matrix Q with orthonormal columns,
!  which is defined as the last n columns of a product of k elementary
!  reflectors of order m

!        Q  =  H(k) . . . H(2) H(1)

!  as returned by DGEQLF.

!  Arguments
!  =========

!  M       (input) INTEGER
!          The number of rows of the matrix Q. M >= 0.

!  N       (input) INTEGER
!          The number of columns of the matrix Q. M >= N >= 0.

!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines the
!          matrix Q. N >= K >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the (n-k+i)-th column must contain the vector which
!          defines the elementary reflector H(i), for i = 1,2,...,k, as
!          returned by DGEQLF in the last k columns of its array
!          argument A.
!          On exit, the m by n matrix Q.

!  LDA     (input) INTEGER
!          The first dimension of the array A. LDA >= max(1,M).

!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DGEQLF.

!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument has an illegal value

!  =====================================================================

!     .. Local Scalars ..
INTEGER   :: i, ii, j, l
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlarf, dscal, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input arguments

info = 0
IF( m < 0 ) THEN
  info = -1
ELSE IF( n < 0 .OR. n > m ) THEN
  info = -2
ELSE IF( k < 0 .OR. k > n ) THEN
  info = -3
ELSE IF( lda < MAX( 1, m ) ) THEN
  info = -5
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DORG2L')
  RETURN
END IF

!     Quick return if possible

IF( n <= 0 ) RETURN

!     Initialise columns 1:n-k to columns of the unit matrix

DO j = 1, n - k
  DO l = 1, m
    a( l, j ) = zero
  END DO
  a( m-n+j, j ) = one
END DO

DO i = 1, k
  ii = n - k + i

!        Apply H(i) to A(1:m-k+i,1:n-k+i) from the left

  a( m-n+ii, ii ) = one
  CALL dlarf( 'Left', m-n+ii, ii-1, a( :, ii ), 1, tau( i ), a, lda )
  CALL dscal( m-n+ii-1, -tau( i ), a( :, ii ), 1 )
  a( m-n+ii, ii ) = one - tau( i )

!        Set A(m-k+i+1:m,n-k+i) to zero

  DO l = m - n + ii + 1, m
    a( l, ii ) = zero
  END DO
END DO
RETURN

!     End of DORG2L

END SUBROUTINE dorg2l


SUBROUTINE dorg2r( m, n, k, a, lda, tau, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. The workspace has been removed from the list of arguments.

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: k, lda, m, n
INTEGER, INTENT(OUT)      :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)     :: tau(:)
REAL (wp), INTENT(IN OUT) :: a(:,:)
!     ..

!  Purpose
!  =======

!  DORG2R generates an m by n real matrix Q with orthonormal columns,
!  which is defined as the first n columns of a product of k elementary
!  reflectors of order m

!        Q  =  H(1) H(2) . . . H(k)

!  as returned by DGEQRF.

!  Arguments
!  =========

!  M       (input) INTEGER
!          The number of rows of the matrix Q. M >= 0.

!  N       (input) INTEGER
!          The number of columns of the matrix Q. M >= N >= 0.

!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines the
!          matrix Q. N >= K >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the i-th column must contain the vector which
!          defines the elementary reflector H(i), for i = 1,2,...,k, as
!          returned by DGEQRF in the first k columns of its array
!          argument A.
!          On exit, the m-by-n matrix Q.

!  LDA     (input) INTEGER
!          The first dimension of the array A. LDA >= max(1,M).

!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DGEQRF.

!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument has an illegal value

!  =====================================================================

!     .. Local Scalars ..
INTEGER   :: i, j, l
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlarf, dscal, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input arguments

info = 0
IF( m < 0 ) THEN
  info = -1
ELSE IF( n < 0 .OR. n > m ) THEN
  info = -2
ELSE IF( k < 0 .OR. k > n ) THEN
  info = -3
ELSE IF( lda < MAX( 1, m ) ) THEN
  info = -5
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DORG2R')
  RETURN
END IF

!     Quick return if possible

IF( n <= 0 ) RETURN

!     Initialise columns k+1:n to columns of the unit matrix

DO j = k + 1, n
  DO l = 1, m
    a( l, j ) = zero
  END DO
  a( j, j ) = one
END DO

DO i = k, 1, -1

!        Apply H(i) to A(i:m,i:n) from the left

  IF( i < n ) THEN
    a( i, i ) = one
    CALL dlarf( 'Left', m-i+1, n-i, a( i:, i ), 1, tau( i ),  &
                a( i:, i+1: ), lda )
  END IF
  IF( i < m ) CALL dscal( m-i, -tau( i ), a( i+1:, i ), 1 )
  a( i, i ) = one - tau( i )

!        Set A(1:i-1,i) to zero

  DO l = 1, i - 1
    a( l, i ) = zero
  END DO
END DO
RETURN

!     End of DORG2R

END SUBROUTINE dorg2r



SUBROUTINE dorgql( m, n, k, a, lda, tau, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. Arguments WORK and LWORK have been removed.

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: k, lda, m, n
INTEGER, INTENT(OUT)      :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)     :: tau(:)
REAL (wp), INTENT(IN OUT) :: a(:,:)
!     ..

!  Purpose
!  =======

!  DORGQL generates an m by n real matrix Q with orthonormal columns,
!  which is defined as the last n columns of a product of k elementary
!  reflectors of order m

!        Q  =  H(k) . . . H(2) H(1)

!  as returned by DGEQLF.

!  Arguments
!  =========

!  M       (input) INTEGER
!          The number of rows of the matrix Q. M >= 0.

!  N       (input) INTEGER
!          The number of columns of the matrix Q. M >= N >= 0.

!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines the
!          matrix Q. N >= K >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the (n-k+i)-th column must contain the vector which
!          defines the elementary reflector H(i), for i = 1,2,...,k, as
!          returned by DGEQLF in the last k columns of its array
!          argument A.
!          On exit, the m by n matrix Q.

!  LDA     (input) INTEGER
!          The first dimension of the array A. LDA >= max(1,M).

!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DGEQLF.

!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument has an illegal value

!  =====================================================================

!     .. Local Scalars ..
INTEGER                :: i, ib, j, kk, l, nb, nbmin, nx
REAL (wp)              :: t(k,k)
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlarfb, dlarft, dorg2l, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX, MIN
!     ..
!     .. External Functions ..
! INTEGER ::            ilaenv
! EXTERNAL           ilaenv
!     ..
!     .. Executable Statements ..

!     Test the input arguments

info = 0
IF( m < 0 ) THEN
  info = -1
ELSE IF( n < 0 .OR. n > m ) THEN
  info = -2
ELSE IF( k < 0 .OR. k > n ) THEN
  info = -3
ELSE IF( lda < MAX( 1, m ) ) THEN
  info = -5
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DORGQL')
  RETURN
END IF

!     Quick return if possible

IF( n <= 0 ) THEN
  RETURN
END IF

!     Determine the block size.

nb = ilaenv( 1, 'DORGQL', ' ', m, n, k, -1 )
nbmin = 2
nx = 0
IF( nb > 1 .AND. nb < k ) THEN

!        Determine when to cross over from blocked to unblocked code.

  nx = MAX( 0, ilaenv( 3, 'DORGQL', ' ', m, n, k, -1 ) )
END IF

IF( nb >= nbmin .AND. nb < k .AND. nx < k ) THEN

!        Use blocked code after the first block.
!        The last kk columns are handled by the block method.

  kk = MIN( k, ( ( k-nx+nb-1 ) / nb )*nb )

!        Set A(m-kk+1:m,1:n-kk) to zero.

  DO j = 1, n - kk
    DO i = m - kk + 1, m
      a( i, j ) = zero
    END DO
  END DO
ELSE
  kk = 0
END IF

!     Use unblocked code for the first or only block.

CALL dorg2l( m-kk, n-kk, k-kk, a, lda, tau, info )

IF( kk > 0 ) THEN

!        Use blocked code

  DO i = k - kk + 1, k, nb
    ib = MIN( nb, k-i+1 )
    IF( n-k+i > 1 ) THEN

!              Form the triangular factor of the block reflector
!              H = H(i+ib-1) . . . H(i+1) H(i)

      CALL dlarft( 'Backward', 'Columnwise', m-k+i+ib-1, ib,  &
                   a( :, n-k+i: ), lda, tau( i: ), t, k )

!              Apply H to A(1:m-k+i+ib-1,1:n-k+i-1) from the left

      CALL dlarfb( 'Left', 'No transpose', 'Backward', 'Columnwise',  &
                  m-k+i+ib-1, n-k+i-1, ib, a( :, n-k+i: ), lda, t, k, a, lda )
    END IF

!           Apply H to rows 1:m-k+i+ib-1 of current block

    CALL dorg2l( m-k+i+ib-1, ib, ib, a( :, n-k+i: ), lda, tau( i: ), info )

!           Set rows m-k+i+ib:m of current block to zero

    DO j = n - k + i, n - k + i + ib - 1
      DO l = m - k + i + ib, m
        a( l, j ) = zero
      END DO
    END DO
  END DO
END IF

RETURN

!     End of DORGQL

END SUBROUTINE dorgql


SUBROUTINE dorgqr( m, n, k, a, lda, tau, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. Arguments WORK and LWORK have been removed.

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: k, lda, m, n
INTEGER, INTENT(OUT)      :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)     :: tau(:)
REAL (wp), INTENT(IN OUT) :: a(:,:)
!     ..

!  Purpose
!  =======

!  DORGQR generates an m by n real matrix Q with orthonormal columns,
!  which is defined as the first n columns of a product of k elementary
!  reflectors of order m

!        Q  =  H(1) H(2) . . . H(k)

!  as returned by DGEQRF.

!  Arguments
!  =========

!  M       (input) INTEGER
!          The number of rows of the matrix Q. M >= 0.

!  N       (input) INTEGER
!          The number of columns of the matrix Q. M >= N >= 0.

!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines the
!          matrix Q. N >= K >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the i-th column must contain the vector which
!          defines the elementary reflector H(i), for i = 1,2,...,k, as
!          returned by DGEQRF in the first k columns of its array
!          argument A.
!          On exit, the m by n matrix Q.

!  LDA     (input) INTEGER
!          The first dimension of the array A. LDA >= max(1,M).

!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DGEQRF.

!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument has an illegal value

!  =====================================================================

!     .. Local Scalars ..
INTEGER                :: i, ib, j, ki, kk, l, nb, nbmin, nx
REAL (wp)              :: t(k,k)
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlarfb, dlarft, dorg2r, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX, MIN
!     ..
!     .. External Functions ..
! INTEGER ::            ilaenv
! EXTERNAL           ilaenv
!     ..
!     .. Executable Statements ..

!     Test the input arguments

info = 0
IF( m < 0 ) THEN
  info = -1
ELSE IF( n < 0 .OR. n > m ) THEN
  info = -2
ELSE IF( k < 0 .OR. k > n ) THEN
  info = -3
ELSE IF( lda < MAX( 1, m ) ) THEN
  info = -5
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DORGQR')
  RETURN
END IF

!     Quick return if possible

IF( n <= 0 ) THEN
  RETURN
END IF

!     Determine the block size.

nb = ilaenv( 1, 'DORGQR', ' ', m, n, k, -1 )
nbmin = 2
nx = 0
IF( nb > 1 .AND. nb < k ) THEN

!        Determine when to cross over from blocked to unblocked code.

  nx = MAX( 0, ilaenv( 3, 'DORGQR', ' ', m, n, k, -1 ) )
END IF

IF( nb >= nbmin .AND. nb < k .AND. nx < k ) THEN

!        Use blocked code after the last block.
!        The first kk columns are handled by the block method.

  ki = ( ( k-nx-1 ) / nb )*nb
  kk = MIN( k, ki+nb )

!        Set A(1:kk,kk+1:n) to zero.

  DO j = kk + 1, n
    DO i = 1, kk
      a( i, j ) = zero
    END DO
  END DO
ELSE
  kk = 0
END IF

!     Use unblocked code for the last or only block.

IF( kk < n ) CALL dorg2r( m-kk, n-kk, k-kk, a( kk+1:, kk+1: ), lda,  &
                          tau( kk+1: ), info )

IF( kk > 0 ) THEN

!        Use blocked code

  DO i = ki + 1, 1, -nb
    ib = MIN( nb, k-i+1 )
    IF( i+ib <= n ) THEN

!              Form the triangular factor of the block reflector
!              H = H(i) H(i+1) . . . H(i+ib-1)

      CALL dlarft( 'Forward', 'Columnwise', m-i+1, ib,  &
                   a( i:, i: ), lda, tau( i: ), t, k )

!              Apply H to A(i:m,i+ib:n) from the left

      CALL dlarfb( 'Left', 'No transpose', 'Forward', 'Columnwise', m-i+1,  &
                   n-i-ib+1, ib, a( i:, i: ), lda, t, k, a( i:, i+ib: ), lda )
    END IF

!           Apply H to rows i:m of current block

    CALL dorg2r( m-i+1, ib, ib, a( i:, i: ), lda, tau( i: ), info )

!           Set rows 1:i-1 of current block to zero

    DO j = i, i + ib - 1
      DO l = 1, i - 1
        a( l, j ) = zero
      END DO
    END DO
  END DO
END IF

RETURN

!     End of DORGQR

END SUBROUTINE dorgqr



SUBROUTINE dorgtr( uplo, n, a, lda, tau, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. Arguments WORK and LWORK have been removed.

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: uplo
INTEGER, INTENT(IN)           :: lda, n
INTEGER, INTENT(OUT)          :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN)         :: tau(:)
REAL (wp), INTENT(IN OUT)     :: a(:,:)
!     ..

!  Purpose
!  =======

!  DORGTR generates a real orthogonal matrix Q which is defined as the
!  product of n-1 elementary reflectors of order n, as returned by
!  DSYTRD:

!  if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),

!  if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).

!  Arguments
!  =========

!  UPLO    (input) CHARACTER*1
!          Specifies whether the upper or lower triangle of the array A
!          holds details of the elementary reflectors, as returned by
!          DSYTRD:
!          = 'U': Upper triangle;
!          = 'L': Lower triangle.

!  N       (input) INTEGER
!          The order of the matrix Q. N >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the vectors which define the elementary reflectors,
!          as returned by DSYTRD.
!          On exit, the n by n orthogonal matrix Q.

!  LDA     (input) INTEGER
!          The leading dimension of the array A. LDA >= max(1,N).

!  TAU     (input) DOUBLE PRECISION array, dimension (N-1)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DSYTRD.

!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value

!  =====================================================================

!     .. Local Scalars ..
LOGICAL :: upper
INTEGER :: i, j
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. External Subroutines ..
! EXTERNAL           dorgql, dorgqr, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input arguments

info = 0
upper = lsame( uplo, 'U' )
IF( .NOT.upper .AND. .NOT.lsame( uplo, 'L' ) ) THEN
  info = -1
ELSE IF( n < 0 ) THEN
  info = -2
ELSE IF( lda < MAX( 1, n ) ) THEN
  info = -4
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DORGTR')
  RETURN
END IF

!     Quick return if possible

IF( n == 0 ) THEN
  RETURN
END IF

IF( upper ) THEN

!        Q was determined by a call to DSYTRD with UPLO = 'U'

!        Shift the vectors which define the elementary reflectors one
!        column to the left, and set the last row and column of Q to
!        those of the unit matrix

  DO j = 1, n - 1
    DO i = 1, j - 1
      a( i, j ) = a( i, j+1 )
    END DO
    a( n, j ) = zero
  END DO
  DO i = 1, n - 1
    a( i, n ) = zero
  END DO
  a( n, n ) = one

!        Generate Q(1:n-1,1:n-1)

  CALL dorgql( n-1, n-1, n-1, a, lda, tau, info )

ELSE

!        Q was determined by a call to DSYTRD with UPLO = 'L'.

!        Shift the vectors which define the elementary reflectors one
!        column to the right, and set the first row and column of Q to
!        those of the unit matrix

  DO j = n, 2, -1
    a( 1, j ) = zero
    DO i = j + 1, n
      a( i, j ) = a( i, j-1 )
    END DO
  END DO
  a( 1, 1 ) = one
  DO i = 2, n
    a( i, 1 ) = zero
  END DO
  IF( n > 1 ) THEN

!           Generate Q(2:n,2:n)

    CALL dorgqr( n-1, n-1, n-1, a( 2:, 2: ), lda, tau, info )
  END IF
END IF

RETURN

!     End of DORGTR

END SUBROUTINE dorgtr


END MODULE dor

MODULE dge
! This module contains translations to ELF90 compatability of the
! LAPACK GEneral routines + DBDSQR

! Translated by Alan Miller
! alan @ vic.cmis.csiro.au    URL: www.ozemail.com.au/~milleraj

! Latest revision - 10 November 1997

USE la_precision, ONLY: wp => dp
USE la_auxmod
USE dblas

CONTAINS



SUBROUTINE dgetrf( m, n, a, lda, ipiv, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

USE dblas3, ONLY: dgemm, dtrsm
USE dla,    ONLY: dlaswp
IMPLICIT NONE

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: lda, m, n
INTEGER, INTENT(OUT)      :: info
!     ..
!     .. Array Arguments ..
INTEGER, INTENT(OUT)      :: ipiv(:)
REAL (wp), INTENT(IN OUT) :: a(:,:)
!     ..

!  Purpose
!  =======

!  DGETRF computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.

!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).

!  This is the right-looking Level 3 BLAS version of the algorithm.

!  Arguments
!  =========

!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.

!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the m by n matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).

!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).

!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!               has been completed, but the factor U is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.

!  =====================================================================

!     .. Local Scalars ..
INTEGER :: i, iinfo, j, jb, nb
!     ..
!     .. External Subroutines ..
! EXTERNAL           dgemm, dgetf2, dlaswp, dtrsm, xerbla
!     ..
!     .. External Functions ..
! INTEGER ::            ilaenv
! EXTERNAL           ilaenv
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
IF( m < 0 ) THEN
  info = -1
ELSE IF( n < 0 ) THEN
  info = -2
ELSE IF( lda < MAX( 1, m ) ) THEN
  info = -4
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DGETRF')
  RETURN
END IF

!     Quick return if possible

IF( m == 0 .OR. n == 0 ) RETURN

!     Determine the block size for this environment.

nb = ilaenv( 1, 'DGETRF', ' ', m, n, -1, -1 )
IF( nb <= 1 .OR. nb >= MIN( m, n ) ) THEN

!        Use unblocked code.

  CALL dgetf2( m, n, a, lda, ipiv, info )
ELSE

!        Use blocked code.

  DO j = 1, MIN( m, n ), nb
    jb = MIN( MIN( m, n )-j+1, nb )

!           Factor diagonal and subdiagonal blocks and test for exact
!           singularity.

    CALL dgetf2( m-j+1, jb, a( j:, j: ), lda, ipiv( j: ), iinfo )

!           Adjust INFO and the pivot indices.

    IF( info == 0 .AND. iinfo > 0 ) info = iinfo + j - 1
    DO i = j, MIN( m, j+jb-1 )
      ipiv( i ) = j - 1 + ipiv( i )
    END DO

!           Apply interchanges to columns 1:J-1.

    CALL dlaswp( j-1, a, j, j+jb-1, ipiv, 1 )

    IF( j+jb <= n ) THEN

!              Apply interchanges to columns J+JB:N.

      CALL dlaswp( n-j-jb+1, a( :, j+jb: ), j, j+jb-1, ipiv, 1 )

!              Compute block row of U.

      CALL dtrsm( 'Left', 'Lower', 'No transpose', 'Unit', jb,  &
                  n-j-jb+1, one, a( j:, j: ), lda, a( j:, j+jb: ), lda )
      IF( j+jb <= m ) THEN

!                 Update trailing submatrix.

        CALL dgemm( 'No transpose', 'No transpose', m-j-jb+1,  &
                    n-j-jb+1, jb, -one, a( j+jb:, j: ), lda,  &
                    a( j:, j+jb: ), lda, one, a( j+jb:, j+jb: ), lda )
      END IF
    END IF
  END DO
END IF
RETURN

!     End of DGETRF

END SUBROUTINE dgetrf



SUBROUTINE dgetrs( trans, n, nrhs, a, lda, ipiv, b, ldb, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

USE dblas3, ONLY: dtrsm
USE dla,    ONLY: dlaswp
IMPLICIT NONE

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: trans
INTEGER, INTENT(IN)           :: lda, ldb, n, nrhs
INTEGER, INTENT(OUT)          :: info
!     ..
!     .. Array Arguments ..
INTEGER, INTENT(IN)           :: ipiv(:)
REAL (wp), INTENT(IN)         :: a(:,:)
REAL (wp), INTENT(IN OUT)     :: b(:,:)
!     ..

!  Purpose
!  =======

!  DGETRS solves a system of linear equations
!     A * X = B  or  A' * X = B
!  with a general n by n matrix A using the LU factorization computed
!  by DGETRF.

!  Arguments
!  =========

!  TRANS   (input) CHARACTER*1
!          Specifies the form of the system of equations.
!          = 'N':  A * X = B  (No transpose)
!          = 'T':  A'* X = B  (Transpose)
!          = 'C':  A'* X = B  (Conjugate transpose = Transpose)

!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.

!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrix B.  NRHS >= 0.

!  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
!          The factors L and U from the factorization A = P*L*U
!          as computed by DGETRF.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).

!  IPIV    (input) INTEGER array, dimension (N)
!          The pivot indices from DGETRF; for 1<=i<=N, row i of the
!          matrix was interchanged with row IPIV(i).

!  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
!          On entry, the right hand side vectors B for the system of
!          linear equations.
!          On exit, the solution vectors, X.

!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,N).

!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value

!  =====================================================================

!     ..
!     .. Local Scalars ..
LOGICAL ::            notran
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! EXTERNAL           lsame
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlaswp, dtrsm, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
notran = lsame( trans, 'N' )
IF( .NOT.notran .AND. .NOT.lsame( trans, 'T' ) .AND. .NOT.  &
  lsame( trans, 'C' ) ) THEN
  info = -1
ELSE IF( n < 0 ) THEN
  info = -2
ELSE IF( nrhs < 0 ) THEN
  info = -3
ELSE IF( lda < MAX( 1, n ) ) THEN
  info = -5
ELSE IF( ldb < MAX( 1, n ) ) THEN
  info = -8
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DGETRS')
  RETURN
END IF

!     Quick return if possible

IF( n == 0 .OR. nrhs == 0 ) RETURN

IF( notran ) THEN

!        Solve A * X = B.

!        Apply row interchanges to the right hand sides.

  CALL dlaswp( nrhs, b, 1, n, ipiv, 1 )

!        Solve L*X = B, overwriting B with X.

  CALL dtrsm( 'Left', 'Lower', 'No transpose', 'Unit', n, nrhs,  &
              one, a, lda, b, ldb )

!        Solve U*X = B, overwriting B with X.

  CALL dtrsm( 'Left', 'Upper', 'No transpose', 'Non-unit', n,  &
              nrhs, one, a, lda, b, ldb )
ELSE

!        Solve A' * X = B.

!        Solve U'*X = B, overwriting B with X.

  CALL dtrsm( 'Left', 'Upper', 'Transpose', 'Non-unit', n, nrhs,  &
              one, a, lda, b, ldb )

!        Solve L'*X = B, overwriting B with X.

  CALL dtrsm( 'Left', 'Lower', 'Transpose', 'Unit', n, nrhs, one,  &
              a, lda, b, ldb )

!        Apply row interchanges to the solution vectors.

  CALL dlaswp( nrhs, b, 1, n, ipiv, -1 )
END IF

RETURN

!     End of DGETRS

END SUBROUTINE dgetrs




SUBROUTINE dgetf2( m, n, a, lda, ipiv, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

USE dblas2, ONLY: dger
IMPLICIT NONE

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: lda, m, n
INTEGER, INTENT(OUT)      :: info
!     ..
!     .. Array Arguments ..
INTEGER, INTENT(OUT)      :: ipiv(:)
REAL (wp), INTENT(IN OUT) :: a(:,:)
!     ..

!  Purpose
!  =======

!  DGETF2 computes an LU factorization of a general m-by-n matrix A
!  using partial pivoting with row interchanges.

!  The factorization has the form
!     A = P * L * U
!  where P is a permutation matrix, L is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and U is upper
!  triangular (upper trapezoidal if m < n).

!  This is the right-looking Level 2 BLAS version of the algorithm.

!  Arguments
!  =========

!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.

!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the m by n matrix to be factored.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).

!  IPIV    (output) INTEGER array, dimension (min(M,N))
!          The pivot indices; for 1 <= i <= min(M,N), row i of the
!          matrix was interchanged with row IPIV(i).

!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
!               has been completed, but the factor U is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.

!  =====================================================================

!     .. Local Scalars ..
INTEGER :: j, jp
!     ..
!     .. External Functions ..
! INTEGER ::            idamax
! EXTERNAL           idamax
!     ..
!     .. External Subroutines ..
! EXTERNAL           dger, dscal, dswap, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
IF( m < 0 ) THEN
  info = -1
ELSE IF( n < 0 ) THEN
  info = -2
ELSE IF( lda < MAX( 1, m ) ) THEN
  info = -4
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DGETF2')
  RETURN
END IF

!     Quick return if possible

IF( m == 0 .OR. n == 0 ) RETURN

DO j = 1, MIN( m, n )

!        Find pivot and test for singularity.

  jp = j - 1 + idamax( m-j+1, a( j:, j ), 1 )
  ipiv( j ) = jp
  
  
  IF( a( jp, j ) /= zero ) THEN

!           Apply the interchange to columns 1:N.

    IF( jp /= j ) CALL dswap( n, a( j, : ), 1, a( jp, : ), 1 )

!           Compute elements J+1:M of J-th column.

    IF( j < m ) CALL dscal( m-j, one / a( j, j ), a( j+1:, j ), 1 )

  ELSE IF( info == 0 ) THEN

    info = j
  END IF

  IF( j+1 <= n ) THEN

!           Update trailing submatrix.

    CALL dger( m-j, n-j, -one, a( j+1:, j ), 1, a( j, j+1: ), 1,  &
               a( j+1:, j+1: ), lda )
  END IF
END DO

RETURN

!     End of DGETF2

END SUBROUTINE dgetf2



END MODULE dge

! Module contains interface to all necessary subroutines from LAPACK

module lapack90


INTERFACE
  SUBROUTINE DSYEV_F90( A, W, JOBZ, UPLO, INFO )
    USE LA_PRECISION, ONLY: WP => DP
    USE LA_AUXMOD
!    USE dsy, ONLY: dsyev
    IMPLICIT NONE
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
    INTEGER, INTENT(OUT), OPTIONAL         :: INFO
    REAL(WP), INTENT(IN OUT)               :: A(:,:)
    REAL(WP), INTENT(OUT)                  :: W(:)
  END SUBROUTINE DSYEV_F90
END INTERFACE

INTERFACE
  SUBROUTINE DGESV_F90( A, B, IPIV, INFO )
    USE la_precision, ONLY: wp => dp
    USE la_auxmod
    USE dblas
    USE dblas2
    USE dblas3
    USE dla
    USE dge, ONLY: dgetrf, dgetrs, dgetf2
    IMPLICIT NONE
    INTEGER, INTENT(OUT), OPTIONAL         :: INFO
    INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IPIV(:)
    REAL(WP), INTENT(IN OUT)               :: A(:,:), B(:,:)
  END SUBROUTINE DGESV_F90
END INTERFACE


end module

SUBROUTINE DGESV_F90( A, B, IPIV, INFO )
!
!  -- LAPACK90 interface driver routine (version 1.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     May 31, 1997
!
! ELF90 version by Alan Miller  23 October 1997
! Latest revision - 14 November 1997
!
!     .. "Use Statements" ..
!      USE LA_PRECISION, ONLY: WP => DP
!      USE LA_AUXMOD, ONLY: ERINFO
!      USE F77_LAPACK, ONLY: GESV_F77 => LA_GESV
USE la_precision, ONLY: wp => dp
USE la_auxmod
!
!     .. "Implicit Statement" ..
      IMPLICIT NONE
!     .. "Scalar Arguments" ..
      INTEGER, INTENT(OUT), OPTIONAL         :: INFO
!     .. "Array Arguments" ..
      INTEGER, INTENT(OUT), OPTIONAL, TARGET :: IPIV(:)
      REAL(WP), INTENT(IN OUT)               :: A(:,:), B(:,:)
!-----------------------------------------------------------------
!
! Purpose
! =======
!
! LA_GESV computes the solution to either a real or complex system of
! linear equations  AX = B,
! where A is a square matrix and X and B are either rectangular
! matrices or vectors.
!
! The LU decomposition with partial pivoting and row interchanges is
! used to factor A as  A = PLU,
! where P is a permutation matrix, L is unit lower triangular, and U is
! upper triangular. The factored form of A is then used to solve the
! system of equations AX = B.
!
! Arguments
! =========
!
! SUBROUTINE LA_GESV ( A, B, IPIV, INFO )
!    <type>(<wp>), INTENT(INOUT) :: A(:,:), <rhs>
!    INTEGER, INTENT(OUT), OPTIONAL :: IPIV(:)
!    INTEGER, INTENT(OUT), OPTIONAL :: INFO
!    where
!    <type> ::= REAL | COMPLEX
!    <wp>   ::= KIND(1.0) | KIND(1.0D0)
!    <rhs>  ::= B(:,:) | B(:)
!
! =====================
!
! A    (input/output) either REAL or COMPLEX square array,
!      shape (:,:), size(A,1) == size(A,2).
!      On entry, the matrix A.
!      On exit, the factors L and U from the factorization A = PLU;
!         the unit diagonal elements of L are not stored.
!
! B    (input/output) either REAL or COMPLEX rectangular array,
!      shape either (:,:) or (:), size(B,1) or size(B) == size(A,1).
!      On entry, the right hand side vector(s) of matrix B for the
!         system of equations AX = B.
!      On exit, if there is no error, the matrix of solution
!         vector(s) X.
!
! IPIV Optional (output) INTEGER array, shape (:),
!      size(IPIV) == size(A,1). If IPIV is present it indice define
!      the permutation matrix P; row i of the matrix was interchanged
!      with row IPIV(i).
!
! INFO Optional (output) INTEGER.
!      If INFO is present
!         = 0: successful exit
!         < 0: if INFO = -k, the k-th argument had an illegal value
!         > 0: if INFO = k, U(k,k) is exactly zero.  The factorization
!              has been completed, but the factor U is exactly
!              singular, so the solution could not be computed.
!      If INFO is not present and an error occurs, then the program is
!         terminated with an error message.
!-------------------------------------
!     .. "Parameters" ..
      CHARACTER(LEN=7), PARAMETER :: SRNAME = 'LA_GESV'
!     .. LOCAL SCALARS ..
      INTEGER                     :: LINFO, ISTAT, ISTAT1, SIPIV
!     .. "Local Pointers" ..
      INTEGER, POINTER            :: LPIV(:)
!     .. "Intrinsic Functions" ..
!      INTRINSIC SIZE, PRESENT, MAX
!     ..
!     .. "Executable Statements" ..
!     ..
      LINFO = 0
      ISTAT = 0
      IF ( PRESENT(IPIV) )THEN
         SIPIV = SIZE(IPIV)
      ELSE
         SIPIV = SIZE(A,1)
      END IF
!     ..
!     .. "Test the arguments" ..
!     ..
    !  print*,A
    !  print*,size(A,2),size(A,1)
      IF( SIZE( A, 2 ) /= SIZE(A,1) .OR. SIZE(A,1) < 0 ) THEN
         LINFO = -1
      ELSE IF( SIZE( B, 1 ) /= SIZE(A,1) .OR. SIZE(B,2) < 0 ) THEN
         LINFO = -2
      ELSE IF( SIPIV /= SIZE(A,1) ) THEN
            LINFO = -3
      ELSE IF( SIZE(A,1) > 0 ) THEN
         IF( PRESENT(IPIV) ) THEN
            LPIV => IPIV
         ELSE
            ALLOCATE( LPIV(SIZE(A,1)), STAT = ISTAT )
         END IF
         IF( ISTAT == 0 ) THEN
!     ..
!        .. "Call LAPACK90 routine" ..
!     ..
            CALL DGESV( SIZE(A,1), SIZE(B,2), A, MAX(1,SIZE(A,1)), LPIV, &
                           B, MAX(1,SIZE(A,1)), LINFO )
         ELSE
            LINFO = -100
         END IF
         IF( .NOT. PRESENT(IPIV) ) THEN
            DEALLOCATE( LPIV, STAT = ISTAT1 )
            IF ( istat1 /= 0 )  &
                 WRITE(*, *) ' Error in deallocating LPIV in LA_DGESV'
         END IF
      END IF

      CALL ERINFO( LINFO, SRNAME, INFO, ISTAT )
!
RETURN

CONTAINS


SUBROUTINE dgesv( n, nrhs, a, lda, ipiv, b, ldb, info )

!  -- LAPACK driver routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

USE dge, ONLY: dgetrf, dgetrs
IMPLICIT NONE

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: lda, ldb, n, nrhs
INTEGER, INTENT(OUT)      :: info
!     ..
!     .. Array Arguments ..
INTEGER, INTENT(OUT)      :: ipiv(:)
REAL (wp), INTENT(IN OUT) :: a(:,:), b(:,:)
!     ..

!  Purpose
!  =======

!  DGESV computes the solution to a real system of linear equations
!     A * X = B,
!  where A is an N by N matrix and X and B are N by NRHS matrices.

!  The LU decomposition with partial pivoting and row interchanges is
!  used to factor A as
!     A = P * L * U,
!  where P is a permutation matrix, L is unit lower triangular, and U is
!  upper triangular.  The factored form of A is then used to solve the
!  system of equations A * X = B.

!  Arguments
!  =========

!  N       (input) INTEGER
!          The number of linear equations, i.e., the order of the
!          matrix A.  N >= 0.

!  NRHS    (input) INTEGER
!          The number of right hand sides, i.e., the number of columns
!          of the matrix B.  NRHS >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the N by N matrix of coefficients A.
!          On exit, the factors L and U from the factorization
!          A = P*L*U; the unit diagonal elements of L are not stored.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).

!  IPIV    (output) INTEGER array, dimension (N)
!          The pivot indices that define the permutation matrix P;
!          row i of the matrix was interchanged with row IPIV(i).

!  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
!          On entry, the N by NRHS matrix of right hand side vectors B
!          for the system of equations A*X = B.
!          On exit, if INFO = 0, the N by NRHS matrix of solution vectors X.

!  LDB     (input) INTEGER
!          The leading dimension of the array B.  LDB >= max(1,N).

!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -k, the k-th argument had an illegal value
!          > 0: if INFO = k, U(k,k) is exactly zero.  The factorization
!               has been completed, but the factor U is exactly
!               singular, so the solution could not be computed.

!  =====================================================================

!     .. External Subroutines ..
! EXTERNAL           dgetrf, dgetrs, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0
IF( n < 0 ) THEN
  info = -1
ELSE IF( nrhs < 0 ) THEN
  info = -2
ELSE IF( lda < MAX( 1, n ) ) THEN
  info = -4
ELSE IF( ldb < MAX( 1, n ) ) THEN
  info = -7
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DGESV ')
  RETURN
END IF

!     Compute the LU factorization of A.

CALL dgetrf( n, n, a, lda, ipiv, info )
IF( info == 0 ) THEN

!        Solve the system A*X = B, overwriting B with X.

  CALL dgetrs( 'No transpose', n, nrhs, a, lda, ipiv, b, ldb,info )
END IF
RETURN

!     End of DGESV

END SUBROUTINE dgesv



END SUBROUTINE DGESV_F90


MODULE dsy
! This module contains translations to ELF90 compatability of the
! LAPACK SYmmetric routines, including DSY (symmetric), DST (symmetric
! tri-diagonal) and DPO (symmetric positive definite) routines.

! Translated by Alan Miller
! alan @ vic.cmis.csiro.au    URL: www.ozemail.com.au/~milleraj

! Latest revision - 3 April 1998

USE la_precision, ONLY: wp => dp
USE la_auxmod
USE dblas
USE dblas2, ONLY: dsyr2, dsymv
USE dblas3, ONLY: dsyr2k
USE dla,    ONLY: dlarfg, dlaev2, dlatrd, dlae2, dlartg, dlasr, dlanst, dlapy2, &
		  dlaset
IMPLICIT NONE


CONTAINS

SUBROUTINE dsytd2( uplo, n, a, lda, d, e, tau, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: uplo
INTEGER, INTENT(IN)           :: lda, n
INTEGER, INTENT(OUT)          :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT)     :: a( :, : )
REAL (wp), INTENT(OUT)        :: d( : ), e( : ), tau( : )
!     ..

!  Purpose
!  =======

!  DSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
!  form T by an orthogonal similarity transformation: Q' * A * Q = T.

!  Arguments
!  =========

!  UPLO    (input) CHARACTER*1
!          Specifies whether the upper or lower triangular part of the
!          symmetric matrix A is stored:
!          = 'U':  Upper triangular
!          = 'L':  Lower triangular

!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
!          n-by-n upper triangular part of A contains the upper
!          triangular part of the matrix A, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading n-by-n lower triangular part of A contains the lower
!          triangular part of the matrix A, and the strictly upper
!          triangular part of A is not referenced.
!          On exit, if UPLO = 'U', the diagonal and first superdiagonal
!          of A are overwritten by the corresponding elements of the
!          tridiagonal matrix T, and the elements above the first
!          superdiagonal, with the array TAU, represent the orthogonal
!          matrix Q as a product of elementary reflectors; if UPLO
!          = 'L', the diagonal and first subdiagonal of A are over-
!          written by the corresponding elements of the tridiagonal
!          matrix T, and the elements below the first subdiagonal, with
!          the array TAU, represent the orthogonal matrix Q as a product
!          of elementary reflectors. See Further Details.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).

!  D       (output) DOUBLE PRECISION array, dimension (N)
!          The diagonal elements of the tridiagonal matrix T:
!          D(i) = A(i,i).

!  E       (output) DOUBLE PRECISION array, dimension (N-1)
!          The off-diagonal elements of the tridiagonal matrix T:
!          E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.

!  TAU     (output) DOUBLE PRECISION array, dimension (N)
!          The scalar factors of the elementary reflectors (see Further
!          Details).

!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value.

!  Further Details
!  ===============

!  If UPLO = 'U', the matrix Q is represented as a product of elementary
!  reflectors

!     Q = H(n-1) . . . H(2) H(1).

!  Each H(i) has the form

!     H(i) = I - tau * v * v'

!  where tau is a real scalar, and v is a real vector with
!  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
!  A(1:i-1,i+1), and tau in TAU(i).

!  If UPLO = 'L', the matrix Q is represented as a product of elementary
!  reflectors

!     Q = H(1) H(2) . . . H(n-1).

!  Each H(i) has the form

!     H(i) = I - tau * v * v'

!  where tau is a real scalar, and v is a real vector with
!  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
!  and tau in TAU(i).

!  The contents of A on exit are illustrated by the following examples
!  with n = 5:

!  if UPLO = 'U':                       if UPLO = 'L':

!    (  d   e   v2  v3  v4 )              (  d                  )
!    (      d   e   v3  v4 )              (  e   d              )
!    (          d   e   v4 )              (  v1  e   d          )
!    (              d   e  )              (  v1  v2  e   d      )
!    (                  d  )              (  v1  v2  v3  e   d  )

!  where d and e denote diagonal and off-diagonal elements of T, and vi
!  denotes an element of the vector defining H(i).

!  =====================================================================

!     .. Parameters ..
REAL (wp), PARAMETER :: half = 0.5D0
!     ..
!     .. Local Scalars ..
LOGICAL   :: upper
INTEGER   :: i
REAL (wp) :: alpha, taui
!     ..
!     .. External Subroutines ..
! EXTERNAL           daxpy, dlarfg, dsymv, dsyr2, xerbla
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! DOUBLE PRECISION ::   ddot
! EXTERNAL           lsame, ddot
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..

!     Test the input parameters

info = 0
upper = lsame( uplo, 'U' )
IF( .NOT.upper .AND. .NOT.lsame( uplo, 'L' ) ) THEN
  info = -1
ELSE IF( n < 0 ) THEN
  info = -2
ELSE IF( lda < MAX( 1, n ) ) THEN
  info = -4
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DSYTD2')
  RETURN
END IF

!     Quick return if possible

IF( n <= 0 ) RETURN

IF( upper ) THEN

!        Reduce the upper triangle of A

  DO i = n - 1, 1, -1

!           Generate elementary reflector H(i) = I - tau * v * v'
!           to annihilate A(1:i-1,i+1)

    CALL dlarfg( i, a( i, i+1 ), a( :, i+1 ), 1, taui )
    e( i ) = a( i, i+1 )

    IF( taui /= zero ) THEN

!              Apply H(i) from both sides to A(1:i,1:i)

      a( i, i+1 ) = one

!              Compute  x := tau * A * v  storing x in TAU(1:i)

      CALL dsymv( uplo, i, taui, a, lda, a( :, i+1 ), 1, zero, tau, 1 )

!              Compute  w := x - 1/2 * tau * (x'*v) * v

      alpha = -half*taui*ddot( i, tau, 1, a( :, i+1 ), 1 )
      CALL daxpy( i, alpha, a( :, i+1 ), 1, tau, 1 )

!              Apply the transformation as a rank-2 update:
!                 A := A - v * w' - w * v'

      CALL dsyr2( uplo, i, -one, a( :, i+1 ), 1, tau, 1, a, lda )

      a( i, i+1 ) = e( i )
    END IF
    d( i+1 ) = a( i+1, i+1 )
    tau( i ) = taui
  END DO
  d( 1 ) = a( 1, 1 )
ELSE

!        Reduce the lower triangle of A

  DO i = 1, n - 1

!           Generate elementary reflector H(i) = I - tau * v * v'
!           to annihilate A(i+2:n,i)

    CALL dlarfg( n-i, a( i+1, i ), a( MIN( i+2, n ):, i ), 1, taui )
    e( i ) = a( i+1, i )

    IF( taui /= zero ) THEN

!              Apply H(i) from both sides to A(i+1:n,i+1:n)

      a( i+1, i ) = one

!              Compute  x := tau * A * v  storing y in TAU(i+1:n)

      CALL dsymv( uplo, n-i, taui, a( i+1:, i+1: ), lda,  &
                  a( i+1:, i ), 1, zero, tau( i+1: ), 1 )

!              Compute  w := x - 1/2 * tau * (x'*v) * v

      alpha = -half*taui*ddot( n-i, tau( i+1: ), 1, a( i+1:, i ),1 )
      CALL daxpy( n-i, alpha, a( i+1:, i ), 1, tau( i+1: ), 1 )

!              Apply the transformation as a rank-2 update:
!                 A := A - v * w' - w * v'

      CALL dsyr2( uplo, n-i, -one, a( i+1:, i ), 1, tau( i+1: ),  &
                  1, a( i+1:, i+1: ), lda )

      a( i+1, i ) = e( i )
    END IF
    d( i ) = a( i, i )
    tau( i ) = taui
  END DO
  d( n ) = a( n, n )
END IF

RETURN

!     End of DSYTD2

END SUBROUTINE dsytd2


SUBROUTINE dsytrd( uplo, n, a, lda, d, e, tau, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. Arguments WORK & LWORK have been removed.

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: uplo
INTEGER, INTENT(IN)           :: lda, n
INTEGER, INTENT(OUT)          :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT)     :: a( :, : )
REAL (wp), INTENT(OUT)        :: d( : ), e( : ), tau( : )
!     ..

!  Purpose
!  =======

!  DSYTRD reduces a real symmetric matrix A to symmetric tridiagonal
!  form T by an orthogonal similarity transformation: Q' * A * Q = T.

!  Arguments
!  =========

!  UPLO    (input) CHARACTER*1
!          Specifies whether the upper or lower triangular part of the
!          symmetric matrix A is stored:
!          = 'U':  Upper triangular
!          = 'L':  Lower triangular

!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
!          n-by-n upper triangular part of A contains the upper
!          triangular part of the matrix A, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading n-by-n lower triangular part of A contains the lower
!          triangular part of the matrix A, and the strictly upper
!          triangular part of A is not referenced.
!          On exit, if UPLO = 'U', the diagonal and first superdiagonal
!          of A are overwritten by the corresponding elements of the
!          tridiagonal matrix T, and the elements above the first
!          superdiagonal, with the array TAU, represent the orthogonal
!          matrix Q as a product of elementary reflectors; if UPLO
!          = 'L', the diagonal and first subdiagonal of A are over-
!          written by the corresponding elements of the tridiagonal
!          matrix T, and the elements below the first subdiagonal, with
!          the array TAU, represent the orthogonal matrix Q as a product
!          of elementary reflectors. See Further Details.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).

!  D       (output) DOUBLE PRECISION array, dimension (N)
!          The diagonal elements of the tridiagonal matrix T:
!          D(i) = A(i,i).

!  E       (output) DOUBLE PRECISION array, dimension (N-1)
!          The off-diagonal elements of the tridiagonal matrix T:
!          E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.

!  TAU     (output) DOUBLE PRECISION array, dimension (N)
!          The scalar factors of the elementary reflectors (see Further
!          Details).

!  INFO    (output) INTEGER
!          = 0:  successful exit.
!          < 0:  if INFO = -i, the i-th argument had an illegal value.

!  Further Details
!  ===============

!  If UPLO = 'U', the matrix Q is represented as a product of elementary
!  reflectors

!     Q = H(n-1) . . . H(2) H(1).

!  Each H(i) has the form

!     H(i) = I - tau * v * v'

!  where tau is a real scalar, and v is a real vector with
!  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
!  A(1:i-1,i+1), and tau in TAU(i).

!  If UPLO = 'L', the matrix Q is represented as a product of elementary
!  reflectors

!     Q = H(1) H(2) . . . H(n-1).

!  Each H(i) has the form

!     H(i) = I - tau * v * v'

!  where tau is a real scalar, and v is a real vector with
!  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
!  and tau in TAU(i).

!  The contents of A on exit are illustrated by the following examples
!  with n = 5:

!  if UPLO = 'U':                       if UPLO = 'L':

!    (  d   e   v2  v3  v4 )              (  d                  )
!    (      d   e   v3  v4 )              (  e   d              )
!    (          d   e   v4 )              (  v1  e   d          )
!    (              d   e  )              (  v1  v2  e   d      )
!    (                  d  )              (  v1  v2  v3  e   d  )

!  where d and e denote diagonal and off-diagonal elements of T, and vi
!  denotes an element of the vector defining H(i).

!  =====================================================================

!     .. Local Scalars ..
LOGICAL :: upper
INTEGER :: i, j, kk, ldwork, nb, nx
!     ..
!     .. Local allocatable arrays ..
REAL (wp), ALLOCATABLE :: work(:,:)
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlatrd, dsyr2k, dsytd2, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! INTEGER ::            ilaenv
! EXTERNAL           lsame, ilaenv
!     ..
!     .. Executable Statements ..

!     Test the input parameters

info = 0
upper = lsame( uplo, 'U' )
IF( .NOT.upper .AND. .NOT.lsame( uplo, 'L' ) ) THEN
  info = -1
ELSE IF( n < 0 ) THEN
  info = -2
ELSE IF( lda < MAX( 1, n ) ) THEN
  info = -4
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DSYTRD')
  RETURN
END IF

!     Quick return if possible

IF( n == 0 ) THEN
  RETURN
END IF

!     Determine the block size.

nb = ilaenv( 1, 'DSYTRD', uplo, n, -1, -1, -1 )
nx = n
IF( nb > 1 .AND. nb < n ) THEN

!        Determine when to cross over from blocked to unblocked code
!        (last block is always handled by unblocked code).

  nx = MAX( nb, ilaenv( 3, 'DSYTRD', uplo, n, -1, -1, -1 ) )
ELSE
  nb = 1
END IF

ldwork = n
ALLOCATE( work(ldwork,nb) )

IF( upper ) THEN

!        Reduce the upper triangle of A.
!        Columns 1:kk are handled by the unblocked method.

  kk = n - ( ( n-nx+nb-1 ) / nb )*nb
  DO i = n - nb + 1, kk + 1, -nb

!           Reduce columns i:i+nb-1 to tridiagonal form and form the
!           matrix W which is needed to update the unreduced part of
!           the matrix

    CALL dlatrd( uplo, i+nb-1, nb, a, lda, e, tau, work, ldwork )

!           Update the unreduced submatrix A(1:i-1,1:i-1), using an
!           update of the form:  A := A - V*W' - W*V'

    CALL dsyr2k( uplo, 'No transpose', i-1, nb, -one, a( :, i: ),  &
                 lda, work, ldwork, one, a, lda )

!           Copy superdiagonal elements back into A, and diagonal
!           elements into D

    DO j = i, i + nb - 1
      a( j-1, j ) = e( j-1 )
      d( j ) = a( j, j )
    END DO
  END DO

!        Use unblocked code to reduce the last or only block

  CALL dsytd2( uplo, kk, a, lda, d, e, tau, info )
ELSE

!        Reduce the lower triangle of A

  DO i = 1, n - nx, nb

!           Reduce columns i:i+nb-1 to tridiagonal form and form the
!           matrix W which is needed to update the unreduced part of
!           the matrix

    CALL dlatrd( uplo, n-i+1, nb, a( i:, i: ), lda, e( i: ),  &
                 tau( i: ), work, ldwork )

!           Update the unreduced submatrix A(i+ib:n,i+ib:n), using
!           an update of the form:  A := A - V*W' - W*V'

    CALL dsyr2k( uplo, 'No transpose', n-i-nb+1, nb, -one, a( i+nb:, i: ),  &
                 lda, work( nb+1:, : ), ldwork, one, a( i+nb:, i+nb: ), lda )

!           Copy subdiagonal elements back into A, and diagonal
!           elements into D

    DO j = i, i + nb - 1
      a( j+1, j ) = e( j )
      d( j ) = a( j, j )
    END DO
  END DO

!        Use unblocked code to reduce the last or only block

  CALL dsytd2( uplo, n-i+1, a( i:, i: ), lda, d( i: ), e( i: ), tau( i: ), &
               info )
END IF

DEALLOCATE( work )
RETURN

!     End of DSYTRD

END SUBROUTINE dsytrd




SUBROUTINE dsteqr( compz, n, d, e, z, ldz, info )

!  -- LAPACK routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     September 30, 1994

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. Argument WORK has been removed.

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: compz
INTEGER, INTENT(IN)           :: ldz, n
INTEGER, INTENT(OUT)          :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT)     :: d( : ), e( : ), z( :, : )
!     ..

!  Purpose
!  =======

!  DSTEQR computes all eigenvalues and, optionally, eigenvectors of a
!  symmetric tridiagonal matrix using the implicit QL or QR method.
!  The eigenvectors of a full or band symmetric matrix can also be found
!  if DSYTRD or DSPTRD or DSBTRD has been used to reduce this matrix to
!  tridiagonal form.

!  Arguments
!  =========

!  COMPZ   (input) CHARACTER*1
!          = 'N':  Compute eigenvalues only.
!          = 'V':  Compute eigenvalues and eigenvectors of the original
!                  symmetric matrix.  On entry, Z must contain the orthogonal
!                  matrix used to reduce the original matrix to tridiagonal
!                  form.
!          = 'I':  Compute eigenvalues and eigenvectors of the tridiagonal
!                  matrix.  Z is initialized to the identity matrix.

!  N       (input) INTEGER
!          The order of the matrix.  N >= 0.

!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the diagonal elements of the tridiagonal matrix.
!          On exit, if INFO = 0, the eigenvalues in ascending order.

!  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On entry, the (n-1) subdiagonal elements of the tridiagonal matrix.
!          On exit, E has been destroyed.

!  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
!          On entry, if  COMPZ = 'V', then Z contains the orthogonal
!          matrix used in the reduction to tridiagonal form.
!          On exit, if INFO = 0, then if  COMPZ = 'V', Z contains the
!          orthonormal eigenvectors of the original symmetric matrix,
!          and if COMPZ = 'I', Z contains the orthonormal eigenvectors
!          of the symmetric tridiagonal matrix.
!          If COMPZ = 'N', then Z is not referenced.

!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.  LDZ >= 1, and if
!          eigenvectors are desired, then  LDZ >= max(1,N).

!  WORK    (workspace) DOUBLE PRECISION array, dimension (max(1,2*N-2))
!          If COMPZ = 'N', then WORK is not referenced.

!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  the algorithm has failed to find all the eigenvalues in a
!                total of 30*N iterations; if INFO = i, then i elements of E
!                have not converged to zero; on exit, D and E contain the
!                elements of a symmetric tridiagonal matrix which is
!                orthogonally similar to the original matrix.

!  =====================================================================

!     .. Parameters ..
REAL (wp), PARAMETER :: two = 2.0D0, three = 3.0D0
INTEGER, PARAMETER   :: maxit = 30
!     ..
!     .. Local Scalars ..
INTEGER :: i, icompz, ii, iscale, j, jtot, k, l, l1, lend,  &
           lendm1, lendp1, lendsv, lm1, lsv, m, mm, mm1,nm1, nmaxit
REAL (wp) :: anorm, b, c, eps, eps2, f, g, p, r, rt1, rt2,  &
             s, safmax, safmin, ssfmax, ssfmin, tst
REAL (wp), ALLOCATABLE :: work(:)
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! DOUBLE PRECISION ::   dlamch, dlanst, dlapy2
! EXTERNAL           lsame, dlamch, dlanst, dlapy2
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlae2, dlaev2, dlartg, dlascl, dlaset, dlasr,  &
!                    dlasrt, dswap, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS, MAX, SIGN, SQRT
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0

IF( lsame( compz, 'N' ) ) THEN
  icompz = 0
ELSE IF( lsame( compz, 'V' ) ) THEN
  icompz = 1
ELSE IF( lsame( compz, 'I' ) ) THEN
  icompz = 2
ELSE
  icompz = -1
END IF

IF( icompz < 0 ) THEN
  info = -1
ELSE IF( n < 0 ) THEN
  info = -2
ELSE IF( ( ldz < 1 ) .OR. ( icompz > 0 .AND. ldz < MAX( 1, n ) ) ) THEN
  info = -6
END IF
IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DSTEQR')
  RETURN
END IF

!     Quick return if possible

IF( n == 0 ) RETURN

IF( n == 1 ) THEN
  IF( icompz == 2 ) z( 1, 1 ) = one
  RETURN
END IF

IF( icompz > 0) ALLOCATE( work( MAX(1, 2*n-2) ) )

!     Determine the unit roundoff and over/underflow thresholds.

eps = dlamch( 'E' )
eps2 = eps**2
safmin = dlamch( 'S' )
safmax = one / safmin
ssfmax = SQRT( safmax ) / three
ssfmin = SQRT( safmin ) / eps2

!     Compute the eigenvalues and eigenvectors of the tridiagonal
!     matrix.

IF( icompz == 2 ) CALL dlaset( 'Full', n, n, zero, one, z )

nmaxit = n*maxit
jtot = 0

!     Determine where the matrix splits and choose QL or QR iteration
!     for each block, according to whether top or bottom diagonal
!     element is smaller.

l1 = 1
nm1 = n - 1

10 IF( l1 > n ) GO TO 160
IF( l1 > 1 ) e( l1-1 ) = zero
IF( l1 <= nm1 ) THEN
  DO m = l1, nm1
    tst = ABS( e( m ) )
    IF( tst == zero ) GO TO 30
    IF( tst <= ( SQRT( ABS( d( m ) ) )*SQRT( ABS( d( m+ 1 ) ) ) )*eps ) THEN
      e( m ) = zero
      GO TO 30
    END IF
  END DO
END IF
m = n

30 l = l1
lsv = l
lend = m
lendsv = lend
l1 = m + 1
IF( lend == l ) GO TO 10

!     Scale submatrix in rows and columns L to LEND

anorm = dlanst( 'I', lend-l+1, d( l: ), e( l: ) )
iscale = 0
IF( anorm == zero ) GO TO 10
IF( anorm > ssfmax ) THEN
  iscale = 1
!  CALL dlascl( 'G', 0, 0, anorm, ssfmax, lend-l+1, 1, d( l: ), n, info )
  d( l:lend ) = (ssfmax / anorm) * d( l:lend )
!  CALL dlascl( 'G', 0, 0, anorm, ssfmax, lend-l, 1, e( l: ), n, info )
  e( l:lend-1 ) = (ssfmax / anorm) * e( l:lend-1 )

ELSE IF( anorm < ssfmin ) THEN
  iscale = 2
!  CALL dlascl( 'G', 0, 0, anorm, ssfmin, lend-l+1, 1, d( l: ), n, info )
  d( l:lend ) = (ssfmin / anorm) * d( l:lend )
!  CALL dlascl( 'G', 0, 0, anorm, ssfmin, lend-l, 1, e( l: ), n, info )
  e( l:lend-1 ) = (ssfmin / anorm) * e( l:lend-1 )
END IF

!     Choose between QL and QR iteration

IF( ABS( d( lend ) ) < ABS( d( l ) ) ) THEN
  lend = lsv
  l = lendsv
END IF

IF( lend > l ) THEN

!        QL Iteration

!        Look for small subdiagonal element.

  40 IF( l /= lend ) THEN
    lendm1 = lend - 1
    DO m = l, lendm1
      tst = ABS( e( m ) )**2
      IF( tst <= ( eps2*ABS( d( m ) ) )*ABS( d( m+1 ) ) + safmin ) GO TO 60
    END DO
  END IF

  m = lend

  60 IF( m < lend ) e( m ) = zero
  p = d( l )
  IF( m == l ) GO TO 80

!        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
!        to compute its eigensystem.

  IF( m == l+1 ) THEN
    IF( icompz > 0 ) THEN
      CALL dlaev2( d( l ), e( l ), d( l+1 ), rt1, rt2, c, s )
      work( l ) = c
      work( n-1+l ) = s
      CALL dlasr( 'R', 'V', 'B', n, 2, work( l: ),  &
                  work( n-1+l: ), z( :, l: ), ldz )
    ELSE
      CALL dlae2( d( l ), e( l ), d( l+1 ), rt1, rt2 )
    END IF
    d( l ) = rt1
    d( l+1 ) = rt2
    e( l ) = zero
    l = l + 2
    IF( l <= lend ) GO TO 40
    GO TO 140
  END IF

  IF( jtot == nmaxit ) GO TO 140
  jtot = jtot + 1

!        Form shift.

  g = ( d( l+1 )-p ) / ( two*e( l ) )
  r = dlapy2( g, one )
  g = d( m ) - p + ( e( l ) / ( g+SIGN( r, g ) ) )

  s = one
  c = one
  p = zero

!        Inner loop

  mm1 = m - 1
  DO i = mm1, l, -1
    f = s*e( i )
    b = c*e( i )
    CALL dlartg( g, f, c, s, r )
    IF( i /= m-1 ) e( i+1 ) = r
    g = d( i+1 ) - p
    r = ( d( i )-g )*s + two*c*b
    p = s*r
    d( i+1 ) = g + p
    g = c*r - b

!           If eigenvectors are desired, then save rotations.

    IF( icompz > 0 ) THEN
      work( i ) = c
      work( n-1+i ) = -s
    END IF

  END DO

!        If eigenvectors are desired, then apply saved rotations.

  IF( icompz > 0 ) THEN
    mm = m - l + 1
    CALL dlasr( 'R', 'V', 'B', n, mm, work( l: ), work( n-1+l: ), &
                z( :, l: ), ldz )
  END IF

  d( l ) = d( l ) - p
  e( l ) = g
  GO TO 40

!        Eigenvalue found.

  80 d( l ) = p

  l = l + 1
  IF( l <= lend ) GO TO 40
  GO TO 140

ELSE

!        QR Iteration

!        Look for small superdiagonal element.

  90 IF( l /= lend ) THEN
    lendp1 = lend + 1
    DO m = l, lendp1, -1
      tst = ABS( e( m-1 ) )**2
      IF( tst <= ( eps2*ABS( d( m ) ) )*ABS( d( m-1 ) )+ safmin )GO TO 110
    END DO
  END IF

  m = lend

  110 IF( m > lend ) e( m-1 ) = zero
  p = d( l )
  IF( m == l ) GO TO 130

!        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
!        to compute its eigensystem.

  IF( m == l-1 ) THEN
    IF( icompz > 0 ) THEN
      CALL dlaev2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2, c, s )
      work( m ) = c
      work( n-1+m ) = s
      CALL dlasr( 'R', 'V', 'F', n, 2, work( m: ),  &
                  work( n-1+m: ), z( :, l-1: ), ldz )
    ELSE
      CALL dlae2( d( l-1 ), e( l-1 ), d( l ), rt1, rt2 )
    END IF
    d( l-1 ) = rt1
    d( l ) = rt2
    e( l-1 ) = zero
    l = l - 2
    IF( l >= lend ) GO TO 90
    GO TO 140
  END IF

  IF( jtot == nmaxit ) GO TO 140
  jtot = jtot + 1

!        Form shift.

  g = ( d( l-1 )-p ) / ( two*e( l-1 ) )
  r = dlapy2( g, one )
  g = d( m ) - p + ( e( l-1 ) / ( g+SIGN( r, g ) ) )

  s = one
  c = one
  p = zero

!        Inner loop

  lm1 = l - 1
  DO i = m, lm1
    f = s*e( i )
    b = c*e( i )
    CALL dlartg( g, f, c, s, r )
    IF( i /= m ) e( i-1 ) = r
    g = d( i ) - p
    r = ( d( i+1 )-g )*s + two*c*b
    p = s*r
    d( i ) = g + p
    g = c*r - b

!           If eigenvectors are desired, then save rotations.

    IF( icompz > 0 ) THEN
      work( i ) = c
      work( n-1+i ) = s
    END IF

  END DO

!        If eigenvectors are desired, then apply saved rotations.

  IF( icompz > 0 ) THEN
    mm = l - m + 1
    CALL dlasr( 'R', 'V', 'F', n, mm, work( m: ), work( n-1+m: ),  &
                z( :, m: ), ldz )
  END IF

  d( l ) = d( l ) - p
  e( lm1 ) = g
  GO TO 90

!        Eigenvalue found.

  130 d( l ) = p

  l = l - 1
  IF( l >= lend ) GO TO 90
  GO TO 140

END IF

!     Undo scaling if necessary

140 IF( iscale == 1 ) THEN
!  CALL dlascl( 'G', 0, 0, ssfmax, anorm, lendsv-lsv+1, 1, d( lsv: ), n, info )
   d( lsv:lendsv ) = (anorm / ssfmax) * d( lsv:lendsv )
!  CALL dlascl( 'G', 0, 0, ssfmax, anorm, lendsv-lsv, 1, e( lsv: ), n, info )
   e( lsv:lendsv-1 ) = (anorm / ssfmax) * e( lsv:lendsv-1 )

ELSE IF( iscale == 2 ) THEN
!  CALL dlascl( 'G', 0, 0, ssfmin, anorm, lendsv-lsv+1, 1, d( lsv: ), n, info )
   d( lsv:lendsv ) = (anorm / ssfmin) * d( lsv:lendsv )
!  CALL dlascl( 'G', 0, 0, ssfmin, anorm, lendsv-lsv, 1, e( lsv: ), n, info )
   e( lsv:lendsv-1 ) = (anorm / ssfmin) * e( lsv:lendsv-1 )
END IF

!     Check for no convergence to an eigenvalue after a total
!     of N*MAXIT iterations.

IF( jtot < nmaxit ) GO TO 10
DO i = 1, n - 1
  IF( e( i ) /= zero ) info = info + 1
END DO
GO TO 190

!     Order eigenvalues and eigenvectors.

160 IF( icompz == 0 ) THEN

!        Use Quick Sort

  CALL dlasrt( 'I', n, d, info )

ELSE

!        Use Selection Sort to minimize swaps of eigenvectors

  DO ii = 2, n
    i = ii - 1
    k = i
    p = d( i )
    DO j = ii, n
      IF( d( j ) < p ) THEN
        k = j
        p = d( j )
      END IF
    END DO
    IF( k /= i ) THEN
      d( k ) = d( i )
      d( i ) = p
      CALL dswap( n, z( :, i ), 1, z( :, k ), 1 )
    END IF
  END DO

  DEALLOCATE( work )
END IF

190 RETURN

!     End of DSTEQR

END SUBROUTINE dsteqr




SUBROUTINE dsterf( n, d, e, info )

!  -- LAPACK routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997

!     .. Scalar Arguments ..
INTEGER, INTENT(IN)       :: n
INTEGER, INTENT(OUT)      :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT) :: d( : ), e( : )
!     ..

!  Purpose
!  =======

!  DSTERF computes all eigenvalues of a symmetric tridiagonal matrix
!  using the Pal-Walker-Kahan variant of the QL or QR algorithm.

!  Arguments
!  =========

!  N       (input) INTEGER
!          The number of rows and columns in the matrix.  N >= 0.

!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, D contains the diagonal elements of the
!          tridiagonal matrix.
!          On exit, D contains the eigenvalues in ascending order.
!          If an error exit is made, the eigenvalues are correct
!          but unordered for indices 1,2,...,INFO-1.

!  E       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, E contains the subdiagonal elements of the
!          tridiagonal matrix in positions 1 through N-1.
!          E(N) is arbitrary.
!          On exit, E has been destroyed.

!  INFO    (output) INTEGER
!          = 0:  successful exit.
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!          > 0:  if INFO = +i, the i-th eigenvalue has not converged
!                after a total of  30*N  iterations.

!     .. Parameters ..
REAL (wp), PARAMETER :: two = 2.0D0
INTEGER, PARAMETER   :: maxit = 30
!     ..
!     .. Local Scalars ..
INTEGER :: i, ii, j, jtot, k, l, l1, lend, lendm1, lendp1,  &
           lm1, m, mm1, nconv, nm1, nmaxit
REAL (wp) :: alpha, bb, c, eps, gamma, oldc, oldgam, p, r,  &
             rt1, rt2, rte, s, sigma, tst
!     ..
!     .. External Functions ..
! DOUBLE PRECISION ::   dlamch, dlapy2
! EXTERNAL           dlamch, dlapy2
!     ..
!     .. External Subroutines ..
! EXTERNAL           dlae2, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          ABS, SIGN, SQRT
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

info = 0

!     Quick return if possible

IF( n < 0 ) THEN
  info = -1
  CALL erinfo(-info, 'DSTERF')
  RETURN
END IF
IF( n <= 1 ) RETURN

!     Determine the unit roundoff for this environment.

eps = dlamch( 'E' )

!     Compute the eigenvalues of the tridiagonal matrix.

DO i = 1, n - 1
  e( i ) = e( i )**2
END DO
e( n ) = zero

nmaxit = n*maxit
sigma = zero
jtot = 0
nconv = 0

!     Determine where the matrix splits and choose QL or QR iteration
!     for each block, according to whether top or bottom diagonal
!     element is smaller.

l1 = 1
nm1 = n - 1

20 IF( l1 > n ) GO TO 160
IF( l1 <= nm1 ) THEN
  DO m = l1, nm1
    tst = SQRT( ABS( e( m ) ) )
    IF( tst <= eps*( ABS( d( m ) )+ABS( d( m+1 ) ) ) ) GO TO 40
  END DO
END IF
m = n

40 l = l1
lend = m
IF( ABS( d( lend ) ) < ABS( d( l ) ) ) THEN
  l = lend
  lend = l1
END IF
l1 = m + 1

IF( lend >= l ) THEN

!        QL Iteration

!        Look for small subdiagonal element.

  50 IF( l /= lend ) THEN
    lendm1 = lend - 1
    DO m = l, lendm1
      tst = SQRT( ABS( e( m ) ) )
      IF( tst <= eps*( ABS( d( m ) )+ABS( d( m+1 ) ) ) ) GO TO 70
    END DO
  END IF

  m = lend

  70 p = d( l )
  IF( m == l ) GO TO 90

!        If remaining matrix is 2 by 2, use DLAE2 to compute its
!        eigenvalues.

  IF( m == l+1 ) THEN
    rte = SQRT( e( l ) )
    CALL dlae2( d( l ), rte, d( l+1 ), rt1, rt2 )
    d( l ) = rt1
    d( l+1 ) = rt2
    nconv = nconv + 2
    l = l + 2
    IF( l <= lend ) GO TO 50
    GO TO 20
  END IF

  IF( jtot == nmaxit ) GO TO 150
  jtot = jtot + 1

!        Form shift.

  rte = SQRT( e( l ) )
  sigma = ( d( l+1 )-p ) / ( two*rte )
  r = dlapy2( sigma, one )
  sigma = p - ( rte / ( sigma+SIGN( r, sigma ) ) )

  c = one
  s = zero
  gamma = d( m ) - sigma
  p = gamma*gamma

!        Inner loop

  mm1 = m - 1
  DO i = mm1, l, -1
    bb = e( i )
    r = p + bb
    e( i+1 ) = s*r
    oldc = c
    c = p / r
    s = bb / r
    oldgam = gamma
    alpha = d( i )
    gamma = c*( alpha-sigma ) - s*oldgam
    d( i+1 ) = oldgam + ( alpha-gamma )
    IF( c /= zero ) THEN
      p = ( gamma*gamma ) / c
    ELSE
      p = oldc*bb
    END IF
  END DO

  e( l ) = s*p
  d( l ) = sigma + gamma
  e( m ) = zero
  GO TO 50

!        Eigenvalue found.

  90 d( l ) = p
  nconv = nconv + 1

  l = l + 1
  IF( l <= lend ) GO TO 50
  GO TO 20

ELSE

!        QR Iteration

!        Look for small superdiagonal element.

  100 IF( l /= lend ) THEN
    lendp1 = lend + 1
    DO m = l, lendp1, -1
      tst = SQRT( ABS( e( m-1 ) ) )
      IF( tst <= eps*( ABS( d( m ) )+ABS( d( m-1 ) ) ) ) GO TO 120
    END DO
  END IF

  m = lend

  120 p = d( l )
  IF( m == l ) GO TO 140

!        If remaining matrix is 2 by 2, use DLAE2 to compute its eigenvalues.

  IF( m == l-1 ) THEN
    rte = SQRT( e( l-1 ) )
    CALL dlae2( d( l ), rte, d( l-1 ), rt1, rt2 )
    d( l ) = rt1
    d( l-1 ) = rt2
    nconv = nconv + 2
    l = l - 2
    IF( l >= lend ) GO TO 100
    GO TO 20
  END IF

  IF( jtot == nmaxit ) GO TO 150
  jtot = jtot + 1

!        Form shift.

  rte = SQRT( e( l-1 ) )
  sigma = ( d( l-1 )-p ) / ( two*rte )
  r = dlapy2( sigma, one )
  sigma = p - ( rte / ( sigma+SIGN( r, sigma ) ) )

  c = one
  s = zero
  gamma = d( m ) - sigma
  p = gamma*gamma

!        Inner loop

  lm1 = l - 1
  DO i = m, lm1
    bb = e( i )
    r = p + bb
    IF( i /= 1 ) e( i-1 ) = s*r
    oldc = c
    c = p / r
    s = bb / r
    oldgam = gamma
    alpha = d( i+1 )
    gamma = c*( alpha-sigma ) - s*oldgam
    d( i ) = oldgam + ( alpha-gamma )
    IF( c /= zero ) THEN
      p = ( gamma*gamma ) / c
    ELSE
      p = oldc*bb
    END IF
  END DO

  e( lm1 ) = s*p
  d( l ) = sigma + gamma
  IF( m /= 1 ) e( m-1 ) = zero
  GO TO 100

!        Eigenvalue found.

  140 d( l ) = p
  nconv = nconv + 1

  l = l - 1
  IF( l >= lend ) GO TO 100
  GO TO 20

END IF

!     Set error -- no convergence to an eigenvalue after a total
!     of N*MAXIT iterations.

150 info = nconv
RETURN

!     Sort eigenvalues in increasing order.

160 DO ii = 2, n
  i = ii - 1
  k = i
  p = d( i )
  DO j = ii, n
    IF( d( j ) < p ) THEN
      k = j
      p = d( j )
    END IF
  END DO
  IF( k /= i ) THEN
    d( k ) = d( i )
    d( i ) = p
  END IF
END DO

RETURN

!     End of DSTERF

END SUBROUTINE dsterf

END MODULE dsy
SUBROUTINE DSYEV_F90( A, W, JOBZ, UPLO, INFO )
!
!  -- LAPACK90 interface driver routine (version 1.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     May 31, 1997
!
! ELF90 translation by Alan Miller    10-Nov-1997
! Latest revision - 23 November 1997

!  .. USE STATEMENTS ..
   USE LA_PRECISION, ONLY: WP => DP
   USE LA_AUXMOD
!   USE F77_LAPACK, ONLY: SYEV_F77 => LA_SYEV, ILAENV_F77 => ILAENV

!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. CHARACTER ARGUMENTS ..
   CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
!  .. SCALAR ARGUMENTS ..
   INTEGER, INTENT(OUT), OPTIONAL         :: INFO
!  .. ARRAY ARGUMENTS ..
   REAL(WP), INTENT(IN OUT)               :: A(:,:)
   REAL(WP), INTENT(OUT)                  :: W(:)
!-----------------------------------------------------------------
!
! Purpose
! =======
!
! LA_SYEV / LA_HEEV computes all eigenvalues and, optionally,
! eigenvectors of a real symmetric or Hermitian matrix A.
!
! =======
!
!    SUBROUTINE LA_SYEV /  LA_HEEV( A, W, JOBZ, UPLO, INFO )
!       <type>(<wp>), INTENT(INOUT) :: A(:,:)
!       REAL(<wp>), INTENT(OUT) :: W(:)
!       CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: JOBZ, UPLO
!       INTEGER, INTENT(OUT), OPTIONAL :: INFO
!    where
!       <type> ::= REAL | COMPLEX
!       <wp>   ::= KIND(1.0) | KIND(1.0D0)
!
! Defaults
! ========
!
! 1. If JOBZ is not present then JOBZ = 'N' is assumed.
!
! 2. If UPLO is not present then UPLO = 'U' is assumed.
!
! Arguments
! =========
!
! A       (input/output) either REAL/COMPLEX square array,
!         shape (:,:), size(A,1) == size(A,2).
!         On entry, the symmetric (Hermitian) matrix A.
!            If UPLO = 'U', the upper triangular part of A contains
!               the upper triangular part of the matrix A.
!            If UPLO = 'L', the lower triangular part of A contains
!               the lower triangular part of the matrix A.
!         On exit:
!            If JOBZ = 'V', then if INFO = 0, A contains the
!               orthonormal eigenvectors of the matrix A.
!            If JOBZ = 'N', then on exit the lower triangle (if UPLO='L')
!               or the upper triangle (if UPLO='U') of A, including the
!               diagonal, is destroyed.
!
! W       (output) REAL array,  shape (:), size(W) == size(A,1) >= 0.
!         If INFO = 0, the eigenvalues in ascending order.
!
! JOBZ    Optional, (input) CHARACTER*1
!         If JOBZ is present then:
!            = 'N':  Compute eigenvalues only;
!            = 'V':  Compute eigenvalues and eigenvectors.
!         otherwise JOBZ = 'N' is assumed.
!
! UPLO    Optional, (input) CHARACTER*1
!         If UPLO is present then:
!            = 'U':  Upper triangle of A is stored;
!            = 'L':  Lower triangle of A is stored.
!         otherwise UPLO = 'U' is assumed.
!
! INFO    Optional, (output) INTEGER
!         If INFO is present:
!            = 0: successful exit
!            < 0: if INFO = -i, the i-th argument had an illegal value
!            > 0: if INFO = i, the algorithm failed to converge; i
!                 off-diagonal elements of an intermediate tridiagonal
!                 form did not converge to zero.
!         If INFO is not present and an error occurs, then the program
!            is terminated with an error message.
!
!-------------------------------------------
!  .. LOCAL PARAMETERS ..
   CHARACTER(LEN=7), PARAMETER :: SRNAME = 'LA_SYEV'
!  .. LOCAL SCALARS ..
   CHARACTER(LEN=1)  :: LJOBZ, LUPLO
   INTEGER           :: N, LINFO, LD, ISTAT
!  .. INTRINSIC FUNCTIONS ..
!   INTRINSIC MAX, PRESENT

!  .. EXECUTABLE STATEMENTS ..
   N = SIZE( A, 1 )
   LINFO = 0
   ISTAT = 0
   LD = MAX(1,N)

   IF( PRESENT(JOBZ) ) THEN
      LJOBZ = JOBZ
   ELSE
      LJOBZ = 'N'
   END IF

   IF( PRESENT(UPLO) ) THEN
      LUPLO = UPLO
   ELSE
      LUPLO = 'U'
   END IF

!  .. TEST THE ARGUMENTS
   IF( SIZE( A, 2 ) /= N .OR. N < 0 )THEN
      LINFO = -1
   ELSE IF( SIZE( W ) /= N )THEN
      LINFO = -2
   ELSE IF( .NOT.LSAME(LJOBZ,'N') .AND. .NOT.LSAME(LJOBZ,'V') )THEN
      LINFO = -3
   ELSE IF( .NOT.LSAME(LUPLO,'U') .AND. .NOT.LSAME(LUPLO,'L') )THEN
      LINFO = -4
   ELSE IF( N > 0 )THEN
!
      IF( LINFO == 0 )THEN
         CALL DSYEV( LJOBZ, LUPLO, N, A, LD, W, LINFO )
      END IF
   END IF
   IF( PRESENT( info ) ) CALL ERINFO(LINFO, SRNAME, INFO, ISTAT)
!
RETURN

CONTAINS


SUBROUTINE dsyev( jobz, uplo, n, a, lda, w, info )

!  -- LAPACK driver routine (version 1.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992

! ELF90 translation by Alan Miller   31-Aug-1997
! N.B. Arguments WORK & LWORK have been removed

USE dblas
USE dla, ONLY: dlansy
USE dsy, ONLY: dsteqr, dsterf, dsytrd
USE dor, ONLY: dorgtr

!     .. Scalar Arguments ..
CHARACTER (LEN=1), INTENT(IN) :: jobz, uplo
INTEGER, INTENT(IN)           :: lda, n
INTEGER, INTENT(OUT)          :: info
!     ..
!     .. Array Arguments ..
REAL (wp), INTENT(IN OUT)     :: a( :, : )
REAL (wp), INTENT(OUT)        :: w( : )
!     ..

!  Purpose
!  =======

!  DSYEV  computes all eigenvalues and, optionally, eigenvectors of a
!  real symmetric matrix A by calling the recommended sequence of LAPACK
!  routines.

!  Arguments
!  =========

!  JOBZ    (input) CHARACTER*1
!          Specifies whether or not to compute the eigenvectors:
!          = 'N':  Compute eigenvalues only.
!          = 'V':  Compute eigenvalues and eigenvectors.

!  UPLO    (input) CHARACTER*1
!          Specifies whether the upper or lower triangular part of the
!          symmetric matrix A is stored:
!          = 'U':  Upper triangular
!          = 'L':  Lower triangular

!  N       (input) INTEGER
!          The number of rows and columns of the matrix A.  N >= 0.

!  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
!          On entry, the symmetric matrix A.  If UPLO = 'U', only the
!          upper triangular part of A is used to define the elements of
!          the symmetric matrix.  If UPLO = 'L', only the lower
!          triangular part of A is used to define the elements of the
!          symmetric matrix.

!          If JOBZ = 'V', then if INFO = 0 on exit, A contains the
!          orthonormal eigenvectors of the matrix A.  If INFO > 0, A
!          contains the eigenvectors associated with only the stored
!          eigenvalues.
!          If JOBZ = 'N', then on exit the lower triangle (if UPLO='L')
!          or the upper triangle (if UPLO='U') of A, including the
!          diagonal, is destroyed.

!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).

!  W       (output) DOUBLE PRECISION array, dimension (N)
!          On exit, if INFO = 0, W contains the eigenvalues in ascending
!          order.  If INFO > 0, the eigenvalues are correct for indices
!          1, 2, ..., INFO-1, but they are unordered and may not be the
!          smallest eigenvalues of the matrix.

!  INFO    (output) INTEGER
!          = 0:  successful exit.
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!          > 0:  if INFO = +i, the algorithm terminated before finding
!                the i-th eigenvalue.

!     .. Local Scalars ..
LOGICAL   :: lower, wantz
INTEGER   :: imax, inde, indtau, iscale, j
REAL (wp) :: anrm, bignum, eps, rmax, rmin, safmin, sigma, smlnum
!     ..
!     .. Local array
REAL (wp) :: work( 4*n )
!     ..
!     .. External Functions ..
! LOGICAL ::            lsame
! DOUBLE PRECISION ::   dlamch, dlansy
! EXTERNAL           lsame, dlamch, dlansy
!     ..
!     .. External Subroutines ..
! EXTERNAL           dorgtr, dscal, dsteqr, dsterf, dsytrd, xerbla
!     ..
!     .. Intrinsic Functions ..
! INTRINSIC          MAX, SQRT
!     ..
!     .. Executable Statements ..

!     Test the input parameters.

wantz = lsame( jobz, 'V' )
lower = lsame( uplo, 'L' )

info = 0
IF( .NOT.( wantz .OR. lsame( jobz, 'N' ) ) ) THEN
  info = -1
ELSE IF( .NOT.( lower .OR. lsame( uplo, 'U' ) ) ) THEN
  info = -2
ELSE IF( n < 0 ) THEN
  info = -3
ELSE IF( lda < MAX( 1, n ) ) THEN
  info = -5
END IF

IF( info /= 0 ) THEN
  CALL erinfo(-info, 'DSYEV ')
  RETURN
END IF

!     Quick return if possible

IF( n == 0 ) THEN
  RETURN
END IF

IF( n == 1 ) THEN
  w( 1 ) = a( 1, 1 )
  IF( wantz ) a( 1, 1 ) = one
  RETURN
END IF

!     Get machine constants.

safmin = dlamch( 'Safe minimum' )
eps = dlamch( 'Precision' )
smlnum = safmin / eps
bignum = one / smlnum
rmin = SQRT( smlnum )
rmax = SQRT( bignum )

!     Scale matrix to allowable range, if necessary.

anrm = dlansy( 'M', uplo, n, a, lda )
iscale = 0
IF( anrm > zero .AND. anrm < rmin ) THEN
  iscale = 1
  sigma = rmin / anrm
ELSE IF( anrm > rmax ) THEN
  iscale = 1
  sigma = rmax / anrm
END IF
IF( iscale == 1 ) THEN
  IF( lower ) THEN
    DO j = 1, n
      CALL dscal( n-j+1, sigma, a( j:, j ), 1 )
    END DO
  ELSE
    DO j = 1, n
      CALL dscal( j, sigma, a( :, j ), 1 )
    END DO
  END IF
END IF

!     Call DSYTRD to reduce symmetric matrix to tridiagonal form.

inde = 1
indtau = inde + n
CALL dsytrd( uplo, n, a, lda, w, work( inde: ), work( indtau: ), info )

!     For eigenvalues only, call DSTERF.  For eigenvectors, first call
!     DORGTR to generate the orthogonal matrix, then call DSTEQR.

IF( .NOT.wantz ) THEN
  CALL dsterf( n, w, work( inde: ), info )
ELSE
  CALL dorgtr( uplo, n, a, lda, work( indtau: ), info )
  CALL dsteqr( jobz, n, w, work( inde: ), a, lda, info )
END IF

!     If matrix was scaled, then rescale eigenvalues appropriately.

IF( iscale == 1 ) THEN
  IF( info == 0 ) THEN
    imax = n
  ELSE
    imax = info - 1
  END IF
  CALL dscal( imax, one / sigma, w, 1 )
END IF

RETURN

!     End of DSYEV

END SUBROUTINE dsyev


END SUBROUTINE DSYEV_F90

MODULE DENSEOP
! 5/Oct/1998-16/Apr/03 by Tomasz Strabel & Ignacy Misztal, University of Georgia.
! Modification 8/15/2001 by Shogo Tsuruta
! Last modified 12/2/2003
USE KINDS; USE LAPACK90
implicit none

INTERFACE SOLVE_S
 MODULE PROCEDURE SOLVS4F, SOLVS8F, SOLVS4T, SOLVS8T
END INTERFACE

INTERFACE CHOL
 MODULE PROCEDURE CHOLS4, CHOLS8, CHOLS4T, CHOLS8T
END INTERFACE

INTERFACE FDET_S
 MODULE PROCEDURE FDETS4, FDETS8, FDETS4P, FDETS8P
END INTERFACE 

INTERFACE INVERSE_S
 MODULE PROCEDURE INVCS4, INVCS8, INVCS4T, INVCS8T
END INTERFACE

INTERFACE EIGEN
 MODULE PROCEDURE SYEV4, SYEV8
END INTERFACE

INTERFACE SOLVE
 MODULE PROCEDURE GESV8, GESV4
END INTERFACE

INTERFACE INVERSE
 MODULE PROCEDURE GEINV8, GEINV4
END INTERFACE

INTERFACE EYE
 MODULE PROCEDURE EYE4, EYE8
END INTERFACE

INTERFACE FSOLVE
 MODULE PROCEDURE FGESV8, FGESV4
END INTERFACE

INTERFACE FCHOL
 MODULE PROCEDURE FCHOLS4, FCHOLS8, FCHOLS4T, FCHOLS8T
END INTERFACE 

INTERFACE FINVERSE_S
 MODULE PROCEDURE FINVCS4, FINVCS8, FINVCS4T, FINVCS8T
END INTERFACE 

INTERFACE FSOLVE_S
 MODULE PROCEDURE FSOLVS4, FSOLVS8, FSOLVS4T, FSOLVS8T
END INTERFACE 

INTERFACE FINVERSE
 MODULE PROCEDURE FGEINV4, FGEINV8
END INTERFACE 

INTERFACE PRINTMAT
 MODULE PROCEDURE PRINTM4, PRINTM8, PRINTM4P, PRINTM8P
END INTERFACE

INTERFACE PACKIT
 MODULE PROCEDURE PACKIT4, PACKIT8
END INTERFACE

INTERFACE UNPACKIT
 MODULE PROCEDURE UNPACKIT4, UNPACKIT8
END INTERFACE

interface diag
  module procedure diag_vec_mat8, diag_mat_vec8, diag_vec_mat4, diag_mat_vec4
end interface  

interface kron
  module procedure kron4,kron8
end interface
	
real(r8)::denseop_tol=1.d-50


CONTAINS

!subroutines with 4/8 in their names work with r4/r8 arguments


SUBROUTINE INVCS4T (x,rank)
!SUBROUTINE INVCS4 TO WORK WITH MATRICES STORED IN PACKED FORM
integer ::i,j,n
integer, intent(out), optional::rank
real(r4)::x(:)
real(r4), allocatable:: x_o(:,:)
n=(-1+sqrt(1.0+8*size(x,1)))/2
allocate(x_o(n,n))

call unpackit(x,x_o)
call inverse_s(x_o,rank)
call packit(x_o,x)
END SUBROUTINE INVCS4T

FUNCTION FINVCS4T(X) RESULT(Y)
 real(r4)::X(:),y(size(x,1))
 y=x
 call inverse_s(y)
END FUNCTION FINVCS4T

FUNCTION FINVCS8T(X) RESULT(Y)
 real(r8)::X(:),y(size(x,1))
 y=x
 call inverse_s(y)
END FUNCTION FINVCS8T

SUBROUTINE INVCS8T (x,rank)
!SUBROUTINE INVCS8 TO WORK WITH MATRICES STORED IN PACKED FORM
integer ::i,j,n
integer, intent(out), optional::rank
real(r8)::x(:)
real(r8), allocatable:: x_o(:,:)
n=(-1+sqrt(1.0+8*size(x,1)))/2
allocate(x_o(n,n))

call unpackit(x,x_o)
call inverse_s(x_o,rank)
call packit(x_o,x)

END SUBROUTINE INVCS8T

SUBROUTINE SOLVS4T (x,v,sol,rank)
!SUBROUTINE INVCS4 TO WORK WITH MATRICES STORED IN PACKED FORM
integer ::i,j,n
integer, intent(out), optional::rank
real(r4)::x(:),v(:),sol(:)
real(r4), allocatable:: x_o(:,:)
n=(-1+sqrt(1.0+8*size(x,1)))/2
allocate(x_o(n,n))
sol=v
call unpackit(x,x_o)
call solvs4(x_o,sol,rank)
deallocate(x_o)
END SUBROUTINE SOLVS4T

FUNCTION FSOLVS4(X,RHS) RESULT(SOL)
 real(r4)::X(:,:),RHS(:),sol(size(x,1))
 sol=0
 call solve_s(x,rhs,sol)
END FUNCTION FSOLVS4

FUNCTION FSOLVS4T(X,RHS) RESULT(SOL)
 real(r4)::X(:),RHS(:),sol(size(rhs,1))
 sol=0
 call solve_s(x,rhs,sol)
END FUNCTION FSOLVS4T

FUNCTION FSOLVS8T(X,RHS) RESULT(SOL)
 real(r8)::X(:),RHS(:),sol(size(rhs,1))
 sol=0
 call solve_s(x,rhs,sol)
END FUNCTION FSOLVS8T

FUNCTION FSOLVS8(X,RHS) RESULT(SOL)
 real(r8)::X(:,:),RHS(:),sol(size(x,1))
 sol=0
 call solve_s(x,rhs,sol)
END FUNCTION FSOLVS8

SUBROUTINE SOLVS8T (x,v,sol,rank)
!SUBROUTINE CHOLS8 TO WORK WITH MATRICES STORED IN PACKED FORM
integer ::i,j,n
integer, intent(out), optional::rank
real(r8)::x(:),v(:),sol(:)
real(r8), allocatable:: x_o(:,:)
n=(-1+sqrt(1.0+8*size(x,1)))/2
allocate(x_o(n,n))
sol=v
call unpackit(x,x_o)
call solvs8(x_o,sol,rank)
deallocate(x_o)
END SUBROUTINE SOLVS8T


SUBROUTINE CHOLS4T (x,rank)
!SUBROUTINE CHOLS4 TO WORK WITH MATRICES STORED IN PACKED FORM
integer ::i,j,n
integer, intent(out), optional::rank
real(r4)::x(:)
real(r4), allocatable:: x_o(:,:)
n=(-1+sqrt(1.0+8*size(x,1)))/2
allocate(x_o(n,n))
call unpackit(x,x_o)
call chols4(x_o,rank)
call packit(x_o,x)
deallocate(x_o)
END SUBROUTINE CHOLS4T

FUNCTION FCHOLS4T(X) RESULT(Y)
 real(r4)::X(:),y(size(x,1))
 y=x
 call chol(y)
END FUNCTION FCHOLS4T

FUNCTION FCHOLS8T(X) RESULT(Y)
 real(r8)::X(:),y(size(x,1))
 y=x
 call chol(y)
END FUNCTION FCHOLS8T

SUBROUTINE CHOLS8T (x,rank)
!SUBROUTINE CHOLS4 TO WORK WITH MATRICES STORED IN PACKED FORM
integer ::i,j,n
integer, intent(out), optional::rank
real(r8)::x(:)
real(r8), allocatable:: x_o(:,:)
n=(-1+sqrt(1.0+8*size(x,1)))/2
allocate(x_o(n,n))

call unpackit(x,x_o)
call chol(x_o,rank)
call packit(x_o,x)

END SUBROUTINE CHOLS8T

SUBROUTINE SOLVS4F(l,rhs,sol,rank)
integer, intent(out), optional::rank
real(r4)::l(:,:),rhs(:),sol(:)
real(r4),allocatable::l_o(:,:)
allocate(l_o(size(l,1),size(l,1)))
l_o=l
sol=rhs
call solvs4(l_o,sol,rank)
deallocate(l_o)
END SUBROUTINE SOLVS4F

SUBROUTINE SOLVS8F(l,rhs,sol,rank)
integer, intent(out), optional::rank
real(r8)::l(:,:),rhs(:),sol(:)
real(r8),allocatable::l_o(:,:)
allocate(l_o(size(l,1),size(l,1)))
l_o=l
sol=rhs
call solvs8(l_o,sol,rank)
deallocate(l_o)
END SUBROUTINE SOLVS8F


SUBROUTINE SOLVS4(l,rhs,rank)
! solve: cholesky & LL'x=rhs
integer::n,i,j,rank_o
integer, intent(out), optional::rank
real(r4)::l(:,:),rhs(:)
real(r4),allocatable::z(:)
allocate(z(size(l,1)))
z=0

rank_o=0
n=size(l,1)

call chols4(l,rank_o) 	! cholesky factorization

z=0			! Lz=y  z=?
do i=1,n
  if (l(i,i)>0)then
    z(i)=(rhs(i)-dot_product(l(i,1:i-1),z(1:i-1)))/l(i,i)
  end if 
end do

rhs=0			! L'x=z x=?
do i=n,1,-1
  if (l(i,i)>0)then
    rhs(i)=(z(i)-dot_product(l(i+1:n,i),rhs(i+1:n)))/l(i,i)
  end if 
end do

if (present(rank)) then
 rank=rank_o
end if

deallocate(z)

END SUBROUTINE SOLVS4

FUNCTION FCHOLS4(X) RESULT(Y)
 real(r4)::X(:,:),y(size(x,1),size(x,1))
 y=x
 call chol(y)
END FUNCTION FCHOLS4

FUNCTION FCHOLS8(X)  RESULT(Y)
 real(r8)::X(:,:),y(size(x,1),size(x,1))
 y=x
 call chol(y)
END FUNCTION FCHOLS8


SUBROUTINE CHOLS4(x,rank)
! cholesky decomposition
integer :: n,i,j,k,rank_o
integer, intent(out), optional::rank
real(r4)::diagsq
real(r4)::x(:,:)
rank_o=0
n=size(x,1)
do i=1,n
   diagsq=x(i,i)-dot_product(x(i,1:i-1),x(i,1:i-1))
   if (abs(diagsq).lt.denseop_tol) then
        x(i,:)=0;x(:,i)=0       !zero row and column
      elseif (diagsq.lt.0) then
        print*,' Matrix not semipositive-definite, row ',i
        stop
!   endif
   else
    rank_o=rank_o+1
   x(i,i)=sqrt(diagsq)
   do j=i+1,n     
      x(j,i)=(x(j,i)-dot_product(x(j,1:i-1),x(i,1:i-1)))/x(i,i)
      x(i,j)=x(j,i)
   enddo
   end if
enddo

! zero upper-diagonals
do i=1,n
   x(i,i+1:n)=0
enddo   

if (present(rank))then
 rank=rank_o
end if
END SUBROUTINE CHOLS4


SUBROUTINE INVCS4(l,rank)
! calculates inverse of LL', where L is cholesky decomposition 
!USE LA_PRECISION, ONLY: WP => DP
integer  ::n,i,j,rank_o
integer, intent(out), optional::rank
real(r4) :: l(:,:)
real(r4) ::w(size(l,1))
n=size(l,1)

rank_o=0

call chols4(l,rank_o) 	! cholesky factorization

do i=1,n
   w(i:n)=0
   if (abs(l(i,i)).gt.denseop_tol) w(i)=1/l(i,i)
   ! forward substitution
   do j=i+1,n
    if (abs(l(j,j)).gt.denseop_tol)  w(j)=-dot_product(l(j,i:j-1),w(i:j-1))/l(j,j)
   enddo
   !backward substitution
   do j=n,i,-1
    if (abs(l(j,j)).gt.denseop_tol)  w(j)=(w(j)-dot_product(l(j+1:n,j),w(j+1:n)))/l(j,j)
   enddo
   l(i:n,i)=w(i:n)
   l(i,i:n)=w(i:n)
enddo   

if (present(rank)) then
 rank=rank_o
end if

END SUBROUTINE INVCS4

FUNCTION FINVCS4(X) RESULT(Y)
 real(r4)::X(:,:),y(size(x,1),size(x,1))
 y=x
 call inverse_s(y)
END FUNCTION FINVCS4

FUNCTION FINVCS8(X) RESULT(Y)
 real(r8)::X(:,:),y(size(x,1),size(x,1))
 y=x
 call inverse_s(y)
END FUNCTION FINVCS8


SUBROUTINE CHOLS8(x,rank)
! cholesky decomposition
integer :: n,i,j,k,rank_o
integer, intent(out), optional::rank
real(r8)::diagsq,d_p
real(r8)::x(:,:)
rank_o=0
n=size(x,1)
do i=1,n
   d_p=0
   do k=1,i-1
      d_p=d_p+x(i,k)*x(i,k)
   enddo
!   diagsq=x(i,i)-dot_product(x(i,1:i-1),x(i,1:i-1))
   diagsq=x(i,i)-d_p
  if (abs(diagsq).lt.denseop_tol) then
        x(i,:)=0;x(:,i)=0       !zero row and column
      elseif (diagsq.lt.0) then
        print*,' Matrix not semipositive-definite, row ',i
        stop
!   endif
   else
    rank_o=rank_o+1
   x(i,i)=sqrt(diagsq)
   do j=i+1,n     
      d_p=0
      do k=1,i-1
         d_p=d_p+x(j,k)*x(i,k)
      end do
!      x(j,i)=(x(j,i)-dot_product(x(j,1:i-1),x(i,1:i-1)))/x(i,i)
      x(j,i)=(x(j,i)-d_p)/x(i,i)
      x(i,j)=x(j,i)
   enddo
   end if
enddo
! zero upper-diagonals

do i=1,n
   x(i,i+1:n)=0
enddo   

if (present(rank))then
 rank=rank_o
end if

END SUBROUTINE CHOLS8


SUBROUTINE SOLVS8(l,rhs,rank)
! solve: cholesky & LL'x=rhs
integer::n,i,j,rank_o
integer, intent(out), optional::rank
real(r8)::l(:,:),rhs(:)
real(r8)::z(size(l,1))
z=0

rank_o=0
n=size(l,1)

call chols8(l,rank_o) 	! cholesky factorization

z=0			! Lz=y  z=?
do i=1,n
  if (l(i,i)>0)then
    z(i)=(rhs(i)-dot_product(l(i,1:i-1),z(1:i-1)))/l(i,i)
  end if 
end do

rhs=0			! L'x=z x=?
do i=n,1,-1
  if (l(i,i)>0)then
    rhs(i)=(z(i)-dot_product(l(i+1:n,i),rhs(i+1:n)))/l(i,i)
  end if 
end do

if (present(rank)) then
 rank=rank_o
end if

END SUBROUTINE SOLVS8

FUNCTION FDETS8(matrix) result(det)
integer::i
real(r8)::matrix(:,:),matrix_o(size(matrix,1),size(matrix,1)),det
matrix_o=matrix
call chol(matrix_o)
det=1
do i=1,size(matrix,1)
   if (matrix_o(i,i)/=0.0) det=det*matrix_o(i,i)
end do
det=det**2
END FUNCTION FDETS8

FUNCTION FDETS4(matrix) result(det)
real(r4)::matrix(:,:),det
real(r8)::matrix_o(size(matrix,1),size(matrix,1))
matrix_o=matrix
det=fdets8(matrix_o)
END FUNCTION FDETS4

FUNCTION FDETS4P(matrix) result(det)
integer::n
real(r4)::matrix(:),det 
real(r4),allocatable::matrix_o(:,:)
n=(-1+sqrt(1.0+8*size(matrix,1)))/2
allocate(matrix_o(n,n))
call unpackit(matrix,matrix_o)
det=fdets4(matrix_o)
END FUNCTION FDETS4P

FUNCTION FDETS8P(matrix) result(det)
integer::n
real(r8)::matrix(:),det 
real(r8),allocatable::matrix_o(:,:)
n=(-1+sqrt(1.0+8*size(matrix,1)))/2
allocate(matrix_o(n,n))
call unpackit(matrix,matrix_o)
det=fdets8(matrix_o)
deallocate(matrix_o)
END FUNCTION FDETS8P

SUBROUTINE INVCS8(l,rank)
! calculates inverse of LL', where L is cholesky decomposition 
!USE LA_PRECISION, ONLY: WP => DP
integer  ::n,i,j,k,rank_o
integer, intent(out), optional::rank
real(r8) :: l(:,:),d_p
real(r8) ::w(size(l,1))
n=size(l,1)

rank_o=0

call chols8(l,rank_o) 	! cholesky factorization

do i=1,n
   w(i:n)=0
   if (abs(l(i,i)).gt.denseop_tol) w(i)=1/l(i,i)
   ! forward substitution
   do j=i+1,n
      d_p=0
      do k=i,j-1
         d_p=d_p+l(j,k)*w(k)
      end do
!      if (abs(l(j,j)).gt.denseop_tol) w(j)=-dot_product(l(j,i:j-1),w(i:j-1))/l(j,j)
      if (abs(l(j,j)).gt.denseop_tol) w(j)=-d_p/l(j,j)
   enddo
   !backward substitution
   do j=n,i,-1
      d_p=0
      do k=j+1,n
         d_p=d_p+l(k,j)*w(k)
      end do
!      if (abs(l(j,j)).gt.denseop_tol) w(j)=(w(j)-dot_product(l(j+1:n,j),w(j+1:n)))/l(j,j)
      if (abs(l(j,j)).gt.denseop_tol) w(j)=(w(j)-d_p)/l(j,j)
   enddo
   l(i:n,i)=w(i:n)
   l(i,i:n)=w(i:n)
enddo   

if (present(rank)) then
 rank=rank_o
end if

END SUBROUTINE INVCS8


FUNCTION POS_SYMM(i,j,n) result (address)
 !finds position of (i,j) element of a n*n matrix upper-stored
 integer :: i,j,n,address
 if (j >= i) then
      address=(i-1)*n-(i*(i-3))/2+j-i
   else
      address=(j-1)*n-(j*(j-3))/2+i-j
 endif
END FUNCTION POS_SYMM

function eye4(m) 
integer::i,n
real(r4)::m(:,:)
real(r4)::eye4(size(m,1),size(m,1))
n=size(m,1)
eye4=0
do i=1,n
 eye4(i,i)=1
end do
end function eye4

function eye8(m) 
integer::i,n
real(r8)::m(:,:)
real(r8)::eye8(size(m,1),size(m,1))
n=size(m,1)
eye8=0
do i=1,n
 eye8(i,i)=1
end do
end function eye8

!-------------------------------------------------------!
!interfaces to work with subroutines of lapack90        !
!-------------------------------------------------------!

FUNCTION FGESV4(X,RHS) RESULT(SOL)
 real(r4)::X(:,:),RHS(:),sol(size(x,1))
 sol=0
 call solve(x,rhs,sol)
END FUNCTION FGESV4

FUNCTION FGESV8(X,RHS) RESULT(SOL)
 real(r8)::X(:,:),RHS(:),sol(size(x,1))
 sol=0
 call solve(x,rhs,sol)
END FUNCTION FGESV8

FUNCTION FGEINV4(X) RESULT(Y)
 real(r4)::X(:,:),y(size(x,1),size(x,1))
 y=x
 call inverse(y)
END FUNCTION FGEINV4

FUNCTION FGEINV8(X) RESULT(Y)
 real(r8)::X(:,:),y(size(x,1),size(x,1))
 y=x
 call inverse(y)
END FUNCTION FGEINV8

SUBROUTINE GEINV8 (x)
USE LAPACK90
 real(r8)::x(:,:)
 real(r8),allocatable::x_o(:,:)
 allocate(x_o(size(x,1),size(x,1)))
 x_o=eye(x_o)
 CALL dgesv_f90(x,x_o)
 x=x_o
END SUBROUTINE GEINV8

SUBROUTINE GEINV4(x)
USE LAPACK90
real(r8), allocatable:: x_o(:,:),x_o1(:,:)
real(r4)::x(:,:)
allocate (x_o(size(x,1),size(x,1)))
allocate (x_o1(size(x,1),size(x,1)))
x_o=eye(x_o1)
x_o1=x
CALL dgesv_f90(x_o1,x_o)
x=x_o
END SUBROUTINE GEINV4

SUBROUTINE GESV8(x,v,sol)
USE LAPACK90
integer::i
real(r8)::x(:,:),v(:),sol(:)
real(r8),allocatable::v_o(:,:),x_o(:,:)

!convert vector v(:) -> v_o(:,1)
allocate(v_o(size(v,1),1))
allocate(x_o(size(x,1),size(x,1)))
x_o=x
 do i=1,size(v,1)
   v_o(i,1)=v(i)
 end do

call dgesv_f90(x_o,v_o)

!rewrite vector v_o(:,1) -> sol(:)
 do i=1,size(v,1)
   sol(i)=v_o(i,1)
 end do

END SUBROUTINE GESV8

SUBROUTINE GESV4(x,v,sol)
USE LAPACK90
integer::i
real(r4)::x(:,:),v(:),sol(:)
real(r8),allocatable::v_o(:,:)
real(r8),allocatable::x_o(:,:)
allocate(x_o(size(x,1),size(x,1)))
x_o=x

!convert vector v(:) -> v_o(:,1)
allocate(v_o(size(v,1),1))
 do i=1,size(v,1)
   v_o(i,1)=v(i)
 end do

call dgesv_f90(x_o,v_o)

!rewrite vector v_o(:,1) -> v(:)
 do i=1,size(v,1)
   sol(i)=v_o(i,1)
 end do
END SUBROUTINE GESV4

SUBROUTINE SYEV8(x,v,d)
USE LAPACK90
real(r8)::x(:,:),v(:),d(:,:)
d=x
call dsyev_f90(d,v,'v')
END SUBROUTINE SYEV8

SUBROUTINE SYEV4(x,v,d)
USE LAPACK90
real(r8), allocatable:: x_o(:,:), v_o(:)
real(r4)::x(:,:),v(:),d(:,:)
allocate (x_o(size(x,1),size(x,1)))
allocate (v_o(size(v,1)))
x_o=x
v_o=v
call dsyev_f90(x_o,v_o,'v')
d=x_o
v=v_o
END SUBROUTINE SYEV4

SUBROUTINE PRINTM4 (matrix,text,fmt,un)
!AL modified to print matrix of any shape
! prints matrix and text(optionaly) using form (optionally) to standard
! output or a unit(optionally)
character(*), optional::text*(*),fmt
character(40)::fmt1
integer :: n,i
integer,optional::un
real (r4):: matrix(:,:)
if (present(text)) then
   if (present(un)) then
      write(un,'(a)')text
     else
      print*,text
    end if
endif
if (present(fmt)) then; fmt1=fmt;else
fmt1='(10g12.4/)';end if
n=(size(matrix,1))
do i=1,n
   if (present(un)) then
         write(un,fmt1)matrix(i,:)
      else
         print fmt1,matrix(i,:)
   endif 
enddo
END SUBROUTINE PRINTM4

SUBROUTINE PRINTM8 (matrix,text,fmt,un)
!AL modified to print matrix of any shape
! prints matrix and text(optionaly) using form (optionally) to standard
! output or a unit(optionally)
character(*), optional::text*(*),fmt
character(40)::fmt1
integer :: n,i
integer,optional::un
real (r8):: matrix(:,:)
if (present(text)) then
   if (present(un)) then
         write(un,'(a)')text
      else
         print*,text; 
   endif
end if
if (present(fmt)) then; fmt1=fmt;else
fmt1='(10g12.4/)';end if
n=(size(matrix,1))
do i=1,n
   if (present(un)) then
         write(un,fmt1)matrix(i,:)
      else
         print fmt1,matrix(i,:)
   endif
enddo
END SUBROUTINE PRINTM8

SUBROUTINE PRINTM4P (matrix,text,fmt,un)
! prints matrix and text(optionally) using form (optionally) to standard
! output or a unit(optionally)
character(*), optional::text*(*),fmt
integer,optional::un
integer :: n,i
real (r4):: matrix(:)
real (r4), allocatable::matrix_f(:,:)
n=(-1+sqrt(1.0+8*size(matrix,1)))/2
allocate(matrix_f(n,n))
call unpackit(matrix,matrix_f)
call printmat(matrix_f,text,fmt,un)
END SUBROUTINE PRINTM4P

SUBROUTINE PRINTM8P (matrix,text,fmt,un)
! prints matrix and text(optionally) using form (optionally) to standard
! output or a unit(optionally)
character(*), optional::text*(*),fmt
integer,optional::un
integer :: n,i
real (r8):: matrix(:)
real (r8), allocatable::matrix_f(:,:)
n=(-1+sqrt(1.0+8*size(matrix,1)))/2
allocate(matrix_f(n,n))
call unpackit(matrix,matrix_f)
call printmat(matrix_f,text,fmt,un)
END SUBROUTINE PRINTM8P


SUBROUTINE PACKIT4(fullmx,packedmx)
!conversion n*n matrix to triangular storage
integer::i,j,n
real(r4)::fullmx(:,:),packedmx(:)
n=size(fullmx,1)
 do i=1,n
  do j=1,i
    packedmx(pos_symm(i,j,n))=fullmx(i,j)
  end do
 end do
END SUBROUTINE PACKIT4

SUBROUTINE PACKIT8(fullmx,packedmx)
!conversion n*n matrix to triangular storage
integer::i,j,n
real(r8)::fullmx(:,:),packedmx(:)
n=size(fullmx,1)
 do i=1,n
  do j=1,i
    packedmx(pos_symm(i,j,n))=fullmx(i,j)
  end do
 end do
END SUBROUTINE PACKIT8


SUBROUTINE UNPACKIT4(packedmx,fullmx)
!conversion from packed to quadratic form
integer::i,j,n
real(r4)::fullmx(:,:),packedmx(:)
fullmx=0
n=size(fullmx,1)
 do i=1,n
  do j=1,i
     fullmx(i,j)=packedmx(pos_symm(i,j,n))
     fullmx(j,i)=packedmx(pos_symm(i,j,n))
  end do
 end do
END SUBROUTINE UNPACKIT4

SUBROUTINE UNPACKIT8(packedmx,fullmx)
!conversion from packed to quadratic form
integer::i,j,n
real(r8)::fullmx(:,:),packedmx(:)
fullmx=0
n=size(fullmx,1)
 do i=1,n
  do j=1,i
     fullmx(i,j)=packedmx(pos_symm(i,j,n))
     fullmx(j,i)=packedmx(pos_symm(i,j,n))
  end do
 end do
END SUBROUTINE UNPACKIT8

function diag_mat_vec8(x) result (y)
! y=diag(x) if x is a matrix and y a vector
real (r8)::x(:,:)
real (r8)::y(size(x,1))
integer::i
!
do i=1,size(y)
   y(i)=x(i,i)
enddo
end function   

function diag_mat_vec4(x) result (y)
! single-precision version
real (r4)::x(:,:)
real (r4)::y(size(x,1))
integer::i
!
do i=1,size(y)
   y(i)=x(i,i)
enddo
end function   



function diag_vec_mat8(x) result (y)
! y=diag(x) if x is a vector and y a matrix
real (r8)::x(:)
real (r8)::y(size(x),size(x))
integer::i
!
y=0
do i=1,size(x)
   y(i,i)=x(i)
enddo
end function   


function diag_vec_mat4(x) result (y)
! single-precision version
real (r4)::x(:)
real (r4)::y(size(x),size(x))
integer::i
!
y=0
do i=1,size(x)
   y(i,i)=x(i)
enddo
end function   

  

RECURSIVE SUBROUTINE POS_DEF(x,text,min_eig,stat)
! Ensures that symmetric X is positive definite with condition number
! >= 1/min_eig; ignores columns/rows with 0 values.
!
! Decomposes X = VDV', where D are eigenvalues and V eigenvectors while 
! ignoring rows/columns with 0 only. 
!
! If min(d) < min_eig * max(d), then .
!	a) if optional variable text is present, it is printed,

!	c) if optional min_eig is present, then:
!		if |d(i)| < min_eig * max(d) then d(i)=min_eig * max(d)
!	   if optional min_eig is missing , then:
!		if |d(i)| < min_eig_const * max(d) then &
!                                        d(i)=min_eig_const * max(d)
!	d) X = VDV'					
!	e) if optional stat is present, it is set to .true.
!

real (r8)::x(:,:)
character (*),optional::text
real (r8), optional::min_eig
real (r8)::v(size(x,1),size(x,1)),d(size(x,1))
real (r8),allocatable::y(:,:)
integer::nzero(size(x,1)),nnz,n,i
logical,optional::stat
real (r8), parameter::min_eig_const=1e-5
!
if (present(stat)) stat=.false.

! if some rows/columns zero, skip them
 n=size(x,1); nnz=0
 do i=1,n
    if (sum(x(i,:)**2)+sum(x(:,i)**2) == 0) cycle
     nnz=nnz+1
     nzero(nnz)=i
 enddo
 
 if (nnz .ne. n) then
    allocate(y(nnz,nnz))
    y=x(nzero(1:nnz),nzero(1:nnz))
    call pos_def(y,text,min_eig,stat)    
    x(nzero(1:nnz),nzero(1:nnz))=y
    deallocate(y)
    return
 endif   

call eigen(x,d,v)

! Set minimum "correct" eigenvalus to a small negative number rather than
! 0; subroutine eigen may return small negative eigenvalue(s) for
! semi-positive matrices. 
! min_eig = relative eigenvalues = eigenvalues / maximum eigenvalues

  if (minval(d) < min_eig) then	
     if (present(stat)) stat=.true.
     if (present(min_eig)) then
         where (d/maxval(d) < min_eig) d= min_eig * maxval(d)
       else
         where (d/maxval(d) < min_eig_const) d= min_eig_const * maxval(d)
     endif 
     if (present(text)) print*,text
     x=0
     x=matmul(matmul(v,diag(d)),transpose(v))
  endif
  
  ! check for compiler bug in Absoft f90 version 6
  !if (maxval(matmul(v,transpose(v))-eye(v)) > 1d-6) then
  !   print*,'eigenvalues incorrect; recompile lapack90r without optimization'
  !   stop
  !endif   
end subroutine  


  function kron8(x,y) result (z)
! z = x Kronecker_product y for real r8

  real (r8)::x(:,:),y(:,:)
  real (r8)::z(size(x,dim=1)*size(y,dim=1), size(x,dim=2)*size(y,dim=2))
  integer::i,j,nx1,nx2,ny1,ny2
 
  nx1=size(x,dim=1); nx2= size(x,dim=2)
  ny1=size(y,dim=1); ny2= size(y,dim=2)
 
  do i=1,ny1
     do j=1,ny2
     !print*,'k',i,j,1+(i-1)*nx1,i*nx1,1+(j-1)*nx2,j*nx2,x,y(i,j)
        z(1+(i-1)*nx1:i*nx1,1+(j-1)*nx2:j*nx2)=x*y(i,j)
     enddo
  enddo
  end function   	


 function kron4(x,y) result (z)
 !z = x Kronecker_product y for real r4

 real (r4)::x(:,:),y(:,:)					       
 real (r4)::z(size(x,dim=1)*size(y,dim=1), size(x,dim=2)*size(y,dim=2))
 integer::i,j,nx1,nx2,ny1,ny2

 nx1=size(x,dim=1); nx2= size(x,dim=2)
 ny1=size(y,dim=1); ny2= size(y,dim=2)

 do i=1,ny1
    do j=1,ny2
    !print*,'k',i,j,1+(i-1)*nx1,i*nx1,1+(j-1)*nx2,j*nx2,x,y(i,j)
       z(1+(i-1)*nx1:i*nx1,1+(j-1)*nx2:j*nx2)=x*y(i,j)
    enddo
 enddo
 end function
 
 
END MODULE DENSEOP
!This is a collection of modules for own use!

module kinds_
! From Ignacy Misztal BLUPF90 distribution 
  integer, parameter :: single = SELECTED_REAL_KIND( 6, 37 )
  integer, parameter :: double = SELECTED_REAL_KIND( 15, 307 )
  integer, parameter :: extended = SELECTED_REAL_KIND( 18, 4931 )
  integer, parameter :: r4 = SELECTED_REAL_KIND( 6, 37 )
  integer, parameter :: r8 = SELECTED_REAL_KIND( 15, 307 )
  integer, parameter :: r16 = SELECTED_REAL_KIND( 18, 4931 )

  ! current precison for hash storage
  integer, parameter :: rh=r8
end module kinds_
!---------------!

Module statrank
use denseop

CONTAINS
function rankmatrix(a,min) result(rank)
!return rank and number of eigenvalues<0 of a matrix (rank: # of eigenvalues>min (optional))
implicit none
real(r8):: a(:,:),min2
real(r8),optional:: min
real(r8):: d(size(a,1)),V(size(a,1),size(a,1))
integer :: rank,i

rank=0
min2=1e-20
if(present(min)) min2=min

call eigen(a,d,V)

do i=1,size(a,1)
  if(d(i)>min2) rank=rank+1
enddo

end function

subroutine rankeigen(a,min) 
!return rank and number of eigenvalues<0 of a matrix (rank: # of eigenvalues>min (optional))
implicit none
real(r8):: a(:,:),min2
real(r8),optional:: min
real(r8):: d(size(a,1)),V(size(a,1),size(a,1))
integer :: rank,i

rank=0
min2=1e-20
if(present(min)) min2=min

call eigen(a,d,V)

do i=1,size(a,1)
  if(d(i)>min2) rank=rank+1
enddo
print *,'rank: ',rank
print *,'eigenvalues',d

end subroutine

function corr(a) result(b)
!copied from IM's REMLf90
! b=corr(a) with accounting for zero diagonals
real (r8)::a(:,:),b(size(a,dim=1),size(a,dim=1))
integer::i,j
!
b=0
do i=1,size(a,dim=1)
   do j=1,size(a,dim=1)
      if (a(i,i)>0 .and. a(j,j)>0) then
	 b(i,j)=a(i,j)/sqrt(a(i,i)*a(j,j))
      endif
   enddo
enddo
end function   	

end module statrank
!-----------------!

module stat
use kinds
contains

! ---------------------------
      function normalcdf(x)
      ! returns cumulative normal density function
      implicit none
      double precision:: normalcdf,x,alnorm,q,pdf
      !
      !normalcdf=alnorm(x,.false.)
       call  NORMP(x, normalcdf, Q, PDF)

      end function


!-----------------------------------
      SUBROUTINE NORMP(Z, P, Q, PDF)
!
!    Normal distribution probabilities accurate to 1.e-15.
!    Z = no. of standard deviations from the mean.
!    P, Q = probabilities to the left & right of Z.   P + Q = 1.
!       PDF = the probability density.
!
!       Based upon algorithm 5666 for the error function, from:
!       Hart, J.F. et al, 'Computer Approximations', Wiley 1968
!
!       Programmer: Alan Miller
!
!    Latest revision - 30 March 1986
!
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DATA P0, P1, P2, P3, P4, P5, P6/220.2068679123761D0, &
           221.2135961699311D0, 112.0792914978709D0, &
           33.91286607838300D0, 6.373962203531650D0, &
          .7003830644436881D0, .3526249659989109D-01/, &
           Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7/440.4137358247522D0, &
           793.8265125199484D0, 637.3336333788311D0, &
           296.5642487796737D0, 86.78073220294608D0, &
           16.06417757920695D0, 1.755667163182642D0, &
           .8838834764831844D-1/, &
           CUTOFF/7.071D0/, ROOT2PI/2.506628274631001D0/
!
      ZABS = ABS(Z)
!
!      |Z| > 37.
!
      IF (ZABS .GT. 37.D0) THEN
        PDF = 0.D0
        IF (Z .GT. 0.D0) THEN
          P = 1.D0
          Q = 0.D0
        ELSE
          P = 0.D0
          Q = 1.D0
        END IF
        RETURN
      END IF
!
!      |Z| <= 37.
!
      EXPNTL = EXP(-0.5D0*ZABS**2)
      PDF = EXPNTL/ROOT2PI
!
!      |Z| < CUTOFF = 10/sqrt(2).
!
      IF (ZABS .LT. CUTOFF) THEN
        P = EXPNTL*((((((P6*ZABS + P5)*ZABS + P4)*ZABS + P3)*ZABS + &
             P2)*ZABS + P1)*ZABS + P0)/(((((((Q7*ZABS + Q6)*ZABS + &
             Q5)*ZABS + Q4)*ZABS + Q3)*ZABS + Q2)*ZABS + Q1)*ZABS + &
             Q0)
!
!      |Z| >= CUTOFF.
!
      ELSE
        P = PDF/(ZABS + 1.D0/(ZABS + 2.D0/(ZABS + 3.D0/(ZABS + 4.D0/ &
         (ZABS + 0.65D0)))))
      END IF
!
      IF (Z .LT. 0.D0) THEN
        Q = 1.D0 - P
      ELSE
        Q = P
        P = 1.D0 - Q
      END IF
      END subroutine

! ---------------------------
      function dnormal(x,mean,var)
      !
      ! Returns probability density of normal density function
      !
      implicit none
      double precision:: dnormal,x,mean,var
      double precision:: pi
!      dnormal=1d0/(sqrt(2 pi) sigma) e^-((x - mu)^2/(2 sigma^2))      
      pi=3.14159265358979d0
      dnormal=(1.d0/sqrt(2.d0*pi*var)) * dexp(- ((x-mean)**2)/(2.d0*var) )
      
      end function
     
  function mean(x) 
  !mean of x, brutal force
  implicit none
  real(r8)::x(:),mean
  
  mean=sum(x)/dble(size(x))
  end function

  function compute_var(x) 
  !variance of x, brutal force
  implicit none
  real(r8)::x(:),mean,compute_var
   
  mean=sum(x)/size(x)
  compute_var=sum((x-mean)**2)/(size(x)-1)
  end function
      
  function compute_covar(x,y) 
  !covariance of xy, brutal force
  implicit none
  real(r8)::x(:),y(:),meanx,meany,compute_covar
  real(r8)::x1(size(x)),y1(size(y))
  if(size(x)/=size(y)) then
    print *,'lengths of x and y differ, covar'
  endif
  
  !x1=x-sum(x)/dble(size(x))
  !y1=y-sum(y)/dble(size(y))
  ! Minvielle p 194
  compute_covar=( &
  sum(x*y)/size(x)-(sum(x)/size(x))*(sum(y)/size(x)) &
  )
  !compute_covar=dot_product(x1,y1)/dble(size(x)-1)
  end function
      
  function compute_corr(x,y) 
  !corr of xy, brutal force
  implicit none
  real(r8)::x(:),y(:),meanx,meany,compute_corr
  if(size(x)/=size(y)) then
    print *,'lengths of x and y differ, covar'
  endif
  
  compute_corr=compute_covar(x,y)/sqrt(compute_var(x)*compute_var(y))
  end function
      
    function compute_corr2(x,y) 
  !corr of xy, brutal force, assuming they are centered
  implicit none
  real(r8)::x(:),y(:),meanx,meany,compute_corr2
  if(size(x)/=size(y)) then
    print *,'lengths of x and y differ, covar'
  endif
  
  compute_corr2=(dot_product(x,y)/sqrt(dot_product(x,x)*dot_product(y,y)))
  end function
      
      
function log_like_normal(e,vare)
! log-likelihood of e~MVN(0,R); R=vare kron I
implicit none
real(r8):: e(:),vare,log_like_normal

!log_like_normal=-.5d0*size(e)*log(2d0*3.14159265d0*vare)-.5d0*sum(e**2)/vare
log_like_normal=-.5d0*size(e)*log(2d0*3.14159265d0*vare)-.5d0*dot_product(e,e)/vare
!if(log_like_normal>0d0) then
!  print *,'log_like_normal>0d0',log_like_normal
!  print *,'vare',vare
!  print *,'e''e',dot_product(e,e)
!  print *,'size(e)',size(e)
!  print *,'first',-.5d0*size(e)*log(2d0*3.14159265d0*vare)
!  print *,'second',-.5d0*dot_product(e,e)/vare
!endif  

end function


function logdchisq(x,ndf) result (logl)!AL
! log-density of sample x in the chi squared distribution with df=ndf
! implicit none
real(r8)::ndf,x,logl
!fromula from wikipedia
logl=log(.5)*ndf/2d0-lngamma(ndf/2d0)+(ndf/2d0-1d0)*log(x)-x/2d0
end function


FUNCTION chi_squared(chi2,ndf) RESULT(prob)
! by Alan Miller, http://users.bigpond.net.au/amiller/logistic/logistic.f90
! Calculate the chi-squared distribution function
! ndf  = number of degrees of freedom
! chi2 = chi-squared value
! prob = probability of a chi-squared value <= chi2 (i.e. the left-hand
!        tail area)

INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307) !=real(8) con DVF
real(dp), INTENT(IN)    :: ndf
REAL (dp), INTENT(IN)  :: chi2
REAL (dp)              :: prob

! Local variables
REAL (dp) :: half = 0.5_dp, x, p

x = half * chi2
p = half * REAL(ndf)
prob = gammad(x, p)
RETURN

END FUNCTION chi_squared



FUNCTION gammad(x, p) RESULT(gamma_prob)
! by Alan Miller, http://users.bigpond.net.au/amiller/logistic/logistic.f90
!  ALGORITHM AS239  APPL. STATIST. (1988) VOL. 37, NO. 3

!  Computation of the Incomplete Gamma Integral

!  Auxiliary functions required: ALNORM = algorithm AS66 (included) & LNGAMMA

!  Converted to be compatible with ELF90 by Alan Miller
!  N.B. The return parameter IFAULT has been removed as ELF90 allows only
!  one output parameter from functions.   An error message is issued instead.

! This revision - 15 October 1996

INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307) !=real(8) con DVF
REAL (dp), INTENT(IN) :: x, p
REAL (dp)             :: gamma_prob

!     Local variables

REAL (dp) :: pn1, pn2, pn3, pn4, pn5, pn6, tol = 1.d-14, oflo = 1.d+37,  &
             xbig = 1.d+8, arg, c, rn, a, b, one = 1._dp, zero = 0._dp, an, &
             two = 2._dp, elimit = -88._dp, plimit = 1000._dp, three = 3._dp, &
             nine = 9._dp

gamma_prob = zero

!      Check that we have valid values for X and P

IF (p <= zero .OR. x < zero) THEN
  WRITE(*, *)'Error: Function gammad.  1st argument < 0 or 2nd argument <= 0'
  RETURN
END IF
IF (x == zero) RETURN

!      Use a normal approximation if P > PLIMIT

IF (p > plimit) THEN
  pn1 = three * SQRT(p) * ((x / p) ** (one / three) + one / (nine * p) - one)
  gamma_prob = alnorm(pn1, .false.)
  RETURN
END IF

!      If X is extremely large compared to P then set gamma_prob = 1

IF (x > xbig) THEN
  gamma_prob = one
  RETURN
END IF

IF (x <= one .OR. x < p) THEN

!      Use Pearson's series expansion.
!      (Note that P is not large enough to force overflow in LNGAMMA)

  arg = p * LOG(x) - x - lngamma(p + one)
  c = one
  gamma_prob = one
  a = p
  DO
    a = a + one
    c = c * x / a
    gamma_prob = gamma_prob + c
    IF (c < tol) EXIT
  END DO
  arg = arg + LOG(gamma_prob)
  gamma_prob = zero
  IF (arg >= elimit) gamma_prob = EXP(arg)

ELSE

!      Use a continued fraction expansion

  arg = p * LOG(x) - x - lngamma(p)
  a = one - p
  b = a + x + one
  c = zero
  pn1 = one
  pn2 = x
  pn3 = x + one
  pn4 = x * b
  gamma_prob = pn3 / pn4
  DO
    a = a + one
    b = b + two
    c = c + one
    an = a * c
    pn5 = b * pn3 - an * pn1
    pn6 = b * pn4 - an * pn2
    IF (ABS(pn6) > zero) THEN
      rn = pn5 / pn6
      IF (ABS(gamma_prob - rn) <= MIN(tol, tol * rn)) EXIT
      gamma_prob = rn
    END IF

    pn1 = pn3
    pn2 = pn4
    pn3 = pn5
    pn4 = pn6
    IF (ABS(pn5) >= oflo) THEN

  !      Re-scale terms in continued fraction if terms are large

      pn1 = pn1 / oflo
      pn2 = pn2 / oflo
      pn3 = pn3 / oflo
      pn4 = pn4 / oflo
    END IF
  END DO
  arg = arg + LOG(gamma_prob)
  gamma_prob = one
  IF (arg >= elimit) gamma_prob = one - EXP(arg)
END IF

RETURN
END FUNCTION gammad


FUNCTION alnorm( x, upper ) RESULT( fn_val )
!  Algorithm AS66 Applied Statistics (1973) vol.22, no.3

!  Evaluates the tail area of the standardised normal curve
!  from x to infinity if upper is .true. or
!  from minus infinity to x if upper is .false.

! ELF90-compatible version by Alan Miller
! Latest revision - 29 November 2001
   IMPLICIT NONE
   INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 100)
   REAL(DP), INTENT(IN)   ::  x
   LOGICAL,   INTENT(IN)  ::  upper
   REAL(DP)               ::  fn_val

   !  Local variables
   REAL(DP), PARAMETER   ::  zero=0.0_DP, one=1.0_DP, half=0.5_DP, con=1.28_DP
   REAL(DP)              ::  z, y
   LOGICAL               ::  up

   !  Machine dependent constants
   REAL(DP), PARAMETER  ::  ltone = 7.0_DP, utzero = 18.66_DP
   REAL(DP), PARAMETER  ::  p = 0.398942280444_DP, q = 0.39990348504_DP,   &
                            r = 0.398942280385_DP, a1 = 5.75885480458_DP,  &
                            a2 = 2.62433121679_DP, a3 = 5.92885724438_DP,  &
                            b1 = -29.8213557807_DP, b2 = 48.6959930692_DP, &
                            c1 = -3.8052E-8_DP, c2 = 3.98064794E-4_DP,     &
                            c3 = -0.151679116635_DP, c4 = 4.8385912808_DP, &
                            c5 = 0.742380924027_DP, c6 = 3.99019417011_DP, &
                            d1 = 1.00000615302_DP, d2 = 1.98615381364_DP,  &
                            d3 = 5.29330324926_DP, d4 = -15.1508972451_DP, &
                            d5 = 30.789933034_DP

   up = upper
   z = x
   IF( z < zero ) THEN
      up = .NOT. up
      z = -z
   END IF
   IF( z <= ltone  .OR.  (up  .AND.  z <= utzero) ) THEN
      y = half*z*z
      IF( z > con ) THEN
         fn_val = r*EXP( -y )/(z+c1+d1/(z+c2+d2/(z+c3+d3/(z+c4+d4/(z+c5+d5/(z+c6))))))
      ELSE
         fn_val = half - z*(p-q*y/(y+a1+b1/(y+a2+b2/(y+a3))))
      END IF
   ELSE
      fn_val = zero
   END IF

   IF( .NOT. up ) fn_val = one - fn_val
   RETURN
END FUNCTION alnorm


FUNCTION lngamma(z) RESULT(lanczos)

!  Uses Lanczos-type approximation to ln(gamma) for z > 0.
!  Reference:
!       Lanczos, C. 'A precision approximation of the gamma
!               function', J. SIAM Numer. Anal., B, 1, 86-96, 1964.
!  Accuracy: About 14 significant digits except for small regions
!            in the vicinity of 1 and 2.

!  Programmer: Alan Miller
!              1 Creswick Street, Brighton, Vic. 3187, Australia
!  e-mail: amiller @ bigpond.net.au
!  Latest revision - 14 October 1996

IMPLICIT NONE
INTEGER, PARAMETER          :: doub_prec = SELECTED_REAL_KIND(15, 60)
REAL(doub_prec), INTENT(IN) :: z
REAL(doub_prec)             :: lanczos

! Local variables

REAL(doub_prec)  :: a(9) = (/ 0.9999999999995183D0, 676.5203681218835D0, &
                              -1259.139216722289D0, 771.3234287757674D0, &
                              -176.6150291498386D0, 12.50734324009056D0, &
                              -0.1385710331296526D0, 0.9934937113930748D-05, &
                               0.1659470187408462D-06 /), zero = 0.D0,   &
                               one = 1.d0, lnsqrt2pi = 0.9189385332046727D0, &
                               half = 0.5d0, sixpt5 = 6.5d0, seven = 7.d0, tmp
INTEGER          :: j

IF (z <= zero) THEN
  WRITE(*, *)'Error: zero or -ve argument for lngamma'
  RETURN
END IF

lanczos = zero
tmp = z + seven
DO j = 9, 2, -1
  lanczos = lanczos + a(j)/tmp
  tmp = tmp - one
END DO
lanczos = lanczos + a(1)
lanczos = LOG(lanczos) + lnsqrt2pi - (z + sixpt5) + (z - half)*LOG(z + sixpt5)
RETURN

END FUNCTION lngamma



FUNCTION ppchi2(pin, v, g) RESULT(fn_val)
! inverse cdf of the chi2 distribution AL

! N.B. Argument IFAULT has been removed.

! This version by Alan Miller
! amiller @ bigpond.net.au
! Latest revision - 27 October 2000

!  Algorithm AS 91   Appl. Statist. (1975) Vol.24, P.35

!  To evaluate the percentage points of the chi-squared
!  probability distribution function.

!  p must lie in the range 0.000002 to 0.999998,
!  v must be positive,
!  g must be supplied and should be equal to ln(gamma(v/2.0))

!  Incorporates the suggested changes in AS R85 (vol.40(1), pp.233-5, 1991)
!  which should eliminate the need for the limited range for p above,
!  though these limits have not been removed from the routine.

!  If IFAULT = 4 is returned, the result is probably as accurate as
!  the machine will allow.

!  Auxiliary routines required: PPND = AS 111 (or AS 241) and GAMMAD = AS 239.

IMPLICIT NONE
INTEGER, PARAMETER    :: dp = SELECTED_REAL_KIND(12, 60)

REAL (dp), INTENT(IN)  :: pin
REAL (dp), INTENT(IN)  :: v
REAL (dp), INTENT(IN)  :: g
REAL (dp)              :: fn_val,p


! Local variables

REAL (dp)  :: a, b, c, p1, p2, q, s1, s2, s3, s4, s5, s6, t, x, xx
INTEGER    :: i, if1

INTEGER, PARAMETER    :: maxit = 20
REAL (dp), PARAMETER  :: aa = 0.6931471806_dp, e = 0.5e-06_dp,         &
                         pmin = 0.000002_dp, pmax = 0.999998_dp,       &
                         zero = 0.0_dp, half = 0.5_dp, one = 1.0_dp,   &
                         two = 2.0_dp, three = 3.0_dp, six = 6.0_dp,   &
                         c1 = 0.01_dp, c2 = 0.222222_dp, c3 = 0.32_dp, &
                         c4 = 0.4_dp, c5 = 1.24_dp, c6 = 2.2_dp,       &
                         c7 = 4.67_dp, c8 = 6.66_dp, c9 = 6.73_dp,     &
                         c10 = 13.32_dp, c11 = 60.0_dp, c12 = 70.0_dp, &
                         c13 = 84.0_dp, c14 = 105.0_dp, c15 = 120.0_dp, &
                         c16 = 127.0_dp, c17 = 140.0_dp, c18 = 175.0_dp, &
                         c19 = 210.0_dp, c20 = 252.0_dp, c21 = 264.0_dp, &
                         c22 = 294.0_dp, c23 = 346.0_dp, c24 = 420.0_dp, &
                         c25 = 462.0_dp, c26 = 606.0_dp, c27 = 672.0_dp, &
                         c28 = 707.0_dp, c29 = 735.0_dp, c30 = 889.0_dp, &
                         c31 = 932.0_dp, c32 = 966.0_dp, c33 = 1141.0_dp, &
                         c34 = 1182.0_dp, c35 = 1278.0_dp, c36 = 1740.0_dp, &
                         c37 = 2520.0_dp, c38 = 5040.0_dp

!       Test arguments and initialise

fn_val = -one
p=pin

! this is somewhat problematic
! if I comment the following if I get correct samples from the truncated X2
! but I get numerical errors coming from the log(p) below in gsmir
!if(pin<pmin) then
!  p=pmin
!else if(pin>pmax)then
!  p=pmax
!endif !AL    
!IF (p < pmin .OR. p > pmax) THEN
!  WRITE(*, *) 'Error in PPCHI2: p must be between 0.000002 & 0.999998'
!  RETURN
!END IF
IF (v <= zero) THEN
  WRITE(*, *) 'Error in PPCHI2: Number of deg. of freedom <= 0'
  RETURN
END IF

xx = half * v
c = xx - one

!       Starting approximation for small chi-squared

IF (v < -c5 * LOG(p)) THEN
  fn_val = (p * xx * EXP(g + xx * aa)) ** (one/xx)
  IF (fn_val < e) GO TO 6
  GO TO 4
END IF

!       Starting approximation for v less than or equal to 0.32

IF (v > c3) GO TO 3
fn_val = c4
a = LOG(one-p)

2 q = fn_val
p1 = one + fn_val * (c7+fn_val)
p2 = fn_val * (c9 + fn_val * (c8 + fn_val))
t = -half + (c7 + two * fn_val) / p1 - (c9 + fn_val * (c10 + three * fn_val)) / p2
fn_val = fn_val - (one - EXP(a + g + half * fn_val + c * aa) * p2 / p1) / t
IF (ABS(q / fn_val - one) > c1) GO TO 2
GO TO 4

!       Call to algorithm AS 241 - note that p has been tested above.

3 CALL ppnd16(p, x, if1)

!       Starting approximation using Wilson and Hilferty estimate

p1 = c2 / v
fn_val = v * (x * SQRT(p1) + one - p1) ** 3

!       Starting approximation for p tending to 1

IF (fn_val > c6 * v + six) fn_val = -two * (LOG(one-p) - c * LOG(half * fn_val) + g)

!       Call to algorithm AS 239 and calculation of seven term Taylor series

4 DO i = 1, maxit
  q = fn_val
  p1 = half * fn_val
  p2 = p - gammad(p1, xx)

  t = p2 * EXP(xx * aa + g + p1 - c * LOG(fn_val))
  b = t / fn_val
  a = half * t - b * c
  s1 = (c19 + a * (c17 + a * (c14 + a * (c13 + a * (c12 + c11 * a))))) / c24
  s2 = (c24 + a * (c29 + a * (c32 + a * (c33 + c35 * a)))) / c37
  s3 = (c19 + a * (c25 + a * (c28 + c31 * a))) / c37
  s4 = (c20 + a * (c27 + c34 * a) + c * (c22 + a * (c30 + c36 * a))) / c38
  s5 = (c13 + c21 * a + c * (c18 + c26 * a)) / c37
  s6 = (c15 + c * (c23 + c16 * c)) / c38
  fn_val = fn_val + t * (one + half * t * s1 - b * c * (s1 - b *   &
           (s2 - b * (s3 - b * (s4 - b * (s5 - b * s6))))))
  IF (ABS(q / fn_val - one) > e) RETURN
END DO

!AL WRITE(*, *) 'Error in PPCHI2: Max. number of iterations exceeded',p

6 RETURN
END FUNCTION ppchi2

SUBROUTINE ppnd16 (p, normal_dev, ifault)

! ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3

! Produces the normal deviate Z corresponding to a given lower
! tail area of P; Z is accurate to about 1 part in 10**16.

! The hash sums below are the sums of the mantissas of the
! coefficients.   They are included for use in checking
! transcription.

! This ELF90-compatible version by Alan Miller - 20 August 1996
! N.B. The original algorithm is as a function; this is a subroutine

IMPLICIT NONE

INTEGER, PARAMETER      :: dp = SELECTED_REAL_KIND(12, 60)
REAL (dp), INTENT(IN)   :: p
INTEGER, INTENT(OUT)    :: ifault
REAL (dp), INTENT(OUT)  :: normal_dev

! Local variables

REAL (dp) :: zero = 0.d0, one = 1.d0, half = 0.5d0,  &
             split1 = 0.425d0, split2 = 5.d0, const1 = 0.180625d0, &
             const2 = 1.6d0, q, r

! Coefficients for P close to 0.5

REAL (dp) :: a0 = 3.3871328727963666080D0, &
             a1 = 1.3314166789178437745D+2, &
             a2 = 1.9715909503065514427D+3, &
             a3 = 1.3731693765509461125D+4, &
             a4 = 4.5921953931549871457D+4, &
             a5 = 6.7265770927008700853D+4, &
             a6 = 3.3430575583588128105D+4, &
             a7 = 2.5090809287301226727D+3, &
             b1 = 4.2313330701600911252D+1, &
             b2 = 6.8718700749205790830D+2, &
             b3 = 5.3941960214247511077D+3, &
             b4 = 2.1213794301586595867D+4, &
             b5 = 3.9307895800092710610D+4, &
             b6 = 2.8729085735721942674D+4, &
             b7 = 5.2264952788528545610D+3
! HASH SUM AB    55.8831928806149014439

! Coefficients for P not close to 0, 0.5 or 1.

REAL (dp) :: c0 = 1.42343711074968357734D0, &
             c1 = 4.63033784615654529590D0, &
             c2 = 5.76949722146069140550D0, &
             c3 = 3.64784832476320460504D0, &
             c4 = 1.27045825245236838258D0, &
             c5 = 2.41780725177450611770D-1, &
             c6 = 2.27238449892691845833D-2, &
             c7 = 7.74545014278341407640D-4, &
             d1 = 2.05319162663775882187D0, &
             d2 = 1.67638483018380384940D0, &
             d3 = 6.89767334985100004550D-1, &
             d4 = 1.48103976427480074590D-1, &
             d5 = 1.51986665636164571966D-2, &
             d6 = 5.47593808499534494600D-4, &
             d7 = 1.05075007164441684324D-9
! HASH SUM CD    49.33206503301610289036

! Coefficients for P near 0 or 1.

REAL (dp) :: e0 = 6.65790464350110377720D0, &
             e1 = 5.46378491116411436990D0, &
             e2 = 1.78482653991729133580D0, &
             e3 = 2.96560571828504891230D-1, &
             e4 = 2.65321895265761230930D-2, &
             e5 = 1.24266094738807843860D-3, &
             e6 = 2.71155556874348757815D-5, &
             e7 = 2.01033439929228813265D-7, &
             f1 = 5.99832206555887937690D-1, &
             f2 = 1.36929880922735805310D-1, &
             f3 = 1.48753612908506148525D-2, &
             f4 = 7.86869131145613259100D-4, &
             f5 = 1.84631831751005468180D-5, &
             f6 = 1.42151175831644588870D-7, &
             f7 = 2.04426310338993978564D-15
! HASH SUM EF    47.52583317549289671629

ifault = 0
q = p - half
IF (ABS(q) <= split1) THEN
  r = const1 - q * q
  normal_dev = q * (((((((a7*r + a6)*r + a5)*r + a4)*r + a3)*r + a2)*r + a1)*r + a0) / &
           (((((((b7*r + b6)*r + b5)*r + b4)*r + b3)*r + b2)*r + b1)*r + one)
  RETURN
ELSE
  IF (q < zero) THEN
    r = p
  ELSE
    r = one - p
  END IF
  IF (r <= zero) THEN
    ifault = 1
    normal_dev = zero
    RETURN
  END IF
  r = SQRT(-LOG(r))
  IF (r <= split2) THEN
    r = r - const2
    normal_dev = (((((((c7*r + c6)*r + c5)*r + c4)*r + c3)*r + c2)*r + c1)*r + c0) / &
             (((((((d7*r + d6)*r + d5)*r + d4)*r + d3)*r + d2)*r + d1)*r + one)
  ELSE
    r = r - split2
    normal_dev = (((((((e7*r + e6)*r + e5)*r + e4)*r + e3)*r + e2)*r + e1)*r + e0) / &
             (((((((f7*r + f6)*r + f5)*r + f4)*r + f3)*r + f2)*r + f1)*r + one)
  END IF
  IF (q < zero) normal_dev = - normal_dev
  RETURN
END IF
END SUBROUTINE ppnd16

end module stat

MODULE mt19937
! A Fortran-program for MT19937: Real number version
 
! Code converted using TO_F90 by Alan Miller
! Date: 1999-11-26  Time: 17:09:23
! Latest revision - 5 February 2002
! A new seed initialization routine has been added based upon the new
! C version dated 26 January 2002.
! This version assumes that integer overflows do NOT cause crashes.
! This version is compatible with Lahey's ELF90 compiler,
! and should be compatible with most full Fortran 90 or 95 compilers.
! Notice the strange way in which umask is specified for ELF90.
 
!   genrand() generates one pseudorandom real number (double) which is
! uniformly distributed on [0,1]-interval, for each call.
! sgenrand(seed) set initial values to the working area of 624 words.
! Before genrand(), sgenrand(seed) must be called once.  (seed is any 32-bit
! integer except for 0).
! Integer generator is obtained by modifying two lines.
!   Coded by Takuji Nishimura, considering the suggestions by
! Topher Cooper and Marc Rieffel in July-Aug. 1997.

! This library is free software; you can redistribute it and/or modify it
! under the terms of the GNU Library General Public License as published by
! the Free Software Foundation; either version 2 of the License, or (at your
! option) any later version.   This library is distributed in the hope that
! it will be useful, but WITHOUT ANY WARRANTY; without even the implied
! warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! See the GNU Library General Public License for more details.
! You should have received a copy of the GNU Library General Public License
! along with this library; if not, write to the Free Foundation, Inc.,
! 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

! Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
! When you use this, send an email to: matumoto@math.keio.ac.jp
! with an appropriate reference to your work.

!***********************************************************************
! Fortran translation by Hiroshi Takano.  Jan. 13, 1999.

!   genrand()      -> double precision function grnd()
!   sgenrand(seed) -> subroutine sgrnd(seed)
!                     integer seed

! This program uses the following standard intrinsics.
!   ishft(i,n): If n > 0, shifts bits in i by n positions to left.
!               If n < 0, shifts bits in i by n positions to right.
!   iand (i,j): Performs logical AND on corresponding bits of i and j.
!   ior  (i,j): Performs inclusive OR on corresponding bits of i and j.
!   ieor (i,j): Performs exclusive OR on corresponding bits of i and j.

!***********************************************************************

IMPLICIT NONE
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12, 60)

! Period parameters
INTEGER, PARAMETER :: n = 624, n1 = n+1, m = 397, mata = -1727483681
!                                    constant vector a
INTEGER, PARAMETER :: umask = -2147483647 - 1
!                                    most significant w-r bits
INTEGER, PARAMETER :: lmask =  2147483647
!                                    least significant r bits
! Tempering parameters
INTEGER, PARAMETER :: tmaskb= -1658038656, tmaskc= -272236544

!                     the array for the state vector
INTEGER, SAVE      :: mt(0:n-1), mti = n1
!                     mti==N+1 means mt[N] is not initialized

PRIVATE
PUBLIC :: dp, sgrnd, grnd, init_genrand

CONTAINS


SUBROUTINE sgrnd(seed)
! This is the original version of the seeding routine.
! It was replaced in the Japanese version in C on 26 January 2002
! It is recommended that routine init_genrand is used instead.

INTEGER, INTENT(IN)   :: seed

!    setting initial seeds to mt[N] using the generator Line 25 of Table 1 in
!    [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.), pp102]

mt(0)= IAND(seed, -1)
DO  mti=1,n-1
  mt(mti) = IAND(69069 * mt(mti-1), -1)
END DO

RETURN
END SUBROUTINE sgrnd
!***********************************************************************

SUBROUTINE init_genrand(seed)
! This initialization is based upon the multiplier given on p.106 of the
! 3rd edition of Knuth, The Art of Computer Programming Vol. 2.

! This version assumes that integer overflow does NOT cause a crash.

INTEGER, INTENT(IN)  :: seed

INTEGER  :: latest

mt(0) = seed
latest = seed
DO mti = 1, n-1
  latest = IEOR( latest, ISHFT( latest, -30 ) )
  latest = latest * 1812433253 + mti
  mt(mti) = latest
END DO



RETURN
END SUBROUTINE init_genrand
!***********************************************************************

FUNCTION grnd() RESULT(fn_val)
REAL (dp) :: fn_val

INTEGER, SAVE :: mag01(0:1) = (/ 0, mata /)
!                        mag01(x) = x * MATA for x=0,1
INTEGER       :: kk, y

! These statement functions have been replaced with separate functions
! tshftu(y) = ISHFT(y,-11)
! tshfts(y) = ISHFT(y,7)
! tshftt(y) = ISHFT(y,15)
! tshftl(y) = ISHFT(y,-18)

IF(mti >= n) THEN
!                       generate N words at one time
  IF(mti == n+1) THEN
!                            if sgrnd() has not been called,
    CALL sgrnd(4357)
!                              a default initial seed is used
  END IF
  
  DO  kk = 0, n-m-1
    y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
    mt(kk) = IEOR(IEOR(mt(kk+m), ISHFT(y,-1)),mag01(IAND(y,1)))
  END DO
  DO  kk = n-m, n-2
    y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
    mt(kk) = IEOR(IEOR(mt(kk+(m-n)), ISHFT(y,-1)),mag01(IAND(y,1)))
  END DO
  y = IOR(IAND(mt(n-1),umask), IAND(mt(0),lmask))
  mt(n-1) = IEOR(IEOR(mt(m-1), ISHFT(y,-1)),mag01(IAND(y,1)))
  mti = 0
END IF

y = mt(mti)
mti = mti + 1
y = IEOR(y, tshftu(y))
y = IEOR(y, IAND(tshfts(y),tmaskb))
y = IEOR(y, IAND(tshftt(y),tmaskc))
y = IEOR(y, tshftl(y))

! old code AL
!IF(y < 0) THEN
!  fn_val = (DBLE(y) + 2.0D0**32) / (2.0D0**32 - 1.0D0) 
!ELSE
!  fn_val = DBLE(y) / (2.0D0**32 - 1.0D0)
!END IF

! to make it (0-1) AL
IF(y < 0) THEN
  fn_val = (DBLE(y) + 2.0D0**32) 
ELSE
  fn_val = DBLE(y) 
END IF
fn_val=(fn_val+0.5d0)/4294967296.d0

RETURN
END FUNCTION grnd


FUNCTION tshftu(y) RESULT(fn_val)
INTEGER, INTENT(IN) :: y
INTEGER             :: fn_val

fn_val = ISHFT(y,-11)
RETURN
END FUNCTION tshftu


FUNCTION tshfts(y) RESULT(fn_val)
INTEGER, INTENT(IN) :: y
INTEGER             :: fn_val

fn_val = ISHFT(y,7)
RETURN
END FUNCTION tshfts


FUNCTION tshftt(y) RESULT(fn_val)
INTEGER, INTENT(IN) :: y
INTEGER             :: fn_val

fn_val = ISHFT(y,15)
RETURN
END FUNCTION tshftt


FUNCTION tshftl(y) RESULT(fn_val)
INTEGER, INTENT(IN) :: y
INTEGER             :: fn_val

fn_val = ISHFT(y,-18)
RETURN
END FUNCTION tshftl

END MODULE mt19937




MODULE Ecuyer_random
! L'Ecuyer's 1996 random number generator.
! Fortran version by Alan.Miller @ vic.cmis.csiro.au
! N.B. This version is compatible with Lahey's ELF90
! http://www.ozemail.com.au/~milleraj
! Latest revision - 30 March 1999

use kinds
IMPLICIT NONE
!INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307) !=real(8) con DVF

! These are unsigned integers in the C version
INTEGER, SAVE :: s1 = 1234, s2 = -4567, s3 = 7890

CONTAINS

SUBROUTINE init_seeds(i1, i2, i3)
IMPLICIT NONE

INTEGER, INTENT(IN) :: i1, i2, i3

s1 = i1
s2 = i2
s3 = i3
IF (IAND(s1,-2) == 0) s1 = i1 - 1023
IF (IAND(s2,-8) == 0) s2 = i2 - 1023
IF (IAND(s3,-16) == 0) s3 = i3 - 1023

RETURN
END SUBROUTINE init_seeds

subroutine get_clock_seeds()
! set random seeds according to time AL
implicit none
integer:: v(8),i
real::b(3)
call date_and_time(values=v)
call init_seeds(v(6),v(7),v(8))
!alternate method (very poor!?)
!do i=1,3
!  call cpu_time(b(i))
!  v(5+i)=transfer(b(i),v(5+i))
!enddo  
!call init_seeds(v(6),v(7),v(8))
!print *,s1,S2,S3

end subroutine

subroutine get_keyb_seeds()
! set  seeds from the keyboard
! needs THREE seeds AL
implicit none
integer:: v(3)
character(100)::in
print *,'random seeds?'
read (*,*)v

call init_seeds(v(1),v(2),v(3))
end subroutine

subroutine print_seeds()
! print  seeds to sd output
implicit none

print *,s1,s2,s3

end subroutine


FUNCTION taus88() RESULT(random_numb)
! Generates a random number between 0 and 1.  Translated from C function in:
! Reference:
! L'Ecuyer, P. (1996) `Maximally equidistributed combined Tausworthe
! generators', Math. of Comput., 65, 203-213.

! The cycle length is claimed to be about 2^(88) or about 3E+26.
! Actually - (2^31 - 1).(2^29 - 1).(2^28 - 1).

IMPLICIT NONE
REAL (r8) :: random_numb

INTEGER   :: b

! N.B. ISHFT(i,j) is a bitwise (non-circular) shift operation;
!      to the left if j > 0, otherwise to the right.

b  = ISHFT( IEOR( ISHFT(s1,13), s1), -19)
s1 = IEOR( ISHFT( IAND(s1,-2), 12), b)
b  = ISHFT( IEOR( ISHFT(s2,2), s2), -25)
s2 = IEOR( ISHFT( IAND(s2,-8), 4), b)
b  = ISHFT( IEOR( ISHFT(s3,3), s3), -11)
s3 = IEOR( ISHFT( IAND(s3,-16), 17), b)
!random_numb = IEOR( IEOR(s1,s2), s3) * 2.3283064365E-10_dp + 0.5_dp
random_numb = IEOR( IEOR(s1,s2), s3) * 2.3283064365E-10_r8 + 0.5_r8

RETURN
END FUNCTION taus88

END MODULE Ecuyer_random
!----------------------!

module random
use Ecuyer_random
use mt19937
use kinds
use stat
INTERFACE invwish
! multi or univariate
  module procedure invwish_mat,&
                   invwish_sc
END INTERFACE


CONTAINS

  function normal01() RESULT(z)
!     z -> n(0,1)
    implicit none
    real(r8):: z,u1,u2
    u1=unif()
    u2=unif()
    z=((-2.d0*log(u1))**0.5d0)*cos(2.d0*3.1416d0*u2)
  end function normal01

  function normal(mean,var) RESULT(z)
!     normal z -> n(mean,var)
    implicit none      
    real(r8):: z,var,mean
    z=normal01()*sqrt(var)+mean
  end function normal 


!---- Subrutinas para obtener normales truncadas; principalmente de programas
!       de Misztal

! ---------------------------------
      function normal_invcdf(p) 
      ! return inverse of CDF, i.e., such x: p=cdf(x)
      !   resticted to |invcdf(x)|<10

!      use kinds
      double precision:: p,x,eps,normal_invcdf
      integer i

      if (p.lt.0. .or. p .gt. 1.) then
         print*,'normal_invcdf: arguments outside of 0..1'
         stop
      endif

      eps=10

      x=0
      do i=1,50
         if (normalcdf(x) .lt. p) then     
           x=x+eps
         else
           x=x-eps
         endif    
         if (abs(x) .gt. 10.1) exit
         eps=eps/2.
      enddo      
      normal_invcdf=x
      end function


      function gen_trunc_normal(a,b) result(x)
! Generates normal distribution N(0,1) truncated to <a,b>

      double precision:: a,b,cdfa,cdfb
      double precision:: un,x


      if (b .lt. a) then
         Print*,'GEN_TRUNC_NORMAL: bound b =',b,' < boound a =',a
         stop
      endif   

      cdfa=normalcdf(a)
      cdfb=normalcdf(b)
1     continue        
      un=unif()  !uniform random number generator UN(0,1)
      x=normal_invcdf(cdfa+(cdfb-cdfa)*un)       
      if ((x.lt.a).or.(x.gt.b)) then
        print *,a,x,b
	print *,cdfa,cdfa+(cdfb-cdfa)*un,cdfb
	print *,'I can''t sample this truncated normal, subroutine gen_trunc_normal'
!        goto 1
      endif
      end function

  
  function bernouilli(p) result(a)
  !  0-1 with p% prob of success
    implicit none
    integer::a
    real(r8)::p
    if(unif()<p) then 
      a=1
    else
      a=0
    endif
  end function
  
  function toss() 
  ! 0-1 with 0.5 probability
  integer:: toss
  toss=bernouilli(.5d0)
  end function
  
  function unif() result(u)
  ! Unif(0-1)
    implicit none
    real(r8) :: u
    u=taus88()
    !u=grnd() ! Mersenne-twister is slower but has a much greater cycle
  end function

  function unifint(bound1,bound2) result(u)
  ! integer between bound1 and bound2
  ! 21/6/06 corrected bug
    implicit none
    integer :: bound1,bound2,u
    ! assuming bound1<bound2
    u=int(unif()*(bound2-bound1+1))+bound1
  end function
  
  

FUNCTION random_beta(aa, bb) RESULT(fn_val)
! from Alan Miller, http://users.bigpond.net.au/amiller/random.html
! slightly modified by AL

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,1]
! FROM A BETA DISTRIBUTION WITH DENSITY
! PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
! USING CHENG'S LOG LOGISTIC METHOD.

!     AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
!     BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)

REAL(r8), INTENT(IN)    :: aa, bb
!LOGICAL, INTENT(IN) :: first AL
REAL(r8)                :: fn_val

!     Local variables
REAL(r8), PARAMETER  :: aln4 = 1.3862944d0
REAL(r8)             :: a, b, g, r, s, x, y, z
REAL(r8), SAVE       :: d, f, h, t, c
LOGICAL, SAVE    :: swap
REAL(r8)      :: zero = 0.0d0, half = 0.5d0, one = 1.0d0, two = 2.0d0,   &
                      vsmall = TINY(1.0d0), vlarge = HUGE(1.0d0)
IF (aa <= zero .OR. bb <= zero) THEN
  WRITE(*, *) 'IMPERMISSIBLE SHAPE PARAMETER VALUE(S)'
  STOP
END IF

!IF (first) THEN           AL             ! Initialization, if necessary
  a = aa
  b = bb
  swap = b > a
  IF (swap) THEN
    g = b
    b = a
    a = g
  END IF
  d = a/b
  f = a+b
  IF (b > one) THEN
    h = SQRT((two*a*b - f)/(f - two))
    t = one
  ELSE
    h = b
    t = one/(one + (a/(vlarge*b))**b)
  END IF
  c = a+h
!END IF

DO
  CALL RANDOM_NUMBER(r) !AL
  CALL RANDOM_NUMBER(x) !AL
  !r=unif()
  !x=unif()
  s = r*r*x
  IF (r < vsmall .OR. s <= zero) CYCLE
  IF (r < t) THEN
    x = LOG(r/(one - r))/h
    y = d*EXP(x)
    z = c*x + f*LOG((one + d)/(one + y)) - aln4
    IF (s - one > z) THEN
      IF (s - s*z > one) CYCLE
      IF (LOG(s) > z) CYCLE
    END IF
    fn_val = y/(one + y)
  ELSE
    IF (4.0d0*s > (one + one/d)**f) CYCLE
    fn_val = one
  END IF
  EXIT
END DO

IF (swap) fn_val = one - fn_val


END FUNCTION random_beta


  

FUNCTION random_Poisson(mu) RESULT(ran_Poisson)
!FUNCTION random_Poisson(mu, first) RESULT(ran_Poisson)
! AL edited to take "first" out and changed to double precision
!**********************************************************************

!     Translated to Fortran 90 by Alan Miller from:
!                                    RANLIB
!
!           Library of Fortran Routines for Random Number Generation
!
!                           Compiled and Written by:
!
!                                Barry W. Brown
!                                 James Lovato
!
!                    Department of Biomathematics, Box 237
!                    The University of Texas, M.D. Anderson Cancer Center
!                    1515 Holcombe Boulevard
!                    Houston, TX      77030
!
! This work was supported by grant CA-16672 from the National Cancer Institute.

!                 GENerate POIsson random deviate Function

!  Generates a single random deviate from a Poisson
!  distribution with mean mu.

!                           Arguments

!  mu --> The mean of the Poisson distribution from which
!         a random deviate is to be generated.
!                           REAL mu

!                           Method

!  For details see:

!      Ahrens, J.H. and Dieter, U.
!      Computer Generation of Poisson Deviates
!      From Modified Normal Distributions.
!      ACM Trans. Math. Software, 8, 2 (June 1982), 163-179


!  MUPREV = PREVIOUS MU, MUOLD = MU AT LAST EXECUTION OF STEP P OR B.
!  TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT
!  COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K) - DEL

!  SEPARATION OF CASES A AND B

IMPLICIT NONE

!     .. Scalar Arguments ..
real(r8), INTENT(IN)    :: mu
!LOGICAL, INTENT(IN) :: first
LOGICAL :: first=.true.
INTEGER             :: ran_Poisson

!INTERFACE
!  FUNCTION random_normal() RESULT (ran_norm)
!    IMPLICIT NONE
!    REAL :: ran_norm
!  END FUNCTION random_normal
!END INTERFACE

!INTERFACE
!  FUNCTION random_exponential() RESULT (ran_exp)
!    IMPLICIT NONE
!    REAL :: ran_exp
!  END FUNCTION random_exponential
!END INTERFACE

!     .. Local Scalars ..
real(r8)    :: a0 = -.5d0, a1 = .3333333d0, a2 = -.2500068d0, a3 = .2000118d0,        &
           a4 = -.1661269d0, a5 = .1421878d0, a6 = -.1384794d0, a7 = .1250060d0,  &
           b1, b2, c, c0, c1, c2, c3, d, del, difmuk, e, fk, fx, fy, g,   &
           omega, p, p0, px, py, q, s, t, u, v, x, xx
INTEGER :: j, k, kflag, l, m
!     ..
!     .. Local Arrays ..
real(r8)    :: fact(10) = (/ 1.d0, 1.d0, 2.d0, 6.d0, 24.d0, 120.d0, 720.d0, 5040.d0, 40320.d0, 362880.d0 /)
real(r8)    :: pp(35)
!     ..
!     ..
!     .. Executable Statements ..
IF (.NOT. first) GO TO 10
IF (mu < 10.0) GO TO 120

!     C A S E  A. (RECALCULATION OF S, D, L IF MU HAS CHANGED)

s = SQRT(mu)
d = 6d0*mu*mu

!     THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL PROBABILITIES FK
!     WHENEVER K >= M(MU).
!     L = INT(MU-1.1484) IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 .

l = INT(mu-1.1484d0)

!     STEP N. NORMAL SAMPLE - random_normal() FOR STANDARD NORMAL DEVIATE

!10 g = mu + s*random_normal()
10 g = mu + s*normal01() !AL
IF (g < 0.0d0) GO TO 20
ran_Poisson = INT(g)

!     STEP I. IMMEDIATE ACCEPTANCE IF ran_Poisson IS LARGE ENOUGH

IF (ran_Poisson.GE.l) RETURN

!     STEP S. SQUEEZE ACCEPTANCE - SAMPLE U

fk = REAL(ran_Poisson)
difmuk = mu - fk
CALL RANDOM_NUMBER(u)
!u=unif() !AL
IF (d*u >= difmuk*difmuk*difmuk) RETURN

!     STEP P. PREPARATIONS FOR STEPS Q AND H.
!             (RECALCULATIONS OF PARAMETERS IF NECESSARY)
!             .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7.
!             THE QUANTITIES B1, B2, C3, C2, C1, C0 ARE FOR THE HERMITE
!             APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK.
!             C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION.

20 IF (.NOT. first) GO TO 30
omega = .3989423d0/s
b1 = .4166667E-1 / mu
b2 = .3d0*b1*b1
c3 = .1428571d0*b1*b2
c2 = b2 - 15.d0*c3
c1 = b1 - 6.d0*b2 + 45.d0*c3
c0 = 1.d0 - b1 + 3.d0*b2 - 15.d0*c3
c = .1069d0/mu
30 IF (g < 0.0d0) GO TO 50

!             'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN)

kflag = 0
GO TO 70

!     STEP Q. QUOTIENT ACCEPTANCE (RARE CASE)

40 IF (fy-u*fy <= py*EXP(px-fx)) RETURN

!     STEP E. EXPONENTIAL SAMPLE - random_expon() FOR STANDARD EXPONENTIAL
!             DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT'
!             (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.)

50 e = random_exponential()
CALL RANDOM_NUMBER(u)
!u=unif()
u = u + u - 1.0
t = 1.8d0 + SIGN(e, u)
IF (t <= (-.6744d0)) GO TO 50
ran_Poisson = INT(mu+s*t)
fk = REAL(ran_Poisson)
difmuk = mu - fk

!             'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN)

kflag = 1
GO TO 70

!     STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION)

60 IF (c*ABS(u) > py*EXP(px+e) - fy*EXP(fx+e)) GO TO 50
RETURN

!     STEP F. 'SUBROUTINE' F. CALCULATION OF PX, PY, FX, FY.
!             CASE ran_Poisson < 10 USES FACTORIALS FROM TABLE FACT

70 IF (ran_Poisson >= 10) GO TO 80
px = -mu
py = mu**ran_Poisson / fact(ran_Poisson + 1)
GO TO 110

!      CASE ran_Poisson >= 10 USES POLYNOMIAL APPROXIMATION
!      A0-A7 FOR ACCURACY WHEN ADVISABLE
!      .8333333E-1=1./12.  .3989423=(2*PI)**(-.5)

80 del = .8333333E-1 / fk
del = del - 4.8*del*del*del
v = difmuk/fk
IF (ABS(v) <= 0.25d0) GO TO 90
px = fk*LOG(1.0+v) - difmuk - del
GO TO 100

90 px = fk*v*v* (((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v+a0) - del
100 py = .3989423d0/SQRT(fk)
110 x = (0.5d0-difmuk)/s
xx = x*x
fx = -0.5d0*xx
fy = omega* (((c3*xx+c2)*xx+c1)*xx+c0)
IF (kflag > 0) THEN
  GO TO 60
ELSE
  GO TO 40
END IF

!     C A S E  B. (START NEW TABLE AND CALCULATE P0 IF NECESSARY)

120 IF (first) THEN
  m = MAX(1, INT(mu))
  l = 0
  p = EXP(-mu)
  q = p
  p0 = p
END IF

!     STEP U. UNIFORM SAMPLE FOR INVERSION METHOD

130 CALL RANDOM_NUMBER(u)
!130 u=unif()
ran_Poisson = 0
IF (u <= p0) RETURN

!     STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE
!             PP-TABLE OF CUMULATIVE POISSON PROBABILITIES
!             (0.458 = PP(9) FOR MU = 10)

IF (l == 0) GO TO 150
j = 1
IF (u > 0.458d0) j = MIN(l, m)
DO k = j, l
  IF (u <= pp(k)) GO TO 180
END DO
IF (l == 35) GO TO 130

!     STEP C. CREATION OF NEW POISSON PROBABILITIES P
!             AND THEIR CUMULATIVES Q = PP(K)

150 l = l + 1
DO k = l, 35
  p = p*mu/REAL(k)
  q = q + p
  pp(k) = q
  IF (u <= q) GO TO 170
END DO
l = 35
GO TO 130

170 l = k
180 ran_Poisson = k
RETURN

END FUNCTION random_Poisson



subroutine random_gamma_v(s,b,fn_val)
!vectorial extension for random gamma AL
IMPLICIT NONE
INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60) !double precision or r8

REAL (dp), INTENT(IN)  :: s, b
REAL (dp)              :: fn_val(:)
integer :: i

do i=1,size(fn_val)
 fn_val(i)=random_gamma(s, b)
enddo

end subroutine

FUNCTION random_gamma(s, b) RESULT(fn_val)

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

!     N.B. This version is in `double precision' and includes scaling

!     FUNCTION GENERATES A RANDOM GAMMA VARIATE.
!     CALLS EITHER random_gamma1 (S > 1.0)
!     OR random_exponential (S = 1.0)
!     OR random_gamma2 (S < 1.0).

!     S = SHAPE PARAMETER OF DISTRIBUTION (0 < REAL).
!     B = Scale parameter

!AL from http://users.bigpond.net.au/amiller/random.html
! modified to include s==1 and to remove first, which is not practical

IMPLICIT NONE
INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60) !double precision or r8

REAL (dp), INTENT(IN)  :: s, b
REAL (dp)              :: fn_val

! Local parameters
REAL (dp), PARAMETER  :: one = 1.0_dp, zero = 0.0_dp

IF (s <= zero) THEN
  WRITE(*, *) 'SHAPE PARAMETER VALUE MUST BE POSITIVE'
  STOP
END IF

IF (s==one) then
  fn_val = random_exponential()
elseif (s > one) THEN
  fn_val = random_gamma1(s)
ELSE IF (s < one) THEN
  fn_val = random_gamma2(s)
END IF

! Now scale the random variable
fn_val = b * fn_val
RETURN

CONTAINS


FUNCTION random_gamma1(s) RESULT(fn_val)

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO GAMMA**(S-1)*EXP(-GAMMA),
! BASED UPON BEST'S T DISTRIBUTION METHOD

!     S = SHAPE PARAMETER OF DISTRIBUTION
!          (1.0 < REAL)

REAL (dp), INTENT(IN)  :: s
REAL (dp)              :: fn_val

!     Local variables
REAL (dp)             :: d, r, g, f, x
REAL (dp), SAVE       :: b, h
REAL (dp), PARAMETER  :: sixty4 = 64.0_dp, three = 3.0_dp, pt75 = 0.75_dp,  &
                         two = 2.0_dp, half = 0.5_dp

IF (s <= one) THEN
  WRITE(*, *) 'IMPERMISSIBLE SHAPE PARAMETER VALUE'
  STOP
END IF

b = s - one
h = SQRT(three*s - pt75)

DO
  CALL RANDOM_NUMBER(r)
  !r=unif()
  g = r - r*r
  IF (g <= zero) CYCLE
  f = (r - half)*h/SQRT(g)
  x = b + f
  IF (x <= zero) CYCLE
  CALL RANDOM_NUMBER(r)
  !r=unif()
  d = sixty4*g*(r*g)**2
  IF (d <= zero) EXIT
  IF (d*x < x - two*f*f) EXIT
  IF (LOG(d) < two*(b*LOG(x/b) - f)) EXIT
END DO
fn_val = x

RETURN
END FUNCTION random_gamma1



FUNCTION random_gamma2(s) RESULT(fn_val)

! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO
! GAMMA2**(S-1) * EXP(-GAMMA2),
! USING A SWITCHING METHOD.

!    S = SHAPE PARAMETER OF DISTRIBUTION
!          (REAL < 1.0)

REAL (dp), INTENT(IN)  :: s
REAL (dp)              :: fn_val

!     Local variables
REAL (dp)             :: r, x, w
REAL (dp), SAVE       :: a, p, c, uf, vr, d
REAL (dp), PARAMETER  :: vsmall = EPSILON(one)

IF (s <= zero .OR. s >= one) THEN
  WRITE(*, *) 'SHAPE PARAMETER VALUE OUTSIDE PERMITTED RANGE'
  STOP
END IF

a = one - s
p = a/(a + s*EXP(-a))
IF (s < vsmall) THEN
  WRITE(*, *) 'SHAPE PARAMETER VALUE TOO SMALL'
  STOP
END IF
c = one/s
uf = p*(vsmall/a)**s
vr = one - vsmall
d = a*LOG(a)

DO
  CALL RANDOM_NUMBER(r)
!  r=unif()
  IF (r >= vr) THEN
    CYCLE
  ELSE IF (r > p) THEN
    x = a - LOG((one - r)/(one - p))
    w = a*LOG(x)-d
  ELSE IF (r > uf) THEN
    x = a*(r/p)**c
    w = x
  ELSE
    fn_val = zero
    RETURN
  END IF

  CALL RANDOM_NUMBER(r)
!  r=unif()
  IF (one-r <= w .AND. r > zero) THEN
    IF (r*(w + one) >= one) CYCLE
    IF (-LOG(r) <= w) CYCLE
  END IF
  EXIT
END DO

fn_val = x
RETURN

END FUNCTION random_gamma2


END FUNCTION random_gamma


FUNCTION random_exponential() RESULT(ran_exp)


! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9

! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
! TO EXP(-random_exponential), USING INVERSION.

IMPLICIT NONE
INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60) !double precision or r8
REAL (dp), PARAMETER  :: zero = 0.0_dp
REAL (dp) :: ran_exp, b

!     Local variable
REAL  :: r

DO
  CALL RANDOM_NUMBER(r)
  !r=unif()
  IF (r > zero) EXIT
END DO

ran_exp = -LOG(r)
RETURN

END FUNCTION random_exponential



      function chin(se,ned) result(u)
! old inverted chi-2 by Luis Varona
! with mean se and ne d.f.
!      se = varianza a priori.
!      ne = grados de credibilidad.
! if se< 1d-50 returns 0d0 AL
      implicit double precision (a-h,o-z)
!      integer:: ne
      double precision:: ned
      if (se<1d-50) then
        u=0d0
	return
      endif
      
!      ned=dble(ne)
1     u1=unif()
      rg=se*14/(ned**.5)
      t=2*rg*u1+(se-rg)
      if (t.lt.0d0) goto 1
      o=(ned/(ned+2))*se
      chiot=-((ned+2d0)/2d0)*log(o)+(-(ned*se/2d0)/o)
      chitt=-((ned+2d0)/2d0)*log(t)+(-(ned*se/2d0)/t)
      ref=dexp(chitt-chiot)
      u1=unif()
      if (u1.lt.ref) then
        u=t
      else
        goto 1
      endif

      return
      end function

  function chin_bounded_not(se,ned,a,b,var) result(out)
  ! sample from a bounded chi-sq between a and b
  ! mean se and ne df 
  !using metropolis-hastings
  !a<b
  ! does not work very well... error?
  implicit none
  real(r8):: a,b,upper,lower,likeold,likenew,ned,var,se,u,out
  real(r8),save::perf(2)=0d0
  lower=a*se
  upper=b*se
  !uniform
  u=var/se+unif()*(upper-lower)
  ! loglikelihood of the old
  likeold=logdchisq(var,ned)
  likenew=logdchisq(u,ned)
  out=metropolis(var,u,likeold,likenew)
  
  end function
  
  function chin_bounded(se,ned,a,b) result(out)
  ! sample from a bounded inverted chi-sq between a and b
  ! mean se and ne df 
  implicit none
  real(r8):: a,b,upper,lower,cdflower,cdfupper,ned,se,u,out
  ! get bounds in the proper scale
  lower=(se*ned)/a
  upper=(se*ned)/b
  cdflower=chi_squared(lower,ned)
  cdfupper=chi_squared(upper,ned)
  !page 66 S&G
  u=unif()
!  out= gqchisq(chi_squared(lower,ned)+u*(chi_squared(upper,ned)-chi_squared(lower,ned)),ned)
!print *,se,ned,lower,upper,cdflower,cdfupper,lngamma(ned/2d0),u
  out= (se*ned)/ &
    ppchi2(cdflower+u*(cdfupper-cdflower),ned,lngamma(ned/2d0))
  
  
  end function
  
  
  function metropolis(old,new,loglikeold,loglikenew) result(u)
  !metropolis-hastings using log-likelihoods
  ! no s\E9 si esta bien
  implicit none
  real(r8)::old,new,loglikeold,loglikenew,u,kk,logratio
!  real(r8),save:: trial(2)=0d0
  
  logratio=loglikenew-loglikeold
  if (logratio>0) then
  ! ratio is higher then 1, accept always
    u=new
  elseif (logratio>log(huge(logratio))) then !overflow
  ! reject
    u=old
  else
    kk=unif()
!    if(kk<exp(logratio)) then
    if(log(kk)<logratio) then !15/10/08
      u=new
!      print *,'accepted'
    else
      u=old
!      print *,'rejected'
    endif
  endif
  
  end function
  
           

    SUBROUTINE CHOLS4_ev(entrada,x)
    ! cholesky decomposition from IM modified by ELM to fit wish from LV
    integer :: n,i,j,k,rank_o
    integer::rank
    double precision::diagsq
    double precision::x(:,:), entrada(:,:)
    double precision:: tol
      tol=1.d-20

    rank_o=0
    x=entrada
    n=size(x,1)
    do i=1,n
       diagsq=x(i,i)-dot_product(x(i,1:i-1),x(i,1:i-1))
       if (abs(diagsq).lt.tol) then
              x(i,:)=0;x(:,i)=0       !zero row and column
       elseif (diagsq.lt.0) then
              print*,' Matrix not semipositive-definite, row ',i
              stop
       else
         rank_o=rank_o+1
       x(i,i)=sqrt(diagsq)
       do j=i+1,n     
            x(j,i)=(x(j,i)-dot_product(x(j,1:i-1),x(i,1:i-1)))/x(i,i)
            x(i,j)=x(j,i)
       enddo
       end if
    enddo

      ! zero upper-diagonals
      do i=1,n
         x(i,i+1:n)=0
      enddo   
    
!    Cambiamos a triangular superior 

    x=transpose(x)
      
      END SUBROUTINE CHOLS4_ev
      
      function invwish_mat(se,ne) result(ve)
      ! sample from the inverted wishart distribution of mean se with ne degrees of freedom
      double precision:: se(:,:),ve(size(se,1),size(se,1)),temp(size(se,1),size(se,1))
      integer :: ne,nrank,irank
      nrank=size(se,1)
      temp=se
      call ginv(temp,nrank,1d-20,irank)
      call wish(nrank,temp,ve,ne)
      call ginv(ve,nrank,1d-20,irank)
      
      end function
      
      function invwish_sc(se,ne) result(ve)
      ! sample from the inverted wishart distribution of mean se with ne degrees of freedom
      double precision:: se,ve,sem(1,1),vem(1,1)
      integer :: ne
      sem(1,1)=se
      vem=invwish_mat(sem,ne)
      ve=vem(1,1)
      
      end function

      subroutine wish(nrank,se,ve,ne)
      ! samples from wishart distribution with ne degrees of freedom and expectation se
      ! a fragment nrank*nrank
      ! this means, we need to divide sum of squares by their degrees of freedom to create se
       implicit double precision(a-h,o-z)
       double precision se(:,:),ve(:,:)
       integer :: n
       double precision t(size(se,1),size(se,1)),a(size(se,1),size(se,1)),&
                        l(size(se,1),size(se,1))
       double precision b(size(se,1),size(se,1))
!      double precision se(nrank,nrank),ve(nrank,nrank)
!      double precision t(nrank,nrank),a(nrank,nrank),l(nrank,nrank)
!       double precision b(nrank,nrank)
       n=size(se,1)
         ia=ne/2

       do 1 i=1,nrank
           au=real(ne-i+1)/2.
           fio=gamdev(au)
         t(i,i)=sqrt(2*fio)
         do 2 j=1,i-1
           u=normal01()
            t(i,j)=0.
            t(j,i)=u
2         continue
1       continue
       do 3 i=1,nrank
          do 4 j=1,nrank
             a(i,j)=0.
             do 5 k=1,nrank
             a(i,j)=a(i,j)+t(i,k)*t(j,k)
5             continue
4          continue
3       continue
       do 6 i=1,nrank
         do  j=1,nrank
           l(i,j)=0.
	 enddo
6      continue
!EV     call choles(se,nrank,l)
       call chols4_ev(se,l)
       do 30 i=1,nrank
          do 40 j=1,nrank
             b(i,j)=0.
             do 50 k=1,nrank
               b(i,j)=b(i,j)+l(k,i)*a(k,j)
50           continue
40        continue
30     continue
       do 31 i=1,nrank
          do 41 j=1,nrank
             ve(i,j)=0.
             do 51 k=1,nrank
               ve(i,j)=ve(i,j)+b(i,k)*l(k,j)
51           continue
41        continue
31     continue
       do 101 i=1,nrank
         do 102 j=1,nrank
           ve(i,j)=ve(i,j)/ne
102      continue	
101    continue

       return
       end subroutine


      function gamdev(a)
       implicit double precision(a-h,o-z)
       if(a.lt.1)then
         print *,'gamma with parameter <1'
	 stop
       endif
       if(a.lt.6)then
         x=1.
           ia=int(a)
         do 11 j=1,ia
            u=unif()
            x=x*u
11        continue
         x=-log(x)
       else
1         u=unif()
         v1=2.*u-1.
         u=unif()
         v2=2.*u-1.
         if(v1**2+v2**2.gt.1.)goto 1
         y=v2/v1
           am=a-1
         s=sqrt(2.*am+1.)
         x=s*y+am
         if (x.le.0.)goto 1
            e=(1.+y**2)*exp(am*log(x/am)-s*y)
         u=unif()
         if (u.gt.e)goto 1
       endif
       gamdev=x
       return
       end function

subroutine ginv(a,n,tol,rank)
! returns generalized inverse of x(n,n). tol is working zero 
! and irank returns the rank of the matrix. rework of rohan fernando's 
! f77 subroutine by i. misztal 05/05/87-05/23/00

 implicit none
 integer n
 real(r8) a(n,n),w(n),tol
 integer i,ii,j,rank
 
 rank=n
 do i=1,n    
    do  j=1,i-1
         a(i:n,i)=a(i:n,i)-a(i,j)*a(i:n,j) 
    enddo
    if (a(i,i).lt.tol) then
         a(i:n,i)=0.0	      
      rank=rank-1
      else
        a(i,i)=sqrt(a(i,i))
        a(i+1:n,i)=a(i+1:n,i)/a(i,i)
    endif
 enddo	 

 do i=1,n
    if (a(i,i).eq.0.) then
         a(i+1:n,i)=0
      else
         a(i,i)=1.0/ a(i,i)
         w(i+1:n)=0
         do  ii=i+1,n                  
             w(ii:n)=w(ii:n)-a(ii:n,ii-1)*a(ii-1,i)
             if (a(ii,ii).eq.0.) then
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
 
 end subroutine


  function rinvgauss(mu,lambda) result(sample)
    !  Random variates from inverse Gaussian distribution
    ! modified to avoid negative roots by taking the larger root
    ! original Michael et al. paper, Amer. Stat. 30:88
    ! AL, 10/7/09
    implicit none
    real(r8):: r1,r2,mu,lambda,sample,w,c,v,p1

    v = normal01()**2
    w=mu*v
    c=mu/(2*lambda)
    
    r2 = mu + c*(w+sqrt(w*(4*lambda+w)))

    r1 = mu**2/r2
    
    ! the original code is in fact
    ! r1 = mu + c*(w-sqrt(w*(4*lambda+w)))
    ! r2 = mu**2/r1
    ! but for small lambda's gives r1<0  (for small lambdas everything is a disaster, because E(samples)/=mu )
    
    
    if(unif()<= mu/(mu+r1))then
      sample=r1
    else
      sample=r2
    endif
    ! to avoid underflows
    if (sample<tiny(sample)) then
      print *,sample,mu
      sample=abs(tiny(sample))
      print *,'resqueezing tau2'
    endif
  end function


end module random
!---------------!





module boots
use kinds
use random

INTERFACE bootstrap
! bootstrap integer vectors, real(r8) vectors
  module procedure bootstrap_int,&
                   bootstrap_r8,&
                   bootstrap_int_some,&
                   bootstrap_r8_some
END INTERFACE

INTERFACE sample
!sample without replacement (swr) integer vectors and real(r8) vectors
  module procedure swr_int,&
                   swr_r8,&
		   swr_chr,&
                   swr_int_some,&
                   swr_r8_some,&
		   swr_chr_some
END INTERFACE



INTERFACE swap
! swap int2 integer or r8 reals
  module procedure swap_int,&
                   swap_r8,&
		   swap_chr
END INTERFACE

INTERFACE importance
! importance sampling
! samples ONE state i from a set of
! n states with pre-assigned probabilities p(1)...p(n)
! weights do not need to sum to one
  module procedure importance_int,&
                   importance_r8,&
                   importance_logical
END INTERFACE

CONTAINS

 !auxiliary function swap
 subroutine swap_int(a,b)
 ! swap a and b integers
 integer ::a,b,aux
 aux=a; a=b; b=aux
 end subroutine

 subroutine swap_r8(a,b)
 ! swap a and b reals
 real(r8) ::a,b,aux
 aux=a; a=b; b=aux
 end subroutine

 subroutine swap_chr(a,b)
 ! swap a and b chrs
 character(len=*) ::a,b
 character(len=max(len(a),len(b)))::aux
 aux=a; a=b; b=aux
 end subroutine

  ! -Sample n with replacement from a set of n elements


  function bootstrap_int(what) result(out)
  ! bootstrap sampling 
  implicit none
  integer:: what(:)
  integer:: n
  integer:: out(size(what))
  integer :: i,pos
  
  n=size(what)
  do i=1,n
    pos=unifint(1,n)
    out(i)=what(pos)
  enddo    
  end function

  function bootstrap_r8(what) result(out)
  ! bootstrap sampling 
  implicit none
  real(r8):: what(:)
  integer:: n
  real(r8):: out(size(what))
  integer :: i,pos
  
  n=size(what)
  do i=1,n
    pos=unifint(1,n)
    out(i)=what(pos)
  enddo    
  end function

  ! -Sample n with replacement from a set of m elements (n might
  ! be smaller or greater than m)

  function bootstrap_int_some(what,n) result(out)
  ! bootstrap sampling 
  implicit none
  integer:: what(:)
  integer:: n,m
  integer:: out(n)
  integer :: i,pos
  
  m=size(what)
  do i=1,n
    pos=unifint(1,m)
    out(i)=what(pos)
  enddo    
  end function

  function bootstrap_r8_some(what,n) result(out)
  ! bootstrap sampling 
  implicit none
  real(r8):: what(:)
  integer:: n,m
  real(r8):: out(n)
  integer :: i,pos
  
  m=size(what)
  do i=1,n
    pos=unifint(1,m)
    out(i)=what(pos)
  enddo    
  end function
  

  function swr_int(what) result(out)
  ! sampling without replacement (in fact it is just permutation of the elements)
  !idea from http://forums.wolfram.com/mathgroup/archive/2001/Jan/msg00087.html
  implicit none
  integer:: what(:)
  integer:: n
  integer:: out(size(what))
  integer :: i,pos
  
  n=size(what)
  out=what
  do i=1,n
    pos=unifint(i,n) !integer between i and n (if i==n, unifint=i=n)
    call swap(out(i),out(pos))
  enddo    
  end function

  function swr_r8(what) result(out)
  ! sampling without replacement (in fact it is just permutation of the elements)
  implicit none
  real(r8):: what(:)
  integer:: n
  real(r8):: out(size(what))
  integer :: i,pos
  
  n=size(what)
  out=what
  do i=1,n
    pos=unifint(i,n)
    call swap(out(i),out(pos))
  enddo    
  end function

  function swr_chr(what) result(out)
  ! sampling without replacement (in fact it is just permutation of the elements)
  implicit none
  character(len=*):: what(:)
  integer:: n
  character(len=len(what)):: out(size(what))
  integer :: i,pos
  
  n=size(what)
  out=what
  do i=1,n
    pos=unifint(i,n)
    call swap(out(i),out(pos))
  enddo    
  end function

  function swr_int_some(what,n) result(out)
  ! sampling without replacement of n out of m (n has to be <m)
  implicit none
  integer:: what(:)
  integer:: n,m
  integer:: temp(size(what)),out(n)
  integer :: i,pos
  
  m=size(what)
  if (n>m) then
    print *,'can''t sample ',n,' elements out of ',m
    stop
  endif
  temp=what
  do i=1,n
    pos=unifint(i,m)
    call swap(temp(i),temp(pos))
  enddo    
  out=temp(1:n)
  end function

  function swr_r8_some(what,n) result(out)
  ! sampling without replacement of n out of m (n has to be <m)
  implicit none
  real(r8):: what(:)
  integer:: n,m
  real(r8):: temp(size(what)),out(n)
  integer :: i,pos
  
  m=size(what)
  if (n>m) then
    print *,'can''t sample ',n,' elements out of ',m
    stop
  endif
  temp=what
  do i=1,n
    pos=unifint(i,m)
    call swap(temp(i),temp(pos))
  enddo    
  out=temp(1:n)
  end function

  function swr_chr_some(what,n) result(out)
  ! sampling without replacement of n out of m (n has to be <m)
  implicit none
  character(len=*):: what(:)
  integer:: n,m
  character(len=len(what)):: temp(size(what)),out(n)
  integer :: i,pos
  
  m=size(what)
  if (n>m) then
    print *,'can''t sample ',n,' elements out of ',m
    stop
  endif
  temp=what
  do i=1,n
    pos=unifint(i,m)
    call swap(temp(i),temp(pos))
  enddo    
  out=temp(1:n)
  end function

  function importance_int(states,weights) result(out)
  ! sample one state with a given weight
  implicit none
  real(r8)::weights(:),p,u
  real(r8):: stw(size(weights))
  integer:: states(:),out,i,n
  !standardize to 1
  stw=weights/sum(weights)
  n=size(weights)
  do i=2,n
    stw(i)=stw(i)+stw(i-1)
  enddo
  u=unif()
  i=0
  do i=1,n
    if (u<stw(i)) exit
  enddo
  out=states(i)
  
  end function
    
  function importance_logical(states,weights) result(out)
  ! sample one state with a given weight
  implicit none
  real(r8)::weights(:),p,u
  real(r8):: stw(size(weights))
  logical:: states(:),out
  integer::i,n
  !standardize to 1
  stw=weights/sum(weights)
  n=size(weights)
  do i=2,n
    stw(i)=stw(i)+stw(i-1)
  enddo
  u=unif()
  i=0
  do i=1,n
    if (u<stw(i)) exit
  enddo
  out=states(i)
  
  end function
    
  function importance_r8(states,weights) result(out)
  ! sample one value with a given weight
  implicit none
  real(r8)::weights(:),p,u
  real(r8):: stw(size(weights))
  real(r8):: states(:),out
  integer:: i,n
  !standardize to 1
  stw=weights/sum(weights)
  n=size(weights)
  do i=2,n
    stw(i)=stw(i)+stw(i-1)
  enddo
  u=unif()
  i=0
  do i=1,n
    if (u<stw(i)) exit
  enddo
  out=states(i)
  
  end function
  
  function log2p(logL) result(p)
  ! passes from probabilities in the log-scale to **standardized**
  ! (sum to 1) probabilities in the regular scale.
  ! ex: logL1=-10,logL2=-11 -> p=(0.73,0.27)
  ! computing using trick to avoid overflows (my thesis, p.48;  S&G,2002
  implicit none
  real(r8):: logL(:),p(size(logL))
  real(r8):: m,den
!  if(any(logL>0d0)) then
!    print *,'logL>0',logL
!    stop
!  endif
  m=maxval(logL)
  den=m+log(sum(exp(logL-m)))
  p=exp(logL-den)
  
  end function  
  

end module boots
!--------------!



module halfstored
! this module solves a half-stored system of equations lhs theta=rhs
! by cholesky decomposition
! and allows for semi-positive definite matrices
!
! double precision
!
! 'lhs' matrix is stocked as follows:
! 11         that is: do i=1,n
! ->                    do j=1,i
! 21 22                   pos=pos+1
! ---->                     a(pos)=kk(i,j)
! 31 32 33              enddo
! ------->            enddo
! 41 42 43 44         or using ihmssf2(i,j)
! ---------->

! ihmssf2 from mtgsam, modified
! chol in Alan Miller page
! rest by AL
! 3/7/07

INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 60)


contains

      
      
      
      INTEGER FUNCTION IHMSSF2(I,J)

!     FUNCTION TO WORK OUT ADDRESS IN A HALFSTORED SYMMETRIC MATRIX
!     
!*          A, packed columnwise in a linear array.  The j-th column of A
!          is stored in the array AP as follows:
!*          AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
!*
! 'a' matrix is stocked as follows:
! 11         that is: do i=1,n
! ->                    do j=1,i
! 21 22                   pos=pos+1
! ---->                     a(pos)=kk(i,j)
! 31 32 33              enddo
! ------->            enddo
! 41 42 43 44         or using ihmssf2(i,j)
! ---------->
 
      IMPLICIT NONE
      INTEGER :: I,J
      
      IF(i<=j)THEN
         IHMSSF2=i+(j-1)*j/2
      ELSE
         IHMSSF2=j+(i-1)*i/2
      END IF
      RETURN
      END function
      

SUBROUTINE chol (a, n, nn, u, nullty, ifault)

!    Algorithm AS6, Applied Statistics, vol.17, (1968)

!    Given a symmetric matrix order n as lower triangle in a( ),
!    calculates an upper triangle, u( ), such that uprime * u = a.
!    a must be positive semi-definite.  eta is set to multiplying
!    factor determining effective zero for pivot.

!    arguments:-
!    a()     = input, a +ve definite matrix stored in lower-triangular form.
!    n       = input, the order of a
!    nn      = input, the size of the a and u arrays >= n*(n+1)/2
!    u()     = output, a lower triangular matrix such that u*u' = a.
!              a & u may occupy the same locations.
!    nullty  = output, the rank deficiency of a.
!    ifault  = output, error indicator
!                    = 1 if n < 1
!                    = 2 if a is not +ve semi-definite
!                    = 3 if nn < n*(n+1)/2
!                    = 0 otherwise

!***********************************************************************

! AL
! from Alan Miller web page
! 'a' matrix is stocked as follows:
! 11         that is: do i=1,n
! ->                    do j=1,i
! 21 22                   pos=pos+1
! ---->                     a(pos)=kk(i,j)
! 31 32 33              enddo
! ------->            enddo
! 41 42 43 44         or using ihmssf2(i,j)
! ---------->
!
! 
! AL: information above is not very clear. After Cholesky, a=LU, where L=U'
! L is lower triangular and U is upper triangular. This subroutine returns the lower triangular L
! stocked in the format above 


IMPLICIT NONE
REAL(dp) , INTENT(IN)  :: a(:)
INTEGER, INTENT(IN)    :: n, nn
REAL(dp) , INTENT(OUT) :: u(:)
INTEGER, INTENT(OUT)   :: nullty, ifault

! Local variables
REAL (dp)              :: eta2, x, w
INTEGER                :: i, icol, ii, irow, j, k, kk, l, m

!       The value of eta will depend on the word-length of the
!       computer being used.  See introductory text.

REAL (dp), PARAMETER   :: eta = 1.d-14, zero = 0.0_dp

ifault = 1
IF (n <=  0) RETURN
ifault = 3
IF (nn < n*(n+1)/2) RETURN
ifault = 2
nullty = 0
j = 1
k = 0
eta2 = eta*eta
ii = 0

!       Factorize column by column, icol = column no.

DO icol = 1, n
  if(mod(icol,1000)==0) print *,'chol in col',icol
  ii = ii + icol
  x = eta2*a(ii)
  l = 0
  kk = 0
  
!       IROW = row number within column ICOL
  
  DO irow = 1, icol
    kk = kk + irow
    k = k + 1
    w = a(k)
    m = j
    DO i = 1, irow
      l = l + 1
      IF (i == irow) EXIT
      w = w - u(l)*u(m)
      m = m + 1
    END DO

    IF (irow == icol) EXIT
    IF (u(l) /= zero) THEN
      u(k) = w / u(l)
    ELSE
      IF (w*w > ABS(x*a(kk))) RETURN
      u(k) = zero
    END IF
  END DO

  IF (ABS(w) > ABS(eta*a(k))) THEN
    IF (w < zero) RETURN
    u(k) = SQRT(w)
  ELSE
    u(k) = zero
    nullty = nullty + 1
  END IF
  j = j + icol
END DO

ifault = 0
RETURN
END SUBROUTINE chol   

function solvecholL(L,b) result(y)
! solves Ly=b where L is a  lower-stored **lower** cholesky decomposition and b and y vectors
implicit none
INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 60)
real(dp):: L(:)

real(dp):: b(:),y(size(b)),rhs
integer:: n,m,i,j,pos
n=size(b); m=(n*(n+1)/2)

if(m/=size(L)) then 
  print *,m,size(L)
  stop
end if
pos=0
y=0d0
do i=1,n
  rhs=b(i) 
  do j=1,i-1 
    pos=pos+1 
      ! correct this element of the rhs for the already solved unknowns
    rhs=rhs-L(pos)*y(j)
  enddo
  pos=pos+1
  if(L(pos)/=0d0) then 
    y(i)=rhs/L(pos)
  else
    y(i)=0d0
  endif
enddo
end function

function solvecholU(L,b) result(y)
! solves Uy=b where U=L' , the lower-stored **lower** cholesky decomposition and b and y vectors
implicit none
real(dp):: L(:)
real(dp):: b(:)
real(dp):: y(size(b)),rhs
integer:: n,m,i,j,pos
n=size(b); m=(n*(n+1)/2)

if(m/=size(L)) then 
  print *,m,size(L)
  stop
end if
pos=m+1 ! backwards
do i=n,1,-1
  pos=pos-1
  rhs=b(i) 
  if(L(pos)/=0d0) then 
    y(i)=rhs/L(pos)
  else
    y(i)=0d0
  endif
  ! update the rhs
  do j=i-1,1,-1 
    pos=pos-1
    ! correct all the rhs for the already solved unknowns
    b(j)=b(j)-L(pos)*y(i)
  enddo
enddo
end function

subroutine solve_hs(xx,xy,sol)
! solves equations xx sol=xy where xx is half-stored as above
! xx is converted to L Cholesky decomposition in the process
!    ifault  = output, error indicator
!                    = 1 if n < 1
!                    = 2 if a is not +ve semi-definite
!                    = 3 if nn < n*(n+1)/2
!                    = 0 otherwise

real(dp)::xx(:)
real(dp)::xy(:),sol(:)
integer:: nullty,ifault,n,m !n size of the equations m=n*(n+1)/2 size of the
! half-stored matrix
character(len=100)::fault(0:3)

fault(0)='ifault = 0 otherwise'
fault(1)='ifault = 1 if n < 1'
fault(2)='ifault = 2 if a is not +ve semi-definite, uncorrect results'
fault(3)='ifault = 3 if nn < n*(n+1)/2'

n=size(xy)
m=size(xx)
if(n/=size(sol)) then 
  print *,'lhs has different size than sol',n,size(sol)
  stop
endif  
if(m/=(n*(n+1)/2)) then
  print *,'inconsistency in system of equations',n,m
  stop
endif

call chol(xx,n,m,xx,nullty,ifault)
print *,'xx factorized'
print *,'redundancies in xx',nullty
print *,'ifault',ifault
print *,fault(ifault)
sol=solvecholL(xx,xy)
print *,'1st solve'
sol=solvecholU(xx,sol)
print *,'2nd solve'
end subroutine

end module halfstored






module prob_haplo_m
! Module to compute theoretical p(IBD|markers) for a couple of haplotypes
! from MG original paper, extended by A.Legarra 14/12/05
use kinds

contains

function computeIBD(haplotype1,haplotype2,mapdist,homozygosity,posQTL,ne,T) 
! This function computes the probability of the allele in a given locus, A
! (or QTL) to be IBD given two haplotypes of a set of markers (integers, but can
! be changed to char) spapced at some distances with a given homozigosity at the
! base generation
! This is formule 1 in MG paper

implicit none
integer :: haplotype1(:),haplotype2(:),posQTL
real(r8) :: homozygosity(:),mapdist(:),computeIBD,p1,p2,T,Ne
integer :: nmarker
real(r8),allocatable:: pcond(:,:)

nmarker=size(haplotype1)

allocate(pcond(0:1,nmarker+1))

! 1st: p(A is IBD and Markers are IBS)
! page 632
pcond=checkpcond(nmarker+1,haplotype1,haplotype2,homozygosity,posQTL,.true.) 
p1=prob_haplo(nmarker+1,posQTL,mapdist,pcond,t,ne)

!2nd p(A is NOT IBD and markers are IBS)
pcond=checkpcond(nmarker+1,haplotype1,haplotype2,homozygosity,posQTL,.false.) 
p2=prob_haplo(nmarker+1,posQTL,mapdist,pcond,t,ne)

computeIBD=p1/(p1+p2)
return
end function


function checkpcond(npos,haplotype1,haplotype2,homozygosity,posA,IBDstatus) 
  ! This function checks the probabilities of observing alleles identical by state in position
  ! i whenever they are really identical by descent (checkpcond(1,:) or not (checkpcond(0,:)
  ! page 632 MG 2001
  ! Another change is needed to account for different allele frequencies at the
  ! base population (p.628), which goes into the homozygosity part
  implicit none
  integer :: posA,pos,i,npos
  real(r8):: checkpcond(0:1,npos),homozygosity(npos-1)
  integer :: haplotype1(npos-1),haplotype2(npos-1)
  logical :: IBDstatus
  
  do i=1,npos
    if (i==posA) then
      if (IBDstatus) then
      ! for the 'A' locus (QTL) either is the same (therefore IBD) or not
        checkpcond(1,i)=1. 
        checkpcond(0,i)=0.
      else
        checkpcond(1,i)=0. 
        checkpcond(0,i)=1.
      endif         	
    else
      pos=i
      if (i>posA) pos=i-1
      if (haplotype1(pos)==haplotype2(pos)) then
        checkpcond(1,i)=1. 
	checkpcond(0,i)=homozygosity(pos)
      else 
        checkpcond(1,i)=0.
	checkpcond(0,i)=1.-homozygosity(pos)	
      endif
    endif
  enddo
return  	
end function

function changedist(distamongmarkers,nmarker,posA)
  ! changes distances from among-marker distances to include the distance of the
  ! puptative QTL in the posA position, placed in the middle of the bracket
  implicit none
  integer :: posA,nmarker
  real(r8) :: distamongmarkers(nmarker),changedist(nmarker+1)

  changedist(1:posA-1)=distamongmarkers(1:posA-2)
  changedist(posA-1)=.5*distamongmarkers(posA-1)
  changedist(posA)=.5*distamongmarkers(posA-1)
  changedist(posA+1:nmarker+1)=distamongmarkers(posA:nmarker)
return
end function


! -- Begin of original MG code--

function prob_haplo(npos,posA,dist,pcond,t,ne)
  ! INPUT VARIABLES:
  integer :: npos !no of positions in haplo
  integer :: posA !position of locus A
  real(r8) :: dist(1:npos-1) ! (npos-1) distances between the positions
  real(r8) :: pcond(0:1,1:npos) !pcond(j,k) = conditionally probability of observing
  ! the identity of the markers / locus A given IBD status j at position k
  real(r8) :: t,ne ! T=no of generations; Ne=effective population size
  ! WORK VARIABLES
  real(r8) :: prob(0:npos,0:1) !prob(i,j) = prob of IBD segment for the last i positions
  !j=1 (j=0) indicates that the recombination on left of
  ! this IBD segment was (not) already accounted for.
  real(r8) :: probsav(0:npos,0:1) !saves previous prob variable

  ! initialise variables
  f0=1.-exp(-0.5*t/ne) !f0 is inbreeding at current generation
  prob=0
  prob(0,0)=(1.-f0)*pcond(0,1) !position 1 is nonIBD
  prob(1,1)=f0*pcond(1,1) !position 1 is IBD
  nseg=1 !no of segments that are in the prob variable
  ! extend every segment with:
  ! x0 : a non-IBD position
  ! x1 : a recombination and an IBD position
  ! _1 : no_recombination, i.e. extend the IBD segment (only if current segment is IBD)
  do i=2,npos
    probsav=prob !save old prob
    prob=0
  	k=1
  	if(i<=posA)k=0 !account for recomb on the left; dont account for recomb on right
  	  	  	  	  ! extend with : x0 which results in segment with 0 IBD_positions at end, i.e. prob(0,:)
  	  f0_0=1.-fr(0.d0,dist(i-1)) !prob of nonIBD position i given nonIBD (i-1); Equation (7)
  	  prob(0,0)=prob(0,0)+probsav(0,0)*f0_0
  	  do j=1,nseg
  	  	prob(0,0)=prob(0,0)+probsav(j,0)*pibd(i,j,1,1)*(1-f0)
  	  	prob(0,0)=prob(0,0)+probsav(j,1)*pibd(i,j,0,1)*(1-f0)
  	  end do
  	! pibd(i,j,kk,k) function gets probability of IBD segment of j positions, which needs
  	!accounting for left recomb (kk=1) or not (kk=0), and accounting for recomb. on
  	! right (k=1) or not (k=0)
  	! extend with : x1 which results in segment with 1 IBD_position at end, i.e. prob(1,:)
  	prob(1,0)=probsav(0,0)*f0
  	do j=1,nseg
  	  prob(1,k)=prob(1,k)+probsav(j,0)*pibd(i,j,1,k)*f0
  	  prob(1,k)=prob(1,k)+probsav(j,1)*pibd(i,j,0,k)*f0
  	end do
  	! extend with : _1 i.e. a segment of i positions goes to one of i+1 positions
  	prob(2:nseg+1,:)=probsav(1:nseg,:)
  	nseg=nseg+1
  	! multiply with conditional probability of observing the identity at position i
  	prob(0,0)=prob(0,0)*pcond(0,i)
  	prob(1:nseg,:)=prob(1:nseg,:)*pcond(1,i)
  end do
  ! account for end segments
  do i=1,nseg
  	prob(i,0)=prob(i,0)*pibd2(i,1)
  	prob(i,1)=prob(i,1)*pibd2(i,0)
  end do
  ! function pibd2(i,k) gets IBD probability of last i positions accounting for
  ! the left recombination(k=0) or not (k=1)
  ! sum all probabilities
  prob_haplo=sum(prob(0:nseg,0:1))
return ! finished

CONTAINS
function pibd(i,no_pos,kk,k)
  integer :: i,no_pos !segment extends for no_pos position to the left of position i
  integer :: kk !kk=1 (kk=0) => (dont) account for recombin. on the left of segment
  integer :: k !k=1 (k=0) => (dont) account for recombin. on the right of the segment
  real(r8) :: distance

  distance=sum(dist(i-j:i-2))
  pibd=0
  if(kk==0 .and. k==0)then
    pibd=f(distance)/f0 ! /f0 conditions on the presence of IBD position
  else if(k==0)then !dont account for right recomb
  	if(i-j-1>0)pibd=fr(distance,dist(i-j-1))/f0
  else if(kk==0)then !dont account for left recomb
  	  pibd=fr(distance,dist(i-1))/f0
  else !account for both recomb.
  	  if(i-j-1>0)pibd=fdr(distance,dist(i-j-1),dist(i-1))/f0
  end if
end function

function pibd2(i,k)
  integer :: i !no of positions that are IBD at end of haplotype
  integer :: k !k=1 (k=0) (dont) account for left recombination
  real(r8) :: distance

  distance=sum(dist(nseg-i+1:npos-1))
  pibd2=0
  if(k==0)then
  	pibd2=f(distance)/f0
  else
  	if(nseg-i>0)pibd2=fr(distance,dist(nseg-i))/f0
  end if
end function

function f(c)
  real(r8) :: c !distance in Morgans
  ! Equation 4 :
  f=exp(-2*c)*(1.-exp(-T*(2*c+.5/ne)))/(2.*ne*(1.-exp(-(2*c+.5/ne))))
end function

function fr(c,c1)
  real(r8) :: c,c1 !c : size of IBD; c1: recombination in next region of size c1
  fr=f(c)-f(c+c1)
end function

function fdr(c,c1,c2)
  real(r8) :: c,c1,c2 !c : size of IBD; c1(c2): recomb in left (right) region of size c1 (c2)
  fdr=fr(c,c1)-fr(c+c2,c1)
end function

end function prob_haplo
! EXAMPLE FOLLOWS

!integer(10):: haplo1,haplo2
!haplo1=1
!haplo1(10)=2
!haplo2=1
!homozygosity=.5
!distamongmarkers=.01
!t=10.d0
!Ne=100.d0
!dist=changedist(distamongmarkers,nmarker,posA)
!p(IBD)=computeIBD(haplo_list(:,i),haplo_list(:,j),dist,homozygosity,posA,ne,T)

end module prob_haplo_m
!---------------------!

module meio
use kinds
use random
contains
  function inhaldane(r)
  ! recombination fraction -> Morgans
  real(r8)::r(:)
  real(r8) :: inhaldane(size(r))
  inhaldane=-.5*log(1.-2*r)
  end function

  function fromhaldane(x)
  !distance -> recombination fraction
  real(r8)::x(:)
  real(r8) :: fromhaldane(size(x))
  fromhaldane=.5*(1-exp(-2.*x))
  end function




 function meiosis (ksome,map,iflag) result(gamete)
 !produces one gamete out of two chromosomes
 ! this is following notes by Fernando and also by an old subroutine from MPE
 implicit none
 integer nloci
 integer ksome(:,:),gamete(size(ksome,1))
 logical::origin1(size(ksome,1))
 integer :: nxover,i
 integer::iflag
 real(r8),allocatable:: places(:)
 real(r8):: length,map(:) !contains distances in Morgan, size(nloci-1)
 real(r8):: acculmap(size(map))
 iflag=0
 nloci=size(ksome,1)
! which ksome is taken first
 if (unif()<.5d0) then
   origin1=.true.
 else
   origin1=.false.
 endif    

 length=sum(map)
 acculmap(1)=map(1)
 do i=2,size(map)
   acculmap(i)=sum(map(1:i))
 enddo
 !sample n of xovers
 nxover=random_poisson(length) !length of ksome in M
 iflag=nxover
! print *,nxover
 allocate(places(nxover)) !where xover happens
 !sample places for xover (in the Morgan map)

 do i=1,nxover
   places(i)=unif()*length
 enddo  

 do i=1,nxover
   where(acculmap>places(i)) !we recombine from left to right (ideally, we keep the left end origin1=T)
     ! and for this reason origin1(1) does not change
     ! and so, map and origin1 have same length
     origin1(2:nloci)=.not.origin1(2:nloci)
   end where
 enddo

 where(origin1) gamete=ksome(:,1)
 where(.not.origin1) gamete=ksome(:,2)
! print *,gamete
 deallocate(places)

 end function

end module meio


!
! MODULE NAME:
!	MISC_timing
! FUNCTION:
!	miscellaneaus utility routines
! CONTAINS:
!	second, ticktock, stop_watch
!
! This module was shamelessly cpied from 
! https://pcbornemann1.ma.tum.de/cgi-bin/trac.cgi/browser/trunk/src/timing/MISC_timing.f90?rev=2&format=raw
! AL
!-----------------------------------------------------------------
!
! NAME:
!	stop_watch_init
! FUNCTION:
!	initialize time measurement
! SYNTAX:
!	CALL stop_watch_init(int, char, rtinfo)
! ON INPUT:
!	i_number:  number of timers in timer structure	INTEGER
!	c_strings: strings describing the code segments	CHARACTER*16
!	p_tsinfo:  runtime info data structure		TYPE (rt_info)
! ON OUTPUT:
!	p_tsinfo: runtime info data structure (updated)	TYPE (rt_info)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
!
! NAME:
!	stop_watch
! FUNCTION:
!	measure time for a code segment and put info in runtime structure
! SYNTAX:
!	CALL stop_watch(char, int, rtinfo)
! ON INPUT:
!	c_action: start or stop the watch		CHARACTER*5
!	i_ident:  identification for the timer		INTEGER
!	p_tsinfo: runtime info data structure		TYPE (rt_info)
! ON OUTPUT:
!	p_tsinfo: runtime info data structure (updated)	TYPE (rt_info)
! CALLS:
!
! COMMENTS:
!
!-----------------------------------------------------------------
! NAME:
!	second
! FUNCTION:
!	calculate actual second for timing
! SYNTAX:
!	real= second()
! ON INPUT:
!
! ON OUTPUT:
!	second:	actual second measured from 1/1/1996	real
! CALLS:
!
! COMMENTS:
!	if no system clock is available, result is negative
!
!-----------------------------------------------------------------
! NAME:
!	ticktock
! FUNCTION:
!	fortran 90 portable timing routine
! SYNTAX:
!	real= ticktock(overhead= real, start= real)
! ON INPUT:
!	overhead:	if present, overhead is subtracted	real
!	start:		first call to tick			real
! ON OUTPUT:
!	overhead:	if present, overhead is computed	real
!	ticktock:	time of program segment			real
! CALLS:
!
! COMMENTS:
!	this routine has different functionalities:
!	on 1st call:
!	        r_sta = ticktock(overhead= r_ovh)
!	    or
!	        r_sta = ticktock()
!	    gives back the starting tick mark for timing in r_sta and, if
!	    requested, the overhead for calling the timing routine in r_ovh
!	on second call:
!	        r_tim= ticktock(start=r_sta)
!	    or
!	        r_tim= ticktock(start=r_sta, overhead= r_ovh)
!	    gives back the time in [s] for the program segment in r_tim,
!	    additionally, the overhead given by r_ovh is subtracted from total
!	    time
!
!	the convention is:
!	noting is given:              first call
!	only overhead is given:       first call
!       only start is given           following call
!	overhead and start are given: following call
!
!-----------------------------------------------------------------
! PUBLIC:
!	stop_watch, stop_watch_init
! COMMENTS:
!
! USES:
!	MISC_globalparam
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version		j. behrens	7/96
!	2. stop_watch changed		j. behrens	1/97
!	   stop_watch_init added
!	3. changed for self consistency	j. behrens	1/97
!	4. amatos-1.0 compliant		j. behrens	11/2000
!
!*****************************************************************
	MODULE MISC_timing
	  INTEGER, PARAMETER :: i_iolog=6 ! can be changed to -1 for non logging
	  PRIVATE

!---------- structure for timing

	  INTEGER, PARAMETER   :: DEF_timings= 10 ! number of different timers

	  TYPE time_info
	    CHARACTER (len=16) :: c_tim
	    REAL               :: r_tim
	    REAL               :: r_lap
	  END TYPE time_info
	  TYPE sw_info
	    INTEGER                                  :: i_num
	    TYPE (time_info), DIMENSION(DEF_timings) :: p_tim
	  END TYPE sw_info

	  PUBLIC :: stop_watch, stop_watch_init, time_info, sw_info, DEF_timings
	  CONTAINS
!*****************************************************************
	  SUBROUTINE stop_watch_init(i_number, c_strings, p_tsinfo)

!---------- local declarations

	  IMPLICIT NONE
	  INTEGER, INTENT(in)                                  :: i_number
	  CHARACTER (len=16), INTENT(in), DIMENSION(i_number)  :: c_strings
	  TYPE (sw_info), INTENT(inout)                        :: p_tsinfo
	  INTEGER                                              :: i_cnt

!---------- consistency check

	  check_con: IF(i_number > DEF_timings) THEN
	    IF(i_iolog > 0) &
	      write(i_iolog,*) 'TIMING: Initialization of timers omitted several'
	    RETURN
	  ELSE check_con

!---------- set number of timers in this structure

	    p_tsinfo%i_num = min(DEF_timings, i_number)

!---------- initialize info structure

	    main_loop: DO i_cnt=1, p_tsinfo%i_num
	      p_tsinfo%p_tim(i_cnt)%c_tim= c_strings(i_cnt)
	      p_tsinfo%p_tim(i_cnt)%r_tim= 0.0
	      p_tsinfo%p_tim(i_cnt)%r_lap= 0.0
	    END DO main_loop
	  END IF check_con

	  RETURN
	  END SUBROUTINE stop_watch_init
	  
!*****************************************************************
	  SUBROUTINE stop_watch(c_action, i_ident, p_tsinfo)

!---------- local declarations

	  IMPLICIT NONE
	  CHARACTER (len= 5), INTENT(in) :: c_action
	  INTEGER, INTENT(in)            :: i_ident
	  TYPE (sw_info), INTENT(inout)  :: p_tsinfo
	  REAL                           :: r_tmp

!---------- consistency check

	  check_con: IF(i_ident > p_tsinfo%i_num) THEN
	    IF(i_iolog > 0) &
	      write(i_iolog,*) 'TIMING: Identification not correct, nothing done'
	    RETURN
	  END IF check_con

!---------- action start:

	  start_stop: IF(c_action(1:4) == 'star') THEN
	    p_tsinfo%p_tim(i_ident)%r_lap= ticktock()

!---------- action stop:

	  ELSE IF(c_action(1:4) == 'stop') THEN start_stop
	    r_tmp= ticktock(start= p_tsinfo%p_tim(i_ident)%r_lap)
	    p_tsinfo%p_tim(i_ident)%r_tim= p_tsinfo%p_tim(i_ident)%r_tim+ r_tmp

!---------- not supported

	  ELSE start_stop
	    IF(i_iolog > 0) &
	      write(i_iolog,*) 'TIMING: No supported action given: ', c_action
	  END IF start_stop

	  RETURN
	  END SUBROUTINE stop_watch

!*****************************************************************
	FUNCTION second() RESULT (times)

!---------- local declarations

	IMPLICIT NONE

	REAL               :: times
!	CHARACTER (len=8)  :: a_date
	INTEGER, PARAMETER :: i_start= 19960101
	INTEGER, PARAMETER :: i_dsecs= 86400
!	INTEGER            :: i_day, i_mult
	INTEGER            :: i_cnt, i_rte
	INTEGER            :: i_total

!---------- CALL date_and_time for day info

!	CALL date_and_time(date= a_date)
!	date_ok: IF(a_date /= '        ') THEN
!	  read(a_date,*) i_day
!	  i_mult= abs(i_day- i_start)
!	ELSE date_ok
!	  i_mult= 1
!	END IF date_ok

!---------- CALL system_clock for maximum precision clock

	CALL system_clock(count= i_cnt, count_rate= i_rte)
	clock_ok: IF(i_rte /= 0) THEN
!	  i_total= i_cnt+ i_mult* i_dsecs
	  i_total= i_cnt
	  times= float(i_total)/ float(i_rte)
	ELSE clock_ok
	  times= -1.0
	END IF clock_ok

	RETURN

	END FUNCTION second
!*****************************************************************
	FUNCTION ticktock(start, overhead) RESULT (timing)

!---------- local declarations

	IMPLICIT NONE

	REAL, OPTIONAL, INTENT(inout) :: overhead
	REAL, OPTIONAL, INTENT(in)    :: start
	REAL                          :: timing
	INTEGER, PARAMETER            :: i_loop= 10
	INTEGER                       :: i
	REAL                          :: r_tm, r_t1, r_tsum

!---------- initialize

	timing=0.0

!---------- check presence of input variables and act accordingly

!---------- second call to ticktock
	start_present: IF(present(start)) THEN
	  overhead_present: IF(present(overhead)) THEN
	    r_tm= second()- start- overhead
	  ELSE overhead_present
	    r_tm= second()- start
	  END IF overhead_present

!---------- first call to ticktock
	ELSE start_present
	  overhead_requested: IF(present(overhead)) THEN
	    r_tsum= 0.0
	    DO i=2, i_loop
	      r_t1= second()
	      r_tm= second()
	      r_tsum= r_tsum+ (r_tm- r_t1)
	    END DO
	    overhead= r_tsum/float(i_loop)
	    r_tm= second()
	  ELSE overhead_requested
	    r_tm= second()
	  END IF overhead_requested
	END IF start_present

!---------- result

	timing= r_tm

	RETURN
	END FUNCTION ticktock
	END MODULE MISC_timing


module mpe_nrm
! this thing is copied from a program by MPE
! "with all due respect"
! indeed old bluppers never die!
! AL 22/07/08
use kinds


integer, allocatable :: ia_nrm(:), ja_nrm(:),ip(:,:)!nind,3
real (r8), allocatable ::  a_nrm(:)
integer:: n_ind,male=2,female=3,unknown=0
contains

!---------------------
 SUBROUTINE INVERT_NRM
!--13/12/00-----------
!Henderson's rules (first computes inbreeding via tabular method)
implicit none
integer :: ind, jnd, isex, iisex, n
real (r8) :: d
real (r8), allocatable :: nrm(:,:), f(:)
logical, allocatable :: nze(:,:)
allocate (nrm(n_ind,n_ind))
allocate (nze(n_ind,n_ind))
allocate (f(n_ind))
nrm=0.d0
nze=.false.

!NRM
do ind=1,n_ind
  nrm(ind,ind)=1.
  if(ip(ind,male) /= unknown .and. ip(ind,female) /= unknown)      &
  nrm(ind,ind)=nrm(ind,ind)+ 0.5*(nrm(ip(ind,male),ip(ind,female)))
  f(ind)=nrm(ind,ind)
  do jnd=1,ind-1
   if(ip(ind,male) /= unknown) &
     nrm(ind,jnd) = 0.5*nrm(jnd,ip(ind,male))
   if(ip(ind,female)/=unknown) &
     nrm(ind,jnd) = nrm(ind,jnd) + 0.5*nrm(jnd,ip(ind,female))
   nrm(jnd,ind)=nrm(ind,jnd)
  enddo !jnd loop
enddo !ind loop

!St. Henderson's rules (old bluppers never die)
nrm=0.
do ind=1,n_ind
  d=1.
  if (ip(ind,male) /= unknown)   d=d-0.25*f(ip(ind,male))
  if (ip(ind,female) /= unknown) d=d-0.25*f(ip(ind,female))
  d=1./d
  nrm(ind,ind)=nrm(ind,ind)+d
  nze(ind,ind)=.true.
  do isex=male,female
    if (ip(ind,isex) /= unknown) then
      nrm(ind,ip(ind,isex))=nrm(ind,ip(ind,isex))-0.5*d      
      nrm(ip(ind,isex),ind)=nrm(ip(ind,isex),ind)-0.5*d      
      nze(ind,ip(ind,isex))=.true.      
      nze(ip(ind,isex),ind)=.true.      
      do iisex=male,female
        if (ip(ind,iisex) /= unknown) then            
          nrm(ip(ind,iisex),ip(ind,isex))=        &
          nrm(ip(ind,iisex),ip(ind,isex))+0.25*d
          nze(ip(ind,iisex),ip(ind,isex))=.true.  
        endif    
      enddo !iisex
    endif
  enddo !isex
enddo !ind

!copy into ia,ja,a form
n=0
do ind=1,n_ind; do jnd=1,n_ind
 if(nze(ind,jnd)) n=n+1
enddo; enddo
allocate (ia_nrm(n_ind+1))
allocate (ja_nrm(n))
allocate (a_nrm(n))
n=0
ia_nrm(1)=1
do ind=1, n_ind
  do jnd=1, n_ind
    if(nze(ind,jnd)) then
      n=n+1
      ja_nrm(n)=jnd
      a_nrm(n)=nrm(ind,jnd)      
    endif
  enddo
  ia_nrm(ind+1)=n+1
enddo
!PRINT*, 'NRM',nrm,'F',f
deallocate (nrm)
deallocate (nze)
deallocate (f)
!-------------------------
 END SUBROUTINE INVERT_NRM
!-------------------------


end module mpe_nrm

module util
use kinds

INTERFACE logical2int
! .true. -> 1
  module procedure log2int_s, &
                   log2int_v, &
                   log2int_m
END INTERFACE

INTERFACE superprintmat
! prints matrices, useful for debug
  module procedure printmat2d, &
                   printmat3d, &
                   printmat4d
END INTERFACE


contains
!hard to classify

function log2int_m(a) result(out)
! 1 if true, 0 if false
logical:: a(:,:)
integer:: out(size(a,1),size(a,2))

out=0
where(a) out=1
end function

function log2int_v(a) result(out)
! 1 if true, 0 if false
logical:: a(:)
integer:: out(size(a,1))

out=0
where(a) out=1
end function

function log2int_s(a) result(out)
! 1 if true, 0 if false
logical:: a
integer:: out

out=0
if(a) out=1
end function


subroutine printmat2d(a)
real(r8)::a(:,:)
!call printmat(a)
integer :: i,j
print *,' 1st 2nd    value'
print *,'-----------------'
do i=1,size(a,1); do j=1,size(a,2)
write(*,'(2i4,a,g10.7)') i,j,'|',a(i,j)
enddo; enddo
end subroutine



subroutine printmat3d(a)
implicit none
real(r8)::a(:,:,:)
integer :: i,j,k
print *,' 1st 2nd 3rd   value'
print *,'--------------------'
do i=1,size(a,1); do j=1,size(a,2); do k=1,size(a,3)
write(*,'(3i4,a,g10.7)') i,j,k,'|',a(i,j,k)
enddo; enddo; enddo
end subroutine

subroutine printmat4d(a)
implicit none
real(r8)::a(:,:,:,:)
integer :: i,j,k,l
print *,' 1st 2nd 3rd 4th   value'
print *,'------------------------'
do i=1,size(a,1); do j=1,size(a,2); do k=1,size(a,3); do l=1,size(a,4)
write(*,'(4i4,a,f10.7)') i,j,k,l,'|',a(i,j,k,l)
!write(*,*) i,j,k,l,'|',a(i,j,k,l)
enddo; enddo; enddo; enddo
end subroutine

function cbind(a,b)
!this is a copy of cbind in R
! cbind=[a b]
implicit none
real(r8):: a(:,:),b(:,:)
real(r8):: cbind(size(a,1),size(a,2)+size(b,2))
if (size(a,1)/=size(b,1)) then
  print *,'wrong size cbind'
  !call printmat(a,'--a--')
  !call printmat(b,'--b--')
  stop
else
  cbind(:,1:size(a,2))=a
  cbind(: , size(a,2)+1:(size(a,2)+size(b,2)))=b
endif
end function

function rbind(a,b)
!this is a copy of rbind in R
! rbind=[a]
!       [b]
implicit none
real(r8):: a(:,:),b(:,:)
real(r8):: rbind(size(a,1)+size(b,1),size(a,2))
if (size(a,2)/=size(b,2)) then
  print *,'wrong size rbind'
  !call printmat(a,'--a--')
  !call printmat(b,'--b--')
  stop
else
  rbind(1:size(a,1),:)=a
  rbind(size(a,1)+1:(size(a,1)+size(b,1)),:)=b
endif
end function

function identity(n)
!identity matrix of size n
implicit none
integer :: i,n
real(r8):: identity(n,n)
identity=0.d0
do i=1,n
  identity(i,i)=1.d0
enddo
end function

  function find_out(this,list) result(pos)
  ! 0 if not found
  ! not efficient
  implicit none
  integer:: this,pos,list(:),n,i
  n=size(list)
  pos=0
  if(any(this==list)) then
  	do i=1,n
    		if (this==list(i)) then
      			pos=i
      			exit
    		endif
  	enddo
  endif
  end function

  function find_out2(this,list) result(pos)
  ! 0 if not found
  ! not efficient
  implicit none
  integer:: this,pos,list(:),n,i,aux(1)
  n=size(list)
  pos=0
  if(any(this==list)) then
  	aux=maxloc( log2int_v(this==list))
	pos=aux(1)
  endif
  end function



end module util



module aux_options
! from IM's blupps1.f90
implicit none
character(len=20):: xc(3)
integer:: nitem
real:: xcnum(3)

contains

  subroutine nums2(a,n,x,xc)
  ! separates array a into items delimited by blanks. character elements are
  ! put into optional character vector xc, decoded numeric values
  ! into optional real vector x, and n contains the number of items. The
  ! dimension of x and xc can be lower than n.
  ! A modification of nums() from f77 to f90
  ! Now accepts real numbers
  ! 2/23/2000

  character (*)::a
  character (*),optional::xc(:)
  real,optional::x(:)
  integer::n,curr,first,last,lena,stat,i

  curr=1;lena=len(a);n=0

  do
    ! search for first nonspace
    first=0
    do i=curr,lena
       if (a(i:i) /= ' ') then
          first=i
          exit
       endif
    enddo
    if (first == 0) exit


    ! search for first space
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
             stop "size of xc in nums2 too small"
       endif
    endif
    if (present(x)) then
       if (size(x) >= n) then
              read(a(first:last),'(f12.0)',iostat=stat)x(n)
              if (stat /=0) x(n)=0
           else
              stop "size of x in nums2 too small"
       endif
    endif

    curr=last+1
  enddo
  end subroutine
  subroutine getoption_unit(io_unit,name,n,x,xc)
  ! In unit io_unit, locates line:
  ! OPTION name str1 str2
  ! where str1, str2 are strings separated by spaces.
  ! Then, it assigns: xc(1)=str1,  xc(2)=str2,...
  ! and attempts to decode strings into real values: x1=value(str1),....
  !
  ! n contains the number of strings.  x and xc are optional and their
  ! dimensions may be smaller than n in which case some strings/values are
  ! not stored.
  !
  ! Upon exit, unit io_p=40 points to line next to the one located.
  !
  ! If the line cannot be located, n=-1
  !

  character (*)::name
  integer::n,io_unit
  real,optional::x(:)
  character (*),optional::xc(:)

  real::x1(100)
  integer::stat,m
  character (50)::xc1(100)
  character (200)::a

  rewind io_unit
  n=-1

  do
     read (io_unit,'(a)',iostat=stat)a
     if (stat /= 0) exit
        call nums2(a,m,x1,xc1)
        if ( m>1 .and. xc1(1) == 'OPTION' .and. xc1(2) == name) then
           n=m-2
           if (present(xc)) xc=xc1(3:size(xc)+2)
           if (present(x)) x=x1(3:size(x)+2)
           exit
        endif
  enddo
  end subroutine
  
end module


module incidence_matrix
use kinds
implicit none
! the purpose of this module is to store and handle sparse incidence matrices
! to accomodate other effects in genomic software using GSRU
! matrix X (NOT SYMMETRIC) is stored in i,j,a format
! products Xy and X'y have to computed efficiently
! also X'Z or, at least, diagonal or X'X (needed for GSRU) whould be computed

! uses TR15581 so it won't be accepted by old compilers

! storage is at random (by now):
! 1 0 2 
! 3 4 0
! can be stored as
! 1 1 1
! 2 1 3
! 2 2 4
! 1 3 2
! but actually if we read data storage is ordered by i:
! 1 1 1
! 1 3 2
! 2 1 3
! 2 2 4



type incm
  integer, allocatable:: i(:)
  integer, allocatable:: j(:)
  real(r8), allocatable:: a(:)
  integer:: ncol
  integer:: nrow
  integer:: nel ! number of elements; so there are 3 nel vectors
  integer:: last ! last occupied element
end type incm

! operations
contains

subroutine init_incm(X,nel)
  implicit none
  type(incm):: X
  integer,optional:: nel
  integer:: n=100
  if(present(nel)) n=nel
  allocate( X%i(n),&
            X%j(n),&
            X%a(n) &
          )
  X%i=0; X%j=0;  X%a=0d0; X%last=0
  X%nel=nel; X%ncol=0; X%nrow=0
end subroutine

subroutine put_incm(X,i,j,a)
  implicit none
  ! adds new values
  type(incm):: X
  integer:: pos,i,j
  real(r8):: a
  pos=X%last+1
  if(pos>X%nel) call reallocate_incm(X,floor(X%nel*1.5d0))
  X%i(pos)=i
  X%j(pos)=j
  X%a(pos)=a
  X%last=pos
  X%nrow=max(i,X%nrow)
  X%ncol=max(j,X%ncol)
end subroutine


function get_incm(X,i,j) result(a)
  implicit none
  ! seeks values
  ! search is horribly bad
  type(incm):: X
  integer:: i,j,k
  real(r8):: a
  a=0d0
  do k=1,X%last
    if ((i==X%i(k)) .and. (j==X%j(k))) then !found
      a=X%a(k)
      return
    endif
  enddo

  print *,i,j,'notfound'  
end function  



subroutine reallocate_incm(X,newsize)
  implicit none
  ! increases X
  
  type(incm):: X,Y
  integer:: newsize,oldsize
  oldsize=X%nel
  print *,'reallocating from ',oldsize,'to',newsize
  ! copy X
  call init_incm(Y,oldsize)
  Y%i=X%i
  Y%j=X%j
  Y%a=X%a
  Y%nel=X%nel
  Y%nrow=X%nrow
  Y%ncol=X%ncol
  Y%last=X%last
  ! redo X
  deallocate(X%i,X%j,X%a)
  call init_incm(X,newsize)
  X%i=Y%i
  X%j=Y%j
  X%a=Y%a
  X%nrow=Y%nrow
  X%ncol=Y%ncol
  X%last=Y%last
end subroutine



subroutine trim_incm(X)
  implicit none
  type(incm):: X
  call reallocate_incm(X,X%last)
end subroutine

subroutine sort_incm(X)
  implicit none
  ! this sorts matrix X in column-wise order:
  ! 1 1 a_11
  ! 2 1 a_21 ... and so on for fast Gauss Seidel
  ! this is the last operation to be performed on X before
  ! computations
  type(incm):: X
  type(incm):: Y
  integer:: i,l,pos

  call init_incm(Y,X%nel)
  Y%nel=X%nel
  Y%nrow=X%nrow
  Y%ncol=X%ncol
  Y%last=X%last
  
  pos=0
  do i=1,X%ncol
    do l=1,X%last
      if(X%j(l)==i) then
        pos=pos+1
	Y%i(pos)=X%i(l)
	Y%j(pos)=X%j(l)
	Y%a(pos)=X%a(l)
      endif
    enddo
  enddo
  deallocate(X%i,X%j,X%a)
  call init_incm(X,Y%nel)
  X%i=Y%i
  X%j=Y%j
  X%a=Y%a
  X%nrow=Y%nrow
  X%ncol=Y%ncol
  X%last=Y%last
end subroutine  




subroutine print_incm(X,verbose)
  implicit none
  ! prints matrix, unordered!!
  type(incm):: X
  integer:: i
  logical,optional:: verbose
  logical:: v=.FALSE.

  if(present(verbose)) v=verbose
  print *,'nrow, ncol, nel, last',X%nrow,X%ncol,X%nel,X%last
  if(v) then
    do i=1,X%nel
      write(*,'(3g20.10)') X%i(i),X%j(i),X%a(i)
    enddo
  endif

end subroutine


function X_times_y(X,y) result(s)
  implicit none
  ! computes X by vector y
  type(incm):: X
  real(r8):: y(:)
  real(r8):: s(X%nrow)
  integer:: l
  if(X%ncol/=size(y)) then
    print *,'wrong sizes'
    stop
  endif

  s=0d0
  do l=1,X%last
    s(X%i(l))=s(X%i(l))+X%a(l)*y(X%j(l))
  enddo
end function 

function Xp_times_y(X,y) result(s)
  implicit none
  ! computes X' by vector y
  type(incm):: X
  real(r8):: y(:)
  real(r8):: s(X%ncol)
  integer:: l
  if(X%nrow/=size(y)) then
    print *,'wrong sizes'
    stop
  endif

  s=0d0
  do l=1,X%last
    s(X%j(l))=s(X%j(l))+X%a(l)*y(X%i(l))
  enddo
end function 

function Xp_row_times_y(X,y,irow) result(s)
  implicit none
  ! computes row i "irow" of X' (col "irow" of X) by vector y
  ! does not assume ordering (hence possibly inefficient)
  type(incm):: X
  real(r8):: y(:)
  real(r8):: s
  integer:: l,irow
  if(X%nrow/=size(y)) then
    print *,'wrong sizes'
    stop
  endif

  s=0d0
  do l=1,X%last
    if(X%j(l)==irow) s=s+X%a(l)*y(X%i(l))
  enddo


end function 


function X_col_times_a(X,a,icol) result(s)
  ! computes a column of X times a scalar
  ! this is for update residual
  type(incm):: X
  real(r8):: a
  real(r8):: s(X%nrow)
  integer:: l,icol

  s=0d0
  do l=1,X%last
    if(X%j(l)==icol) s(X%i(l))=s(X%i(l))+X%a(l)*a
  enddo
end function 


function diag_XpX(X) result(s)
  implicit none
  ! gets the diagonal of XpX
  ! used in GSRU
  type(incm):: X
  real(r8):: s(X%ncol)
  integer:: l

  s=0d0
  do l=1,X%last
    s(X%j(l))=s(X%j(l))+X%a(l)**2
  enddo
end function

!function update_1_GSRU(X,e,i,xpx,invvar,vare) result(s)
!  implicit none
!  ! updates 1 unknown by GSRU in the equation
!  ! X'X/vare + invvar = X'y/vare
!  type(incm):: X
!  real(r8):: e(:)i,lhs,invvar,rhs
!  lhs=xpx(i)+invvar
!  rhs=Xp_row_times_






function GSRU(X,y,D,vare,init_sol) result(sol)
  implicit none
  ! solves by GSRU 
  ! the equations XX'/vare +D = X'y/vare
  type(incm):: X
  real(r8):: y(:),D(:),vare,conv,sol(X%ncol),e(size(y)), &
             xpx(x%ncol),lhs,rhs,val
  real(r8),optional:: init_sol(:) !initial solution (if provided)
  integer:: i,ndata,neq,iter

  conv=1d0
  if(present(init_sol))then
  	sol=init_sol
	e=y-X_times_y(X,sol)
  else
  	sol=0d0; e=y; 
  endif	
  neq=X%ncol; ndata=X%nrow
  xpx=diag_XpX(X)
  do iter=1,10000
    conv=0d0
    do i=1,neq
      lhs=xpx(i)/vare+D(i)
      rhs= Xp_row_times_y(X,e,i)/vare+xpx(i)/vare*sol(i)
      val=rhs/lhs
      e=e- X_col_times_a(X,val-sol(i),i)
      conv=conv+(sol(i)-val)**2
      sol(i)=val
    enddo
    print *,iter,conv

    if(conv<1d-12) then
      print *,iter,conv
      exit
    endif
  enddo

end function



  subroutine pcgru(X,y,D,vare,sol,maxiter)
  ! preconditioned conjugated gradients
  implicit none
  type(incm)::X
  real(r8)::D(:),sol(:)
   ! solve X'X/vare + D  sol = X'y/vare by preconditioned conjugate gradient for densem A
   ! with D=diagonal of var-covar matrix of sol
   real (r8)::y(:)

   real (r8),allocatable::m(:),r(:),p(:),z(:),w(:),rhs(:)
   real (r8)::alpha,beta,tau,oldtau,conv,val,vare
   integer::i,j,k,n,ndata,top
   logical:: verbose
   integer,optional:: maxiter

   top=100000
   if(present(maxiter)) top=maxiter

   n=size(sol);ndata=size(y)
   allocate(m(size(sol)))

   allocate (r(n),p(n),z(n),w(n),rhs(n))
   ! find the preconditioner
   !diagonal preconditioner
      m=diag_XpX(X)+D
      do i=1,n
         if (m(i) /= 0) then
           m(i)=1d0/m(i)
         else
           m(i)=0
        endif
      enddo

   ! build rhs
   rhs=Xp_times_y(X,y)/vare
   sol=0d0
   r=rhs

   do k=1,top
      z=m*r
      tau=dot_product(z,r)
      if (k == 1) then
         beta=0; p=z
       else
         beta=tau/oldtau
         p=z+beta*p
      end if
      !w=matmul(a,p)
      w= Xp_times_y( X,X_times_y(X,p) )/vare +D*p      !    matmul(transpose(X),matmul(X,p))/vare+D*p ! if id vare
      alpha=tau/dot_product(p,w)
      sol=sol+alpha*p
      if (mod(k,100) /= 0) then
           r=r-alpha*w
         else
           r=rhs - Xp_times_y( X,X_times_y(X,sol) )/vare -D*sol !matmul(transpose(X),matmul(X,sol))/vare - D*sol
      endif
      !conv=(alpha**2)*dot_product(p,p)/dot_product(sol,sol)
      conv=dot_product(r,r)/dot_product(rhs,rhs)
      if((mod(k,1)==0))  print*,'round ',k,'   convergence=',conv
      if (conv < 1e-14) exit
      oldtau=tau
   enddo
   deallocate(m)
   deallocate (r,p,z,w)
   print*,k,' iterations,   convergence criterion=',conv

  end subroutine

end module incidence_matrix

