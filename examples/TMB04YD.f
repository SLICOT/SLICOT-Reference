*     MB04YD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX
      PARAMETER        ( MMAX = 20, NMAX = 20 )
      INTEGER          MNMIN
      PARAMETER        ( MNMIN = MIN( MMAX, NMAX ) )
      INTEGER          LDU, LDV
      PARAMETER        ( LDU = MMAX, LDV = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 6*MNMIN - 5 )
*     .. Local Scalars ..
      DOUBLE PRECISION RELTOL, THETA, TOL
      INTEGER          I, INFO, IWARN, J, M, MINMN, N, RANK, RANK1
      CHARACTER*1      JOBU, JOBV
      LOGICAL          LJOBUU, LJOBVU
*     .. Local Arrays ..
      DOUBLE PRECISION DWORK(LDWORK), E(MNMIN-1), Q(MNMIN),
     $                 U(LDU,MNMIN), V(LDV,MNMIN)
      LOGICAL          INUL(MNMIN)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB04YD
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, THETA, RANK, TOL, RELTOL, JOBU, JOBV
      MINMN = MIN( M, N )
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99988 ) M
      ELSE IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99987 ) N
      ELSE IF ( RANK.GT.MINMN ) THEN
         WRITE ( NOUT, FMT = 99986 ) RANK
      ELSE IF ( RANK.LT.0 .AND. THETA.LT.ZERO ) THEN
         WRITE ( NOUT, FMT = 99985 ) THETA
      ELSE
         READ ( NIN, FMT = * ) ( Q(I), I = 1,MINMN )
         READ ( NIN, FMT = * ) ( E(I), I = 1,MINMN-1 )
         RANK1 = RANK
         LJOBUU = LSAME( JOBU, 'U' )
         LJOBVU = LSAME( JOBV, 'U' )
         IF ( LJOBUU ) READ ( NIN, FMT = * )
     $                      ( ( U(I,J), J = 1,MINMN ), I = 1,M )
         IF ( LJOBVU ) READ ( NIN, FMT = * )
     $                      ( ( V(I,J), J = 1,MINMN ), I = 1,N )
*        Initialise the array INUL.
         DO 20 I = 1, MINMN
            INUL(I) = .FALSE.
   20    CONTINUE
         IF ( LJOBUU.OR.LJOBVU ) READ ( NIN, FMT = * )
     $                                ( INUL(I), I = 1,MINMN )
*        Compute the number of singular values of J > THETA.
         CALL MB04YD( JOBU, JOBV, M, N, RANK, THETA, Q, E, U, LDU, V,
     $                LDV, INUL, TOL, RELTOL, DWORK, LDWORK, IWARN,
     $                INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( IWARN.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99993 ) IWARN
               WRITE ( NOUT, FMT = 99984 ) RANK
            END IF
            WRITE ( NOUT, FMT = 99997 )
            DO 160 I = 1, MINMN - 1
               WRITE ( NOUT, FMT = 99996 ) I, I, Q(I), I, (I+1), E(I)
  160       CONTINUE
            WRITE ( NOUT, FMT = 99995 ) MINMN, MINMN, Q(MINMN)
            IF ( RANK1.LT.0 ) WRITE ( NOUT, FMT = 99994 ) RANK, THETA
            IF ( .NOT.LSAME( JOBV, 'N' ) ) THEN
               WRITE ( NOUT, FMT = 99992 )
               DO 180 I = 1, N
                  WRITE ( NOUT, FMT = 99991 ) ( V(I,J), J = 1,MINMN )
  180          CONTINUE
            END IF
            IF ( ( .NOT.LSAME( JOBU, 'N' ) ) .AND.
     $           ( .NOT.LSAME( JOBV, 'N' ) ) )
     $           WRITE ( NOUT, FMT = 99990 )
            IF ( .NOT.LSAME( JOBU, 'N' ) ) THEN
               WRITE ( NOUT, FMT = 99989 )
               DO 200 I = 1, M
                  WRITE ( NOUT, FMT = 99991 ) ( U(I,J), J = 1,MINMN )
  200          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB04YD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04YD = ',I2)
99997 FORMAT (' The transformed bidiagonal matrix J is',/)
99996 FORMAT (2(' (',I1,',',I1,') = ',F7.4,2X))
99995 FORMAT (' (',I1,',',I1,') = ',F7.4)
99994 FORMAT (/' J has ',I2,' singular values >',F7.4,/)
99993 FORMAT (' IWARN on exit from MB04YD = ',I2,/)
99992 FORMAT (' The product of the right-hand Givens rotation matrices',
     $       ' equals ')
99991 FORMAT (20(1X,F8.4))
99990 FORMAT (' ')
99989 FORMAT (' The product of the left-hand Givens rotation matrices ',
     $       'equals ')
99988 FORMAT (/' M is out of range.',/' M = ',I5)
99987 FORMAT (/' N is out of range.',/' N = ',I5)
99986 FORMAT (/' RANK is out of range.',/' RANK = ',I5)
99985 FORMAT (/' THETA must be at least zero.',/' THETA = ',F8.4)
99984 FORMAT (/' The computed rank of matrix J = ',I3,/)
      END
