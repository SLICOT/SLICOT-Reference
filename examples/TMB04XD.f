*     MB04XD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX
      PARAMETER        ( MMAX = 20, NMAX = 20 )
      INTEGER          LDA, LDU, LDV
      PARAMETER        ( LDA = MMAX, LDU = MMAX, LDV = NMAX )
      INTEGER          MAXMN, MNMIN
      PARAMETER        ( MAXMN = MAX( MMAX, NMAX ),
     $                   MNMIN = MIN( MMAX, NMAX ) )
      INTEGER          LENGQ
      PARAMETER        ( LENGQ = 2*MNMIN-1 )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( 2*NMAX, NMAX*( NMAX+1 )/2 )
     $                          + MAX( 2*MNMIN + MAXMN, 8*MNMIN - 5 ) )
*     .. Local Scalars ..
      DOUBLE PRECISION RELTOL, THETA, THETA1, TOL
      INTEGER          I, INFO, IWARN, J, K, LOOP, M, MINMN, N, NCOLU,
     $                 NCOLV, RANK, RANK1
      CHARACTER*1      JOBU, JOBV
      LOGICAL          LJOBUA, LJOBUS, LJOBVA, LJOBVS, WANTU, WANTV
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), Q(LENGQ),
     $                 U(LDU,MMAX), V(LDV,NMAX)
      LOGICAL          INUL(MAXMN)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB04XD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, RANK, THETA, TOL, RELTOL, JOBU, JOBV
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99983 ) M
      ELSE IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99982 ) N
      ELSE IF ( RANK.GT.MNMIN ) THEN
         WRITE ( NOUT, FMT = 99981 ) RANK
      ELSE IF ( RANK.LT.0 .AND. THETA.LT.ZERO ) THEN
         WRITE ( NOUT, FMT = 99980 ) THETA
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,M )
         RANK1 = RANK
         THETA1 = THETA
*        Compute a basis for the left and right singular subspace of A.
         CALL MB04XD( JOBU, JOBV, M, N, RANK, THETA, A, LDA, U, LDU, V,
     $                LDV, Q, INUL, TOL, RELTOL, DWORK, LDWORK, IWARN,
     $                INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( IWARN.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) IWARN
               WRITE ( NOUT, FMT = 99996 ) RANK
            ELSE
               IF ( RANK1.LT.0 ) WRITE ( NOUT, FMT = 99996 ) RANK
            END IF
            IF ( THETA1.LT.ZERO ) WRITE ( NOUT, FMT = 99995 ) THETA
            LJOBUA = LSAME( JOBU, 'A' )
            LJOBUS = LSAME( JOBU, 'S' )
            LJOBVA = LSAME( JOBV, 'A' )
            LJOBVS = LSAME( JOBV, 'S' )
            WANTU = LJOBUA.OR.LJOBUS
            WANTV = LJOBVA.OR.LJOBVS
            WRITE ( NOUT, FMT = 99994 )
            MINMN = MIN( M, N )
            LOOP = MINMN - 1
            DO 20 I = 1, LOOP
               K = I + MINMN
               WRITE ( NOUT, FMT = 99993 ) I, I, Q(I), I, I + 1, Q(K)
   20       CONTINUE
            WRITE ( NOUT, FMT = 99992 ) MINMN, MINMN, Q(MINMN)
            IF ( WANTU ) THEN
               NCOLU = M
               IF ( LJOBUS ) NCOLU = MINMN
               WRITE ( NOUT, FMT = 99986 )
               DO 40 I = 1, M
                  WRITE ( NOUT, FMT = 99985 ) ( U(I,J), J = 1,NCOLU )
   40          CONTINUE
               WRITE ( NOUT, FMT = 99991 ) NCOLU
               WRITE ( NOUT, FMT = 99990 )
               DO 60 I = 1, NCOLU
                  WRITE ( NOUT, FMT = 99989 ) I, INUL(I)
   60          CONTINUE
            END IF
            IF ( WANTV ) THEN
               NCOLV = N
               IF ( LJOBVS ) NCOLV = MINMN
               WRITE ( NOUT, FMT = 99984 )
               DO 80 I = 1, N
                  WRITE ( NOUT, FMT = 99985 ) ( V(I,J), J = 1,NCOLV )
   80          CONTINUE
               WRITE ( NOUT, FMT = 99988 ) NCOLV
               WRITE ( NOUT, FMT = 99987 )
               DO 100 J = 1, NCOLV
                  WRITE ( NOUT, FMT = 99989 ) J, INUL(J)
  100          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB04XD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04XD = ',I2)
99997 FORMAT (' IWARN on exit from MB04XD = ',I2,/)
99996 FORMAT (' The computed rank of matrix A = ',I3,/)
99995 FORMAT (' The computed value of THETA = ',F7.4,/)
99994 FORMAT (' The elements of the partially diagonalized bidiagonal ',
     $       'matrix are',/)
99993 FORMAT (2(' (',I1,',',I1,') = ',F7.4,2X))
99992 FORMAT (' (',I1,',',I1,') = ',F7.4,/)
99991 FORMAT (/' Left singular subspace corresponds to the i-th column',
     $       '(s) of U for which ',/' INUL(i) = .TRUE., i = 1,...,',I1,
     $       /)
99990 FORMAT ('  i    INUL(i)',/)
99989 FORMAT (I3,L8)
99988 FORMAT (/' Right singular subspace corresponds to the j-th colum',
     $       'n(s) of V for which ',/' INUL(j) = .TRUE., j = 1,...,',I1,
     $       /)
99987 FORMAT ('  j    INUL(j)',/)
99986 FORMAT (' Matrix U',/)
99985 FORMAT (20(1X,F8.4))
99984 FORMAT (/' Matrix V',/)
99983 FORMAT (/' M is out of range.',/' M = ',I5)
99982 FORMAT (/' N is out of range.',/' N = ',I5)
99981 FORMAT (/' RANK is out of range.',/' RANK = ',I5)
99980 FORMAT (/' THETA must be at least zero.',/' THETA = ',F8.4)
      END
