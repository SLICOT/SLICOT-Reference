*     MB02ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX, LMAX
      PARAMETER        ( MMAX = 20, NMAX = 20, LMAX = 20 )
      INTEGER          LDC, LDX
      PARAMETER        ( LDC = MAX( MMAX, NMAX+LMAX ), LDX = NMAX )
      INTEGER          LENGQ
      PARAMETER        ( LENGQ = 2*MIN(MMAX,NMAX+LMAX)-1 )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX+2*LMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX(2, MAX( MMAX, NMAX+LMAX ) +
     $                            2*MIN( MMAX, NMAX+LMAX ),
     $                            MIN( MMAX, NMAX+LMAX ) +
     $                            MAX( ( NMAX+LMAX )*( NMAX+LMAX-1 )/2,
     $                              MMAX*( NMAX+LMAX-( MMAX-1 )/2 ) ) +
     $                            MAX( 6*(NMAX+LMAX)-5, LMAX*LMAX +
     $                                 MAX( NMAX+LMAX, 3*LMAX ) ) ) )
      INTEGER          LBWORK
      PARAMETER        ( LBWORK = NMAX+LMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION RELTOL, THETA, THETA1, TOL
      INTEGER          I, INFO, IWARN, J, K, L, LOOP, M, MINMNL, N,
     $                 RANK, RANK1
*     .. Local Arrays ..
      DOUBLE PRECISION C(LDC,NMAX+LMAX), DWORK(LDWORK),
     $                 Q(LENGQ), X(LDX,LMAX)
      INTEGER          IWORK(LIWORK)
      LOGICAL          BWORK(LBWORK), INUL(NMAX+LMAX)
*     .. External Subroutines ..
      EXTERNAL         MB02ND
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, L, RANK, THETA, TOL, RELTOL
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99982 ) M
      ELSE IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99983 ) N
      ELSE IF ( L.LT.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99981 ) L
      ELSE IF ( RANK.GT.MIN( MMAX, NMAX ) ) THEN
         WRITE ( NOUT, FMT = 99980 ) RANK
      ELSE IF ( RANK.LT.0 .AND. THETA.LT.ZERO ) THEN
         WRITE ( NOUT, FMT = 99979 ) THETA
      ELSE
         READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N+L ), I = 1,M )
         RANK1 = RANK
         THETA1 = THETA
*        Compute the solution to the TLS problem Ax = b.
         CALL MB02ND( M, N, L, RANK, THETA, C, LDC, X, LDX, Q, INUL,
     $                TOL, RELTOL, IWORK, DWORK, LDWORK, BWORK, IWARN,
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
            WRITE ( NOUT, FMT = 99994 )
            MINMNL = MIN( M, N+L )
            LOOP = MINMNL - 1
            DO 20 I = 1, LOOP
               K = I + MINMNL
               WRITE ( NOUT, FMT = 99993 ) I, I, Q(I), I, I + 1, Q(K)
   20       CONTINUE
            WRITE ( NOUT, FMT = 99992 ) MINMNL, MINMNL, Q(MINMNL)
            WRITE ( NOUT, FMT = 99991 )
            DO 60 J = 1, L
               DO 40 I = 1, N
                  WRITE ( NOUT, FMT = 99990 ) X(I,J)
   40          CONTINUE
               IF ( J.LT.L ) WRITE ( NOUT, FMT = 99989 )
   60       CONTINUE
            WRITE ( NOUT, FMT = 99987 ) N + L, N + L
            WRITE ( NOUT, FMT = 99985 )
            DO 80 I = 1, MAX( M, N + L )
               WRITE ( NOUT, FMT = 99984 ) ( C(I,J), J = 1,N+L )
   80       CONTINUE
            WRITE ( NOUT, FMT = 99986 )
            DO 100 J = 1, N + L
               WRITE ( NOUT, FMT = 99988 ) J, INUL(J)
  100       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02ND = ',I2)
99997 FORMAT (' IWARN on exit from MB02ND = ',I2,/)
99996 FORMAT (' The computed rank of the TLS approximation  = ',I3,/)
99995 FORMAT (' The computed value of THETA = ',F7.4,/)
99994 FORMAT (' The elements of the partially diagonalized bidiagonal ',
     $       'matrix are',/)
99993 FORMAT (2(' (',I1,',',I1,') = ',F7.4,2X))
99992 FORMAT (' (',I1,',',I1,') = ',F7.4,/)
99991 FORMAT (' The solution X to the TLS problem is ',/)
99990 FORMAT (1X,F8.4)
99989 FORMAT (' ')
99988 FORMAT (I3,L8)
99987 FORMAT (/' Right singular subspace corresponds to the first ',I2,
     $       ' components of the j-th ',/' column of C for which INUL(',
     $       'j) = .TRUE., j = 1,...,',I2,/)
99986 FORMAT (/'  j    INUL(j)',/)
99985 FORMAT (' Matrix C',/)
99984 FORMAT (20(1X,F8.4))
99983 FORMAT (/' N is out of range.',/' N = ',I5)
99982 FORMAT (/' M is out of range.',/' M = ',I5)
99981 FORMAT (/' L is out of range.',/' L = ',I5)
99980 FORMAT (/' RANK is out of range.',/' RANK = ',I5)
99979 FORMAT (/' THETA must be at least zero.',/' THETA = ',F8.4)
      END
