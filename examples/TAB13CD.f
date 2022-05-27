*     AB13CD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 10, MMAX = 10, PMAX = 10 )
      INTEGER          LDA, LDB, LDC, LDD
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX )
      INTEGER          LCWORK
      PARAMETER        ( LCWORK = ( NMAX + MMAX )*( NMAX + PMAX ) +
     $                              3*MAX( MMAX, PMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 4*NMAX*NMAX + 2*MMAX*MMAX +
     $                            2*PMAX*PMAX + 3*NMAX*MMAX +
     $                            2*NMAX*PMAX + MMAX*PMAX + 10*NMAX +
     $                            6*MAX( MMAX, PMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION FPEAK, HNORM, TOL
      INTEGER          I, INFO, J, M, N, NP
*     .. Local Arrays ..
      LOGICAL          BWORK(2*NMAX)
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK)
      COMPLEX*16       CWORK( LCWORK )
*     .. External Functions ..
      DOUBLE PRECISION AB13CD
      EXTERNAL         AB13CD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, NP
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) M
      ELSE IF ( NP.LT.0 .OR. NP.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99988 ) NP
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,NP )
         READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,NP )
         READ ( NIN, FMT = * ) TOL
*        Computing the Hinf norm
         HNORM = AB13CD( N, M, NP, A, LDA, B, LDB, C, LDC, D, LDD, TOL,
     $                   IWORK, DWORK, LDWORK, CWORK, LCWORK, BWORK,
     $                   INFO )
*
         IF ( INFO.EQ.0 ) THEN
            WRITE ( NOUT, FMT = 99997 )
            WRITE ( NOUT, FMT = 99991 ) HNORM
            FPEAK = DWORK(2)
            WRITE ( NOUT, FMT = 99996 )
            WRITE ( NOUT, FMT = 99991 ) FPEAK
         ELSE
            WRITE( NOUT, FMT = 99998 ) INFO
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB13CD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (/' INFO on exit from AB13CD =',I2)
99997 FORMAT (/' The H_infty norm of the system is'/)
99996 FORMAT (/' The peak frequency is'/)
99992 FORMAT (10(1X,F8.4))
99991 FORMAT (D17.10)
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' NP is out of range.',/' NP = ',I5)
      END
