*     AB13FD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA
      PARAMETER        ( LDA = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 3*NMAX*( NMAX + 2 ) )
      INTEGER          LCWORK
      PARAMETER        ( LCWORK = NMAX*( NMAX + 3 ) )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, N
      DOUBLE PRECISION BETA, OMEGA, TOL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK)
      COMPLEX*16       CWORK(LCWORK)
*     .. External Subroutines ..
      EXTERNAL         AB13FD, UD01MD
*     ..
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
*     Read N, TOL and next A (row wise).
      READ ( NIN, FMT = * ) N, TOL
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         DO 10 I = 1, N
            READ ( NIN, FMT = * ) ( A(I,J), J = 1, N )
   10    CONTINUE
*
         WRITE ( NOUT, FMT = 99998 ) N, TOL
         CALL UD01MD( N, N, 5, NOUT, A, LDA, 'A', INFO )
*
         CALL AB13FD( N, A, LDA, BETA, OMEGA, TOL, DWORK, LDWORK, CWORK,
     $                LCWORK, INFO )
*
         IF ( INFO.NE.0 )
     $      WRITE ( NOUT, FMT = 99996 ) INFO
         WRITE ( NOUT, FMT = 99997 ) BETA, OMEGA
      END IF
*
99999 FORMAT (' AB13FD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' N =', I2, 3X, 'TOL =', D10.3)
99997 FORMAT (' Stability radius :', D18.11, /
     *        ' Minimizing omega :', D18.11)
99996 FORMAT (' INFO on exit from AB13FD = ', I2)
99995 FORMAT (/' N is out of range.',/' N = ',I5)
      END
