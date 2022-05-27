*     AB13ED EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA
      PARAMETER        ( LDA = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 3*NMAX*( NMAX + 1 ) )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, N
      DOUBLE PRECISION HIGH, LOW, TOL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK)
*     .. External Subroutines ..
      EXTERNAL         AB13ED, UD01MD
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
         CALL UD01MD( N, N, 5, NOUT, A, LDA, 'Matrix A', INFO )
*
         CALL AB13ED( N, A, LDA, LOW, HIGH, TOL, DWORK, LDWORK, INFO )
         IF ( INFO.EQ.0 ) THEN
            WRITE ( NOUT, FMT = 99997 ) LOW, HIGH
         ELSE
            WRITE ( NOUT, FMT = 99996 ) INFO
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB13ED EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' N =', I4, 2X, 'TOL =', D10.3)
99997 FORMAT (' LOW  =', D18.11, /' HIGH =', D18.11)
99996 FORMAT (' INFO on exit from AB13ED = ', I2)
99995 FORMAT (/' N is out of range.',/' N = ',I5)
      END
