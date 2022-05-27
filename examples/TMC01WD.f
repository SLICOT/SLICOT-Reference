*     MC01WD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          DPMAX
      PARAMETER        ( DPMAX = 10 )
*     .. Local Scalars ..
      DOUBLE PRECISION U1, U2
      INTEGER          DP, I, INFO
*     .. Local Arrays ..
      DOUBLE PRECISION P(DPMAX+1), Q(DPMAX+1)
*     .. External Subroutines ..
      EXTERNAL         MC01WD
*     .. Executable Statements ..
*
      WRITE ( NOUT,FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) DP
      IF ( DP.LE.-1 .OR. DP.GT.DPMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) DP
      ELSE
         READ ( NIN, FMT = * ) ( P(I), I = 1,DP+1 )
         READ ( NIN, FMT = * ) U1, U2
*        Compute Q(x) and R(x) from P(x) = (x**2+U2*x+U1) * Q(x) + R(x).
         CALL MC01WD( DP, P, U1, U2, Q, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 0, DP - 2
               WRITE ( NOUT, FMT = 99996 ) I, Q(I+3)
   20       CONTINUE
            WRITE ( NOUT, FMT = 99995 ) Q(1) + Q(2)*U2, Q(2)
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MC01WD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01WD = ',I2)
99997 FORMAT (' The coefficients of the quotient polynomial Q(x) are ',
     $       //' power of x     coefficient ')
99996 FORMAT (2X,I5,9X,F9.4)
99995 FORMAT (/' The coefficients of the remainder polynomial R(x) are '
     $       ,//' power of x     coefficient ',/'      0         ',F9.4,
     $       /'      1         ',F9.4)
99994 FORMAT (/' DP is out of range.',/' DP = ',I5)
      END
