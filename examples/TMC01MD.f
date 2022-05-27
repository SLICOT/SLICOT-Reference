*     MC01MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          DPMAX
      PARAMETER        ( DPMAX = 20 )
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA
      INTEGER          DP, I, INFO, K
*     .. Local Arrays ..
      DOUBLE PRECISION P(DPMAX+1), Q(DPMAX+1)
*     .. External Subroutines ..
      EXTERNAL         MC01MD
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) DP, ALPHA, K
      IF ( DP.LE.-1 .OR. DP.GT.DPMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) DP
      ELSE
         READ ( NIN, FMT = * ) ( P(I), I = 1,DP+1 )
*        Compute the leading K coefficients of the shifted polynomial.
         CALL MC01MD( DP, ALPHA, K, P, Q, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 ) ALPHA
            DO 20 I = 1, K
               WRITE ( NOUT, FMT = 99996 ) I - 1, Q(I)
   20       CONTINUE
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MC01MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01MD = ',I2)
99997 FORMAT (' ALPHA = ',F8.4,//' The coefficients of the shifted pol',
     $       'ynomial are ',//' power of (x-ALPHA)     coefficient ')
99996 FORMAT (5X,I5,15X,F9.4)
99995 FORMAT (/' DP is out of range.',/' DP = ',I5)
      END
