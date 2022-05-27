*     MC01SD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          DPMAX
      PARAMETER        ( DPMAX = 10 )
*     .. Local Scalars ..
      INTEGER          BETA, DP, I, INFO, S, T
*     .. Local Arrays ..
      DOUBLE PRECISION MANT(DPMAX+1), P(DPMAX+1)
      INTEGER          E(DPMAX+1), IWORK(DPMAX+1)
C     .. External Functions ..
      DOUBLE PRECISION DLAMCH
      EXTERNAL         DLAMCH
*     .. External Subroutines ..
      EXTERNAL         MC01SD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) DP
      IF ( DP.LE.-1 .OR. DP.GT.DPMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) DP
      ELSE
         READ ( NIN, FMT = * ) ( P(I), I = 1,DP+1 )
*        Compute the coefficients of the scaled polynomial Q(x).
         CALL MC01SD( DP, P, S, T, MANT, E, IWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            BETA = DLAMCH( 'Base' )
            WRITE ( NOUT, FMT = 99995 ) BETA, S, T
            WRITE ( NOUT,FMT = 99997 )
            DO 20 I = 0, DP
               WRITE ( NOUT, FMT = 99996 ) I, P(I+1)
   20       CONTINUE
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MC01SD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01SD = ',I2)
99997 FORMAT (/' The coefficients of the scaled polynomial Q(x) = s*P(',
     $       'tx) are ',//' power of x     coefficient ')
99996 FORMAT (2X,I5,9X,F9.4)
99995 FORMAT (' The base of the machine (BETA) = ',I2,//' The scaling ',
     $       'factors are s = BETA**(',I3,') and t = BETA**(',I3,')')
99994 FORMAT (/' DP is out of range.',/' DP =',I5)
      END
