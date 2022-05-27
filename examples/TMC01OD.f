*     MC01OD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX
      PARAMETER        ( KMAX = 10 )
*     .. Local Scalars ..
      INTEGER          I, INFO, K
*     .. Local Arrays ..
      DOUBLE PRECISION DWORK(2*KMAX+2), IMP(KMAX+1), IMZ(KMAX),
     $                 REP(KMAX+1), REZ(KMAX)
*     .. External Subroutines ..
      EXTERNAL         MC01OD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) K
      IF ( K.LT.0 .OR. K.GT.KMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) K
      ELSE
         READ ( NIN, FMT = * ) ( REZ(I), IMZ(I), I = 1,K )
*        Compute the coefficients of P(x) from the given zeros.
         CALL MC01OD( K, REZ, IMZ, REP, IMP, DWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            WRITE ( NOUT, FMT = 99996 )
     $            ( I, REP(I+1), IMP(I+1), I = 0,K )
         END IF
      END IF
      STOP
*
99999 FORMAT (' MC01OD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01OD = ',I2)
99997 FORMAT (' The coefficients of the polynomial P(x) are ',//' powe',
     $       'r of x     real part     imag part ')
99996 FORMAT (2X,I5,8X,F9.4,5X,F9.4)
99995 FORMAT (/' K is out of range.',/' K = ',I5)
      END
