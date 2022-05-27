*     MC01ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          DPMAX
      PARAMETER        ( DPMAX = 20 )
*     .. Local Scalars ..
      DOUBLE PRECISION VI, VR, XI, XR
      INTEGER          DP, I, INFO
*     .. Local Arrays ..
      DOUBLE PRECISION P(DPMAX+1)
*     .. External Subroutines ..
      EXTERNAL         MC01ND
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) DP, XR, XI
      IF ( DP.LE.-1 .OR. DP.GT.DPMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) DP
      ELSE
         READ ( NIN, FMT = * ) ( P(I), I = 1,DP+1 )
*        Evaluate the polynomial at the given (complex) point.
         CALL MC01ND( DP, XR, XI, P, VR, VI, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 ) XR, XI, VR
            WRITE ( NOUT, FMT = 99996 ) XR, XI, VI
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MC01ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01ND = ',I2)
99997 FORMAT (' Real      part of P(',F6.2,SP,F6.2,'*j ) = ',SS,F8.4)
99996 FORMAT (/' Imaginary part of P(',F6.2,SP,F6.2,'*j ) = ',SS,F8.4)
99995 FORMAT (/' DP is out of range.',/' DP = ',I5)
      END
