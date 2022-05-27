*     MC01VD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, C, Z1IM, Z1RE, Z2IM, Z2RE
      INTEGER          INFO
*     .. External Subroutines ..
      EXTERNAL         MC01VD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) A, B, C
*     Solve the quadratic equation A*x**2 + B*x + C = 0.
      CALL MC01VD( A, B, C, Z1RE, Z1IM, Z2RE, Z2IM, INFO )
*
      IF ( INFO.NE.0 ) THEN
         WRITE ( NOUT, FMT = 99998 ) INFO
      ELSE
         WRITE ( NOUT, FMT = 99997 )
         WRITE ( NOUT, FMT = 99996 ) Z1RE, Z1IM, Z2RE, Z2IM
      END IF
*
      STOP
*
99999 FORMAT (' MC01VD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01VD = ',I2)
99997 FORMAT (' The roots of the quadratic equation are ')
99996 FORMAT (/' x = ',F8.4,2X,SP,F8.4,'*j',SS,/' x = ',F8.4,2X,SP,F8.4,
     $       '*j')
      END
