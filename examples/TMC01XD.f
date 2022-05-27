*     MC01XD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NEV
      PARAMETER        ( NEV = 3 )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 42 )
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
*     .. Local Scalars ..
      INTEGER          I, INFO, J
      DOUBLE PRECISION ALPHA, BETA, DELTA, GAMMA
*     .. Local Arrays ..
      DOUBLE PRECISION DWORK(LDWORK), EVI(NEV), EVQ(NEV), EVR(NEV),
     $                 RT(2)
*     .. External Subroutines ..
      EXTERNAL         MC01XD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) ALPHA, BETA, GAMMA, DELTA
*     Compute the roots of the polynomial.
      CALL MC01XD( ALPHA, BETA, GAMMA, DELTA, EVR, EVI, EVQ,
     $             DWORK, LDWORK, INFO )
*
      IF ( INFO.NE.0 ) THEN
         WRITE ( NOUT, FMT = 99998 ) INFO
      ELSE
         WRITE ( NOUT, FMT = 99995 )
         WRITE ( NOUT, FMT = 99996 ) ( EVR(J), J = 1, 3 )
*
         WRITE ( NOUT, FMT = 99994 )
         WRITE ( NOUT, FMT = 99996 ) ( EVI(J), J = 1, 3 )
*
         WRITE ( NOUT, FMT = 99993 )
         WRITE ( NOUT, FMT = 99996 ) ( EVQ(J), J = 1, 3 )
*
         WRITE ( NOUT, FMT = 99992 )
         DO 20 I = 1, 3
            IF ( EVQ(I).NE.ZERO ) THEN
               RT(1) = EVR(I)/EVQ(I)
               RT(2) = EVI(I)/EVQ(I)
               WRITE ( NOUT, FMT = 99996 ) ( RT(J), J = 1, 2 )
            END IF
   20    CONTINUE
*
      END IF
      STOP
*
99999 FORMAT (' MC01XD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01XD = ',I2)
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (' Real part of the numerators of the roots')
99994 FORMAT (' Imaginary part of the numerators of the roots')
99993 FORMAT (' Denominators of the roots')
99992 FORMAT (' Roots of the polynomial',/1X)
      END

