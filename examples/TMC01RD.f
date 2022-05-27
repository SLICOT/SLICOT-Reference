*     MC01RD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          DP1MAX, DP2MAX, DP3MAX
      PARAMETER        ( DP1MAX = 10, DP2MAX = 10, DP3MAX = 10 )
      INTEGER          LENP3
      PARAMETER        ( LENP3 = MAX(DP1MAX+DP2MAX,DP3MAX)+1 )
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA
      INTEGER          DP1, DP2, DP3, I, INFO
*     .. Local Arrays ..
      DOUBLE PRECISION P1(DP1MAX+1), P2(DP2MAX+1), P3(LENP3)
*    $                 P3(DP1MAX+DP2MAX+DP3MAX+1)
*     .. External Subroutines ..
      EXTERNAL         MC01RD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) DP1
      IF ( DP1.LE.-2 .OR. DP1.GT.DP1MAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) DP1
      ELSE
         READ ( NIN, FMT = * ) ( P1(I), I = 1,DP1+1 )
         READ ( NIN, FMT = * ) DP2
         IF ( DP2.LE.-2 .OR. DP2.GT.DP2MAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) DP2
         ELSE
            READ ( NIN, FMT = * ) ( P2(I), I = 1,DP2+1 )
            READ ( NIN, FMT = * ) DP3
            IF ( DP3.LE.-2 .OR. DP3.GT.DP3MAX ) THEN
               WRITE ( NOUT, FMT = 99992 ) DP3
            ELSE
               READ ( NIN, FMT = * ) ( P3(I), I = 1,DP3+1 )
            END IF
            READ ( NIN, FMT = * ) ALPHA
*           Compute the coefficients of the polynomial P(x).
            CALL MC01RD( DP1, DP2, DP3, ALPHA, P1, P2, P3, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 ) DP3
               IF ( DP3.GE.0 ) THEN
                  WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 0, DP3
                     WRITE ( NOUT, FMT = 99995 ) I, P3(I+1)
   20             CONTINUE
               END IF
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MC01RD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01RD = ',I2)
99997 FORMAT (' Degree of the resulting polynomial P(x) = ',I2)
99996 FORMAT (/' The coefficients of P(x) are ',//' power of x     coe',
     $       'fficient ')
99995 FORMAT (2X,I5,9X,F9.4)
99994 FORMAT (/' DP1 is out of range.',/' DP1 = ',I5)
99993 FORMAT (/' DP2 is out of range.',/' DP2 = ',I5)
99992 FORMAT (/' DP3 is out of range.',/' DP3 = ',I5)
      END
