*     MC01TD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          DPMAX
      PARAMETER        ( DPMAX = 10 )
*     .. Local Scalars ..
      INTEGER          DP, DPP, I, INFO, IWARN, NZ
      LOGICAL          STABLE
      CHARACTER*1      DICO
*     .. Local Arrays ..
      DOUBLE PRECISION DWORK(2*DPMAX+2), P(DPMAX+1)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MC01TD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = * )
      READ ( NIN, FMT = * ) DP, DICO
      IF ( DP.LE.-1 .OR. DP.GT.DPMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) DP
      ELSE
         DPP = DP
         READ ( NIN, FMT = * ) ( P(I), I = 1,DP+1 )
*        Determine whether or not the given polynomial P(x) is stable.
         CALL MC01TD( DICO, DP, P, STABLE, NZ, DWORK, IWARN, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( IWARN.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) IWARN
               WRITE ( NOUT, FMT = 99996 ) DPP, DP
            END IF
            IF ( STABLE ) THEN
               WRITE ( NOUT, FMT = 99995 )
            ELSE
               WRITE ( NOUT, FMT = 99994 )
               IF ( LSAME( DICO, 'D' ) ) THEN
                  WRITE ( NOUT, FMT = 99992 ) NZ
               ELSE
                  WRITE ( NOUT, FMT = 99991 ) NZ
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MC01TD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01TD = ',I2)
99997 FORMAT (' IWARN on exit from MC01TD = ',I2,/)
99996 FORMAT (' The degree of the polynomial P(x) has been reduced fro',
     $       'm ',I2,' to ',I2,/)
99995 FORMAT (' The polynomial P(x) is stable ')
99994 FORMAT (' The polynomial P(x) is unstable ')
99993 FORMAT (/' DP is out of range. ',/' DP = ',I5)
99992 FORMAT (/' The number of zeros of P(x) outside the unit ',
     $       'circle = ',I2)
99991 FORMAT (/' The number of zeros of P(x) in the right ',
     $       'half-plane = ',I2)
      END
