*     TD05AD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NP1MAX, MP1MAX
      PARAMETER        ( NP1MAX = 20, MP1MAX = 20 )
*     .. Local Scalars ..
      DOUBLE PRECISION VALI, VALR, W
      INTEGER          I, INFO, MP1, NP1
      CHARACTER*1      UNITF, OUTPUT
*     .. Local Arrays ..
      DOUBLE PRECISION A(NP1MAX), B(MP1MAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TD05AD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) NP1, MP1, W, UNITF, OUTPUT
      IF ( NP1.LE.0 .OR. NP1.GT.NP1MAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) NP1
      ELSE
         READ ( NIN, FMT = * ) ( A(I), I = 1,NP1 )
         IF ( MP1.LE.0 .OR. MP1.GT.MP1MAX ) THEN
            WRITE ( NOUT, FMT = 99994 ) MP1
         ELSE
            READ ( NIN, FMT = * ) ( B(I), I = 1,MP1 )
*           Find the real and imaginary parts of G(jW), where
*           W = 1.0 radian.
            CALL TD05AD( UNITF, OUTPUT, NP1, MP1, W, A, B, VALR, VALI,
     $                   INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               IF ( LSAME( OUTPUT, 'C' ) ) THEN
                  WRITE ( NOUT, FMT = 99997 ) VALR, VALI
               ELSE
                  WRITE ( NOUT, FMT = 99996 ) VALR, VALI
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TD05AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TD05AD = ',I2)
99997 FORMAT (' Complex value of G(jW) = ',F8.4,1X,F8.4,'*j')
99996 FORMAT (' Magnitude of G(jW) = ',F8.4,' dBs, Phase of G(jW) = ',
     $       F8.4,' degrees ')
99995 FORMAT (/' NP1 is out of range.',/' NP1 = ',I5)
99994 FORMAT (/' MP1 is out of range.',/' MP1 = ',I5)
      END
