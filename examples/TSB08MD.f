*     SB08MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          DAMAX
      PARAMETER        ( DAMAX = 10 )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 5*DAMAX+5 )
*     .. Local Scalars ..
      DOUBLE PRECISION RES
      INTEGER          DA, I, INFO
      CHARACTER*1      ACONA
*     .. Local Arrays ..
      DOUBLE PRECISION A(DAMAX+1), DWORK(LDWORK), E(DAMAX+1)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SB08MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
      READ ( NIN, FMT = '()' )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = * ) DA, ACONA
      IF ( DA.LE.-1 .OR. DA.GT.DAMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) DA
      ELSE
         READ ( NIN, FMT = * ) ( A(I), I = 1,DA+1 )
*        Compute the spectral factorization of the given polynomial.
         CALL SB08MD( ACONA, DA, A, RES, E, DWORK, LDWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( LSAME( ACONA, 'A' ) ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 20 I = 0, DA
                  WRITE ( NOUT, FMT = 99995 ) 2*I, A(I+1)
   20          CONTINUE
               WRITE ( NOUT, FMT = * )
            END IF
            WRITE ( NOUT, FMT = 99996 )
            DO 40 I = 0, DA
               WRITE ( NOUT, FMT = 99995 ) I, E(I+1)
   40       CONTINUE
            WRITE ( NOUT, FMT = 99994 ) RES
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' SB08MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB08MD = ',I2)
99997 FORMAT (' The coefficients of the polynomial B(s) are ',//' powe',
     $       'r of s     coefficient ')
99996 FORMAT (' The coefficients of the spectral factor E(s) are ',
     $       //' power of s     coefficient ')
99995 FORMAT (2X,I5,9X,F9.4)
99994 FORMAT (/' RES = ',1P,E8.1)
99993 FORMAT (/' DA is out of range.',/' DA = ',I5)
      END
