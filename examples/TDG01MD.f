*     DG01MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 128 )
*     .. Local Scalars ..
      INTEGER          I, INFO, N
      CHARACTER*1      INDI
*     .. Local Arrays ..
      DOUBLE PRECISION XI(NMAX), XR(NMAX)
*     .. External Subroutines ..
      EXTERNAL         DG01MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, INDI
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( XR(I), XI(I), I = 1,N )
*        Find the Fourier transform of the given complex signal.
         CALL DG01MD( INDI, N, XR, XI, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) I, XR(I), XI(I)
   20       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' DG01MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from DG01MD = ',I2)
99997 FORMAT (' Components of Fourier transform are',//'   i',6X,
     $       'XR(i)',6X,'XI(i)',/)
99996 FORMAT (I4,3X,F8.4,3X,F8.4)
99995 FORMAT (/' N is out of range.',/' N = ',I5)
      END
