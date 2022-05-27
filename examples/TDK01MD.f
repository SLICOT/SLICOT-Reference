*     DK01MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
*     .. Local Scalars ..
      CHARACTER*1      TYPE
      INTEGER          I, INFO, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX)
*     .. External Subroutines ..
      EXTERNAL         DK01MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, TYPE
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( A(I), I = 1,N )
*        Apply a Hamming window to the given signal.
         CALL DK01MD( TYPE, N, A, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) I, A(I)
   20       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' DK01MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from DK01MD = ',I2)
99997 FORMAT (' Components of the windowing function are',//'   k     ',
     $       ' A(k)',/)
99996 FORMAT (I4,3X,F8.4)
99995 FORMAT (/' N is out of range.',/' N = ',I5)
      END
