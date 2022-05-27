*     DF01MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 129 )
*     .. Local Scalars ..
      DOUBLE PRECISION DT
      INTEGER          I, INFO, N
      CHARACTER*1      SICO
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX), DWORK(NMAX+1)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         DF01MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, DT, SICO
      IF ( N.LE.1 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( A(I), I = 1,N )
*        Compute the sine/cosine transform of the given real signal.
         CALL DF01MD( SICO, N, DT, A, DWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( LSAME( SICO, 'S' ) ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) I, A(I)
   20          CONTINUE
            ELSE
               WRITE ( NOUT, FMT = 99996 )
               DO 40 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) I, A(I)
   40          CONTINUE
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' DF01MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from DF01MD = ',I2)
99997 FORMAT (' Components of sine transform are',//'   i',6X,'A(i)',/)
99996 FORMAT (' Components of cosine transform are',//'   i',6X,'A(i)',
     $       /)
99995 FORMAT (I4,3X,F8.4)
99994 FORMAT (/' N is out of range.',/' N = ',I5)
      END
