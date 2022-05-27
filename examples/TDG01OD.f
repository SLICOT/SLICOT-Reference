*     DG01OD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 128 )
*     .. Local Scalars ..
      INTEGER          I, INFO, N
      CHARACTER*1      SCR, WGHT
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX), W(NMAX)
*     .. External Subroutines ..
      EXTERNAL         DG01OD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, SCR, WGHT
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( A(I), I = 1,N )
*        Compute the Hartley transform.
         CALL DG01OD( SCR, WGHT, N, A, W, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 10 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) I, A(I)
   10       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' DG01OD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from DG01OD = ',I2)
99997 FORMAT ('   Hartley transform ',//'   i    A(i)',/)
99996 FORMAT (I4,1X,F8.4)
99995 FORMAT (/' N is out of range.',/' N = ',I5)
      END
