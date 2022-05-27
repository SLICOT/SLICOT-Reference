*     DE01OD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 128 )
*     .. Local Scalars ..
      INTEGER          I, INFO, N
      CHARACTER*1      CONV
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX), B(NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         DE01OD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, CONV
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( A(I), B(I), I = 1,N )
*        Perform convolution on A and B.
         CALL DE01OD( CONV, N, A, B, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( LSAME( CONV, 'C' ) ) THEN
               WRITE ( NOUT, FMT = 99997 )
            ELSE
               WRITE ( NOUT, FMT = 99996 )
            END IF
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99995 ) I, A(I)
   20       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' DE01OD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from DE01OD = ',I2)
99997 FORMAT ('   Convolution ',//'   i    A(i)',/)
99996 FORMAT ('   Deconvolution ',//'   i    A(i)',/)
99995 FORMAT (I4,1X,F8.4)
99994 FORMAT (/' N is out of range.',/' N = ',I5)
      END
