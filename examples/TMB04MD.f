*     MB04MD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA
      PARAMETER        ( LDA = NMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, N
      DOUBLE PRECISION MAXRED
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), SCALE(NMAX)
*     .. External Subroutines ..
      EXTERNAL         MB04MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, MAXRED
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
*        Balance matrix A.
         CALL MB04MD( N, MAXRED, A, LDA, SCALE, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,N )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99994 ) ( SCALE(I), I = 1,N )
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB04MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04MD = ',I2)
99997 FORMAT (' The balanced matrix is ')
99996 FORMAT (20(1X,F10.4))
99994 FORMAT (/' SCALE is ',/20(1X,F10.4))
99993 FORMAT (/' N is out of range.',/' N = ',I5)
      END
