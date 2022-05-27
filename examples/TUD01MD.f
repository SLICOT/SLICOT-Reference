*     UD01MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX
      PARAMETER        ( MMAX = 20, NMAX = 20 )
      INTEGER          LDA
      PARAMETER        ( LDA = MMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, L, M, N
      CHARACTER*72     TEXT
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX)
*     .. External Subroutines ..
      EXTERNAL         UD01MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, L, TEXT
      IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99996 ) M
      ELSE IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99997 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,M )
*        Print out the matrix A.
         CALL UD01MD( M, N, L, NOUT, A, LDA, TEXT, INFO )
         IF ( INFO.NE.0 ) WRITE ( NOUT, FMT = 99998 ) INFO
      END IF
      STOP
*
99999 FORMAT (' UD01MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from UD01MD = ',I2)
99997 FORMAT (/' N is out of range.',/' N = ',I5)
99996 FORMAT (/' M is out of range.',/' M = ',I5)
      END
