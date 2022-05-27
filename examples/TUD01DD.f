*     UD01DD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX
      PARAMETER        ( MMAX = 10, NMAX = 10 )
      INTEGER          LDA
      PARAMETER        ( LDA = NMAX )
*     .. Local Scalars ..
      INTEGER          INFO, INFO1, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX)
*     .. External Subroutines ..
      EXTERNAL         UD01DD, UD01MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) M
      ELSE IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
*        Read the coefficients of the matrix polynomial P(s).
         CALL UD01DD( M, N, NIN, A, LDA, INFO )
         IF ( INFO.GE.0 ) THEN
*           Write the matrix A.
            CALL UD01MD( M, N, 5, NOUT, A, LDA, ' Matrix A', INFO1 )
            IF ( INFO1.NE.0 )
     $         WRITE ( NOUT, FMT = 99998 ) INFO1
         END IF
         IF ( INFO.NE.0 )
     $      WRITE ( NOUT, FMT = 99997 ) INFO
      END IF
      STOP
*
99999 FORMAT (' UD01DD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' INFO on exit from UD01MD = ',I2)
99997 FORMAT (' INFO on exit from UD01DD = ',I2)
99995 FORMAT (/' N is out of range.',/' N = ',I5)
99994 FORMAT (/' M is out of range.',/' M = ',I5)
      END
