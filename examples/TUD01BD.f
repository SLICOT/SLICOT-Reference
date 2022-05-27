*     UD01BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MPMAX, NPMAX, DPMAX
      PARAMETER        ( MPMAX = 10, NPMAX = 10, DPMAX = 5 )
      INTEGER          LDP1, LDP2
      PARAMETER        ( LDP1 = MPMAX, LDP2 = NPMAX )
*     .. Local Scalars ..
      INTEGER          DP, INFO, L, MP, NP
*     .. Local Arrays ..
      DOUBLE PRECISION P(LDP1,LDP2,DPMAX)
*     .. External Subroutines ..
      EXTERNAL         UD01BD, UD01ND
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) MP, NP, DP
      IF ( MP.LE.0 .OR. MP.GT.MPMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) MP
      ELSE IF ( NP.LE.0 .OR. NP.GT.NPMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) NP
      ELSE IF ( DP.LT.0 .OR. DP.GT.DPMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) DP
      ELSE
*        Read the coefficients of the matrix polynomial P(s).
         CALL UD01BD( MP, NP, DP, NIN, P, LDP1, LDP2, INFO )
         IF ( INFO.EQ.0 ) THEN
            WRITE ( NOUT, 99996 ) MP, NP, DP
*           Write the coefficients of the matrix polynomial P(s).
            L = 5
            CALL UD01ND( MP, NP, DP, L, NOUT, P, LDP1, LDP2, ' P',
     $                   INFO )
            IF ( INFO.NE.0 )
     $         WRITE ( NOUT, FMT = 99997 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99998 ) INFO
         END IF
      END IF
      STOP
*
99999 FORMAT (' UD01BD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' INFO on exit from UD01BD = ',I2)
99997 FORMAT (' INFO on exit from UD01ND = ',I2)
99996 FORMAT (' MP =', I2, 2X, ' NP =', I2, 3X, 'DP =', I2)
99995 FORMAT (/' NP is out of range.',/' NP = ',I5)
99994 FORMAT (/' MP is out of range.',/' MP = ',I5)
99993 FORMAT (/' DP is out of range.',/' DP = ',I5)
      END
