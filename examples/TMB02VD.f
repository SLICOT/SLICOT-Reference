*     MB02VD EXAMPLE PROGRAM TEXT
*
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX
      PARAMETER        ( MMAX = 20, NMAX = 20 )
      INTEGER          LDA, LDB
      PARAMETER        ( LDA = NMAX, LDB = MMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, M, N
      CHARACTER*1      TRANS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,NMAX)
      INTEGER          IPIV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         MB02VD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, TRANS
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99994 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,M )
*           Solve the linear system using the LU factorization.
            CALL MB02VD( TRANS, M, N, A, LDA, IPIV, B, LDB, INFO )
*
            IF ( INFO.EQ.0 ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 10 I = 1, M
                  WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,N )
   10          CONTINUE
            ELSE
               WRITE ( NOUT, FMT = 99998 ) INFO
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02VD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02VD = ',I2)
99997 FORMAT (' The solution matrix is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
99994 FORMAT (/' M is out of range.',/' M = ',I5)
      END
