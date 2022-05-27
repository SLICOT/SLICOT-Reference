*     MB01TD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDB
      PARAMETER        ( LDA = NMAX, LDB = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX-1 )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,NMAX), DWORK(LDWORK)
*     .. External Subroutines ..
      EXTERNAL         MB01TD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,N )
*        Compute the matrix product A*B.
         CALL MB01TD( N, A, LDA, B, LDB, DWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,N )
   20       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB01TD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB01TD = ',I2)
99997 FORMAT (' The matrix product A*B is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
      END
