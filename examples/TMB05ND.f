*     MB05ND EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDEX, LDEXIN, LDWORK
      PARAMETER        ( LDA = NMAX, LDEX = NMAX, LDEXIN = NMAX,
     $                   LDWORK = NMAX*( NMAX+1 ) )
*     .. Local Scalars ..
      DOUBLE PRECISION DELTA, TOL
      INTEGER          I, INFO, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), EX(LDEX,NMAX),
     $                 EXINT(LDEXIN,NMAX)
      INTEGER          IWORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         MB05ND
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, DELTA, TOL
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
*        Find the matrix exponential of A*DELTA and its integral.
         CALL MB05ND( N, DELTA, A, LDA, EX, LDEX, EXINT, LDEXIN, TOL,
     $                IWORK, DWORK, LDWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( EX(I,J), J = 1,N )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99995 )
            DO 40 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( EXINT(I,J), J = 1,N )
   40       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB05ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB05ND = ',I2)
99997 FORMAT (' The solution matrix exp(A*DELTA) is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' and its integral is ')
99994 FORMAT (/' N is out of range.',/' N = ',I5)
      END
