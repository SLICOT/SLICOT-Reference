*     AB07ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDD
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = MMAX,
     $                   LDD = MMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 4*MMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, M, N
      DOUBLE PRECISION RCOND
*     .. Local Arrays ..
      INTEGER          IWORK(2*MMAX)
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK)
*     .. External Subroutines ..
      EXTERNAL         AB07ND
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,M )
            READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,M )
*           Find the inverse of the ssr (A,B,C,D).
            CALL AB07ND( N, M, A, LDA, B, LDB, C, LDC, D, LDD, RCOND,
     $                   IWORK, DWORK, LDWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 )
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,N )
   20          CONTINUE
               WRITE ( NOUT, FMT = 99995 )
               DO 40 I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,M )
   40          CONTINUE
               WRITE ( NOUT, FMT = 99994 )
               DO 60 I = 1, M
                  WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,N )
   60          CONTINUE
               WRITE ( NOUT, FMT = 99993 )
               DO 80 I = 1, M
                  WRITE ( NOUT, FMT = 99996 ) ( D(I,J), J = 1,M )
   80          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB07ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB07ND = ',I2)
99997 FORMAT (' The state dynamics matrix of the inverse system is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The input/state matrix of the inverse system is ')
99994 FORMAT (/' The state/output matrix of the inverse system is ')
99993 FORMAT (/' The feedthrough matrix of the inverse system is ')
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (/' M is out of range.',/' M = ',I5)
      END
