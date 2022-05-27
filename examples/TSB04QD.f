*     SB04QD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDZ
      PARAMETER        ( LDA = NMAX, LDB = MMAX, LDC = NMAX,
     $                   LDZ = MMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = 4*NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( 1, 2*NMAX*NMAX+9*NMAX, 5*MMAX,
     $                   NMAX+MMAX ) )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,MMAX),
     $                 DWORK(LDWORK), Z(LDZ,MMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         SB04QD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,M )
            READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,M ), I = 1,N )
*           Find the solution matrix X.
            CALL SB04QD( N, M, A, LDA, B, LDB, C, LDC, Z, LDZ, IWORK,
     $                   DWORK, LDWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 )
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,M )
   20          CONTINUE
               WRITE ( NOUT, FMT = 99995 )
               DO 40 I = 1, M
                  WRITE ( NOUT, FMT = 99996 ) ( Z(I,J), J = 1,M )
   40          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB04QD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB04QD = ',I2)
99997 FORMAT (' The solution matrix X is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The orthogonal matrix Z is ')
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' M is out of range.',/' M = ',I5)
      END
