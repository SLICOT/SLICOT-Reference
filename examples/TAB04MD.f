*     AB04MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDD
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, BETA
      INTEGER          I, INFO, J, M, N, P
      CHARACTER*1      TYPE
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK)
      INTEGER          IWORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         AB04MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TYPE, ALPHA, BETA
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), I = 1,N ), J = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
            IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99991 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), I = 1,P ), J = 1,N )
               READ ( NIN, FMT = * ) ( ( D(I,J), I = 1,P ), J = 1,M )
*              Transform the parameters (A,B,C,D).
               CALL AB04MD( TYPE, N, M, P, ALPHA, BETA, A, LDA, B, LDB,
     $                      C, LDC, D, LDD, IWORK, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 )
                  DO 20 I = 1, N
                     WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,N )
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99995 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,M )
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99994 )
                  DO 60 I = 1, P
                     WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,N )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99990 )
                  DO 80 I = 1, P
                     WRITE ( NOUT, FMT = 99996 ) ( D(I,J), J = 1,M )
   80             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB04MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB04MD = ',I2)
99997 FORMAT (' The transformed state matrix is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The transformed input matrix is ')
99994 FORMAT (/' The transformed output matrix is ')
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' P is out of range.',/' P = ',I5)
99990 FORMAT (/' The transformed input/output matrix is ')
      END
