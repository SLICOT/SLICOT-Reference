*     SB04PD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX
      PARAMETER        ( MMAX = 20, NMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDU, LDV
      PARAMETER        ( LDA = MMAX, LDB = NMAX, LDC = MMAX,
     $                   LDU = MMAX, LDV = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 1 + 2*MMAX + MAX( 3*MMAX, 5*NMAX,
     $                                            2*( NMAX + MMAX ) ) )
*     .. Local Scalars ..
      CHARACTER        DICO, FACTA, FACTB, TRANA, TRANB
      INTEGER          I, INFO, ISGN, J, M, N
      DOUBLE PRECISION SCALE
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,MMAX), B(LDB,NMAX), C(LDC,NMAX),
     $                 DWORK(LDWORK), U(LDU,MMAX), V(LDV,NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SB04PD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, ISGN, DICO, FACTA, FACTB, TRANA, TRANB
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) M
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,M ), I = 1,M )
         IF ( LSAME( FACTA, 'F' ) )
     $      READ ( NIN, FMT = * ) ( ( U(I,J), J = 1,M ), I = 1,M )
         IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) N
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,N )
            IF ( LSAME( FACTB, 'F' ) )
     $         READ ( NIN, FMT = * ) ( ( V(I,J), J = 1,N ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,M )
*           Find the solution matrix X.
            CALL SB04PD( DICO, FACTA, FACTB, TRANA, TRANB, ISGN, M, N,
     $                   A, LDA, U, LDU, B, LDB, V, LDV, C, LDC, SCALE,
     $                   DWORK, LDWORK, INFO )
*
            IF ( INFO.NE.0 )
     $         WRITE ( NOUT, FMT = 99998 ) INFO
            IF ( INFO.EQ.0 .OR. INFO.EQ.M+N+1 ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 20 I = 1, M
                  WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,N )
   20          CONTINUE
               WRITE ( NOUT, FMT = 99995 ) SCALE
               IF ( LSAME( FACTA, 'N' ) ) THEN
                  WRITE ( NOUT, FMT = 99994 )
                  DO 40 I = 1, M
                     WRITE ( NOUT, FMT = 99996 ) ( U(I,J), J = 1,M )
   40             CONTINUE
               END IF
               IF ( LSAME( FACTB, 'N' ) ) THEN
                  WRITE ( NOUT, FMT = 99993 )
                  DO 60 I = 1, N
                     WRITE ( NOUT, FMT = 99996 ) ( V(I,J), J = 1,N )
   60             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB04PD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB04PD = ',I2)
99997 FORMAT (' The solution matrix X is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' Scaling factor = ',F8.4)
99994 FORMAT (/' The orthogonal matrix U is ')
99993 FORMAT (/' The orthogonal matrix V is ')
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' N is out of range.',/' N = ',I5)
      END
