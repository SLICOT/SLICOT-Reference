*     SB02RD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDG, LDQ, LDS, LDT, LDV, LDX
      PARAMETER        ( LDA = NMAX, LDG = NMAX, LDQ = NMAX,
     $                   LDS = 2*NMAX, LDT = NMAX, LDV = NMAX,
     $                   LDX = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MAX( 2*NMAX, NMAX*NMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 5 + 4*NMAX*NMAX + 8*NMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION FERR, RCOND, SEP
      INTEGER          I, INFO, J, N
      CHARACTER        DICO, FACT, HINV, JOB, LYAPUN, SCAL, SORT, TRANA,
     $                 UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), G(LDG,NMAX),
     $                 Q(LDQ,NMAX), S(LDS,2*NMAX), T(LDT,NMAX),
     $                 V(LDV,NMAX), WI(2*NMAX), WR(2*NMAX), X(LDX,NMAX)
      INTEGER          IWORK(LIWORK)
      LOGICAL          BWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SB02RD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, JOB, DICO, HINV, TRANA, UPLO, SCAL, SORT,
     $                      FACT, LYAPUN
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         IF ( LSAME( JOB, 'X' ) .OR. LSAME( JOB, 'A' ) .OR.
     $        LSAME( FACT, 'N' ) .OR. LSAME( LYAPUN, 'O' ) )
     $      READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( .NOT.LSAME( JOB, 'X' ) .AND. LSAME( FACT, 'F' ) ) THEN
            READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,N ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( V(I,J), J = 1,N ), I = 1,N )
         END IF
         READ ( NIN, FMT = * ) ( ( Q(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( G(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( JOB, 'C' ) .OR. LSAME( JOB, 'E' ) )
     $      READ ( NIN, FMT = * ) ( ( X(I,J), J = 1,N ), I = 1,N )
*        Find the solution matrix X.
         CALL SB02RD( JOB, DICO, HINV, TRANA, UPLO, SCAL, SORT, FACT,
     $                LYAPUN, N, A, LDA, T, LDT, V, LDV, G, LDG, Q, LDQ,
     $                X, LDX, SEP, RCOND, FERR, WR, WI, S, LDS, IWORK,
     $                DWORK, LDWORK, BWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         END IF
         IF ( INFO.EQ.0 .OR. INFO.EQ.7 ) THEN
            IF ( LSAME( JOB, 'X' ) .OR. LSAME( JOB, 'A' ) ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( X(I,J), J = 1,N )
   20          CONTINUE
            END IF
            IF ( LSAME( JOB, 'C' ) .OR. LSAME( JOB, 'A' ) ) THEN
               WRITE ( NOUT, FMT = 99994 ) SEP
               WRITE ( NOUT, FMT = 99993 ) RCOND
            END IF
            IF ( LSAME( JOB, 'E' ) .OR. LSAME( JOB, 'A' ) )
     $         WRITE ( NOUT, FMT = 99992 ) FERR
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB02RD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB02RD = ',I2)
99997 FORMAT (' The solution matrix X is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
99994 FORMAT (/' Estimated separation = ',F8.4)
99993 FORMAT (/' Estimated reciprocal condition number = ',F8.4)
99992 FORMAT (/' Estimated error bound = ',F8.4)
      END
