*     SB03UD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDC, LDT, LDU, LDX
      PARAMETER        ( LDA = NMAX, LDC = NMAX, LDT = NMAX,
     $                   LDU = NMAX, LDX = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX*NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( 3, 2*NMAX*NMAX ) +
     $                                 NMAX*NMAX + 2*NMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION FERR, RCOND, SCALE, SEPD
      INTEGER          I, INFO, J, N
      CHARACTER*1      DICO, FACT, JOB, LYAPUN, TRANA, UPLO
*     .. Local Arrays ..
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), C(LDC,NMAX), DWORK(LDWORK),
     $                 T(LDT,NMAX), U(LDU,NMAX), X(LDX,NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SB03UD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
      DICO = 'D'
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, JOB, FACT, TRANA, UPLO, LYAPUN
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         IF ( LSAME( JOB, 'C' ) .OR. LSAME( JOB, 'E' ) )
     $                               READ ( NIN, FMT = * ) SCALE
         IF ( LSAME( FACT, 'N' ) .OR. ( LSAME( LYAPUN, 'O' ) .AND.
     $                             .NOT.LSAME( JOB, 'X') ) )
     $      READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( FACT, 'F' ) ) THEN
            READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,N ), I = 1,N )
            IF ( LSAME( LYAPUN, 'O' ) )
     $         READ ( NIN, FMT = * ) ( ( U(I,J), J = 1,N ), I = 1,N )
         END IF
         IF ( .NOT.LSAME( JOB, 'S' ) )
     $      READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( JOB, 'C' ) .OR. LSAME( JOB, 'E' ) )
     $      READ ( NIN, FMT = * ) ( ( X(I,J), J = 1,N ), I = 1,N )
*        Solve the discrete-time Lyapunov matrix equation and/or
*        estimate the condition and error bound on the solution.
         CALL SB03UD( JOB, FACT, TRANA, UPLO, LYAPUN, N, SCALE, A, LDA,
     $                T, LDT, U, LDU, C, LDC, X, LDX, SEPD, RCOND, FERR,
     $                DWORK(1), DWORK(N+1), IWORK, DWORK(2*N+1),
     $                LDWORK-2*N, INFO )
*
         IF ( INFO.EQ.0 ) THEN
            IF ( LSAME( JOB, 'X' ) .OR. LSAME( JOB, 'A' ) ) THEN
               WRITE ( NOUT, FMT = 99996 )
               DO 10 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( X(I,J), J = 1,N )
   10          CONTINUE
               WRITE ( NOUT, FMT = 99993 ) SCALE
            END IF
            IF ( LSAME( JOB, 'S' ) .OR. LSAME( JOB, 'C' )
     $                             .OR. LSAME( JOB, 'A' ) )
     $         WRITE ( NOUT, FMT = 99992 ) SEPD
            IF ( LSAME( JOB, 'C' ) .OR. LSAME( JOB, 'A' ) )
     $         WRITE ( NOUT, FMT = 99991 ) RCOND
            IF ( LSAME( JOB, 'E' ) .OR. LSAME( JOB, 'A' ) )
     $         WRITE ( NOUT, FMT = 99990 ) FERR
         ELSE
            WRITE ( NOUT, FMT = 99998 ) INFO
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB03UD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB03UD =',I2)
99996 FORMAT (' The solution matrix X is')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' Scaling factor = ',F8.4)
99992 FORMAT (/' Estimated separation = ',F8.4)
99991 FORMAT (/' Estimated reciprocal condition number = ',F8.4)
99990 FORMAT (/' Estimated error bound = ',F8.4)
      END
