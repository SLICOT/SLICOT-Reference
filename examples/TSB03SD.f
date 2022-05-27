*     SB03SD EXAMPLE PROGRAM TEXT
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
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION FERR, RCOND, SCALE, SEPD
      INTEGER          I, INFO1, INFO2, J, N
      CHARACTER*1      DICO, FACT, JOB, LYAPUN, TRANA, TRANAT, UPLO
*     .. Local Arrays ..
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), C(LDC,NMAX), DWORK(LDWORK),
     $                 T(LDT,NMAX), U(LDU,NMAX), X(LDX,NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         DLACPY, MA02ED, MB01RU, SB03MD, SB03SD
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
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( FACT, 'F' ) ) READ ( NIN, FMT = * )
     $                         ( ( U(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'Full', N, N, A, LDA, T, LDT )
         CALL DLACPY( 'Full', N, N, C, LDC, X, LDX )
*        Solve the discrete-time Lyapunov matrix equation.
         CALL SB03MD( DICO, 'X', FACT, TRANA, N, T, LDT, U, LDU, X, LDX,
     $                SCALE, SEPD, FERR, DWORK(1), DWORK(N+1), IWORK,
     $                DWORK(2*N+1), LDWORK-2*N, INFO1 )
*
         IF ( INFO1.EQ.0 ) THEN
            WRITE ( NOUT, FMT = 99996 )
            DO 10 I = 1, N
               WRITE ( NOUT, FMT = 99995 ) ( X(I,J), J = 1,N )
   10       CONTINUE
            IF ( LSAME( LYAPUN, 'R' ) ) THEN
               IF( LSAME( TRANA, 'N' )  ) THEN
                  TRANAT = 'T'
               ELSE
                  TRANAT = 'N'
               END IF
               CALL MB01RU( UPLO, TRANAT, N, N, ZERO, ONE, X, LDX,
     $                      U, LDU, X, LDX, DWORK, N*N, INFO2 )
               CALL MA02ED( UPLO, N, X, LDX )
               CALL MB01RU( UPLO, TRANAT, N, N, ZERO, ONE, C, LDC,
     $                      U, LDU, C, LDC, DWORK, N*N, INFO2 )
            END IF
*           Estimate the condition and error bound on the solution.
            CALL SB03SD( JOB, 'F', TRANA, UPLO, LYAPUN, N, SCALE, A,
     $                   LDA, T, LDT, U, LDU, C, LDC, X, LDX, SEPD,
     $                   RCOND, FERR, IWORK, DWORK, LDWORK, INFO2 )
*
            IF ( INFO2.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) INFO2
            ELSE
               WRITE ( NOUT, FMT = 99993 ) SCALE
               WRITE ( NOUT, FMT = 99992 ) SEPD
               WRITE ( NOUT, FMT = 99991 ) RCOND
               WRITE ( NOUT, FMT = 99990 ) FERR
            END IF
         ELSE
            WRITE ( NOUT, FMT = 99998 ) INFO1
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB03SD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB03MD =',I2)
99997 FORMAT (' INFO on exit from SB03SD =',I2)
99996 FORMAT (' The solution matrix X is')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' Scaling factor = ',F8.4)
99992 FORMAT (/' Estimated separation = ',F8.4)
99991 FORMAT (/' Estimated reciprocal condition number = ',F8.4)
99990 FORMAT (/' Estimated error bound = ',F8.4)
      END
