*     SB02SD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDG, LDQ, LDT, LDU, LDX
      PARAMETER        ( LDA = NMAX, LDG = NMAX, LDQ = NMAX, LDT = NMAX,
     $                   LDU = NMAX, LDX = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX*NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 8*NMAX*NMAX + 10*NMAX )
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION FERR, RCND, RCOND, SEPD
      INTEGER          I, INFO1, INFO2, INFO3, IS, IU, IW, J, N, N2,
     $                 SDIM
      CHARACTER*1      FACT, JOB, JOBS, LYAPUN, TRANA, TRANAT, UPLO
*     .. Local Arrays ..
      LOGICAL          BWORK(2*NMAX)
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), AS(LDA,NMAX), DWORK(LDWORK),
     $                 G(LDG,NMAX), Q(LDQ,NMAX), T(LDT,NMAX),
     $                 U(LDU,NMAX), X(LDX,NMAX)
*     .. External Functions ..
      LOGICAL          LSAME, SELECT
      EXTERNAL         LSAME, SELECT
*     .. External Subroutines ..
      EXTERNAL         DGEES, DGESV, DLACPY, DLASET, DSWAP, DSYMM,
     $                 MA02AD, MA02ED, MB01RU, SB02MD, SB02SD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, JOB, FACT, TRANA, UPLO, LYAPUN
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( Q(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( G(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'Full', N, N, A, LDA, AS, LDA )
         CALL DLACPY( UPLO, N, N, Q, LDQ, X, LDX )
         N2 = 2*N
         IS = 2*N2 + 1
         IU = IS + N2*N2
         IW = IU + N2*N2
*        Solve the discrete-time Riccati equation.
         CALL SB02MD( 'discrete', 'direct', UPLO, 'no scaling',
     $                'stable', N, AS, LDA, G, LDG, X, LDX, RCND,
     $                DWORK(1), DWORK(N2+1), DWORK(IS), N2, DWORK(IU),
     $                N2, IWORK, DWORK(IW), LDWORK-IW+1, BWORK, INFO1 )
*
         IF ( INFO1.EQ.0 ) THEN
            WRITE ( NOUT, FMT = 99995 )
            DO 10 I = 1, N
               WRITE ( NOUT, FMT = 99994 ) ( X(I,J), J = 1,N )
   10       CONTINUE
            IF ( LSAME( FACT, 'F' ) .OR. LSAME( LYAPUN, 'R' ) ) THEN
               CALL DLASET( 'Full', N, N, ZERO, ONE, DWORK, N )
               CALL DSYMM( 'Left', UPLO, N, N, ONE, G, LDG, X, LDX,
     $                     ONE, DWORK, N )
               IF ( LSAME( TRANA, 'N' ) ) THEN
*                 Compute Ac = inv(I_n + G*X)*A.
                  CALL DLACPY( 'Full', N, N, A, LDA, T, LDT )
                  CALL DGESV( N, N, DWORK, N, IWORK, T, LDT, INFO3 )
               ELSE
*                 Compute Ac = A*inv(I_n + X*G)
                  CALL MA02AD( 'Full', N, N, A, LDA, T, LDT )
                  CALL DGESV( N, N, DWORK, N, IWORK, T, LDT, INFO3 )
                  DO 20 J = 2, N
                     CALL DSWAP( J-1, T(1,J), 1, T(J,1), LDT )
   20             CONTINUE
               END IF
*              Compute the Schur factorization of Ac.
               JOBS = 'V'
               CALL DGEES( JOBS, 'Not ordered', SELECT, N, T, LDT, SDIM,
     $                     DWORK(1), DWORK(N+1), U, LDU, DWORK(2*N+1),
     $                     LDWORK-2*N, BWORK, INFO3 )
               IF( INFO3.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99996 ) INFO3
                  STOP
               END IF
            END IF
*
            IF ( LSAME( LYAPUN, 'R' ) ) THEN
               IF( LSAME( TRANA, 'N' )  ) THEN
                  TRANAT = 'T'
               ELSE
                  TRANAT = 'N'
               END IF
*
               CALL MB01RU( UPLO, TRANAT, N, N, ZERO, ONE, X, LDX,
     $                      U, LDU, X, LDX, DWORK, N*N, INFO2 )
               CALL MA02ED( UPLO, N, X, LDX )
               CALL MB01RU( UPLO, TRANAT, N, N, ZERO, ONE, G, LDG,
     $                      U, LDU, G, LDG, DWORK, N*N, INFO2 )
               CALL MB01RU( UPLO, TRANAT, N, N, ZERO, ONE, Q, LDQ,
     $                      U, LDU, Q, LDQ, DWORK, N*N, INFO2 )
            END IF
*           Estimate the condition and error bound on the solution.
            CALL SB02SD( JOB, FACT, TRANA, UPLO, LYAPUN, N, A, LDA, T,
     $                   LDT, U, LDU, G, LDG, Q, LDQ, X, LDX, SEPD,
     $                   RCOND, FERR, IWORK, DWORK, LDWORK, INFO2 )
*
            IF ( INFO2.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) INFO2
            END IF
            IF ( INFO2.EQ.0 .OR. INFO2.EQ.N+1 ) THEN
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
99999 FORMAT (' SB02SD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB02MD =',I2)
99997 FORMAT (' INFO on exit from SB02SD =',I2)
99996 FORMAT (' INFO on exit from DGEES  =',I2)
99995 FORMAT (' The solution matrix X is')
99994 FORMAT (20(1X,F8.4))
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' Estimated separation = ',F8.4)
99991 FORMAT (/' Estimated reciprocal condition number = ',F8.4)
99990 FORMAT (/' Estimated error bound = ',F8.4)
      END
