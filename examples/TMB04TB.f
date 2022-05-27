*     MB04TB/MB04WR EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NBMAX, NMAX
      PARAMETER        ( NBMAX = 64, NMAX = 421 )
      INTEGER          LDA, LDB, LDG, LDQ, LDRES, LDU1, LDU2, LDV1,
     $                 LDV2, LDWORK
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDG = NMAX, LDQ = NMAX,
     $                   LDRES = NMAX, LDU1 = NMAX, LDU2 = NMAX,
     $                   LDV1 = NMAX, LDV2 = NMAX,
     $                   LDWORK = NBMAX*( 16*NMAX + 1 ) )
*     .. Local Scalars ..
      CHARACTER*1      TRANA, TRANB, TRANV1
      INTEGER          I, INFO, J, N
      DOUBLE PRECISION TEMP
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA, NMAX), B(LDB, NMAX), CSL(2*NMAX),
     $                 CSR(2*NMAX), DWORK(LDWORK), G(LDG, NMAX),
     $                 Q(LDQ, NMAX), RES(LDRES,5*NMAX), TAUL(NMAX),
     $                 TAUR(NMAX), U1(LDU1, NMAX), U2(LDU2, NMAX),
     $                 V1(LDV1, NMAX), V2(LDV2, NMAX)
*     .. External Functions ..
*     .. External Functions ..
      LOGICAL          LSAME
      DOUBLE PRECISION DLANGE, DLAPY2, MA02JD
      EXTERNAL         DLANGE, DLAPY2, LSAME, MA02JD
*     .. External Subroutines ..
      EXTERNAL         DGEMM, DLACPY, DLASET, MB04TB, MB04WR
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  N, TRANA, TRANB
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'All', N, N, A, LDA, RES, LDRES )
         READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'All', N, N, B, LDB, RES(1,N+1), LDRES )
         READ ( NIN, FMT = * ) ( ( G(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'All', N, N, G, LDG, RES(1,2*N+1), LDRES )
         READ ( NIN, FMT = * ) ( ( Q(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'All', N, N, Q, LDQ, RES(1,3*N+1), LDRES )
         CALL MB04TB( TRANA, TRANB, N, 1, A, LDA, B, LDB, G, LDG, Q,
     $                LDQ, CSL, CSR, TAUL, TAUR, DWORK, LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            CALL DLACPY( 'All', N, N, A, LDA, U1, LDU1 )
            CALL DLACPY( 'All', N, N, Q, LDQ, U2, LDU2 )
            CALL MB04WR( 'U', TRANA, N, 1, U1, LDU1, U2, LDU2, CSL,
     $                   TAUL, DWORK, LDWORK, INFO )
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) INFO
            ELSE
               CALL DLACPY( 'All', N, N, Q, LDQ, V2, LDV2 )
               CALL DLACPY( 'All', N, N, B, LDB, V1, LDV1 )
               CALL MB04WR( 'V', TRANB, N, 1, V1, LDV1, V2, LDV2,
     $                      CSR, TAUR, DWORK, LDWORK, INFO )
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99997 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99996 )
                  IF ( LSAME( TRANA, 'N' ) ) THEN
                     DO 10  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( U1(I,J), J = 1,N ), ( U2(I,J), J = 1,N )
10                   CONTINUE
                     DO 20  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( -U2(I,J), J = 1,N ), ( U1(I,J), J = 1,N )
20                   CONTINUE
                     WRITE ( NOUT, FMT = 99991 ) MA02JD( .FALSE.,
     $                       .FALSE., N, U1, LDU1, U2, LDU2,
     $                       RES(1,4*N+1), LDRES )
                  ELSE
                     DO 30  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( U1(J,I), J = 1,N ), ( U2(I,J), J = 1,N )
30                   CONTINUE
                     DO 40  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( -U2(I,J), J = 1,N ), ( U1(J,I), J = 1,N )
40                   CONTINUE
                     WRITE ( NOUT, FMT = 99991 ) MA02JD( .TRUE.,
     $                       .FALSE., N, U1, LDU1, U2, LDU2,
     $                       RES(1,4*N+1), LDRES )
                  END IF
                  WRITE ( NOUT, FMT = 99995 )
                  CALL DLASET( 'All', N, N, ZERO, ZERO, Q, LDQ )
                  IF ( LSAME( TRANA, 'N' ) ) THEN
                     CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO,
     $                            A(2,1), LDA )
                     DO 50  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( A(I,J), J = 1,N ), ( G(I,J), J = 1,N )
50                   CONTINUE
                  ELSE
                     CALL DLASET( 'Upper', N-1, N-1, ZERO, ZERO,
     $                            A(1,2), LDA )
                     DO 60  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( A(J,I), J = 1,N ), ( G(I,J), J = 1,N )
60                   CONTINUE
                  END IF
                  IF ( LSAME( TRANB, 'N' ) ) THEN
                     IF ( N.GT.1 ) THEN
                        CALL DLASET( 'Upper', N-2, N-2, ZERO, ZERO,
     $                               B(1,3), LDB )
                     END IF
                     DO 70  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( Q(I,J), J = 1,N ), ( B(I,J), J = 1,N )
70                   CONTINUE
                  ELSE
                     IF ( N.GT.1 ) THEN
C                        CALL DLASET( 'Lower', N-2, N-2, ZERO, ZERO,
C     $                               B(3,1), LDB )
                     END IF
                     DO 80  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( Q(I,J), J = 1,N ), ( B(J,I), J = 1,N )
80                   CONTINUE
                  END IF
C
                  IF ( LSAME( TRANB, 'N' ) ) THEN
                     TRANV1 = 'T'
                  ELSE
                     TRANV1 = 'N'
                  END IF
                  CALL DGEMM( TRANA, TRANV1, N, N, N, ONE, RES, LDRES,
     $                        V1, LDV1, ZERO, RES(1,4*N+1), LDRES )
                  CALL DGEMM( 'No Transpose', 'Transpose', N, N, N,
     $                        -ONE, RES(1,2*N+1), LDRES, V2, LDV2, ONE,
     $                        RES(1,4*N+1), LDRES )
                  CALL DGEMM( TRANA, TRANA, N, N, N, -ONE, U1, LDU1,
     $                        A, LDA, ONE, RES(1,4*N+1), LDRES )
                  TEMP = DLANGE( 'Frobenius', N, N, RES(1,4*N+1),
     $                           LDRES, DWORK )
                  CALL DGEMM( TRANA, 'Transpose', N, N, N, ONE, RES,
     $                        LDRES, V2, LDV2, ZERO, RES(1,4*N+1),
     $                        LDRES )
                  CALL DGEMM( 'No Transpose', TRANV1, N, N, N, ONE,
     $                        RES(1,2*N+1), LDRES, V1, LDV1, ONE,
     $                        RES(1,4*N+1), LDRES )
                  CALL DGEMM( TRANA, 'No Transpose', N, N, N, -ONE,
     $                        U1, LDU1, G, LDG, ONE, RES(1,4*N+1),
     $                        LDRES )
                  CALL DGEMM( 'No Transpose', TRANB, N, N, N, -ONE,
     $                        U2, LDU2, B, LDB, ONE, RES(1,4*N+1),
     $                        LDRES )
                  TEMP = DLAPY2( TEMP, DLANGE( 'Frobenius', N, N,
     $                                 RES(1,4*N+1), LDRES, DWORK ) )
                  CALL DGEMM( 'No Transpose', TRANV1, N, N, N, ONE,
     $                        RES(1,3*N+1), LDRES, V1, LDV1, ZERO,
     $                        RES(1,4*N+1), LDRES )
                  CALL DGEMM( TRANB, 'Transpose', N, N, N, -ONE,
     $                        RES(1,N+1), LDRES, V2, LDV2, ONE,
     $                        RES(1,4*N+1), LDRES )
                  CALL DGEMM( 'No Transpose', TRANA, N, N, N, ONE,
     $                        U2, LDU2, A, LDA, ONE, RES(1,4*N+1),
     $                        LDRES )
                  TEMP = DLAPY2( TEMP, DLANGE( 'Frobenius', N, N,
     $                                 RES(1,4*N+1), LDRES, DWORK ) )
                  CALL DGEMM( 'No Transpose', 'Transpose', N, N, N, ONE,
     $                        RES(1,3*N+1), LDRES, V2, LDV2, ZERO,
     $                        RES(1,4*N+1), LDRES )
                  CALL DGEMM( TRANB, TRANV1, N, N, N, ONE, RES(1,N+1),
     $                        LDRES, V1, LDV1, ONE, RES(1,4*N+1),
     $                        LDRES )
                  CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N,
     $                        ONE, U2, LDU2, G, LDG, ONE, RES(1,4*N+1),
     $                        LDRES )
                  CALL DGEMM( TRANA, TRANB, N, N, N, -ONE, U1, LDU1,
     $                        B, LDB, ONE, RES(1,4*N+1), LDRES )
                  TEMP = DLAPY2( TEMP, DLANGE( 'Frobenius', N, N,
     $                                 RES(1,4*N+1), LDRES, DWORK ) )
                  WRITE ( NOUT, FMT = 99990 ) TEMP
C
                  WRITE ( NOUT, FMT = 99994 )
                  IF ( LSAME( TRANB, 'N' ) ) THEN
                     DO 90  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( V1(J,I), J = 1,N ), ( V2(J,I), J = 1,N )
90                   CONTINUE
                     DO 100  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( -V2(J,I), J = 1,N ), ( V1(J,I), J = 1,N )
100                  CONTINUE
                     WRITE ( NOUT, FMT = 99989 ) MA02JD( .TRUE.,
     $                       .TRUE., N, V1, LDV1, V2, LDV2,
     $                       RES(1,4*N+1), LDRES )
                  ELSE
                     DO 110  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( V1(I,J), J = 1,N ), ( V2(J,I), J = 1,N )
110                  CONTINUE
                     DO 120  I = 1, N
                        WRITE (NOUT, FMT = 99993)
     $                     ( -V2(J,I), J = 1,N ), ( V1(I,J), J = 1,N )
120                  CONTINUE
                     WRITE ( NOUT, FMT = 99989 ) MA02JD( .FALSE.,
     $                       .TRUE., N, V1, LDV1, V2, LDV2,
     $                       RES(1,4*N+1), LDRES )
                  END IF
               END IF
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MB04TB EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04TB = ',I2)
99997 FORMAT (' INFO on exit from MB04WR = ',I2)
99996 FORMAT (' The orthogonal symplectic factor U is ')
99995 FORMAT (/' The factor R is ')
99994 FORMAT (/' The orthogonal symplectic factor V is ')
99993 FORMAT (20(1X,F9.4))
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (/' Orthogonality of U: || U^T U - I ||_F = ',G7.2)
99990 FORMAT (/' Residual: || H*V - U*R ||_F = ',G7.2)
99989 FORMAT (/' Orthogonality of V: || V^T V - I ||_F = ',G7.2)
      END
