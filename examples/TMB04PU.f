*     MB04PU/MB04WP EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE, TWO
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 100 )
      INTEGER          LDA, LDQG, LDRES, LDU1, LDU2, LDWORK
      PARAMETER        ( LDA  = NMAX, LDQG = NMAX, LDRES  = NMAX,
     $                   LDU1 = NMAX, LDU2 = NMAX, LDWORK = 2*NMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA, NMAX), CS(2*NMAX), DWORK(LDWORK),
     $                 QG(LDQG, NMAX+1), RES(LDRES,3*NMAX+1), TAU(NMAX),
     $                 U1(LDU1,NMAX), U2(LDU2, NMAX)
*     .. External Functions ..
      DOUBLE PRECISION MA02ID, MA02JD
      EXTERNAL         MA02ID, MA02JD
*     .. External Subroutines ..
      EXTERNAL         DGEMM, DLACPY, DLASET, DSCAL, DSYMM, DSYR,
     $                 DSYR2K, DTRMM, MB04PU, MB04WP
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  N
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'All', N, N, A, LDA, RES(1,N+1), LDRES )
         READ ( NIN, FMT = * ) ( ( QG(I,J), J = 1,N+1 ), I = 1,N )
         CALL DLACPY( 'All', N, N+1, QG, LDQG, RES(1,2*N+1), LDRES )
         CALL MB04PU( N, 1, A, LDA, QG, LDQG, CS, TAU, DWORK, LDWORK,
     $                INFO )
         INFO = 0
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            CALL DLACPY( 'Lower', N, N, A, LDA, U1, LDU1 )
            CALL DLACPY( 'Lower', N, N, QG, LDQG, U2, LDU2 )
            CALL MB04WP( N, 1, U1, LDU1, U2, LDU2, CS, TAU, DWORK,
     $                   LDWORK, INFO )
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) INFO
            ELSE
               IF ( N.GT.2 )
     $            CALL DLASET( 'Lower', N-2, N-2, ZERO, ZERO, A(3,1),
     $                         LDA )
               IF ( N.GT.1 )
     $            CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO, QG(2,1),
     $                         LDQG )
               WRITE ( NOUT, FMT = 99996 )
               DO 10  I = 1, N
                  WRITE (NOUT, FMT = 99993)
     $                  ( U1(I,J), J = 1,N ), ( U2(I,J), J = 1,N )
10             CONTINUE
               DO 20  I = 1, N
                  WRITE (NOUT, FMT = 99993)
     $                  ( -U2(I,J), J = 1,N ), ( U1(I,J), J = 1,N )
20             CONTINUE
               WRITE ( NOUT, FMT = 99991 ) MA02JD( .FALSE., .FALSE., N,
     $                 U1, LDU1, U2, LDU2, RES, LDRES )
               WRITE ( NOUT, FMT = 99995 )
               DO 30  I = 1, N
                  WRITE (NOUT, FMT = 99993) ( A(I,J), J = 1,N )
30             CONTINUE
               WRITE ( NOUT, FMT = 99994 )
               DO 40  I = 1, N
                  WRITE (NOUT, FMT = 99993) ( QG(I,J), J = 1,N+1 )
40             CONTINUE
C
               CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE,
     $                     U1, LDU1, A, LDA, ZERO, RES, LDRES )
               CALL DGEMM( 'No Transpose', 'Transpose', N, N, N, -ONE,
     $                     RES, LDRES, U1, LDU1, ONE, RES(1,N+1),
     $                     LDRES )
               CALL DGEMM( 'No Transpose', 'Transpose', N, N, N, ONE,
     $                     U2, LDU2, A, LDA, ZERO, RES, LDRES )
               CALL DGEMM( 'No Transpose', 'Transpose', N, N, N, ONE,
     $                     RES, LDRES, U2, LDU2, ONE, RES(1,N+1),
     $                     LDRES )
               CALL DSYMM ( 'Right', 'Upper', N, N, ONE, QG(1,2), LDQG,
     $                      U1, LDU1, ZERO, RES, LDRES )
               CALL DGEMM( 'No Transpose', 'Transpose', N, N, N, -ONE,
     $                     RES, LDRES, U2, LDU2, ONE, RES(1,N+1),
     $                     LDRES )
               CALL DLACPY( 'All', N, N, U2, LDU2, RES, LDRES )
               DO 50 I = 1, N
                   CALL DSCAL( N, QG(I,I), RES(1,I), 1 )
50             CONTINUE
               CALL DGEMM( 'No Transpose', 'Transpose', N, N, N, -ONE,
     $                     RES, LDRES, U1, LDU1, ONE, RES(1,N+1),
     $                     LDRES )
               CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE,
     $                     U2, LDU2, A, LDA, ZERO, RES, LDRES )
               CALL DSYR2K( 'Lower', 'No Transpose', N, N, ONE, RES,
     $                      LDRES, U1, LDU1, ONE, RES(1,2*N+1), LDRES )
               CALL DSCAL( N, ONE/TWO, QG(1,2), LDQG+1 )
               CALL DLACPY( 'Full', N, N, U2, LDU2, RES, LDRES )
               CALL DTRMM(  'Right', 'Upper' , 'No Transpose',
     $                      'Not unit', N, N, ONE, QG(1,2), LDQG,
     $                       RES, LDRES )
               CALL DSYR2K( 'Lower', 'No Transpose', N, N, ONE, RES,
     $                      LDRES, U2, LDU2, ONE, RES(1,2*N+1), LDRES )
               DO 60  I = 1, N
                  CALL DSYR( 'Lower', N, -QG(I,I), U1(1,I), 1,
     $                       RES(1,2*N+1), LDRES )
60             CONTINUE
               CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE,
     $                     U1, LDU1, A, LDA, ZERO, RES, LDRES )
               CALL DSYR2K( 'Upper', 'No Transpose', N, N, ONE, RES,
     $                      LDRES, U2, LDU2, ONE, RES(1,2*N+2), LDRES )
               CALL DLACPY( 'Full', N, N, U1, LDU1, RES, LDRES )
               CALL DTRMM(  'Right', 'Upper' , 'No Transpose',
     $                      'Not unit', N, N, ONE, QG(1,2), LDQG,
     $                       RES, LDRES )
               CALL DSYR2K( 'Upper', 'No Transpose', N, N, -ONE, RES,
     $                      LDRES, U1, LDU1, ONE, RES(1,2*N+2), LDRES )
               DO 70  I = 1, N
                  CALL DSYR( 'Upper', N, QG(I,I), U2(1,I), 1,
     $                       RES(1,2*N+2), LDRES )
70             CONTINUE
C
               WRITE ( NOUT, FMT = 99990 )  MA02ID( 'Hamiltonian',
     $                'Frobenius', N, RES(1,N+1), LDRES, RES(1,2*N+1),
     $                LDRES, DWORK )
            END IF
         END IF
      END IF
*
99999 FORMAT (' TMB04PU EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04PU = ',I2)
99997 FORMAT (' INFO on exit from MB04WP = ',I2)
99996 FORMAT (' The symplectic orthogonal factor U is ')
99995 FORMAT (/' The reduced matrix A is ')
99994 FORMAT (/' The reduced matrix QG is ')
99993 FORMAT (20(1X,F9.4))
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (/' Orthogonality of U: || U''*U - I ||_F = ',G7.2)
99990 FORMAT (/' Residual: || H - U*R*U'' ||_F = ',G7.2)
      END
