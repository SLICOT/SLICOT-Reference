*     MB03XD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 100 )
      INTEGER          LDA, LDQG, LDRES, LDT, LDU1, LDU2, LDV1, LDV2,
     $                 LDWORK
      PARAMETER        ( LDA = NMAX, LDQG = NMAX, LDRES = NMAX,
     $                   LDT = NMAX, LDU1 = NMAX, LDU2 = NMAX,
     $                   LDV1 = NMAX, LDV2 = NMAX,
     $                   LDWORK = 3*NMAX*NMAX + 7*NMAX )
*     .. Local Scalars ..
      CHARACTER*1      BALANC, JOB, JOBU, JOBV
      INTEGER          I, ILO, INFO, J, N
      DOUBLE PRECISION TEMP
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA, NMAX), DWORK(LDWORK), QG(LDQG, NMAX+1),
     $                 RES(LDRES,3*NMAX+1), SCALE(NMAX), T(LDT,NMAX),
     $                 U1(LDU1,NMAX), U2(LDU2, NMAX), V1(LDV1,NMAX),
     $                 V2(LDV2, NMAX), WI(NMAX), WR(NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      DOUBLE PRECISION DLANGE, DLAPY2, MA02JD
      EXTERNAL         DLANGE, DLAPY2, LSAME, MA02JD
*     .. External Subroutines ..
      EXTERNAL         DGEMM, DLACPY, MB03XD, MB04DD
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  N, BALANC, JOB, JOBU, JOBV
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99988 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'All', N, N, A, LDA, RES(1,N+1), LDRES )
         READ ( NIN, FMT = * ) ( ( QG(I,J), J = 1,N+1 ), I = 1,N )
         CALL DLACPY( 'All', N, N+1, QG, LDQG, RES(1,2*N+1), LDRES )
         INFO = 0
         CALL MB03XD( BALANC, JOB, JOBU, JOBV, N, A, LDA, QG, LDQG,
     $                T, LDT, U1, LDU1, U2, LDU2, V1, LDV1, V2, LDV2,
     $                WR, WI, ILO, SCALE, DWORK, LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20  I = 1, N
               WRITE ( NOUT, FMT = 99996 ) I, WR(I), WI(I)
20          CONTINUE
            IF ( LSAME( JOB, 'S' ).OR.LSAME( JOB, 'G' ) ) THEN
               WRITE ( NOUT, FMT = 99995 )
               DO 30  I = 1, N
                  WRITE ( NOUT, FMT = 99990 ) ( A(I,J), J = 1,N )
30             CONTINUE
               WRITE ( NOUT, FMT = 99994 )
               DO 40  I = 1, N
                  WRITE ( NOUT, FMT = 99990 ) ( T(I,J), J = 1,N )
40             CONTINUE
            END IF
            IF ( LSAME( JOB, 'G' ) ) THEN
               WRITE ( NOUT, FMT = 99993 )
               DO 50  I = 1, N
                  WRITE ( NOUT, FMT = 99990 ) ( QG(I,J+1), J = 1,N )
50             CONTINUE
            END IF
C
            IF ( LSAME( JOB, 'G' ).AND.LSAME( JOBU, 'U' ).AND.
     $           LSAME( JOBV, 'V' ) ) THEN
               CALL MB04DD( BALANC, N, RES(1,N+1), LDRES, RES(1,2*N+1),
     $                      LDRES, I, DWORK, INFO )
               CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE,
     $                     RES(1,N+1), LDRES, V1, LDV1, ZERO, RES,
     $                     LDRES )
               CALL DSYMM ( 'Left', 'Upper', N, N, -ONE, RES(1,2*N+2),
     $                      LDRES, V2, LDV2, ONE, RES, LDRES )
               CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N,
     $                     -ONE, U1, LDU1, T, LDT, ONE, RES, LDRES )
               TEMP = DLANGE( 'Frobenius', N, N, RES, LDRES, DWORK )
               CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE,
     $                     RES(1,N+1), LDRES, V2, LDV2, ZERO, RES,
     $                     LDRES )
               CALL DSYMM( 'Left', 'Upper', N, N, ONE, RES(1,2*N+2),
     $                     LDRES, V1, LDV1, ONE, RES, LDRES )
               CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N,
     $                     -ONE, U1, LDU1, QG(1,2), LDQG, ONE, RES,
     $                     LDRES )
               CALL DGEMM( 'No Transpose', 'Transpose', N, N, N,
     $                     -ONE, U2, LDU2, A, LDA, ONE, RES, LDRES )
               TEMP = DLAPY2( TEMP, DLANGE( 'Frobenius', N, N, RES,
     $                                      LDRES, DWORK ) )
               CALL DSYMM( 'Left', 'Lower', N, N, ONE, RES(1,2*N+1),
     $                     LDRES, V1, LDV1, ZERO, RES, LDRES )
               CALL DGEMM( 'Transpose', 'No Transpose', N, N, N, ONE,
     $                     RES(1,N+1), LDRES, V2, LDV2, ONE, RES,
     $                     LDRES )
               CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE,
     $                     U2, LDU2, T, LDT, ONE, RES, LDRES )
               TEMP = DLAPY2( TEMP, DLANGE( 'Frobenius', N, N, RES,
     $                                      LDRES, DWORK ) )


               CALL DSYMM( 'Left', 'Lower', N, N, ONE, RES(1,2*N+1),
     $                     LDRES, V2, LDV2, ZERO, RES, LDRES )
               CALL DGEMM( 'Transpose', 'No Transpose', N, N, N, -ONE,
     $                     RES(1,N+1), LDRES, V1, LDV1, ONE, RES,
     $                     LDRES )
               CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE,
     $                     U2, LDU2, QG(1,2), LDQG, ONE, RES, LDRES )
               CALL DGEMM( 'No Transpose', 'Transpose', N, N, N,
     $                     -ONE, U1, LDU1, A, LDA, ONE, RES, LDRES )
               TEMP = DLAPY2( TEMP, DLANGE( 'Frobenius', N, N, RES,
     $                                      LDRES, DWORK ) )
               WRITE ( NOUT, FMT = 99987 ) TEMP
            END IF
C
            IF ( LSAME( JOBU, 'U' ) ) THEN
               WRITE ( NOUT, FMT = 99992 )
               DO 60  I = 1, N
                  WRITE ( NOUT, FMT = 99990 )
     $               ( U1(I,J), J = 1,N ), ( U2(I,J), J = 1,N )
60             CONTINUE
               DO 70  I = 1, N
                  WRITE ( NOUT, FMT = 99990 )
     $               ( -U2(I,J), J = 1,N ), ( U1(I,J), J = 1,N )
70             CONTINUE
               WRITE ( NOUT, FMT = 99986 ) MA02JD( .FALSE., .FALSE., N,
     $                 U1, LDU1, U2, LDU2, RES, LDRES )
            END IF
            IF ( LSAME( JOBV, 'V' ) ) THEN
               WRITE ( NOUT, FMT = 99991 )
               DO 80  I = 1, N
                  WRITE ( NOUT, FMT = 99990 )
     $               ( V1(I,J), J = 1,N ), ( V2(I,J), J = 1,N )
80             CONTINUE
               DO 90  I = 1, N
                  WRITE ( NOUT, FMT = 99990 )
     $               ( -V2(I,J), J = 1,N ), ( V1(I,J), J = 1,N )
90             CONTINUE
               WRITE ( NOUT, FMT = 99985 ) MA02JD( .FALSE., .FALSE., N,
     $                 V1, LDV1, V2, LDV2, RES, LDRES )
            END IF
            IF ( LSAME( BALANC, 'S' ).OR.LSAME( BALANC, 'B' ) ) THEN
               WRITE ( NOUT, FMT = 99989 )
               DO 100  I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) I, SCALE(I)
100            CONTINUE
            END IF
         END IF
      END IF
*
99999 FORMAT (' MB03XD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB03XD = ',I2)
99997 FORMAT (' The eigenvalues are',//'   i',6X,
     $        'WR(i)',6X,'WI(i)',/)
99996 FORMAT (I4,3X,F8.4,3X,F8.4)
99995 FORMAT (/' The matrix S of the reduced matrix is')
99994 FORMAT (/' The matrix T of the reduced matrix is')
99993 FORMAT (/' The matrix G of the reduced matrix is')
99992 FORMAT (/' The orthogonal symplectic factor U is')
99991 FORMAT (/' The orthogonal symplectic factor V is')
99990 FORMAT (20(1X,F19.16))
99989 FORMAT (/' The diagonal scaling factors are ',//'   i',6X,
     $        'SCALE(i)',/)
99988 FORMAT (/' N is out of range.',/' N = ',I5)
99987 FORMAT (/' Residual: || H*V - U*R ||_F = ',G7.2)
99986 FORMAT (/' Orthogonality of U: || U^T U - I ||_F = ',G7.2)
99985 FORMAT (/' Orthogonality of V: || V^T V - I ||_F = ',G7.2)
      END
