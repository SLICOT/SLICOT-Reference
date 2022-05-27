*     MB03XP EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 200 )
      INTEGER          LDA, LDB, LDQ, LDRES, LDZ, LDWORK
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDQ = NMAX,
     $                   LDRES = NMAX, LDWORK = NMAX, LDZ = NMAX )
*     .. Local Scalars ..
      INTEGER          I, IHI, ILO, INFO, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), ALPHAI(NMAX), ALPHAR(NMAX),
     $                 B(LDA,NMAX), BETA(NMAX), DWORK(LDWORK),
     $                 Q(LDQ,NMAX), RES(LDRES,3*NMAX), Z(LDZ,NMAX)
*     .. External Functions ..
      DOUBLE PRECISION DLANGE
      EXTERNAL         DLANGE
*     .. External Subroutines ..
      EXTERNAL         DGEMM, MB03XP
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  N, ILO, IHI
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'All', N, N, A, LDA, RES(1,N+1), LDRES )
         READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,N )
         CALL DLACPY( 'All', N, N, B, LDB, RES(1,2*N+1), LDRES )
         CALL MB03XP( 'S', 'I', 'I', N, ILO, IHI, A, LDA, B, LDB, Q,
     $                LDQ, Z, LDZ, ALPHAR, ALPHAI, BETA, DWORK, LDWORK,
     $                INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99996 )
            DO 10  I = 1, N
               WRITE (NOUT, FMT = 99991) ( A(I,J), J = 1,N )
10          CONTINUE
            CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE,
     $                  RES(1,N+1), LDRES, Z, LDZ, ZERO, RES, LDRES )
            CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, -ONE,
     $                  Q, LDQ, A, LDA, ONE, RES, LDRES )
            WRITE ( NOUT, FMT = 99989 ) DLANGE( 'Frobenius', N, N, RES,
     $                                          LDRES, DWORK )
            WRITE ( NOUT, FMT = 99995 )
            DO 20  I = 1, N
               WRITE (NOUT, FMT = 99991) ( B(I,J), J = 1,N )
20          CONTINUE
            CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE,
     $                  RES(1,2*N+1), LDRES, Q, LDQ, ZERO, RES, LDRES )
            CALL DGEMM( 'No Transpose', 'No Transpose', N, N, N, -ONE,
     $                  Z, LDZ, B, LDB, ONE, RES, LDRES )
            WRITE ( NOUT, FMT = 99988 ) DLANGE( 'Frobenius', N, N, RES,
     $                                          LDRES, DWORK )
            WRITE ( NOUT, FMT = 99994 )
            DO 30  I = 1, N
               WRITE (NOUT, FMT = 99991) ( Q(I,J), J = 1,N )
30          CONTINUE
            CALL DGEMM( 'Transpose', 'No Transpose', N, N, N, ONE, Q,
     $                  LDQ, Q, LDQ, ONE, RES, LDRES )
            DO 40  I = 1, N
               RES(I,I) = RES(I,I) - ONE
40          CONTINUE
            WRITE ( NOUT, FMT = 99987 ) DLANGE( 'Frobenius', N, N, RES,
     $                                          LDRES, DWORK )
            WRITE ( NOUT, FMT = 99993 )
            DO 50  I = 1, N
               WRITE (NOUT, FMT = 99991) ( Z(I,J), J = 1,N )
50          CONTINUE
            CALL DGEMM( 'Transpose', 'No Transpose', N, N, N, ONE, Z,
     $                  LDZ, Z, LDZ, ONE, RES, LDRES )
            DO 60 I = 1, N
               RES(I,I) = RES(I,I) - ONE
60          CONTINUE
            WRITE ( NOUT, FMT = 99986 ) DLANGE( 'Frobenius', N, N, RES,
     $                                          LDRES, DWORK )
            WRITE ( NOUT, FMT = 99992 )
            DO 70  I = 1, N
               WRITE ( NOUT, FMT = 99991 )
     $                 ALPHAR(I), ALPHAI(I), BETA(I)
70          CONTINUE
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MB03XP EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB03XP = ',I2)
99996 FORMAT (' The reduced matrix A is ')
99995 FORMAT (/' The reduced matrix B is ')
99994 FORMAT (/' The orthogonal factor Q is ')
99993 FORMAT (/' The orthogonal factor Z is ')
99992 FORMAT (/4X,'ALPHAR',4X,'ALPHAI',4X,'BETA')
99991 FORMAT (1000(1X,F9.4))
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' Residual: || A*Z - Q*S ||_F = ',G7.2)
99988 FORMAT (/' Residual: || B*Q - Z*T ||_F = ',G7.2)
99987 FORMAT (/' Orthogonality of Q: || Q''*Q - I ||_F = ',G7.2)
99986 FORMAT (/' Orthogonality of Z: || Z''*Z - I ||_F = ',G7.2)
      END
