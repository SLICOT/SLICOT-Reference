*     MB03VD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, PMAX
      PARAMETER        ( NMAX = 20, PMAX = 20 )
      INTEGER          LDA1, LDA2, LDQ1, LDQ2, LDTAU
      PARAMETER        ( LDA1 = NMAX, LDA2 = NMAX, LDQ1 = NMAX,
     $                   LDQ2 = NMAX, LDTAU = NMAX-1 )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX )
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
*     .. Local Scalars ..
      DOUBLE PRECISION SSQ
      INTEGER          I, IHI, ILO, INFO, J, K, KP1, N, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA1,LDA2,PMAX), AS(LDA1,LDA2,PMAX),
     $                 DWORK(LDWORK), Q(LDQ1,LDQ2,PMAX),
     $                 QTA(LDQ1,NMAX), TAU(LDTAU,PMAX)
*     .. External Functions ..
      DOUBLE PRECISION DLANGE, DLAPY2
      EXTERNAL         DLANGE, DLAPY2
*     .. External Subroutines ..
      EXTERNAL         DGEMM, DLACPY, DLASET, MB03VD, MB03VY
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
      WRITE (NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, P, ILO, IHI
      IF ( N.LT.0 .OR. N.GT.MIN( LDA1, LDA2 ) ) THEN
         WRITE ( NOUT, FMT = 99991 ) N
      ELSE
         IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
            WRITE ( NOUT, FMT = 99990 ) P
         ELSE
*           Read matrices A_1, ..., A_p from the input file.
            DO 10 K = 1, P
               READ ( NIN, FMT = * )
     $            ( ( A(I,J,K), J = 1, N ), I = 1, N )
               CALL DLACPY( 'F', N, N, A(1,1,K), LDA1, AS(1,1,K), LDA1 )
   10       CONTINUE
*           Reduce to the periodic Hessenberg form.
            CALL MB03VD( N, P, ILO, IHI, A, LDA1, LDA2, TAU, LDTAU,
     $                   DWORK, INFO )
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99996 )
               DO 30 K = 1, P
                  CALL DLACPY( 'L', N, N, A(1,1,K), LDA1, Q(1,1,K),
     $                         LDQ1 )
                  IF ( N.GT.1 ) THEN
                     IF ( N.GT.2 .AND. K.EQ.1 ) THEN
                        CALL DLASET( 'L', N-2, N-2, ZERO, ZERO,
     $                               A(3,1,K), LDA1 )
                     ELSE IF ( K.GT.1 ) THEN
                        CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                               A(2,1,K), LDA1 )
                     END IF
                  END IF
                  WRITE ( NOUT, FMT = 99995 ) K
                  DO 20 I = 1, N
                     WRITE ( NOUT, FMT = 99994 ) ( A(I,J,K), J = 1, N )
   20             CONTINUE
   30          CONTINUE
*              Accumulate the transformations.
               CALL MB03VY( N, P, ILO, IHI, Q, LDQ1, LDQ2, TAU, LDTAU,
     $                      DWORK, LDWORK, INFO )
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99997 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99993 )
                  DO 50 K = 1, P
                     WRITE ( NOUT, FMT = 99995 ) K
                     DO 40 I = 1, N
                        WRITE ( NOUT, FMT = 99994 )
     $                        ( Q(I,J,K), J = 1, N )
   40                CONTINUE
   50             CONTINUE
*                 Compute error.
                  SSQ = ZERO
                  DO 60 K = 1, P
                     KP1 = K+1
                     IF( KP1.GT.P ) KP1 = 1
*                    Compute NORM (Z' * A * Z - Aout)
                     CALL DGEMM( 'T', 'N', N, N, N, ONE, Q(1,1,K), LDQ1,
     $                           AS(1,1,K), LDA1, ZERO, QTA, LDQ1 )
                     CALL DGEMM( 'N', 'N', N, N, N, ONE, QTA, LDQ1,
     $                           Q(1,1,KP1), LDQ1, -ONE, A(1,1,K),
     $                           LDA1 )
                     SSQ = DLAPY2( SSQ,
     $                             DLANGE( 'Frobenius', N, N, A(1,1,K),
     $                                     LDA1, DWORK ) )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99992 ) SSQ
               END IF
            END IF
         END IF
      END IF
      STOP
99999 FORMAT (' MB03VD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' INFO on exit from MB03VD = ', I2)
99997 FORMAT (' INFO on exit from MB03VY = ', I2)
99996 FORMAT (' Reduced matrices')
99995 FORMAT (/' K = ', I5)
99994 FORMAT (8F8.4)
99993 FORMAT (/' Transformation matrices')
99992 FORMAT (/,' NORM (Q''*A*Q - Aout) = ', 1PD12.5)
99991 FORMAT (/, ' N is out of range.',/' N = ', I5)
99990 FORMAT (/, ' P is out of range.',/' P = ', I5)
      END
