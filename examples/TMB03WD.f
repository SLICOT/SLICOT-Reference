*     MB03WD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, PMAX
      PARAMETER        ( NMAX = 20, PMAX = 20 )
      INTEGER          LDA1, LDA2, LDTAU, LDZ1, LDZ2, LDZTA
      PARAMETER        ( LDA1 = NMAX, LDA2 = NMAX, LDTAU = NMAX-1,
     $                   LDZ1 = NMAX, LDZ2 = NMAX, LDZTA = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX, NMAX + PMAX - 2 ) )
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
*     .. Local Scalars ..
      DOUBLE PRECISION SSQ
      INTEGER          I, IHI, IHIZ, ILO, ILOZ, INFO, J, K, KP1, N, P
      CHARACTER        COMPZ, JOB
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA1,LDA2,PMAX), AS(LDA1,LDA2,PMAX),
     $                 DWORK(LDWORK), TAU(LDTAU,PMAX), WI(NMAX),
     $                 WR(NMAX), Z(LDZ1,LDZ2,PMAX), ZTA(LDZTA,NMAX)
*     .. External Functions ..
      DOUBLE PRECISION DLANGE, DLAPY2
      LOGICAL          LSAME
      EXTERNAL         DLANGE, DLAPY2, LSAME
*     .. External Subroutines ..
      EXTERNAL         DGEMM, DLACPY, MB03VD, MB03VY, MB03WD, MB03WX
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, P, ILO, IHI, ILOZ, IHIZ, JOB, COMPZ
      IF ( N.LT.0 .OR. N.GT.MIN( LDA1, LDA2 ) ) THEN
         WRITE ( NOUT, FMT = 99988 ) N
      ELSE
         IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
            WRITE ( NOUT, FMT = 99987 ) P
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
               WRITE ( NOUT, FMT = 99997 ) INFO
            ELSE
               IF ( LSAME( COMPZ, 'V' ) ) THEN
                  DO 20 K = 1, P
                     CALL DLACPY( 'L', N, N, A(1,1,K), LDA1, Z(1,1,K),
     $                            LDZ1 )
   20             CONTINUE
*                 Accumulate the transformations.
                  CALL MB03VY( N, P, ILO, IHI, Z, LDZ1, LDZ2, TAU,
     $                         LDTAU, DWORK, LDWORK, INFO )
                  IF ( INFO.NE.0 ) THEN
                     WRITE ( NOUT, FMT = 99996 ) INFO
                     STOP
                  ELSE
*                    Reduce to the periodic Schur form.
                     CALL MB03WD( JOB, COMPZ, N, P, ILO, IHI, ILOZ,
     $                            IHIZ, A, LDA1, LDA2, Z, LDZ1, LDZ2,
     $                            WR, WI, DWORK, LDWORK, INFO )
                     IF ( INFO.GT.0 ) THEN
                        WRITE ( NOUT, FMT = 99998 ) INFO
                        WRITE ( NOUT, FMT = 99991 )
                        DO 30 I = MAX( ILO, INFO + 1 ), IHI
                           WRITE ( NOUT, FMT = 99990 ) WR(I), WI(I)
   30                   CONTINUE
                        STOP
                     END IF
                     IF ( INFO.LT.0 ) THEN
                        WRITE ( NOUT, FMT = 99998 ) INFO
                     ELSE
*                       Store the isolated eigenvalues.
                        CALL MB03WX( ILO-1, P, A, LDA1, LDA2, WR, WI,
     $                               INFO )
                        IF ( IHI.LT.N )
     $                     CALL MB03WX( N-IHI, P, A(IHI+1,IHI+1,1),
     $                                  LDA1, LDA2, WR(IHI+1),
     $                                  WI(IHI+1), INFO )
                        WRITE ( NOUT, FMT = 99991 )
                        DO 40 I = 1, N
                           WRITE ( NOUT, FMT = 99990 ) WR(I), WI(I)
   40                   CONTINUE
                        WRITE ( NOUT, FMT = 99995 )
                        DO 60 K = 1, P
                           WRITE ( NOUT, FMT = 99994 ) K
                           DO 50 I = 1, N
                              WRITE ( NOUT, FMT = 99993 )
     $                              ( A(I,J,K), J = 1, N )
   50                      CONTINUE
   60                   CONTINUE
                        WRITE ( NOUT, FMT = 99992 )
                        DO 80 K = 1, P
                           WRITE ( NOUT, FMT = 99994 ) K
                           DO 70 I = 1, N
                              WRITE ( NOUT, FMT = 99993 )
     $                              ( Z(I,J,K), J = 1, N )
   70                      CONTINUE
   80                   CONTINUE
*                       Compute error.
                        SSQ = ZERO
                        DO 90 K = 1, P
                           KP1 = K+1
                           IF( KP1.GT.P ) KP1 = 1
*                          Compute NORM (Z' * A * Z - Aout)
                           CALL DGEMM( 'T', 'N', N, N, N, ONE, Z(1,1,K),
     $                                 LDZ1, AS(1,1,K), LDA1, ZERO, ZTA,
     $                                 LDZTA )
                           CALL DGEMM( 'N', 'N', N, N, N, ONE, ZTA,
     $                                 LDZTA, Z(1,1,KP1), LDZ1, -ONE,
     $                                 A(1,1,K), LDA1 )
                           SSQ = DLAPY2( SSQ,
     $                                   DLANGE( 'Frobenius', N, N,
     $                                           A(1,1,K), LDA1,
     $                                           DWORK ) )
   90                   CONTINUE
                        WRITE ( NOUT, FMT = 99989 ) SSQ
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
99999 FORMAT (' MB03WD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' INFO on exit from MB03WD = ', I2)
99997 FORMAT (' INFO on exit from MB03VD = ', I2)
99996 FORMAT (' INFO on exit from MB03VY = ', I2)
99995 FORMAT (/' Reduced matrices')
99994 FORMAT (/' K = ', I5)
99993 FORMAT (8F8.4)
99992 FORMAT (/' Transformation matrices')
99991 FORMAT ( ' Computed eigenvalues'/)
99990 FORMAT (4X,'( ', F17.6,' ,', F17.6,' )')
99989 FORMAT (/,' NORM (Z''*A*Z - Aout) = ', 1PD12.5)
99988 FORMAT (/, ' N is out of range.',/' N = ', I5)
99987 FORMAT (/, ' P is out of range.',/' P = ', I5)
      END
