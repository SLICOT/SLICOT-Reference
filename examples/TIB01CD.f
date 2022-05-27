*     IB01CD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          LDA, LDB, LDC, LDD, LDR, LDU, LDV, LDW1, LDW2,
     $                 LDW4, LDW5, LDWORK, LDY, LIWORK, LMAX, MMAX,
     $                 NMAX, NOBRMX, NSMPMX
      PARAMETER        ( LMAX = 5, MMAX = 5, NOBRMX = 20, NSMPMX = 2000,
     $                   NMAX = NOBRMX - 1, LDA = NMAX, LDB = NMAX,
     $                   LDC  = LMAX, LDD = LMAX, LDV = NMAX,
     $                   LDR  = MAX( 2*( MMAX + LMAX )*NOBRMX,
     $                               3*MMAX*NOBRMX ), LDU = NSMPMX,
     $                   LDW1 = MAX( LMAX*( NOBRMX - 1 )*NMAX + NMAX +
     $                               MAX( 6*MMAX, 4*LMAX )*NOBRMX,
     $                               LMAX*NOBRMX*NMAX +
     $                               MAX( LMAX*( NOBRMX - 1 )*NMAX +
     $                                    3*NMAX + LMAX +
     $                                    ( 2*MMAX + LMAX )*NOBRMX,
     $                                    2*LMAX*( NOBRMX - 1 )*NMAX +
     $                                    NMAX*NMAX + 8*NMAX,
     $                                    NMAX +
     $                                    4*( MMAX*NOBRMX + NMAX ) ) ),
     $                   LDW2 = LMAX*NOBRMX*NMAX +
     $                          MMAX*NOBRMX*( NMAX + LMAX )*
     $                          ( MMAX*( NMAX + LMAX ) + 1 ) +
     $                          MAX( ( NMAX + LMAX )**2,
     $                          4*MMAX*( NMAX + LMAX ) + 1 ),
     $                   LDW4 = NSMPMX*LMAX*NMAX*( MMAX + 1 ) +
     $                          MAX( NMAX +
     $                               MAX( 2*NMAX*NMAX + NMAX,
     $                                    MMAX +
     $                                    MAX( 2*NMAX*( MMAX + 1 ),
     $                                         MMAX ),
     $                                    6*NMAX*( MMAX + 1 ) ),
     $                               2*MMAX*MMAX*NMAX + 6*MMAX ),
     $                   LDW5 = ( LMAX*MMAX + NMAX*( MMAX + 1 ) )*
     $                          NMAX*( MMAX + 1 ) +
     $                          MAX( ( LMAX*MMAX +
     $                               LMAX*NMAX*( MMAX + 1 ) )*
     $                               NMAX*( MMAX + 1 ) +
     $                               NMAX*NMAX*MMAX + LMAX*NMAX +
     $                               MAX( 2*NMAX*NMAX + NMAX,
     $                                    MMAX +
     $                                    MAX( 2*NMAX*( MMAX + 1 ),
     $                                         MMAX ),
     $                                    6*NMAX*( MMAX + 1 ) ),
     $                               2*MMAX*MMAX*NMAX + 6*MMAX ),
     $                   LDWORK = MAX( 6*( MMAX + LMAX )*NOBRMX,
     $                                 ( MMAX + LMAX )*( 4*NOBRMX*
     $                                 ( MMAX + LMAX + 2 ) - 2 ),
     $                                 ( MMAX + LMAX )*4*NOBRMX*
     $                                 ( NOBRMX + 1 ), LDW1, LDW2,
     $                                 3 + ( NMAX + MMAX + LMAX )*NMAX +
     $                                 MAX( 5*NMAX, 3,
     $                                      MIN( LDW4, LDW5 ) ) ),
     $                   LDY = NSMPMX,
     $                   LIWORK = MAX( ( MMAX + LMAX )*NOBRMX,
     $                                 MMAX*NOBRMX + NMAX,
     $                                 MMAX*( NMAX + LMAX ),
     $                                 NMAX*MMAX + NMAX, MMAX )
     $                 )
*     .. Local Scalars ..
      LOGICAL          NGIVEN
      CHARACTER        ALG, BATCH, COMUSE, CONCT, CTRL, JOB, JOBBD,
     $                 JOBCK, JOBD, JOBDA, JOBX0, METH, METHA
      INTEGER          I, ICYCLE, II, INFO, IWARN, J, L, M, N, NCYCLE,
     $                 NGIV, NOBR, NSAMPL, NSMP
      DOUBLE PRECISION RCOND, TOL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA, NMAX), B(LDB, MMAX), C(LDC, NMAX),
     $                 D(LDD, MMAX), DUM(1), DWORK(LDWORK),
     $                 R(LDR, 2*(MMAX+LMAX)*NOBRMX),
     $                 SV(LMAX*NOBRMX), U(LDU, MMAX), V(LDV, NMAX),
     $                 X0(NMAX), Y(LDY, LMAX)
      INTEGER          IWORK(LIWORK)
      LOGICAL          BWORK(1)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         IB01AD, IB01BD, IB01CD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
*     If the value of N is positive, it will be taken as system order.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) NOBR, N, M, L, NSMP, RCOND, TOL
      READ ( NIN, FMT = * ) METH, ALG, JOBD, BATCH, CONCT, CTRL, JOB,
     $                      COMUSE, JOBX0
      IF ( LSAME( BATCH, 'F' ) ) THEN
         READ ( NIN, FMT = * ) NCYCLE
      ELSE
         NCYCLE = 1
      END IF
      NSAMPL = NCYCLE*NSMP
*
      NGIVEN = N.GT.0
      IF( NGIVEN )
     $   NGIV = N
      IF ( NOBR.LE.0 .OR. NOBR.GT.NOBRMX ) THEN
         WRITE ( NOUT, FMT = 99997 ) NOBR
      ELSE IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99996 ) M
      ELSE IF ( L.LE.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) L
      ELSE IF ( NSMP.LT.0 .OR. NSMP.GT.NSMPMX .OR.
     $        ( NSMP.LT.2*( M + L + 1 )*NOBR - 1 .AND.
     $          LSAME( BATCH, 'O' ) ) .OR.
     $        ( NSAMPL.LT.2*( M + L + 1 )*NOBR - 1 .AND.
     $          LSAME( BATCH, 'L' ) ) .OR.
     $          NSMP.LT.2*NOBR .AND. ( LSAME( BATCH, 'F' ) .OR.
     $                                 LSAME( BATCH, 'I' ) ) ) THEN
         WRITE ( NOUT, FMT = 99994 ) NSMP
      ELSE IF ( NCYCLE.LE.0 .OR. NSAMPL.GT.NSMPMX ) THEN
         WRITE ( NOUT, FMT = 99993 ) NCYCLE
      ELSE IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99983 ) N
      ELSE
*        Read the matrices U and Y from the input file.
         IF ( M.GT.0 )
     $      READ ( NIN, FMT = * )
     $                         ( ( U(I,J), J = 1, M ), I = 1, NSAMPL )
         READ ( NIN, FMT = * ) ( ( Y(I,J), J = 1, L ), I = 1, NSAMPL )
*        Force some options, depending on the specifications.
         IF ( LSAME( METH, 'C' ) ) THEN
            METHA = 'M'
            JOBDA = 'N'
         ELSE
            METHA = METH
            JOBDA = JOBD
         END IF
*        The covariances and Kalman gain matrix are not computed.
         JOBCK = 'N'
         IF ( LSAME( JOB, 'A' ) .OR. LSAME( JOB, 'C' ) ) THEN
            JOBBD = 'D'
         ELSE
            JOBBD = JOB
         END IF
         IF ( LSAME( COMUSE, 'C' ) ) THEN
            JOB = 'C'
         ELSE IF ( LSAME( COMUSE, 'U' ) ) THEN
            JOB = 'A'
         END IF
*        Compute the  R  factor from a QR (or Cholesky) factorization
*        of the Hankel-like matrix (or correlation matrix).
         DO 10 ICYCLE = 1, NCYCLE
            II = ( ICYCLE - 1 )*NSMP + 1
            IF ( NCYCLE.GT.1 ) THEN
               IF ( ICYCLE.GT.1 )      BATCH = 'I'
               IF ( ICYCLE.EQ.NCYCLE ) BATCH = 'L'
            END IF
            CALL IB01AD( METHA, ALG, JOBDA, BATCH, CONCT, CTRL, NOBR, M,
     $                   L, NSMP, U(II,1), LDU, Y(II,1), LDY, N, R, LDR,
     $                   SV, RCOND, TOL, IWORK, DWORK, LDWORK, IWARN,
     $                   INFO )
   10    CONTINUE
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( IWARN.NE.0 )
     $         WRITE ( NOUT, FMT = 99990 ) IWARN
            IF( NGIVEN )
     $         N = NGIV
*           Compute the system matrices and x0.
            CALL IB01BD( METH, JOB, JOBCK, NOBR, N, M, L, NSMP, R,
     $                   LDR, A, LDA, C, LDC, B, LDB, D, LDD, DUM, 1,
     $                   DUM, 1, DUM, 1, DUM, 1, RCOND, IWORK, DWORK,
     $                   LDWORK, BWORK, IWARN, INFO )
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99982 ) INFO
            ELSE
               IF ( IWARN.NE.0 )
     $            WRITE ( NOUT, FMT = 99981 ) IWARN
               CALL IB01CD( JOBX0, COMUSE, JOBBD, N, M, L, NSMP, A, LDA,
     $                      B, LDB, C, LDC, D, LDD, U, LDU, Y, LDY, X0,
     $                      V, LDV, RCOND, IWORK, DWORK, LDWORK, IWARN,
     $                      INFO )
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99992 ) INFO
               ELSE
                  IF ( IWARN.NE.0 )
     $               WRITE ( NOUT, FMT = 99991 ) IWARN
                  IF ( LSAME( JOB, 'A' ) .OR. LSAME( JOB, 'C' ) ) THEN
                     WRITE ( NOUT, FMT = 99989 )
                     DO 20 I = 1, N
                        WRITE ( NOUT, FMT = 99988 ) ( A(I,J), J = 1,N )
   20                CONTINUE
                     WRITE ( NOUT, FMT = 99987 )
                     DO 30 I = 1, L
                        WRITE ( NOUT, FMT = 99988 ) ( C(I,J), J = 1,N )
   30                CONTINUE
                  END IF
                  IF ( LSAME( COMUSE, 'C' ) ) THEN
                     WRITE ( NOUT, FMT = 99986 )
                     DO 40 I = 1, N
                        WRITE ( NOUT, FMT = 99988 ) ( B(I,J), J = 1,M )
   40                CONTINUE
                     IF ( LSAME( JOBBD, 'D' ) ) THEN
                        WRITE ( NOUT, FMT = 99985 )
                        DO 50 I = 1, L
                           WRITE ( NOUT, FMT = 99988 )
     $                           ( D(I,J), J = 1,M )
   50                   CONTINUE
                     END IF
                  END IF
                  IF ( LSAME( JOBX0, 'X' ) ) THEN
                     WRITE ( NOUT, FMT = 99984 )
                     WRITE ( NOUT, FMT = 99988 ) ( X0(I), I = 1,N )
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
99999 FORMAT ( ' IB01CD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT ( ' INFO on exit from IB01AD = ',I2)
99997 FORMAT (/' NOBR is out of range.',/' NOBR = ', I5)
99996 FORMAT (/' M is out of range.',/' M = ', I5)
99995 FORMAT (/' L is out of range.',/' L = ', I5)
99994 FORMAT (/' NSMP is out of range.',/' NSMP = ', I5)
99993 FORMAT (/' NCYCLE is out of range.',/' NCYCLE = ', I5)
99992 FORMAT ( ' INFO on exit from IB01CD = ',I2)
99991 FORMAT ( ' IWARN on exit from IB01CD = ',I2)
99990 FORMAT ( ' IWARN on exit from IB01AD = ',I2)
99989 FORMAT (/' The system state matrix A is ')
99988 FORMAT (20(1X,F8.4))
99987 FORMAT (/' The system output matrix C is ')
99986 FORMAT (/' The system input matrix B is ')
99985 FORMAT (/' The system input-output matrix D is ')
99984 FORMAT (/' The initial state vector x0 is ')
99983 FORMAT (/' N is out of range.',/' N = ', I5)
99982 FORMAT ( ' INFO on exit from IB01BD = ',I2)
99981 FORMAT ( ' IWARN on exit from IB01BD = ',I2)
      END
