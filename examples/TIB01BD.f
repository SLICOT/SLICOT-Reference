*     IB01BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          LDA, LDB, LDC, LDD, LDK, LDQ, LDR, LDRY, LDS,
     $                 LDU, LDW1, LDW2, LDW3, LDWORK, LDY, LIWORK, LMAX,
     $                 MMAX, NMAX, NOBRMX, NSMPMX
      PARAMETER        ( LMAX = 5, MMAX = 5, NOBRMX = 20, NSMPMX = 2000,
     $                   NMAX = NOBRMX - 1, LDA = NMAX, LDB = NMAX,
     $                   LDC  = LMAX, LDD  = LMAX, LDK = NMAX,
     $                   LDQ  = NMAX, LDRY = LMAX, LDS = NMAX,
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
     $                   LDW3 = MAX( 4*NMAX*NMAX + 2*NMAX*LMAX +
     $                               LMAX*LMAX +
     $                               MAX( 3*LMAX, NMAX*LMAX ),
     $                               14*NMAX*NMAX + 12*NMAX + 5 ),
     $                   LDWORK = MAX( 6*( MMAX + LMAX )*NOBRMX,
     $                                 ( MMAX + LMAX )*( 4*NOBRMX*
     $                                 ( MMAX + LMAX + 2 ) - 2 ),
     $                                 ( MMAX + LMAX )*4*NOBRMX*
     $                                 ( NOBRMX + 1 ), LDW1, LDW2,
     $                                 LDW3 ),
     $                   LDY = NSMPMX,
     $                   LIWORK = MAX( ( MMAX + LMAX )*NOBRMX,
     $                                 MMAX*NOBRMX + NMAX, LMAX*NOBRMX,
     $                                 MMAX*( NMAX + LMAX ), NMAX*NMAX )
     $                 )
*     .. Local Scalars ..
      LOGICAL          NGIVEN
      CHARACTER        ALG, BATCH, CONCT, CTRL, JOB, JOBCK, JOBD, JOBDA,
     $                 METH, METHA
      INTEGER          I, ICYCLE, II, INFO, IWARN, J, L, M, N, NCYCLE,
     $                 NGIV, NOBR, NSAMPL, NSMP
      DOUBLE PRECISION RCOND, TOL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA, NMAX), B(LDB, MMAX), C(LDC, NMAX),
     $                 D(LDD, MMAX), DWORK(LDWORK), K(LDK, LMAX),
     $                 Q(LDQ, NMAX), R(LDR, 2*(MMAX+LMAX)*NOBRMX),
     $                 RY(LDRY, LMAX), S(LDS, LMAX), SV(LMAX*NOBRMX),
     $                 U(LDU, MMAX), Y(LDY, LMAX)
      INTEGER          IWORK(LIWORK)
      LOGICAL          BWORK(2*NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         IB01AD, IB01BD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
*     If the value of N is positive, it will be taken as system order.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) NOBR, N, M, L, NSMP, RCOND, TOL
      READ ( NIN, FMT = * ) METH, ALG, JOBD, BATCH, CONCT, CTRL, JOB,
     $                      JOBCK
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
         WRITE ( NOUT, FMT = 99980 ) N
      ELSE
*        Read the matrices U and Y from the input file.
         IF ( M.GT.0 )
     $      READ ( NIN, FMT = * )
     $                         ( ( U(I,J), J = 1, M ), I = 1, NSAMPL )
         READ ( NIN, FMT = * ) ( ( Y(I,J), J = 1, L ), I = 1, NSAMPL )
*        Read A and C matrices, if METH <> 'M' and JOB = 'B' or 'D'.
         IF ( .NOT.LSAME( METH, 'M' ) .AND.
     $           ( LSAME( JOB,  'B' ) .OR. LSAME( JOB, 'D' ) ) ) THEN
            DO 10 I = 1, N
               READ ( NIN, FMT = * ) ( A(I,J), J = 1, N )
   10       CONTINUE
            DO 20 I = 1, L
               READ ( NIN, FMT = * ) ( C(I,J), J = 1, N )
   20       CONTINUE
         END IF
*        Force some options for IB01AD, depending on the specifications.
         IF ( LSAME( METH, 'C' ) ) THEN
            METHA = 'M'
            JOBDA = 'N'
         ELSE
            METHA = METH
            JOBDA = JOBD
         END IF
*        Compute the  R  factor from a QR (or Cholesky) factorization
*        of the Hankel-like matrix (or correlation matrix).
         DO 30 ICYCLE = 1, NCYCLE
            II = ( ICYCLE - 1 )*NSMP + 1
            IF ( NCYCLE.GT.1 ) THEN
               IF ( ICYCLE.GT.1 )      BATCH = 'I'
               IF ( ICYCLE.EQ.NCYCLE ) BATCH = 'L'
            END IF
            CALL IB01AD( METHA, ALG, JOBDA, BATCH, CONCT, CTRL, NOBR, M,
     $                   L, NSMP, U(II,1), LDU, Y(II,1), LDY, N, R, LDR,
     $                   SV, RCOND, TOL, IWORK, DWORK, LDWORK, IWARN,
     $                   INFO )
   30    CONTINUE
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( IWARN.NE.0 )
     $         WRITE ( NOUT, FMT = 99990 ) IWARN
            IF( NGIVEN )
     $         N = NGIV
*           Compute the system matrices.
            CALL IB01BD( METH, JOB, JOBCK, NOBR, N, M, L, NSMP, R,
     $                   LDR, A, LDA, C, LDC, B, LDB, D, LDD, Q, LDQ,
     $                   RY, LDRY, S, LDS, K, LDK, RCOND, IWORK, DWORK,
     $                   LDWORK, BWORK, IWARN, INFO )
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99992 ) INFO
            ELSE
               IF ( IWARN.NE.0 )
     $            WRITE ( NOUT, FMT = 99991 ) IWARN
               IF ( LSAME( JOB, 'A' ) .OR. LSAME( JOB, 'C' ) ) THEN
                  WRITE ( NOUT, FMT = 99989 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99988 ) ( A(I,J), J = 1,N )
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99987 )
                  DO 50 I = 1, L
                     WRITE ( NOUT, FMT = 99988 ) ( C(I,J), J = 1,N )
   50             CONTINUE
               END IF
               IF ( .NOT.LSAME( JOB, 'C' ) ) THEN
                  WRITE ( NOUT, FMT = 99986 )
                  DO 60 I = 1, N
                     WRITE ( NOUT, FMT = 99988 ) ( B(I,J), J = 1,M )
   60             CONTINUE
               END IF
               IF ( LSAME( JOB, 'A' ) .OR. LSAME( JOB, 'D' ) ) THEN
                  WRITE ( NOUT, FMT = 99985 )
                  DO 70 I = 1, L
                     WRITE ( NOUT, FMT = 99988 ) ( D(I,J), J = 1,M )
   70             CONTINUE
               END IF
               IF ( LSAME( JOBCK, 'K' ) ) THEN
                  WRITE ( NOUT, FMT = 99984 )
                  DO 80 I = 1, N
                     WRITE ( NOUT, FMT = 99988 ) ( K(I,J), J = 1,L )
   80             CONTINUE
               END IF
               IF ( .NOT.LSAME( JOBCK, 'N' ) ) THEN
                  WRITE ( NOUT, FMT = 99983 )
                  DO 90 I = 1, N
                     WRITE ( NOUT, FMT = 99988 ) ( Q(I,J), J = 1,N )
   90             CONTINUE
                  WRITE ( NOUT, FMT = 99982 )
                  DO 100 I = 1, L
                     WRITE ( NOUT, FMT = 99988 ) ( RY(I,J), J = 1,L )
  100             CONTINUE
                  WRITE ( NOUT, FMT = 99981 )
                  DO 110 I = 1, N
                     WRITE ( NOUT, FMT = 99988 ) ( S(I,J), J = 1,L )
  110             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
99999 FORMAT ( ' IB01BD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT ( ' INFO on exit from IB01AD = ',I2)
99997 FORMAT (/' NOBR is out of range.',/' NOBR = ', I5)
99996 FORMAT (/' M is out of range.',/' M = ', I5)
99995 FORMAT (/' L is out of range.',/' L = ', I5)
99994 FORMAT (/' NSMP is out of range.',/' NSMP = ', I5)
99993 FORMAT (/' NCYCLE is out of range.',/' NCYCLE = ', I5)
99992 FORMAT ( ' INFO on exit from IB01BD = ',I2)
99991 FORMAT ( ' IWARN on exit from IB01BD = ',I2)
99990 FORMAT ( ' IWARN on exit from IB01AD = ',I2)
99989 FORMAT (/' The system state matrix A is ')
99988 FORMAT (20(1X,F8.4))
99987 FORMAT (/' The system output matrix C is ')
99986 FORMAT (/' The system input matrix B is ')
99985 FORMAT (/' The system input-output matrix D is ')
99984 FORMAT (/' The Kalman gain matrix K is ')
99983 FORMAT (/' The state covariance matrix Q is ')
99982 FORMAT (/' The output covariance matrix Ry is ')
99981 FORMAT (/' The state-output cross-covariance matrix S is ')
99980 FORMAT (/' N is out of range.',/' N = ', I5)
      END
