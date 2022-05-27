*     FB01RD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDK, LDQ, LDR, LDS
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX, LDK = NMAX,
     $                   LDQ = MMAX, LDR = PMAX, LDS = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX*(PMAX+NMAX+1),
     $                                 NMAX*(PMAX+NMAX)+2*PMAX,
     $                                 NMAX*(NMAX+MMAX+2), 3*PMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, ISTEP, J, M, N, P
      CHARACTER*1      JOBK, MULTBQ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 DIAG(PMAX), DWORK(LDWORK), K(LDK,PMAX),
     $                 Q(LDQ,MMAX), R(LDR,PMAX), S(LDS,NMAX)
      INTEGER          IWORK(PMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         DCOPY, FB01RD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, JOBK, TOL, MULTBQ
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( S(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF ( LSAME( MULTBQ, 'N' ) ) READ ( NIN, FMT = * )
     $                               ( ( Q(I,J), J = 1,M ), I = 1,M )
            IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99992 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( R(I,J), J = 1,P ), I = 1,P )
*              Save the strict lower triangle of R in its strict upper
*              triangle and the diagonal in the array DIAG.
               DO 10 I = 2, P
                  CALL DCOPY( I, R(I,1), LDR, R(1,I), 1 )
   10          CONTINUE
               CALL DCOPY( P, R, LDR+1, DIAG, 1 )
*              Perform three iterations of the (Kalman) filter recursion
*              (in square root covariance form).
               ISTEP = 1
   20          CONTINUE
                  CALL FB01RD( JOBK, MULTBQ, N, M, P, S, LDS, A, LDA,
     $                         B, LDB, Q, LDQ, C, LDC, R, LDR, K, LDK,
     $                         TOL, IWORK, DWORK, LDWORK, INFO )
                  ISTEP = ISTEP + 1
                  IF ( INFO.EQ.0 .AND. ISTEP.LE.3 ) THEN
*                    Restore the lower triangle of R.
                     DO 30 I = 2, P
                        CALL DCOPY( I, R(1,I), 1, R(I,1), LDR )
   30                CONTINUE
                     CALL DCOPY( P, DIAG, 1, R, LDR+1 )
                     GO TO 20
                  END IF
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99995 ) ( S(I,J), J = 1,N )
   40             CONTINUE
                  IF ( LSAME( JOBK, 'K' ) ) THEN
                     WRITE ( NOUT, FMT = 99996 )
                     DO 60 I = 1, N
                        WRITE ( NOUT, FMT = 99995 ) ( K(I,J), J = 1,P )
   60                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' FB01RD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from FB01QD = ',I2)
99997 FORMAT (' The square root of the state covariance matrix is ')
99996 FORMAT (/' The Kalman gain matrix is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' M is out of range.',/' M = ',I5)
99992 FORMAT (/' P is out of range.',/' P = ',I5)
      END
