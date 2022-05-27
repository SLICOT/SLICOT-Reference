*     FB01SD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDAINV, LDB, LDC, LDQINV, LDRINV, LDSINV
      PARAMETER        ( LDAINV = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDQINV = MMAX, LDRINV = PMAX, LDSINV = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX*(NMAX + 2*MMAX) + 3*MMAX,
     $                                (NMAX + PMAX)*(NMAX + 1) + 2*NMAX,
     $                                 3*NMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, ISTEP, J, M, N, P
      CHARACTER*1      JOBX, MULTAB, MULTRC
*     .. Local Arrays ..
      DOUBLE PRECISION AINV(LDAINV,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 DIAG(MMAX), DWORK(LDWORK), E(PMAX),
     $                 QINV(LDQINV,MMAX), RINV(LDRINV,PMAX),
     $                 RINVY(PMAX), SINV(LDSINV,NMAX), X(NMAX), Z(MMAX)
      INTEGER          IWORK(NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         DCOPY, FB01SD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, JOBX, TOL, MULTAB, MULTRC
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( AINV(I,J), J = 1,N ), I = 1,N )
         IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) P
         ELSE
            READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
            IF ( LSAME( MULTRC, 'N' ) ) READ ( NIN, FMT = * )
     $                               ( ( RINV(I,J), J = 1,P ), I = 1,P )
            IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
               WRITE ( NOUT, FMT = 99992 ) M
            ELSE
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
               READ ( NIN, FMT = * ) ( ( QINV(I,J), J = 1,M ), I = 1,M )
               READ ( NIN, FMT = * ) ( ( SINV(I,J), J = 1,N ), I = 1,N )
               READ ( NIN, FMT = * ) ( Z(J), J = 1,M )
               READ ( NIN, FMT = * ) ( X(J), J = 1,N )
               READ ( NIN, FMT = * ) ( RINVY(J), J = 1,P )
*              Save the strict upper triangle of QINV in its strict
*              lower triangle and the diagonal in the array DIAG.
               DO 10 I = 2, M
                  CALL DCOPY( I, QINV(1,I), 1, QINV(I,1), LDQINV )
   10          CONTINUE
               CALL DCOPY( M, QINV, LDQINV+1, DIAG, 1 )
*              Perform three iterations of the (Kalman) filter recursion
*              (in square root information form).
               ISTEP = 1
   20          CONTINUE
                  CALL FB01SD( JOBX, MULTAB, MULTRC, N, M, P, SINV,
     $                         LDSINV, AINV, LDAINV, B, LDB, RINV,
     $                         LDRINV, C, LDC, QINV, LDQINV, X, RINVY,
     $                         Z, E, TOL, IWORK, DWORK, LDWORK, INFO )
                  ISTEP = ISTEP + 1
                  IF ( INFO.EQ.0 .AND. ISTEP.LE.3 ) THEN
*                    Restore the upper triangle of QINV.
                     DO 30 I = 2, M
                        CALL DCOPY( I, QINV(I,1), LDQINV, QINV(1,I), 1 )
   30                CONTINUE
                     CALL DCOPY( M, DIAG, 1, QINV, LDQINV+1 )
                     GO TO 20
                  END IF
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99996 ) ( SINV(I,J), J = 1,N )
   40             CONTINUE
                  IF ( LSAME( JOBX, 'X' ) ) THEN
                     WRITE ( NOUT, FMT = 99995 )
                     DO 50 I = 1, N
                        WRITE ( NOUT, FMT = 99994 ) I, X(I)
   50                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' FB01SD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from FB01SD = ',I2)
99997 FORMAT (' The inverse of the square root of the state covariance',
     $       ' matrix is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The components of the estimated filtered state are ',
     $       //'   k       X(k)',/)
99994 FORMAT (I4,3X,F8.4)
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' P is out of range.',/' P = ',I5)
      END
