*     FB01VD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, LMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, LMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDK, LDP, LDQ, LDR
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = LMAX, LDK = NMAX,
     $                   LDP = NMAX, LDQ = MMAX, LDR = LMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( LMAX*NMAX + 3*LMAX, NMAX*NMAX,
     $                                 MMAX*NMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, J, L, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 DWORK(LDWORK), K(LDK,LMAX), P(LDP,NMAX),
     $                 Q(LDQ,MMAX), R(LDR,LMAX)
      INTEGER          IWORK(LMAX)
*     .. External Subroutines ..
      EXTERNAL         DCOPY, FB01VD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, L, TOL
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( P(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( Q(I,J), J = 1,M ), I = 1,M )
            IF ( L.LE.0 .OR. L.GT.LMAX ) THEN
               WRITE ( NOUT, FMT = 99991 ) L
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,L )
               READ ( NIN, FMT = * ) ( ( R(I,J), J = 1,L ), I = 1,L )
*              Perform one iteration of the (Kalman) filter recursion.
               CALL FB01VD( N, M, L, P, LDP, A, LDA, B, LDB, C, LDC,
     $                      Q, LDQ, R, LDR, K, LDK, TOL, IWORK, DWORK,
     $                      LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 )
                  DO 20 I = 1, N
                     CALL DCOPY( I-1, P(1,I), 1, P(I,1), LDP )
                     WRITE ( NOUT, FMT = 99994 ) ( P(I,J), J = 1,N )
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99996 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99994 ) ( K(I,J), J = 1,L )
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99995 )
                  DO 60 I = 1, L
                     WRITE ( NOUT, FMT = 99994 ) ( R(I,J), J = 1,L )
   60             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' FB01VD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from FB01VD = ',I3)
99997 FORMAT (' The state covariance matrix is ')
99996 FORMAT (/' The Kalman filter gain matrix is ')
99995 FORMAT (/' The square root of the covariance matrix of the innov',
     $          'ations is ')
99994 FORMAT (20(1X,F8.4))
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' L is out of range.',/' P = ',I5)
      END
