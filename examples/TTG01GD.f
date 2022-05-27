*     TG01GD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          LMAX, NMAX, MMAX, PMAX
      PARAMETER        ( LMAX = 20, NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDD, LDE
      PARAMETER        ( LDA = LMAX, LDB = LMAX, LDC = PMAX,
     $                   LDD = PMAX, LDE = LMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MIN( LMAX, NMAX ) +
     $                            MAX( 3*NMAX - 1, MMAX, LMAX ) +
     $                            2*LMAX*NMAX + LMAX*MMAX + PMAX*NMAX )
*     .. Local Scalars ..
      CHARACTER*1      JOBS
      INTEGER          I, INFO, INFRED, J, L, LR, M, N, NR, P, RANKE
      DOUBLE PRECISION TOL
*     .. Local Arrays ..
      INTEGER          IWORK(NMAX)
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX),   C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK), E(LDE,NMAX)
*     .. External Subroutines ..
      EXTERNAL         TG01GD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) L, N, M, P, JOBS, TOL
      IF ( L.LT.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) L
      ELSE
         IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
            WRITE ( NOUT, FMT = 99988 ) N
         ELSE
            READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,L )
            READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,L )
            IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
               WRITE ( NOUT, FMT = 99987 ) M
            ELSE
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,L )
               IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
                  WRITE ( NOUT, FMT = 99986 ) P
               ELSE
                  READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
                  READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*                 Find the reduced descriptor system
*                 (A-lambda E,B,C,D).
                  CALL TG01GD( JOBS, L, N, M, P, A, LDA, E, LDE, B, LDB,
     $                         C, LDC, D, LDD, LR, NR, RANKE, INFRED,
     $                         TOL, IWORK, DWORK, LDWORK, INFO )
*
                  IF ( INFO.NE.0 ) THEN
                     WRITE ( NOUT, FMT = 99998 ) INFO
                  ELSE
                     WRITE ( NOUT, FMT = 99994 ) RANKE
                     WRITE ( NOUT, FMT = 99997 )
                     DO 10 I = 1, LR
                        WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,NR )
   10                CONTINUE
                     WRITE ( NOUT, FMT = 99996 )
                     DO 20 I = 1, LR
                        WRITE ( NOUT, FMT = 99995 ) ( E(I,J), J = 1,NR )
   20                CONTINUE
                     WRITE ( NOUT, FMT = 99993 )
                     DO 30 I = 1, LR
                        WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   30                CONTINUE
                     WRITE ( NOUT, FMT = 99992 )
                     DO 40 I = 1, P
                        WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,NR )
   40                CONTINUE
                     WRITE ( NOUT, FMT = 99991 )
                     DO 50 I = 1, P
                        WRITE ( NOUT, FMT = 99995 ) ( D(I,J), J = 1,M )
   50                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TG01GD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TG01GD = ',I2)
99997 FORMAT (/' The reduced state dynamics matrix is ')
99996 FORMAT (/' The reduced descriptor matrix is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (' Rank of matrix E   =', I5)
99993 FORMAT (/' The reduced input/state matrix is ')
99992 FORMAT (/' The reduced state/output matrix is ')
99991 FORMAT (/' The transformed feedthrough matrix is ')
99989 FORMAT (/' L is out of range.',/' L = ',I5)
99988 FORMAT (/' N is out of range.',/' N = ',I5)
99987 FORMAT (/' M is out of range.',/' M = ',I5)
99986 FORMAT (/' P is out of range.',/' P = ',I5)
      END
