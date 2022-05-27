*     AB09JD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX, NVMAX, NVPMAX, NWMAX, NWMMAX, PMAX
      PARAMETER        ( MMAX = 20, NMAX = 20, NVMAX = 10, NWMAX = 10,
     $                   PMAX = 20, NVPMAX = NVMAX + PMAX,
     $                   NWMMAX = NWMAX + MMAX )
      INTEGER          LDA, LDAV, LDAW, LDB, LDBV, LDBW,
     $                 LDC, LDCV, LDCW, LDD, LDDV, LDDW
      PARAMETER        ( LDA = NMAX, LDAV = NVMAX, LDAW = NWMAX,
     $                   LDB = NMAX, LDBV = NVMAX, LDBW = NWMAX,
     $                   LDC = PMAX, LDCV = PMAX,  LDCW = MMAX,
     $                   LDD = PMAX, LDDV = PMAX,  LDDW = MMAX )
      INTEGER          LIW1, LIW2, LIW3, LIWORK
      PARAMETER        ( LIW1 = 2*MAX( MMAX, PMAX ),
     $                   LIW2 = MAX( NVPMAX, NWMMAX ) + NMAX + 6,
     $                   LIW3 = MAX( 2*NVMAX + PMAX + 2,
     $                               2*NWMAX + MMAX + 2 ) )
      PARAMETER        ( LIWORK = MAX( LIW1, LIW2, LIW3 ) )
      INTEGER          LDW1, LDW2, LDW3, LDW4, LDWORK
      PARAMETER        ( LDW1 = 2*NVPMAX*( NVPMAX + PMAX ) + PMAX*PMAX +
     $                          MAX( 2*NVPMAX*NVPMAX +
     $                               MAX( 11*NVPMAX + 16, PMAX*NVPMAX ),
     $                               NVPMAX*NMAX +
     $                               MAX( NVPMAX*NMAX + NMAX*NMAX,
     $                                    PMAX*NMAX, PMAX*MMAX ) ) )
      PARAMETER        ( LDW2 = 2*NWMMAX*( NWMMAX + MMAX ) + MMAX*MMAX +
     $                          MAX( 2*NWMMAX*NWMMAX +
     $                               MAX( 11*NWMMAX + 16, MMAX*NWMMAX ),
     $                               NWMMAX*NMAX +
     $                               MAX( NWMMAX*NMAX + NMAX*NMAX,
     $                                    MMAX*NMAX, PMAX*MMAX ) ) )
      PARAMETER        ( LDW3 = NMAX*( 2*NMAX + MAX( NMAX, MMAX, PMAX )
     $                                 + 5 ) + ( NMAX*( NMAX + 1 ) )/2 )
      PARAMETER        ( LDW4 = NMAX*( MMAX + PMAX + 2 ) + 2*MMAX*PMAX +
     $                          MIN( NMAX, MMAX ) +
     $                          MAX( 3*MMAX + 1,
     $                               MIN( NMAX, MMAX ) + PMAX ) )
      PARAMETER        ( LDWORK = MAX( LDW1, LDW2, LDW3, LDW4 ) )
*     .. Local Scalars ..
      LOGICAL          LEFTW, RIGHTW
      DOUBLE PRECISION ALPHA, TOL1, TOL2
      INTEGER          I, INFO, IWARN, J, M, N, NR, NS, NV, NW, P
      CHARACTER*1      DICO, EQUIL, JOBINV, JOBV, JOBW, ORDSEL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), AV(LDAV,NVMAX), AW(LDAW,NWMAX),
     $                 B(LDB,MMAX), BV(LDBV,PMAX),  BW(LDBW,MMAX),
     $                 C(LDC,NMAX), CV(LDCV,NVMAX), CW(LDCW,NWMAX),
     $                 D(LDD,MMAX), DV(LDDV,PMAX),  DW(LDDW,MMAX),
     $                 DWORK(LDWORK), HSV(NMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         AB09JD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, NV, NW, NR, ALPHA, TOL1, TOL2,
     $                      JOBV, JOBW, JOBINV, DICO, EQUIL, ORDSEL
      LEFTW  = .NOT.LSAME( JOBV, 'N' )
      RIGHTW = .NOT.LSAME( JOBW, 'N' )
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF( P.LE.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99988 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
               IF( LEFTW ) THEN
                  IF( NV.LT.0 .OR. NV.GT.NVMAX ) THEN
                     WRITE ( NOUT, FMT = 99986 ) NV
                  ELSE
                     IF( NV.GT.0 ) THEN
                        READ ( NIN, FMT = * )
     $                     ( ( AV(I,J), J = 1,NV ), I = 1,NV )
                        READ ( NIN, FMT = * )
     $                     ( ( BV(I,J), J = 1,P  ), I = 1,NV )
                        READ ( NIN, FMT = * )
     $                     ( ( CV(I,J), J = 1,NV ), I = 1,P )
                     END IF
                     IF( LEFTW )
     $                  READ ( NIN, FMT = * )
     $                     ( ( DV(I,J), J = 1,P ), I = 1,P )
                  END IF
               END IF
               IF( RIGHTW ) THEN
                  IF( NW.LT.0 .OR. NW.GT.NWMAX ) THEN
                     WRITE ( NOUT, FMT = 99985 ) NW
                  ELSE
                     IF( NW.GT.0 ) THEN
                        READ ( NIN, FMT = * )
     $                     ( ( AW(I,J), J = 1,NW ), I = 1,NW )
                        READ ( NIN, FMT = * )
     $                     ( ( BW(I,J), J = 1,M  ), I = 1,NW )
                        READ ( NIN, FMT = * )
     $                     ( ( CW(I,J), J = 1,NW ), I = 1,M )
                     END IF
                     READ ( NIN, FMT = * )
     $                     ( ( DW(I,J), J = 1,M  ), I = 1,M )
                  END IF
               END IF
*              Find a reduced ssr for (A,B,C,D).
               CALL AB09JD( JOBV, JOBW, JOBINV, DICO, EQUIL, ORDSEL, N,
     $                      NV, NW, M, P, NR, ALPHA,  A, LDA, B, LDB,
     $                      C, LDC, D, LDD, AV, LDAV, BV, LDBV,
     $                      CV, LDCV, DV, LDDV, AW, LDAW, BW, LDBW,
     $                      CW, LDCW, DW, LDDW, NS, HSV, TOL1, TOL2,
     $                      IWORK, DWORK, LDWORK, IWARN, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF( IWARN.NE.0 ) WRITE ( NOUT, FMT = 99994 ) IWARN
                  WRITE ( NOUT, FMT = 99997 ) NR
                  WRITE ( NOUT, FMT = 99987 )
                  WRITE ( NOUT, FMT = 99995 ) ( HSV(J), J = 1, NS )
                  IF( NR.GT.0 ) THEN
                     WRITE ( NOUT, FMT = 99996 )
                     DO 20 I = 1, NR
                        WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,NR )
   20                CONTINUE
                     WRITE ( NOUT, FMT = 99993 )
                     DO 40 I = 1, NR
                        WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   40                CONTINUE
                     WRITE ( NOUT, FMT = 99992 )
                     DO 60 I = 1, P
                        WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,NR )
   60                CONTINUE
                  END IF
                  WRITE ( NOUT, FMT = 99991 )
                  DO 70 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( D(I,J), J = 1,M )
   70             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB09JD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB09JD = ',I2)
99997 FORMAT (/' The order of reduced model = ',I2)
99996 FORMAT (/' The reduced state dynamics matrix Ar is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (' IWARN on exit from AB09JD = ',I2)
99993 FORMAT (/' The reduced input/state matrix Br is ')
99992 FORMAT (/' The reduced state/output matrix Cr is ')
99991 FORMAT (/' The reduced input/output matrix Dr is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
99987 FORMAT (/' The Hankel singular values of weighted ALPHA-stable',
     $         ' part are')
99986 FORMAT (/' NV is out of range.',/' NV = ',I5)
99985 FORMAT (/' NW is out of range.',/' NW = ',I5)
      END
