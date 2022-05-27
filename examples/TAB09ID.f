*     AB09ID EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, MWMAX, NMAX, NVMAX, NWMAX, PMAX, PVMAX
      PARAMETER        ( MMAX = 20, MWMAX = 20,
     $                   NMAX = 20, NVMAX = 20, NWMAX = 20,
     $                   PMAX = 20, PVMAX = 20 )
      INTEGER          LDA, LDAV, LDAW, LDB, LDBV, LDBW,
     $                 LDC, LDCV, LDCW, LDD, LDDV, LDDW
      PARAMETER        ( LDA = NMAX, LDAV = NVMAX, LDAW = NWMAX,
     $                   LDB = NMAX, LDBV = NVMAX, LDBW = NWMAX,
     $                   LDC = PMAX, LDCV = PVMAX, LDCW = MMAX,
     $                   LDD = PMAX, LDDV = PVMAX, LDDW = MMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MAX( 2*NMAX,
     $                                 NVMAX + MAX( PMAX, PVMAX ),
     $                                 NWMAX + MAX( MMAX, MWMAX ) ) )
      INTEGER          LDW1, LDW2, LDW3, LDW4, LDW5, LDW6, LDW7, LDW8,
     $                 LDWORK
      PARAMETER        ( LDW1 = NMAX + NVMAX, LDW2 = NMAX + NWMAX,
     $                   LDW3 = MAX( LDW1*( LDW1 + MAX( LDW1, PVMAX ) +
     $                               5 ), NMAX*( PMAX + 5 ) ),
     $                   LDW4 = MAX( LDW2*( LDW2 + MAX( LDW2, MWMAX ) +
     $                               5 ), NMAX*( MMAX + 5 ) ),
     $                   LDW5 = PVMAX*( NVMAX + PVMAX ) + PVMAX*NVMAX +
     $                          MAX( NVMAX*( NVMAX + 5 ), 4*PVMAX,
     $                               PVMAX*( PVMAX + 2 ), 4*PMAX ),
     $                   LDW6 = MAX( PMAX, PVMAX )*( 2*NVMAX +
     $                               MAX( PMAX, PVMAX ) ) +
     $                               MAX( LDW5, NVMAX +
     $                                    MAX( NVMAX, 3*PMAX, 3*PVMAX )
     $                                       ),
     $                   LDW7 = MAX( NWMAX + MAX( NWMAX, 3*MMAX ),
     $                               2*NWMAX*MAX( MMAX, MWMAX ) +
     $                               NWMAX + MAX( NWMAX, 3*MMAX,
     $                                                   3*MWMAX ) ),
     $                   LDW8 = MWMAX*( NWMAX + MWMAX ) +
     $                          MAX( NWMAX*( NWMAX + 5 ), 4*MWMAX,
     $                               MWMAX*( MWMAX + 2 ), 4*MMAX ) )
      PARAMETER        ( LDWORK = MAX( LDW6, LDW7, LDW8,
     $                                 2*NMAX*NMAX +
     $                                   MAX( 1, LDW3, LDW4,
     $                                        2*NMAX*NMAX + 5*NMAX,
     $                                        NMAX*MAX( MMAX, PMAX ) ) )
     $                  )
*     .. Local Scalars ..
      LOGICAL          LEFTW, RIGHTW
      DOUBLE PRECISION ALPHA, ALPHAC, ALPHAO, TOL1, TOL2
      INTEGER          I, INFO, IWARN, J, M, MW, N, NR, NS, NV, NW, P,
     $                 PV
      CHARACTER*1      DICO, EQUIL, JOB, JOBC, JOBO, ORDSEL, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), AV(LDAV,NVMAX), AW(LDAW,NWMAX),
     $                 B(LDB,MMAX), BV(LDBV,PMAX),  BW(LDBW,MWMAX),
     $                 C(LDC,NMAX), CV(LDCV,NVMAX), CW(LDCW,NWMAX),
     $                 D(LDD,MMAX), DV(LDDV,PMAX),  DW(LDDW,MWMAX),
     $                 DWORK(LDWORK), HSV(NMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         AB09ID
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, NV, PV, NW, MW, NR,
     $                      ALPHA, ALPHAC, ALPHAO, TOL1, TOL2,
     $                      DICO, JOBC, JOBO, JOB, WEIGHT,
     $                      EQUIL, ORDSEL
      LEFTW  = LSAME( WEIGHT, 'L' ) .OR. LSAME( WEIGHT, 'B' )
      RIGHTW = LSAME( WEIGHT, 'R' ) .OR. LSAME( WEIGHT, 'B' )
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1, N )
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
     $                    ( ( AV(I,J), J = 1,NV ), I = 1,NV )
                        READ ( NIN, FMT = * )
     $                    ( ( BV(I,J), J = 1,P ),  I = 1,NV )
                        IF( PV.LE.0 .OR. PV.GT.PVMAX ) THEN
                           WRITE ( NOUT, FMT = 99985 ) PV
                        ELSE
                           READ ( NIN, FMT = * )
     $                       ( ( CV(I,J), J = 1,NV ), I = 1,PV )
                        END IF
                     END IF
                     IF( PV.LE.0 .OR. PV.GT.PVMAX ) THEN
                        WRITE ( NOUT, FMT = 99985 ) PV
                     ELSE
                        READ ( NIN, FMT = * )
     $                    ( ( DV(I,J), J = 1,P ), I = 1,PV )
                     END IF
                  END IF
               END IF
               IF( RIGHTW ) THEN
                  IF( NW.LT.0 .OR. NW.GT.NWMAX ) THEN
                     WRITE ( NOUT, FMT = 99984 ) NW
                  ELSE
                     IF( NW.GT.0 ) THEN
                        READ ( NIN, FMT = * )
     $                    ( ( AW(I,J), J = 1,NW ), I = 1,NW )
                        IF( MW.LE.0 .OR. MW.GT.MWMAX ) THEN
                           WRITE ( NOUT, FMT = 99983 ) MW
                        ELSE
                           READ ( NIN, FMT = * )
     $                       ( ( BW(I,J), J = 1,MW ), I = 1,NW )
                        END IF
                        READ ( NIN, FMT = * )
     $                    ( ( CW(I,J), J = 1,NW ), I = 1,M )
                     END IF
                     IF( MW.LE.0 .OR. MW.GT.MWMAX ) THEN
                        WRITE ( NOUT, FMT = 99983 ) MW
                     ELSE
                        READ ( NIN, FMT = * )
     $                     ( ( DW(I,J), J = 1,MW ), I = 1,M )
                     END IF
                  END IF
               END IF
*              Find a reduced ssr for (A,B,C,D).
               CALL AB09ID( DICO, JOBC, JOBO, JOB, WEIGHT, EQUIL,
     $                      ORDSEL, N, M, P, NV, PV, NW, MW, NR, ALPHA,
     $                      ALPHAC, ALPHAO, A, LDA, B, LDB, C, LDC, D,
     $                      LDD, AV, LDAV, BV, LDBV, CV, LDCV, DV, LDDV,
     $                      AW, LDAW, BW, LDBW, CW, LDCW, DW, LDDW,
     $                      NS, HSV, TOL1, TOL2, IWORK, DWORK, LDWORK,
     $                      IWARN, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF( IWARN.NE.0) WRITE ( NOUT, FMT = 99982 ) IWARN
                  WRITE ( NOUT, FMT = 99997 ) NR
                  WRITE ( NOUT, FMT = 99987 )
                  WRITE ( NOUT, FMT = 99995 ) ( HSV(J), J = 1, NS )
                  IF( NR.GT.0 ) WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,NR )
   20             CONTINUE
                  IF( NR.GT.0 ) WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   40             CONTINUE
                  IF( NR.GT.0 ) WRITE ( NOUT, FMT = 99992 )
                  DO 60 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,NR )
   60             CONTINUE
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
99999 FORMAT (' AB09ID EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB09ID = ',I2)
99997 FORMAT (/' The order of reduced model = ',I2)
99996 FORMAT (/' The reduced state dynamics matrix Ar is ')
99995 FORMAT (20(1X,F8.4))
99993 FORMAT (/' The reduced input/state matrix Br is ')
99992 FORMAT (/' The reduced state/output matrix Cr is ')
99991 FORMAT (/' The reduced input/output matrix Dr is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
99987 FORMAT (/' The Hankel singular values of weighted ALPHA-stable',
     $         ' part are')
99986 FORMAT (/' NV is out of range.',/' NV = ',I5)
99985 FORMAT (/' PV is out of range.',/' PV = ',I5)
99984 FORMAT (/' NW is out of range.',/' NW = ',I5)
99983 FORMAT (/' MW is out of range.',/' MW = ',I5)
99982 FORMAT (' IWARN on exit from AB09ID = ',I2)
      END
