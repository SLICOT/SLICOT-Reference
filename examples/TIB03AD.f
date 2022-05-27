*     IB03AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER           NIN, NOUT
      PARAMETER         ( NIN = 5, NOUT = 6 )
      INTEGER           LDU, LDY, LIWORK, LMAX, MMAX, NMAX, NNMAX,
     $                  NOBRMX, NSMPMX
      PARAMETER         ( LMAX = 2, MMAX = 3, NOBRMX = 10, NNMAX = 12,
     $                    NMAX = 4, NSMPMX = 1024,
     $                    LDU  = NSMPMX, LDY = NSMPMX,
     $                    LIWORK = MAX( MMAX + LMAX, MMAX*NOBRMX + NMAX,
     $                                  MMAX*( NMAX + LMAX ) ) )
      INTEGER           BSNM, L0, L1M, L2M, LDW1, LDW2, LDW3, LDW4,
     $                  LDW5, LDW6, LDW7, LDW8, LDWORK, LTHS, LW1, LW2,
     $                  LW3, LW4, LXM
      PARAMETER         ( BSNM = NNMAX*( LMAX + 2 ) + 1,
     $                    LTHS = NMAX*( LMAX + MMAX + 1 ) + LMAX*MMAX,
     $                    L0   = MAX( NMAX*( NMAX + LMAX ),
     $                                NMAX + MMAX + LMAX ),
     $                    L1M  = NSMPMX*LMAX +
     $                           MAX( 2*NNMAX,
     $                                ( NMAX + LMAX )*( NMAX + MMAX ) +
     $                                2*NMAX + L0 ),
     $                    LXM  = BSNM*LMAX + LTHS,
     $                    L2M  = MAX( LXM*LXM, 3*LXM + NSMPMX*LMAX ),
     $                    LDW1 = MAX( 2*( LMAX*NOBRMX - LMAX )*NMAX +
     $                                2*NMAX,
     $                                ( LMAX*NOBRMX - LMAX )*NMAX +
     $                                NMAX*NMAX + 7*NMAX,
     $                                LMAX*NOBRMX*NMAX +
     $                                MAX( ( LMAX*NOBRMX - LMAX )*NMAX +
     $                                     2*NMAX + LMAX +
     $                                     ( 2*MMAX + LMAX )*NOBRMX,
     $                                     2*( LMAX*NOBRMX - LMAX )*NMAX
     $                                   + NMAX*NMAX + 8*NMAX,
     $                                     NMAX + 4*( MMAX*NOBRMX +
     $                                                NMAX ) + 1,
     $                                     MMAX*NOBRMX + 3*NMAX + LMAX )
     $                              ),
     $                    LDW2 = LMAX*NOBRMX*NMAX +
     $                           MMAX*NOBRMX*( NMAX + LMAX )*
     $                           ( MMAX*( NMAX + LMAX ) + 1 ) +
     $                           MAX( ( NMAX + LMAX )**2,
     $                           4*MMAX*( NMAX + LMAX ) + 1 ),
     $                    LDW3 = NSMPMX*LMAX*( NMAX + 1 ) + 2*NMAX +
     $                           MAX( 2*NMAX*NMAX, 4*NMAX ),
     $                    LDW4 = NMAX*( NMAX + 1 ) + 2*NMAX +
     $                           MAX( NMAX*LMAX*( NMAX + 1 ) +
     $                           2*NMAX*NMAX + LMAX*NMAX, 4*NMAX ),
     $                    LDW5 = NSMPMX*LMAX + ( NMAX + LMAX )*
     $                           ( NMAX + MMAX ) + 3*NMAX + MMAX + LMAX,
     $                    LDW6 = NSMPMX*LMAX + ( NMAX + LMAX )*
     $                           ( NMAX + MMAX ) + NMAX +
     $                           MAX( 1, NMAX*NMAX*LMAX + NMAX*LMAX +
     $                                NMAX, NMAX*NMAX +
     $                                MAX( NMAX*NMAX +
     $                                     NMAX*MAX( NMAX, LMAX ) +
     $                                     6*NMAX + MIN( NMAX, LMAX ),
     $                                     NMAX*MMAX ) ),
     $                    LDW7 = MAX( BSNM*BSNM, 3*BSNM + NSMPMX ),
     $                    LDW8 = NSMPMX*LMAX + ( NMAX + LMAX )*
     $                           ( NMAX + MMAX ) + 3*NMAX + MMAX + LMAX,
     $                    LW1  = MAX( 2*( MMAX + LMAX )*NOBRMX*
     $                                ( 2*( MMAX + LMAX )*( NOBRMX + 1 )
     $                                  + 3 ) + LMAX*NOBRMX,
     $                                4*( MMAX + LMAX )*NOBRMX*
     $                                ( MMAX + LMAX )*NOBRMX +
     $                                ( NMAX + LMAX )*( NMAX + MMAX ) +
     $                                MAX( LDW1, LDW2 ),
     $                                ( NMAX + LMAX )*( NMAX + MMAX ) +
     $                                NMAX + NMAX*NMAX + 2 +
     $                                NMAX*( NMAX + MMAX + LMAX ) +
     $                                MAX( 5*NMAX, 2, MIN( LDW3, LDW4 ),
     $                                     LDW5, LDW6 ) ),
     $                    LW2  = NSMPMX*LMAX +
     $                           MAX( 5, NSMPMX + 2*BSNM + NSMPMX*BSNM +
     $                                   MAX( 2*NNMAX + BSNM, LDW7 ) ),
     $                    LW3  = MAX( LDW8, NSMPMX*LMAX +
     $                                ( NMAX + LMAX )*( 2*NMAX + MMAX )+
     $                                2*NMAX ),
     $                    LW4  = MAX( 5, NSMPMX*LMAX + 2*LXM +
     $                                NSMPMX*LMAX*( BSNM + LTHS ) +
     $                                MAX( L1M + LXM, NSMPMX*LMAX + L1M,
     $                                     L2M ) ),
     $                    LDWORK = MAX( LW1, LW2, LW3, LW4 ) )
*     .. Local Scalars ..
      LOGICAL           INIT1, INITB, INITL, INITN, INITS
      CHARACTER*1       ALG, INIT, STOR
      INTEGER           BSN, I, INFO, INI, ITER, ITERCG, ITMAX1, ITMAX2,
     $                  IWARN, J, L, L1, L2, LPAR, LX, M, N, NN, NOBR,
     $                  NPRINT, NS, NSMP
      DOUBLE PRECISION  TOL1, TOL2
*     .. Array Arguments ..
      INTEGER           IWORK(LIWORK)
      DOUBLE PRECISION  DWORK(LDWORK), U(LDU,MMAX), X(LXM), Y(LDY,LMAX)
*     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
*     .. External Subroutines ..
      EXTERNAL          IB03AD
*     .. Intrinsic Functions ..
      INTRINSIC         MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) NOBR, M, L, NSMP, N, NN, ITMAX1, ITMAX2,
     $                      NPRINT, TOL1, TOL2, INIT, ALG, STOR
      INITL = LSAME( INIT, 'L' )
      INITS = LSAME( INIT, 'S' )
      INITB = LSAME( INIT, 'B' )
      INITN = LSAME( INIT, 'N' )
      INIT1 = INITL .OR. INITB
      IF( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) M
      ELSE
         IF( L.LE.0 .OR. L.GT.LMAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) L
         ELSE
            NS = N
            IF( INIT1 ) THEN
               IF( NOBR.LE.0 .OR. NOBR.GT.NOBRMX ) THEN
                  WRITE ( NOUT, FMT = 99991 ) NOBR
                  STOP
               ELSEIF( NSMP.LT.2*( M + L + 1 )*NOBR - 1 ) THEN
                  WRITE ( NOUT, FMT = 99990 ) NSMP
                  STOP
               ELSEIF( N.EQ.0 .OR. N.GE.NOBR ) THEN
                  WRITE ( NOUT, FMT = 99989 ) N
                  STOP
               END IF
               IF ( N.LT.0 )
     $            N = NOBR - 1
            ELSE
               IF( NSMP.LT.0 ) THEN
                  WRITE ( NOUT, FMT = 99990 ) NSMP
                  STOP
               ELSEIF( N.LT.0 .OR. N.GT.NMAX ) THEN
                  WRITE ( NOUT, FMT = 99989 ) N
                  STOP
               END IF
            END IF
            IF( NN.LT.0 .OR. NN.GT.NNMAX ) THEN
               WRITE ( NOUT, FMT = 99988 ) NN
            ELSE
               BSN = NN*( L + 2 ) + 1
               L1  = BSN*L
               L2  = N*( L + M + 1 ) + L*M
               LX  = L1 + L2
               INI = 1
               IF ( INITL ) THEN
                  LPAR = L1
               ELSEIF ( INITS ) THEN
                  INI  = L1 + 1
                  LPAR = L2
               ELSEIF ( INITN ) THEN
                  LPAR = LX
               END IF
               IF( INIT1 )
     $            N = NS
*              Read the input-output data, initial parameters, and seed.
               READ ( NIN, FMT = * ) ( ( U(I,J), J = 1,M ), I = 1,NSMP )
               READ ( NIN, FMT = * ) ( ( Y(I,J), J = 1,L ), I = 1,NSMP )
               IF ( .NOT.INITB )
     $            READ ( NIN, FMT = * ) ( X(I), I = INI,INI+LPAR-1 )
               IF ( INITS .OR. INITB )
     $            READ ( NIN, FMT = * ) ( DWORK(I), I = 1,4 )
*              Solve a Wiener system identification problem.
               CALL IB03AD( INIT, ALG, STOR, NOBR, M, L, NSMP, N, NN,
     $                      ITMAX1, ITMAX2, NPRINT, U, LDU, Y, LDY,
     $                      X, LX, TOL1, TOL2, IWORK, DWORK, LDWORK,
     $                      IWARN, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF( IWARN.NE.0 ) WRITE ( NOUT, FMT = 99987 ) IWARN
                  ITER   = DWORK(3)
                  ITERCG = DWORK(4)
                  WRITE ( NOUT, FMT = 99997 ) DWORK(2)
                  WRITE ( NOUT, FMT = 99996 ) ITER, ITERCG,
     $                                        IWORK(1), IWORK(2)
*                 Recompute LX is necessary.
                  IF ( INIT1 .AND. NS.LT.0 )
     $               LX = L1 + N*( L + M + 1 ) + L*M
                  WRITE ( NOUT, FMT = 99994 )
                  WRITE ( NOUT, FMT = 99995 ) ( X(I), I = 1, LX )
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' IB03AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from IB03AD = ',I4)
99997 FORMAT (/' Final 2-norm of the residuals = ',D15.7)
99996 FORMAT (/' Number of iterations                     = ', I7,
     $        /' Number of conjugate gradients iterations = ', I7,
     $        /' Number of function evaluations           = ', I7,
     $        /' Number of Jacobian evaluations           = ', I7)
99995 FORMAT (10(1X,F8.4))
99994 FORMAT (/' Final approximate solution is ' )
99993 FORMAT (/' M is out of range.',/' M = ',I5)
99992 FORMAT (/' L is out of range.',/' L = ',I5)
99991 FORMAT (/' NOBR is out of range.',/' NOBR = ',I5)
99990 FORMAT (/' NSMP is out of range.',/' NSMP = ',I5)
99989 FORMAT (/' N is out of range.',/' N = ',I5)
99988 FORMAT (/' NN is out of range.',/' NN = ',I5)
99987 FORMAT (' IWARN on exit from IB03AD = ',I4)
      END
