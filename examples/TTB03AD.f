*     TB03AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          MAXMP
      PARAMETER        ( MAXMP = MAX( MMAX, PMAX ) )
      INTEGER          LDA, LDB, LDC, LDD, LDPCO1, LDPCO2, LDQCO1,
     $                 LDQCO2, LDVCO1, LDVCO2, NMAXP1
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = MAXMP,
     $                   LDD = MAXMP, LDPCO1 = MAXMP, LDPCO2 = MAXMP,
     $                   LDQCO1 = MAXMP, LDQCO2 = MAXMP, LDVCO1 = MAXMP,
     $                   LDVCO2 = NMAX, NMAXP1 = NMAX+1 )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX + MAXMP )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX + MAX( NMAX, 3*MAXMP ),
     $                                 MAXMP*( MAXMP + 2 ) ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INDBLK, INFO, J, K, KPCOEF, M, N, NR, P, PORM,
     $                 PORP
      CHARACTER*1      EQUIL, LERI
      LOGICAL          LLERI
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MAXMP), C(LDC,NMAX),
     $                 D(LDD,MAXMP), DWORK(LDWORK),
     $                 PCOEFF(LDPCO1,LDPCO2,NMAXP1),
     $                 QCOEFF(LDQCO1,LDQCO2,NMAXP1),
     $                 VCOEFF(LDVCO1,LDVCO2,NMAXP1)
      INTEGER          INDEX(MAXMP), IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TB03AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL, LERI, EQUIL
      LLERI = LSAME( LERI, 'L' )
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99987 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99986 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99985 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*              Find the right pmr which is equivalent to the ssr
*              C*inv(sI-A)*B+D.
               CALL TB03AD( LERI, EQUIL, N, M, P, A, LDA, B, LDB, C,
     $                      LDC, D, LDD, NR, INDEX, PCOEFF, LDPCO1,
     $                      LDPCO2, QCOEFF, LDQCO1, LDQCO2, VCOEFF,
     $                      LDVCO1, LDVCO2, TOL, IWORK, DWORK, LDWORK,
     $                      INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 ) NR
                  DO 20 I = 1, NR
                     WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,NR )
   20             CONTINUE
                  INDBLK = 0
                  DO 40 I = 1, N
                     IF ( IWORK(I).NE.0 ) INDBLK = INDBLK + 1
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99995 ) ( IWORK(I), I = 1,INDBLK )
                  WRITE ( NOUT, FMT = 99994 )
                  DO 60 I = 1, NR
                     WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,M )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99993 )
                  DO 80 I = 1, P
                     WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,NR )
   80             CONTINUE
                  IF ( LLERI ) THEN
                     PORM = P
                     PORP = M
                     WRITE ( NOUT, FMT = 99992 ) INDBLK
                  ELSE
                     PORM = M
                     PORP = P
                     WRITE ( NOUT, FMT = 99991 ) INDBLK
                  END IF
                  WRITE ( NOUT, FMT = 99990 ) ( INDEX(I), I = 1,PORM )
                  KPCOEF = 0
                  DO 100 I = 1, PORM
                     KPCOEF = MAX( KPCOEF, INDEX(I) )
  100             CONTINUE
                  KPCOEF = KPCOEF + 1
                  WRITE ( NOUT, FMT = 99989 )
                  DO 140 I = 1, PORM
                     DO 120 J = 1, PORM
                        WRITE ( NOUT, FMT = 99996 )
     $                        ( PCOEFF(I,J,K), K = 1,KPCOEF )
  120                CONTINUE
  140             CONTINUE
                  WRITE ( NOUT, FMT = 99988 )
                  IF ( LLERI ) THEN
                     DO 180 I = 1, PORM
                        DO 160 J = 1, PORP
                           WRITE ( NOUT, FMT = 99996 )
     $                           ( QCOEFF(I,J,K), K = 1,KPCOEF )
  160                   CONTINUE
  180                CONTINUE
                  ELSE
                     DO 220 I = 1, PORP
                        DO 200 J = 1, PORM
                           WRITE ( NOUT, FMT = 99996 )
     $                           ( QCOEFF(I,J,K), K = 1,KPCOEF )
  200                   CONTINUE
  220                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB03AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB03AD = ',I2)
99997 FORMAT (' The order of the minimal state-space representation = ',
     $       I2,//' The transformed state dynamics matrix of a minimal',
     $       ' realization is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' and the dimensions of its diagonal blocks are ',/20(I5)
     $       )
99994 FORMAT (/' The transformed input/state matrix of a minimal reali',
     $       'zation is ')
99993 FORMAT (/' The transformed state/output matrix of a minimal real',
     $       'ization is ')
99992 FORMAT (/' The observability index of the transformed minimal sy',
     $       'stem representation = ',I2)
99991 FORMAT (/' The controllability index of the transformed minimal ',
     $       'system representation = ',I2)
99990 FORMAT (/' INDEX is ',/20(I5))
99989 FORMAT (/' The denominator matrix P(s) is ')
99988 FORMAT (/' The numerator matrix Q(s) is ')
99987 FORMAT (/' N is out of range.',/' N = ',I5)
99986 FORMAT (/' M is out of range.',/' M = ',I5)
99985 FORMAT (/' P is out of range.',/' P = ',I5)
      END
