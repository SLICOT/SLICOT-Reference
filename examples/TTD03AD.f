*     TD03AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, PMAX, KDCMAX, NMAX
      PARAMETER        ( MMAX = 8, PMAX = 8, KDCMAX = 8, NMAX = 8 )
      INTEGER          MAXMP
      PARAMETER        ( MAXMP = MAX( MMAX, PMAX ) )
      INTEGER          LDA, LDB, LDC, LDD, LDDCOE, LDPCO1, LDPCO2,
     $                 LDQCO1, LDQCO2, LDUCO1, LDUCO2, LDVCO1, LDVCO2
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = MAXMP,
     $                   LDD = MAXMP, LDDCOE = MAXMP,
     $                   LDPCO1 = MAXMP, LDPCO2 = MAXMP,
     $                   LDQCO1 = MAXMP, LDQCO2 = MAXMP,
     $                   LDUCO1 = MAXMP, LDUCO2 = MAXMP,
     $                   LDVCO1 = MAXMP, LDVCO2 = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX + MAXMP )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX + MAX( NMAX, 3*MAXMP ),
     $                                 MAXMP*( MAXMP + 2 ) ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      CHARACTER*1      EQUIL, LERI, ROWCOL
      INTEGER          I, INDBLK, INFO, J, K, KDCOEF, M, MAXINP, N, NR,
     $                 P, PORMD, PORMP
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MAXMP), C(LDC,NMAX),
     $                 D(LDD,MAXMP), DCOEFF(LDDCOE,KDCMAX),
     $                 DWORK(LDWORK), PCOEFF(LDPCO1,LDPCO2,NMAX+1),
     $                 QCOEFF(LDQCO1,LDQCO2,NMAX+1),
     $                 UCOEFF(LDUCO1,LDUCO2,KDCMAX),
     $                 VCOEFF(LDVCO1,LDVCO2,NMAX+1)
      INTEGER          INDEXD(MAXMP), INDEXP(MAXMP), IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TD03AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, P, TOL, ROWCOL, LERI, EQUIL
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99986 ) M
      ELSE IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99985 ) P
      ELSE
         PORMD = P
         IF ( LSAME( ROWCOL, 'C' ) ) PORMD = M
         PORMP = M
         IF ( LSAME( LERI, 'R' ) ) PORMP = P
         READ ( NIN, FMT = * ) ( INDEXD(I), I = 1,PORMD )
*
         KDCOEF = 0
         N = 0
         DO 20 I = 1, PORMD
            KDCOEF = MAX( KDCOEF, INDEXD(I) )
            N = N + INDEXD(I)
   20    CONTINUE
         KDCOEF = KDCOEF + 1
*
         IF ( KDCOEF.LE.0 .OR. KDCOEF.GT.KDCMAX ) THEN
            WRITE ( NOUT, FMT = 99984 ) KDCOEF
         ELSE
            READ ( NIN, FMT = * )
     $         ( ( DCOEFF(I,J), J = 1,KDCOEF ), I = 1,PORMD )
            READ ( NIN, FMT = * )
     $         ( ( ( UCOEFF(I,J,K), K = 1,KDCOEF ), J = 1,M ), I = 1,P )
*           Find a relatively prime left pmr for the given transfer
*           function.
            CALL TD03AD( ROWCOL, LERI, EQUIL, M, P, INDEXD, DCOEFF,
     $                   LDDCOE, UCOEFF, LDUCO1, LDUCO2, NR, A, LDA, B,
     $                   LDB, C, LDC, D, LDD, INDEXP, PCOEFF, LDPCO1,
     $                   LDPCO2, QCOEFF, LDQCO1, LDQCO2, VCOEFF, LDVCO1,
     $                   LDVCO2, TOL, IWORK, DWORK, LDWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 ) NR
               DO 40 I = 1, NR
                  WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,NR )
   40          CONTINUE
               WRITE ( NOUT, FMT = 99995 )
               DO 60 I = 1, NR
                  WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,M )
   60          CONTINUE
               WRITE ( NOUT, FMT = 99994 )
               DO 80 I = 1, P
                  WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,NR )
   80          CONTINUE
               WRITE ( NOUT, FMT = 99993 )
               DO 100 I = 1, P
                  WRITE ( NOUT, FMT = 99996 ) ( D(I,J), J = 1,M )
  100          CONTINUE
               INDBLK = 0
               DO 120 I = 1, N
                  IF ( IWORK(I).NE.0 ) INDBLK = INDBLK + 1
  120          CONTINUE
               IF ( LSAME( LERI, 'L' ) ) THEN
                  WRITE ( NOUT, FMT = 99992 ) INDBLK,
     $                  ( IWORK(I), I = 1,INDBLK )
                  WRITE ( NOUT, FMT = 99990 ) ( INDEXP(I), I = 1,P )
               ELSE
                  WRITE ( NOUT, FMT = 99991 ) INDBLK,
     $                  ( IWORK(I), I = 1,INDBLK )
                  WRITE ( NOUT, FMT = 99989 ) ( INDEXP(I), I = 1,M )
               END IF
               MAXINP = 0
               DO 140 I = 1, PORMP
                  MAXINP = MAX( MAXINP, INDEXP(I) )
  140          CONTINUE
               MAXINP = MAXINP + 1
               WRITE ( NOUT, FMT = 99988 )
               DO 180 I = 1, PORMP
                  DO 160 J = 1, PORMP
                     WRITE ( NOUT, FMT = 99996 )
     $                     ( PCOEFF(I,J,K), K = 1,MAXINP )
  160             CONTINUE
  180          CONTINUE
               WRITE ( NOUT, FMT = 99987 )
               DO 220 I = 1, PORMP
                  DO 200 J = 1, PORMD
                     WRITE ( NOUT, FMT = 99996 )
     $                     ( QCOEFF(I,J,K), K = 1,MAXINP )
  200             CONTINUE
  220          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TD03AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TD03AD = ',I2)
99997 FORMAT (' The order of the resulting minimal realization = ',I2,
     $       //' The state dynamics matrix A is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The input/state matrix B is ')
99994 FORMAT (/' The state/output matrix C is ')
99993 FORMAT (/' The direct transmission matrix D is ')
99992 FORMAT (/' The observability index of the minimal realization = ',
     $       I2,//' The dimensions of the diagonal blocks of the state',
     $       ' dynamics matrix are ',/20(I5))
99991 FORMAT (/' The controllability index of the minimal realization ',
     $       '= ',I2,//' The dimensions of the diagonal blocks of the ',
     $       'state dynamics matrix are ',/20(I5))
99990 FORMAT (/' The row degrees of the denominator matrix P(s) are',
     $       /20(I5))
99989 FORMAT (/' The column degrees of the denominator matrix P(s) are',
     $       /20(I5))
99988 FORMAT (/' The denominator matrix P(s) is ')
99987 FORMAT (/' The numerator matrix Q(s) is ')
99986 FORMAT (/' M is out of range.',/' M = ',I5)
99985 FORMAT (/' P is out of range.',/' P = ',I5)
99984 FORMAT (/' KDCOEF is out of range.',/' KDCOEF = ',I5)
      END
