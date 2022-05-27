*     TD04AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, PMAX, KDCMAX, NMAX
      PARAMETER        ( MMAX = 10, PMAX = 10, KDCMAX = 10, NMAX = 10 )
      INTEGER          MAXMP
      PARAMETER        ( MAXMP = MAX( MMAX, PMAX ) )
      INTEGER          LDDCOE, LDUCO1, LDUCO2, LDA, LDB, LDC, LDD
      PARAMETER        ( LDDCOE = MAXMP, LDUCO1 = MAXMP,
     $                   LDUCO2 = MAXMP, LDA = NMAX, LDB = NMAX,
     $                   LDC = MAXMP, LDD = MAXMP )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX + MAXMP )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX + MAX( NMAX, 3*MAXMP ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INDBLK, INFO, J, K, KDCOEF, M, N, NR, P, PORM
      CHARACTER*1      ROWCOL
      LOGICAL          LROWCO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MAXMP), C(LDC,NMAX),
     $                 D(LDD,MAXMP), DCOEFF(LDDCOE,KDCMAX),
     $                 DWORK(LDWORK), UCOEFF(LDUCO1,LDUCO2,KDCMAX)
      INTEGER          INDEX(MAXMP), IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TD04AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, P, TOL, ROWCOL
      LROWCO = LSAME( ROWCOL, 'R' )
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) M
      ELSE IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) P
      ELSE
         PORM = P
         IF ( .NOT.LROWCO ) PORM = M
         READ ( NIN, FMT = * ) ( INDEX(I), I = 1,PORM )
*
         N = 0
         KDCOEF = 0
         DO 20 I = 1, PORM
            N = N + INDEX(I)
            KDCOEF = MAX( KDCOEF, INDEX(I) )
   20    CONTINUE
         KDCOEF = KDCOEF + 1
*
         IF ( KDCOEF.LE.0 .OR. KDCOEF.GT.KDCMAX ) THEN
            WRITE ( NOUT, FMT = 99988 ) KDCOEF
         ELSE
            READ ( NIN, FMT = * )
     $         ( ( DCOEFF(I,J), J = 1,KDCOEF ), I = 1,PORM )
            READ ( NIN, FMT = * )
     $         ( ( ( UCOEFF(I,J,K), K = 1,KDCOEF ), J = 1,M ), I = 1,P )
*           Find a minimal state-space representation (A,B,C,D).
            CALL TD04AD( ROWCOL, M, P, INDEX, DCOEFF, LDDCOE, UCOEFF,
     $                   LDUCO1, LDUCO2, NR, A, LDA, B, LDB, C, LDC, D,
     $                   LDD, TOL, IWORK, DWORK, LDWORK, INFO )
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
               IF ( LROWCO ) THEN
                  WRITE ( NOUT, FMT = 99992 ) INDBLK,
     $                       ( IWORK(I), I = 1,INDBLK )
               ELSE
                  WRITE ( NOUT, FMT = 99991 ) INDBLK,
     $                       ( IWORK(I), I = 1,INDBLK )
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TD04AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TD04AD = ',I2)
99997 FORMAT (' The order of the minimal realization = ',I2,//' The st',
     $       'ate dynamics matrix A of a minimal realization is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The input/state matrix B of a minimal realization is ')
99994 FORMAT (/' The state/output matrix C of a minimal realization is '
     $       )
99993 FORMAT (/' The direct transmission matrix D is ')
99992 FORMAT (/' The observability index of a minimal state-space repr',
     $       'esentation = ',I2,//' The dimensions of the diagonal blo',
     $       'cks of the state dynamics matrix are',/20(1X,I2))
99991 FORMAT (/' The controllability index of a minimal state-space re',
     $       'presentation = ',I2,//' The dimensions of the diagonal b',
     $       'locks of the state dynamics matrix are',/20(1X,I2))
99990 FORMAT (/' M is out of range.',/' M = ',I5)
99989 FORMAT (/' P is out of range.',/' P = ',I5)
99988 FORMAT (/' KDCOEF is out of range.',/' KDCOEF = ',I5)
      END
