*     TB04AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          MAXMP
      PARAMETER        ( MAXMP = MAX( MMAX, PMAX ) )
      INTEGER          LDA, LDB, LDC, LDD, LDDCOE, LDUCO1, LDUCO2,
     $                 NMAXP1
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = MAXMP,
     $                   LDD = MAXMP, LDDCOE = MAXMP, LDUCO1 = MAXMP,
     $                   LDUCO2 = MAXMP, NMAXP1 = NMAX+1 )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX + MAXMP )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX*( NMAX + 1 ) +
     $                            MAX( NMAX*MAXMP + 2*NMAX +
     $                                 MAX( NMAX, MAXMP ), 3*MAXMP ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL1, TOL2
      INTEGER          I, II, IJ, INDBLK, INFO, J, JJ, KDCOEF, M, N,
     $                 NR, P, PORM, PORP
      CHARACTER*1      ROWCOL
      CHARACTER*132    ULINE
      LOGICAL          LROWCO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MAXMP), C(LDC,NMAX),
     $                 D(LDD,MAXMP), DCOEFF(LDDCOE,NMAXP1),
     $                 DWORK(LDWORK), UCOEFF(LDUCO1,LDUCO2,NMAXP1)
      INTEGER          INDEX(MAXMP), IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TB04AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL1, TOL2, ROWCOL
      LROWCO = LSAME( ROWCOL, 'R' )
      ULINE(1:20) = ' '
      DO 20 I = 21, 132
         ULINE(I:I) = '-'
   20 CONTINUE
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99986 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99985 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99984 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*              Find the transfer matrix T(s) of (A,B,C,D).
               CALL TB04AD( ROWCOL, N, M, P, A, LDA, B, LDB, C, LDC, D,
     $                      LDD, NR, INDEX, DCOEFF, LDDCOE, UCOEFF,
     $                      LDUCO1, LDUCO2, TOL1, TOL2, IWORK, DWORK,
     $                      LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 ) NR
                  DO 40 I = 1, NR
                     WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,NR )
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99995 )
                  DO 60 I = 1, NR
                     WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,M )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99994 )
                  DO 80 I = 1, P
                     WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,NR )
   80             CONTINUE
                  INDBLK = 0
                  DO 100 I = 1, N
                     IF ( IWORK(I).NE.0 ) INDBLK = INDBLK + 1
  100             CONTINUE
                  IF ( LROWCO ) THEN
                     PORM = P
                     PORP = M
                     WRITE ( NOUT, FMT = 99993 ) INDBLK,
     $                          ( IWORK(I), I = 1,INDBLK )
                  ELSE
                     PORM = M
                     PORP = P
                     WRITE ( NOUT, FMT = 99992 ) INDBLK,
     $                          ( IWORK(I), I = 1,INDBLK )
                  END IF
                  WRITE ( NOUT, FMT = 99991 ) ( INDEX(I), I = 1,PORM )
                  WRITE ( NOUT, FMT = 99990 )
                  KDCOEF = 0
                  DO 120 I = 1, PORM
                     KDCOEF = MAX( KDCOEF, INDEX(I) )
  120             CONTINUE
                  KDCOEF = KDCOEF + 1
                  DO 160 II = 1, PORM
                     DO 140 JJ = 1, PORP
                        WRITE ( NOUT, FMT = 99989 ) II, JJ,
     $                    ( UCOEFF(II,JJ,IJ), IJ = 1,KDCOEF )
                        WRITE ( NOUT, FMT = 99988 ) ULINE(1:7*KDCOEF+21)
                        WRITE ( NOUT, FMT = 99987 )
     $                        ( DCOEFF(II,IJ), IJ = 1,KDCOEF )
  140                CONTINUE
  160             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB04AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB04AD = ',I2)
99997 FORMAT (' The order of the transformed state-space representatio',
     $       'n = ',I2,//' The transformed state dynamics matrix A is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The transformed input/state matrix B is ')
99994 FORMAT (/' The transformed state/output matrix C is ')
99993 FORMAT (/' The controllability index of the transformed state-sp',
     $       'ace representation = ',I2,//' The dimensions of the diag',
     $       'onal blocks of the transformed A are ',/20(I5))
99992 FORMAT (/' The observability index of the transformed state-spac',
     $       'e representation = ',I2,//' The dimensions of the diagon',
     $       'al blocks of the transformed A are ',/20(I5))
99991 FORMAT (/' The degrees of the denominator polynomials are',/20(I5)
     $       )
99990 FORMAT (/' The coefficients of polynomials in the transfer matri',
     $       'x T(s) are ')
99989 FORMAT (/' element (',I2,',',I2,') is ',20(1X,F6.2))
99988 FORMAT (1X,A)
99987 FORMAT (20X,20(1X,F6.2))
99986 FORMAT (/' N is out of range.',/' N = ',I5)
99985 FORMAT (/' M is out of range.',/' M = ',I5)
99984 FORMAT (/' P is out of range.',/' P = ',I5)
      END
