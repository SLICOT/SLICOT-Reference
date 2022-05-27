*     SG02AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          NMAX2M, NMAX2, NMMAX
      PARAMETER        ( NMAX2M = 2*NMAX+MMAX, NMAX2 = 2*NMAX,
     $                   NMMAX  = MAX(NMAX,MMAX) )
      INTEGER          LDA, LDB, LDE, LDL, LDQ, LDR, LDS, LDT, LDU, LDX
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDE = NMAX, LDL = NMAX,
     $                   LDQ = MAX(NMAX,PMAX), LDR = MAX(MMAX,PMAX),
     $                   LDS = NMAX2M, LDT = NMAX2M, LDU = NMAX2,
     $                   LDX = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MAX(MMAX,NMAX2) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX(14*NMAX+23,16*NMAX,2*NMAX+MMAX,
     $                                3*MMAX) )
      INTEGER          LBWORK
      PARAMETER        ( LBWORK = NMAX2 )
*     .. Local Scalars ..
      DOUBLE PRECISION RCONDU, TOL
      INTEGER          I, INFO, IWARN, J, M, N, P
      CHARACTER*1      ACC, DICO, FACT, JOBB, JOBL, SCAL, SORT, UPLO
      LOGICAL          LJOBB
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX),  ALFAI(NMAX2),  ALFAR(NMAX2),
     $                 B(LDB,NMMAX), BETA(NMAX2),   DWORK(LDWORK),
     $                 E(LDE,NMAX),  L(LDL,MMAX),   Q(LDQ,NMAX),
     $                 R(LDR,MMAX),  S(LDS,NMAX2M), T(LDT,NMAX2),
     $                 U(LDU,NMAX2), X(LDX,NMAX)
      INTEGER          IWORK(LIWORK)
      LOGICAL          BWORK(LBWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SG02AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL, DICO, JOBB, FACT, UPLO, JOBL,
     $                      SCAL, SORT, ACC
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99994 ) M
         ELSE
            LJOBB = LSAME( JOBB, 'B' )
            IF ( LJOBB ) THEN
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            ELSE
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,N )
            END IF
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99993 ) P
            ELSE
               IF ( LSAME( FACT, 'N' ) .OR. LSAME( FACT, 'D' ) ) THEN
                  READ ( NIN, FMT = * )
     $                                 ( ( Q(I,J), J = 1,N ), I = 1,N )
               ELSE
                  READ ( NIN, FMT = * )
     $                                 ( ( Q(I,J), J = 1,N ), I = 1,P )
               END IF
               IF ( LJOBB ) THEN
                  IF ( LSAME( FACT, 'N' ) .OR. LSAME( FACT, 'C' ) ) THEN
                      READ ( NIN, FMT = * )
     $                                  ( ( R(I,J), J = 1,M ), I = 1,M )
                  ELSE
                      READ ( NIN, FMT = * )
     $                                  ( ( R(I,J), J = 1,M ), I = 1,P )
                  END IF
                  IF ( LSAME( JOBL, 'N' ) )
     $                READ ( NIN, FMT = * )
     $                                  ( ( L(I,J), J = 1,M ), I = 1,N )
               END IF
*              Find the solution matrix X.
               CALL SG02AD( DICO, JOBB, FACT, UPLO, JOBL, SCAL, SORT,
     $                      ACC, N, M, P, A, LDA, E, LDE, B, LDB, Q,
     $                      LDQ, R, LDR, L, LDL, RCONDU, X, LDX, ALFAR,
     $                      ALFAI, BETA, S, LDS, T, LDT, U, LDU, TOL,
     $                      IWORK, DWORK, LDWORK, BWORK, IWARN, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 )
                  DO 20 I = 1, N
                     WRITE ( NOUT, FMT = 99996 ) ( X(I,J), J = 1,N )
   20             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SG02AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SG02AD = ',I2)
99997 FORMAT (' The solution matrix X is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
99994 FORMAT (/' M is out of range.',/' M = ',I5)
99993 FORMAT (/' P is out of range.',/' P = ',I5)
      END
