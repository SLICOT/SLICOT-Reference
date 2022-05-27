*     TC04AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, PMAX, KPCMAX, NMAX
      PARAMETER        ( MMAX = 5, PMAX = 5, KPCMAX = 5, NMAX = 5 )
      INTEGER          MAXMP
      PARAMETER        ( MAXMP = MAX( MMAX, PMAX ) )
      INTEGER          LDPCO1, LDPCO2, LDQCO1, LDQCO2, LDA, LDB, LDC,
     $                 LDD
      PARAMETER        ( LDPCO1 = MAXMP, LDPCO2 = MAXMP,
     $                   LDQCO1 = MAXMP, LDQCO2 = MAXMP,
     $                   LDA = NMAX, LDB = NMAX, LDC = MAXMP,
     $                   LDD = MAXMP )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = 2*MAXMP )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = ( MAXMP )*( MAXMP+4 ) )
*     .. Local Scalars ..
      DOUBLE PRECISION RCOND
      INTEGER          I, INFO, J, K, KPCOEF, M, N, P, PORM, PORP
      CHARACTER*1      LERI
      LOGICAL          LLERI
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MAXMP), C(LDC,NMAX),
     $                 D(LDD,MAXMP), PCOEFF(LDPCO1,LDPCO2,KPCMAX),
     $                 QCOEFF(LDQCO1,LDQCO2,KPCMAX), DWORK(LDWORK)
      INTEGER          INDEX(MAXMP), IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TC04AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, P, LERI
      LLERI = LSAME( LERI, 'L' )
      IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) M
      ELSE IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) P
      ELSE
         PORM = P
         IF ( .NOT.LLERI ) PORM = M
         READ ( NIN, FMT = * ) ( INDEX(I), I = 1,PORM )
         PORP = M
         IF ( .NOT.LLERI ) PORP = P
         KPCOEF = 0
         DO 20 I = 1, PORM
            KPCOEF = MAX( KPCOEF, INDEX(I) )
   20    CONTINUE
         KPCOEF = KPCOEF + 1
         IF ( KPCOEF.LE.0 .OR. KPCOEF.GT.KPCMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) KPCOEF
         ELSE
            READ ( NIN, FMT = * )
     $         ( ( ( PCOEFF(I,J,K), K = 1,KPCOEF ), J = 1,PORM ),
     $                              I = 1,PORM )
            READ ( NIN, FMT = * )
     $         ( ( ( QCOEFF(I,J,K), K = 1,KPCOEF ), J = 1,PORP ),
     $                              I = 1,PORM )
*           Find a ssr of the given left pmr.
            CALL TC04AD( LERI, M, P, INDEX, PCOEFF, LDPCO1, LDPCO2,
     $                   QCOEFF, LDQCO1, LDQCO2, N, RCOND, A, LDA, B,
     $                   LDB, C, LDC, D, LDD, IWORK, DWORK, LDWORK,
     $                   INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 ) N, RCOND
               WRITE ( NOUT, FMT = 99996 )
               DO 40 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,N )
   40          CONTINUE
               WRITE ( NOUT, FMT = 99994 )
               DO 60 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   60          CONTINUE
               WRITE ( NOUT, FMT = 99993 )
               DO 80 I = 1, P
                  WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,N )
   80          CONTINUE
               WRITE ( NOUT, FMT = 99992 )
               DO 100 I = 1, P
                  WRITE ( NOUT, FMT = 99995 ) ( D(I,J), J = 1,M )
  100          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TC04AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TC04AD = ',I2)
99997 FORMAT (' The order of the resulting state-space representation ',
     $       ' =  ',I2,//' RCOND = ',F4.2)
99996 FORMAT (/' The state dynamics matrix A is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' The input/state matrix B is ')
99993 FORMAT (/' The state/output matrix C is ')
99992 FORMAT (/' The direct transmission matrix D is ')
99991 FORMAT (/' M is out of range.',/' M = ',I5)
99990 FORMAT (/' P is out of range.',/' P = ',I5)
99989 FORMAT (/' KPCOEF is out of range.',/' KPCOEF = ',I5)
      END
