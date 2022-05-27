*     TC05AD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, PMAX, KPCMAX
      PARAMETER        ( MMAX = 20, PMAX = 20, KPCMAX = 20 )
      INTEGER          MAXMP
      PARAMETER        ( MAXMP = MAX( MMAX, PMAX ) )
      INTEGER          LDCFRE, LDPCO1, LDPCO2, LDQCO1, LDQCO2
      PARAMETER        ( LDCFRE = MAXMP, LDPCO1 = MAXMP,
     $                   LDPCO2 = MAXMP, LDQCO1 = MAXMP,
     $                   LDQCO2 = MAXMP )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 2*MAXMP )
      INTEGER          LZWORK
      PARAMETER        ( LZWORK = ( MAXMP )*( MAXMP+2 ) )
*     .. Local Scalars ..
      COMPLEX*16       SVAL
      DOUBLE PRECISION RCOND
      INTEGER          I, INFO, J, K, KPCOEF, M, P, PORM, PORP
      CHARACTER*1      LERI
      LOGICAL          LLERI
*     .. Local Arrays ..
      COMPLEX*16       CFREQR(LDCFRE,MAXMP), ZWORK(LZWORK)
      DOUBLE PRECISION DWORK(LDWORK), PCOEFF(LDPCO1,LDPCO2,KPCMAX),
     $                 QCOEFF(LDQCO1,LDQCO2,KPCMAX)
      INTEGER          INDEX(MAXMP), IWORK(MAXMP)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TC05AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, P, SVAL, LERI
      LLERI = LSAME( LERI, 'L' )
      IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) M
      ELSE IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) P
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
            WRITE ( NOUT, FMT = 99993 ) KPCOEF
         ELSE
            READ ( NIN, FMT = * )
     $         ( ( ( PCOEFF(I,J,K), K = 1,KPCOEF ), J = 1,PORM ),
     $                              I = 1,PORM )
            READ ( NIN, FMT = * )
     $         ( ( ( QCOEFF(I,J,K), K = 1,KPCOEF ), J = 1,PORP ),
     $                              I = 1,PORM )
*           Find the standard frequency response matrix of left pmr
*           at 0.5*j.
            CALL TC05AD( LERI, M, P, SVAL, INDEX, PCOEFF, LDPCO1,
     $                   LDPCO2, QCOEFF, LDQCO1, LDQCO2, RCOND, CFREQR,
     $                   LDCFRE, IWORK, DWORK, ZWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 ) RCOND
               DO 40 I = 1, PORM
                  WRITE ( NOUT, FMT = 99996 )
     $                  ( CFREQR(I,J), J = 1,PORP )
   40          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TC05AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TC05AD = ',I2)
99997 FORMAT (' RCOND = ',F4.2,//' The frequency response matrix T(SVA',
     $       'L) is ')
99996 FORMAT (20(' (',F5.2,',',F5.2,') ',:))
99995 FORMAT (/' M is out of range.',/' M = ',I5)
99994 FORMAT (/' P is out of range.',/' P = ',I5)
99993 FORMAT (/' KPCOEF is out of range.',/' KPCOEF = ',I5)
      END
