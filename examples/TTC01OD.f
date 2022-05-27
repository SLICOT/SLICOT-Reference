*     TC01OD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, PMAX, INDMAX
      PARAMETER        ( MMAX = 20, PMAX = 20, INDMAX = 20 )
      INTEGER          MAXMP
      PARAMETER        ( MAXMP = MAX( MMAX, PMAX ) )
      INTEGER          LDPCO1, LDPCO2, LDQCO1, LDQCO2
      PARAMETER        ( LDPCO1 = MAXMP, LDPCO2 = MAXMP,
     $                   LDQCO1 = MAXMP, LDQCO2 = MAXMP )
*     .. Local Scalars ..
      INTEGER          I, INDLIM, INFO, J, K, M, P, PORM
      CHARACTER*1      LERI
      LOGICAL          LLERI
*     .. Local Arrays ..
      DOUBLE PRECISION PCOEFF(LDPCO1,LDPCO2,INDMAX),
     $                 QCOEFF(LDQCO1,LDQCO2,INDMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TC01OD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, P, INDLIM, LERI
      LLERI = LSAME( LERI, 'L' )
      IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) M
      ELSE IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) P
      ELSE IF ( INDLIM.LE.0 .OR. INDLIM.GT.INDMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) INDLIM
      ELSE
         PORM = P
         IF ( .NOT.LLERI ) PORM = M
         READ ( NIN, FMT = * )
     $      ( ( ( PCOEFF(I,J,K), K = 1,INDLIM ), J = 1,PORM ),
     $                           I = 1,PORM )
         READ ( NIN, FMT = * )
     $      ( ( ( QCOEFF(I,J,K), K = 1,INDLIM ), J = 1,M ), I = 1,P )
*        Find the dual right pmr of the given left pmr.
         CALL TC01OD( LERI, M, P, INDLIM, PCOEFF, LDPCO1, LDPCO2,
     $                QCOEFF, LDQCO1, LDQCO2, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 40 I = 1, PORM
               DO 20 J = 1, PORM
                  WRITE ( NOUT, FMT = 99996 ) I, J,
     $              ( PCOEFF(I,J,K), K = 1,INDLIM )
   20          CONTINUE
   40       CONTINUE
            WRITE ( NOUT, FMT = 99995 )
            DO 80 I = 1, M
               DO 60 J = 1, P
                  WRITE ( NOUT, FMT = 99996 ) I, J,
     $              ( QCOEFF(I,J,K), K = 1,INDLIM )
   60          CONTINUE
   80       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' TC01OD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TC01OD = ',I2)
99997 FORMAT (' The coefficients of the denominator matrix of the dual',
     $       ' system are ')
99996 FORMAT (/' element (',I2,',',I2,') is ',20(1X,F6.2))
99995 FORMAT (//' The coefficients of the numerator matrix of the dual',
     $       ' system are ')
99994 FORMAT (/' M is out of range.',/' M = ',I5)
99993 FORMAT (/' P is out of range.',/' P = ',I5)
99992 FORMAT (/' INDLIM is out of range.',/' INDLIM = ',I5)
      END
