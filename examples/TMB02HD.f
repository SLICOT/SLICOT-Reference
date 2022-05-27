*     MB02HD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX, LMAX, MMAX, MLMAX, NMAX, NUMAX
      PARAMETER        ( KMAX = 20, LMAX  = 20, MMAX = 20, MLMAX = 10,
     $                   NMAX = 20, NUMAX = 10 )
      INTEGER          LDRB, LDTC, LDTR, LDWORK
      PARAMETER        ( LDRB = ( MLMAX + NUMAX + 1 )*LMAX,
     $                   LDTC = ( MLMAX + 1 )*KMAX, LDTR = KMAX )
      PARAMETER        ( LDWORK = LDRB*LMAX + ( 2*NUMAX + 1 )*LMAX*KMAX
     $                            + 2*LDRB*( KMAX + LMAX ) + LDRB
     $                            + 6*LMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, K, L, LENR, M, ML, N, NU, S
      CHARACTER        TRIU
*     .. Local Arrays ..
      DOUBLE PRECISION DWORK(LDWORK), RB(LDRB,NMAX*LMAX),
     $                 TC(LDTC,LMAX), TR(LDTR,NMAX*LMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB02HD
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) K, L, M, ML, N, NU, TRIU
      IF( K.LT.0 .OR. K.GT.KMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) K
      ELSE IF( L.LT.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) L
      ELSE IF( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) M
      ELSE IF( ML.LT.0 .OR. ML.GT.MLMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) ML
      ELSE IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE IF( NU.LT.0 .OR. NU.GT.NUMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) NU
      ELSE
         READ ( NIN, FMT = * ) ( ( TC(I,J), J = 1,L ), I = 1,(ML+1)*K )
         READ ( NIN, FMT = * ) ( ( TR(I,J), J = 1,NU*L ), I = 1,K )
         S = ( MIN( M*K, N*L ) + L - 1 ) / L
*        Compute the banded R factor.
         CALL MB02HD( TRIU, K, L, M, ML, N, NU, 0, S, TC, LDTC, TR,
     $                LDTR, RB, LDRB, DWORK, LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            LENR = ( ML + NU + 1 )*L
            IF ( LSAME( TRIU, 'T' ) )  LENR = ( ML + NU )*L + 1
            LENR = MIN( LENR, N*L )
            DO 10  I = 1, LENR
               WRITE ( NOUT, FMT = 99996 ) ( RB(I,J), J = 1,
     $                                       MIN( N*L, M*K ) )
   10       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02HD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02HD = ',I2)
99997 FORMAT (/' The lower triangular factor R in banded storage ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' NU is out of range.',/' NU = ',I5)
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' ML is out of range.',/' ML = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' L is out of range.',/' L = ',I5)
99990 FORMAT (/' K is out of range.',/' K = ',I5)
      END
