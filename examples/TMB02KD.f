*     MB02KD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX, LMAX, MMAX, NMAX, RMAX
      PARAMETER        ( KMAX = 20, LMAX = 20, MMAX = 20, NMAX = 20,
     $                   RMAX = 20 )
      INTEGER          LDB, LDC, LDTC, LDTR, LDWORK
      PARAMETER        ( LDB  = LMAX*NMAX, LDC  = KMAX*MMAX,
     $                   LDTC = MMAX*KMAX, LDTR = KMAX,
     $                   LDWORK = 2*( KMAX*LMAX + KMAX*RMAX
     $                            + LMAX*RMAX + 1 )*( MMAX + NMAX ) )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, K, L, M, N, R
      CHARACTER        LDBLK, TRANS
      DOUBLE PRECISION ALPHA, BETA
*     .. Local Arrays .. (Dimensioned for TRANS = 'N'.)
      DOUBLE PRECISION B(LDB,RMAX), C(LDC,RMAX), DWORK(LDWORK),
     $                 TC(LDTC,LMAX), TR(LDTR,NMAX*LMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB02KD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  K, L, M, N, R, LDBLK, TRANS
      IF( K.LE.0 .OR. K.GT.KMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) K
      ELSE IF( L.LE.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) L
      ELSE IF( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) M
      ELSE IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) N
      ELSE IF( R.LE.0 .OR. R.GT.RMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         IF ( LSAME( LDBLK, 'R' ) ) THEN
            READ ( NIN, FMT = * ) ( ( TC(I,J), J = 1,L ),
     $                              I = 1,(M-1)*K )
            READ ( NIN, FMT = * ) ( ( TR(I,J), J = 1,N*L ), I = 1,K )
         ELSE
            READ ( NIN, FMT = * ) ( ( TC(I,J), J = 1,L ), I = 1,M*K )
            READ ( NIN, FMT = * ) ( ( TR(I,J), J = 1,(N-1)*L ),
     $                              I = 1,K )
         END IF
         IF ( LSAME( TRANS, 'N' ) ) THEN
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,R ), I = 1,N*L )
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,R ), I = 1,M*K )
         END IF
         ALPHA = ONE
         BETA  = ZERO
         CALL MB02KD( LDBLK, TRANS, K, L, M, N, R, ALPHA, BETA, TC,
     $                LDTC, TR, LDTR, B, LDB, C, LDC, DWORK, LDWORK,
     $                INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( LSAME( TRANS, 'N' ) ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 10  I = 1, M*K
                  WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,R )
   10          CONTINUE
            ELSE
               WRITE ( NOUT, FMT = 99996 )
               DO 20  I = 1, N*L
                  WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,R )
   20          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02KD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02KD = ',I2)
99997 FORMAT (' The product C = T * B is ')
99996 FORMAT (' The product C = T^T * B is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' K is out of range.',/' K = ',I5)
99993 FORMAT (/' L is out of range.',/' L = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' N is out of range.',/' N = ',I5)
99990 FORMAT (/' R is out of range.',/' R = ',I5)
      END
