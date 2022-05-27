*     MB02GD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX, NMAX, NLMAX
      PARAMETER        ( KMAX = 20, NMAX = 20, NLMAX = 20 )
      INTEGER          LDRB, LDT, LDWORK
      PARAMETER        ( LDRB = ( NLMAX + 1 )*KMAX, LDT = KMAX*NMAX,
     $                   LDWORK = ( NLMAX + 1 )*KMAX*KMAX +
     $                            ( 3 + NLMAX )*KMAX )
*     .. Local Scalars ..
      INTEGER          I, J, INFO, K, M, N, NL, SIZR
      CHARACTER        TRIU, TYPET
*     .. Local Arrays dimensioned for TYPET = 'R' ..
      DOUBLE PRECISION DWORK(LDWORK), RB(LDRB, NMAX*KMAX),
     $                 T(LDT, NMAX*KMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB02GD
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) K, N, NL, TRIU
      TYPET = 'R'
      M = ( NL + 1 )*K
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE IF( NL.LE.0 .OR. NL.GT.NLMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) NL
      ELSE IF( K.LE.0 .OR. K.GT.KMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) K
      ELSE
         READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,M ), I = 1,K )
*        Compute the banded Cholesky factor.
         CALL MB02GD( TYPET, TRIU, K, N, NL, 0, N, T, LDT, RB, LDRB,
     $                DWORK, LDWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            IF ( LSAME( TRIU, 'T' ) ) THEN
               SIZR = NL*K + 1
            ELSE
               SIZR = ( NL + 1 )*K
            END IF
            DO 10  I = 1, SIZR
               WRITE ( NOUT, FMT = 99996 ) ( RB(I,J), J = 1, N*K )
   10       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02GD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02GD = ',I2)
99997 FORMAT (/' The upper Cholesky factor in banded storage format ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
99994 FORMAT (/' NL is out of range.',/' NL = ',I5)
99993 FORMAT (/' K is out of range.',/' K = ',I5)
      END
