*     MB02JD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX, LMAX, MMAX, NMAX
      PARAMETER        ( KMAX = 10, LMAX = 10, MMAX = 20, NMAX = 20 )
      INTEGER          LDR, LDQ, LDTC, LDTR, LDWORK
      PARAMETER        ( LDR  = NMAX*LMAX, LDQ  = MMAX*KMAX,
     $                   LDTC = MMAX*KMAX, LDTR = KMAX,
     $                   LDWORK = ( MMAX*KMAX + NMAX*LMAX )
     $                            *( LMAX + 2*KMAX ) + 6*LMAX
     $                            + MMAX*KMAX + NMAX*LMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, K, L, M, N, S
      CHARACTER        JOB
*     .. Local Arrays ..
      DOUBLE PRECISION DWORK(LDWORK), Q(LDQ,NMAX*LMAX),
     $                 R(LDR,NMAX*LMAX), TC(LDTC,LMAX),
     $                 TR(LDTR,NMAX*LMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB02JD
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) K, L, M, N, JOB
      IF( K.LE.0 .OR. K.GT.KMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) K
      ELSE IF( L.LE.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) L
      ELSE IF( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) M
      ELSE IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( TC(I,J), J = 1,L ), I = 1,M*K )
         READ ( NIN, FMT = * ) ( ( TR(I,J), J = 1,( N - 1 )*L ),
     $                             I = 1,K )
         S = ( MIN( M*K, N*L ) + L - 1 ) / L
*        Compute the required part of the QR factorization.
         CALL MB02JD( JOB, K, L, M, N, 0, S, TC, LDTC, TR, LDTR, Q, LDQ,
     $                R, LDR, DWORK, LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( LSAME( JOB, 'Q' ) ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 10  I = 1, M*K
                  WRITE ( NOUT, FMT = 99995 )
     $                  ( Q(I,J), J = 1, MIN( N*L, M*K ) )
   10          CONTINUE
            END IF
            WRITE ( NOUT, FMT = 99996 )
            DO 20  I = 1, N*L
               WRITE ( NOUT, FMT = 99995 )
     $               ( R(I,J), J = 1, MIN( N*L, M*K ) )
   20       CONTINUE
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MB02JD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02JD = ',I2)
99997 FORMAT (/' The factor Q is ')
99996 FORMAT (/' The factor R is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' K is out of range.',/' K = ',I5)
99993 FORMAT (/' L is out of range.',/' L = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' N is out of range.',/' N = ',I5)
      END
