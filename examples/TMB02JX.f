*     MB02JX EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX, LMAX, MMAX, NMAX
      PARAMETER        ( KMAX = 20, LMAX = 20, MMAX = 20, NMAX = 20 )
      INTEGER          LDR, LDQ, LDTC, LDTR, LDWORK
      PARAMETER        ( LDR  = NMAX*LMAX, LDQ  = MMAX*KMAX,
     $                   LDTC = MMAX*KMAX, LDTR = KMAX,
     $                   LDWORK = ( MMAX*KMAX + NMAX*LMAX )
     $                            *( LMAX + 2*KMAX ) + 5*LMAX
     $                            + MMAX*KMAX + NMAX*LMAX )
*     .. Local Scalars ..
      CHARACTER        JOB
      INTEGER          I, INFO, J, K, L, M, N, RNK
      DOUBLE PRECISION TOL1, TOL2
*     .. Local Arrays ..
      INTEGER          JPVT(NMAX*LMAX)
      DOUBLE PRECISION DWORK(LDWORK), Q(LDQ,NMAX*LMAX),
     $                 R(LDR,NMAX*LMAX), TC(LDTC,LMAX),
     $                 TR(LDTR,NMAX*LMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB02JX
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) K, L, M, N, TOL1, TOL2, JOB
      IF( K.LE.0 .OR. K.GT.KMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) K
      ELSE IF( L.LE.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) L
      ELSE IF( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) M
      ELSE IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99988 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( TC(I,J), J = 1,L ), I = 1,M*K )
         READ ( NIN, FMT = * ) ( ( TR(I,J), J = 1,( N - 1 )*L ),
     $                             I = 1,K )
*        Compute the required part of the QR factorization.
         CALL MB02JX( JOB, K, L, M, N, TC, LDTC, TR, LDTR, RNK, Q, LDQ,
     $                R, LDR, JPVT, TOL1, TOL2, DWORK, LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99994 ) RNK
            IF ( LSAME( JOB, 'Q' ) ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 10  I = 1, M*K
                  WRITE ( NOUT, FMT = 99993 ) ( Q(I,J), J = 1, RNK )
   10          CONTINUE
            END IF
            WRITE ( NOUT, FMT = 99996 )
            DO 20  I = 1, N*L
               WRITE ( NOUT, FMT = 99993 ) ( R(I,J), J = 1, RNK )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99995 )
            WRITE ( NOUT, FMT = 99992 ) ( JPVT(I),
     $                                    I = 1, MIN( M*K, N*L ) )
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02JX EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02JX = ',I2)
99997 FORMAT (/' The factor Q is ')
99996 FORMAT (/' The factor R is ')
99995 FORMAT (/' The column permutation is ')
99994 FORMAT (/' Numerical rank ',/' RNK = ',I5)
99993 FORMAT (20(1X,F8.4))
99992 FORMAT (20(1X,I4))
99991 FORMAT (/' K is out of range.',/' K = ',I5)
99990 FORMAT (/' L is out of range.',/' L = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' N is out of range.',/' N = ',I5)
      END
