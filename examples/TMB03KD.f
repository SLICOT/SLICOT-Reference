*     MB03KD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            KMAX, NMAX
      PARAMETER          ( KMAX = 6, NMAX = 50 )
      INTEGER            LDA1, LDA2, LDQ1, LDQ2, LDWORK, LIWORK
      PARAMETER          ( LDA1 = NMAX, LDA2 = NMAX, LDQ1 = NMAX,
     $                     LDQ2 = NMAX,
     $                     LDWORK = MAX( 42*KMAX + NMAX, 80*KMAX - 48,
     $                                   KMAX + MAX( 2*NMAX, 8*KMAX ) ),
     $                     LIWORK = MAX( 4*KMAX, 2*KMAX + NMAX ) )
      DOUBLE PRECISION   HUND, ZERO
      PARAMETER          ( HUND = 1.0D2, ZERO = 0.0D0 )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ, DEFL, JOB, STRONG
      INTEGER            H, I, IHI, ILO, INFO, IWARN, J, K, L, M, N, P
      DOUBLE PRECISION   TOL
*
*     .. Local Arrays ..
      LOGICAL            SELECT( NMAX )
      INTEGER            IWORK( LIWORK ), IXQ( KMAX ), IXT( KMAX ),
     $                   LDQ( KMAX ), LDT( KMAX ), ND( KMAX ),
     $                   NI( KMAX ), QIND( KMAX ), S( KMAX ),
     $                   SCAL( NMAX )
      DOUBLE PRECISION   A( LDA1, LDA2, KMAX ), ALPHAI( NMAX ),
     $                   ALPHAR( NMAX ), BETA( NMAX ), DWORK( LDWORK),
     $                   Q( LDQ1, LDQ2, KMAX ), QK( NMAX*NMAX*KMAX ),
     $                   T( NMAX*NMAX*KMAX )
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           DLACPY, MB03BD, MB03KD
*
*     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX
*
*     .. Executable Statements ..
*
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) JOB, DEFL, COMPQ, STRONG, K, N, H, ILO, IHI
      IF( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
         TOL = HUND
         READ( NIN, FMT = * ) ( S( I ), I = 1, K )
         READ( NIN, FMT = * ) ( ( ( A( I, J, L ), J = 1, N ),
     $                                I = 1, N ), L = 1, K )
         IF( LSAME( COMPQ, 'U' ) )
     $      READ( NIN, FMT = * ) ( ( ( Q( I, J, L ), J = 1, N ),
     $                                   I = 1, N ), L = 1, K )
         IF( LSAME( COMPQ, 'P' ) ) THEN
            READ( NIN, FMT = * ) ( QIND( I ), I = 1, K )
            DO 10 L = 1, K
               IF( QIND( L ).GT.0 )
     $            READ( NIN, FMT = * ) ( ( Q( I, J, QIND( L ) ),
     $                                    J = 1, N ), I = 1, N )
   10       CONTINUE
         END IF
         IF( LSAME( JOB, 'E' ) )
     $      JOB = 'S'
*        Compute the eigenvalues and the transformed matrices.
         CALL MB03BD( JOB, DEFL, COMPQ, QIND, K, N, H, ILO, IHI, S, A,
     $                LDA1, LDA2, Q, LDQ1, LDQ2, ALPHAR, ALPHAI, BETA,
     $                SCAL, IWORK, LIWORK, DWORK, LDWORK, IWARN, INFO )
*
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE IF( IWARN.EQ.0 ) THEN
*           Prepare the data for calling MB03KD, which uses different
*           data structures and reverse ordering of the factors.
            DO 20 L = 1, K
               ND(  L ) = MAX( 1, N )
               NI(  L ) = 0
               LDT( L ) = MAX( 1, N )
               IXT( L ) = ( L - 1 )*LDT( L )*N + 1
               LDQ( L ) = MAX( 1, N )
               IXQ( L ) = IXT( L )
               IF( L.LE.INT( K/2 ) ) THEN
                  I = S( K - L + 1 )
                  S( K - L + 1 ) = S( L )
                  S( L ) = I
               END IF
   20       CONTINUE
            DO 30 L = 1, K
               CALL DLACPY( 'Full', N, N, A( 1, 1, K-L+1 ), LDA1,
     $                      T( IXT( L ) ), LDT( L ) )
   30       CONTINUE
            IF( LSAME( COMPQ, 'U' ) .OR. LSAME( COMPQ, 'I' ) ) THEN
               COMPQ = 'U'
               DO 40 L = 1, K
                  CALL DLACPY( 'Full', N, N, Q( 1, 1, K-L+1 ), LDQ1,
     $                         QK( IXQ( L ) ), LDQ( L ) )
   40          CONTINUE
            ELSE IF( LSAME( COMPQ, 'P' ) ) THEN
               COMPQ = 'W'
               DO 50 L = 1, K
                  IF( QIND( L ).LT.0 )
     $                QIND( L ) = 2
                  P = QIND( L )
                  IF( P.NE.0 )
     $               CALL DLACPY( 'Full', N, N, Q( 1, 1, K-P+1 ), LDQ1,
     $                            QK( IXQ( P ) ), LDQ( P ) )
   50          CONTINUE
            END IF
*           Select eigenvalues with negative real part.
            DO 60 I = 1, N
               SELECT( I ) = ALPHAR( I ).LT.ZERO
   60       CONTINUE
            WRITE( NOUT, FMT = 99996 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAR( I ), I = 1, N )
            WRITE( NOUT, FMT = 99994 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAI( I ), I = 1, N )
            WRITE( NOUT, FMT = 99993 )
            WRITE( NOUT, FMT = 99995 ) (   BETA( I ), I = 1, N )
            WRITE( NOUT, FMT = 99992 )
            WRITE( NOUT, FMT = 99991 ) (   SCAL( I ), I = 1, N )
*           Compute the transformed matrices, after reordering the
*           eigenvalues.
            CALL MB03KD( COMPQ, QIND, STRONG, K, N, H, ND, NI, S,
     $                   SELECT, T, LDT, IXT, QK, LDQ, IXQ, M, TOL,
     $                   IWORK, DWORK, LDWORK, INFO )
            IF( INFO.NE.0 ) THEN
               WRITE( NOUT, FMT = 99990 ) INFO
            ELSE
               WRITE( NOUT, FMT = 99989 )
               DO 80 L = 1, K
                  P = K - L + 1
                  WRITE( NOUT, FMT = 99988 ) L
                  DO 70 I = 1, N
                     WRITE( NOUT, FMT = 99995 )
     $                    ( T( IXT( P ) + I - 1 + ( J - 1 )*LDT( P ) ),
     $                       J = 1, N )
   70             CONTINUE
   80          CONTINUE
               IF( LSAME( COMPQ, 'U' ) .OR. LSAME( COMPQ, 'I' ) ) THEN
                  WRITE( NOUT, FMT = 99987 )
                  DO 100 L = 1, K
                     P = K - L + 1
                     WRITE( NOUT, FMT = 99988 ) L
                     DO 90 I = 1, N
                        WRITE( NOUT, FMT = 99995 )
     $                       ( QK( IXQ( P ) + I - 1 +
     $                           ( J - 1 )*LDQ( P ) ), J = 1, N )
   90                CONTINUE
  100             CONTINUE
               ELSE IF( LSAME( COMPQ, 'W' ) ) THEN
                  WRITE( NOUT, FMT = 99987 )
                  DO 120 L = 1, K
                     IF( QIND( L ).GT.0 ) THEN
                        P = K - QIND( L ) + 1
                        WRITE( NOUT, FMT = 99988 ) QIND( L )
                        DO 110 I = 1, N
                           WRITE( NOUT, FMT = 99995 )
     $                          ( QK( IXQ( P ) + I - 1 +
     $                              ( J - 1 )*LDQ( P ) ), J = 1, N )
  110                   CONTINUE
                     END IF
  120             CONTINUE
               END IF
            END IF
         ELSE
            WRITE( NOUT, FMT = 99979 ) IWARN
         END IF
      END IF
      STOP
*
99999 FORMAT( 'MB03KD EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT( 'INFO on exit from MB03BD = ', I2 )
99996 FORMAT( 'The vector ALPHAR is ' )
99995 FORMAT( 50( 1X, F8.4 ) )
99994 FORMAT( 'The vector ALPHAI is ' )
99993 FORMAT( 'The vector BETA is ' )
99992 FORMAT( 'The vector SCAL is ' )
99991 FORMAT( 50( 1X, I5 ) )
99990 FORMAT( 'INFO on exit from MB03KD = ', I2 )
99989 FORMAT( 'The matrix A on exit is ' )
99988 FORMAT( 'The factor ', I2, ' is ' )
99987 FORMAT( 'The matrix Q on exit is ' )
99986 FORMAT( 'LDT', 3I5 )
99985 FORMAT( 'IXT', 3I5 )
99984 FORMAT( 'LDQ', 3I5 )
99983 FORMAT( 'IXQ', 3I5 )
99982 FORMAT( 'ND' , 3I5 )
99981 FORMAT( 'NI' , 3I5)
99980 FORMAT( 'SELECT', 3L5 )
99979 FORMAT( 'IWARN on exit from MB03BD = ', I2 )
      END
