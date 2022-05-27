*     MB03BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            KMAX, NMAX
      PARAMETER          ( KMAX = 6, NMAX = 50 )
      INTEGER            LDA1, LDA2, LDQ1, LDQ2, LDWORK, LIWORK
      PARAMETER          ( LDA1 = NMAX, LDA2 = NMAX, LDQ1 = NMAX,
     $                     LDQ2 = NMAX,
     $                     LDWORK = KMAX + MAX( 2*NMAX, 8*KMAX ),
     $                     LIWORK = 2*KMAX + NMAX )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ, DEFL, JOB
      INTEGER            H, I, IHI, ILO, INFO, IWARN, J, K, L, N
*
*     .. Local Arrays ..
      INTEGER            IWORK( LIWORK ), QIND( KMAX ), S( KMAX ),
     $                   SCAL( NMAX )
      DOUBLE PRECISION   A( LDA1, LDA2, KMAX ), ALPHAI( NMAX ),
     $                   ALPHAR( NMAX ), BETA( NMAX ), DWORK( LDWORK),
     $                   Q( LDQ1, LDQ2, KMAX )
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           MB03BD
*
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*
*     .. Executable Statements ..
*
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) JOB, DEFL, COMPQ, K, N, H, ILO, IHI
      IF( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
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
*        Compute the eigenvalues and the transformed matrices, if
*        required.
         CALL MB03BD( JOB, DEFL, COMPQ, QIND, K, N, H, ILO, IHI, S, A,
     $                LDA1, LDA2, Q, LDQ1, LDQ2, ALPHAR, ALPHAI, BETA,
     $                SCAL, IWORK, LIWORK, DWORK, LDWORK, IWARN, INFO )
*
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE IF( IWARN.EQ.0 ) THEN
            IF( LSAME( JOB, 'S' ) .OR. LSAME( JOB, 'T' ) ) THEN
               WRITE( NOUT, FMT = 99996 )
               DO 30 L = 1, K
                  WRITE( NOUT, FMT = 99988 ) L
                  DO 20 I = 1, N
                     WRITE( NOUT, FMT = 99995 ) ( A( I, J, L ), J = 1, N
     $                                          )
   20             CONTINUE
   30          CONTINUE
            END IF
            IF( LSAME( COMPQ, 'U' ) .OR. LSAME( COMPQ, 'I' ) ) THEN
               WRITE( NOUT, FMT = 99994 )
               DO 50 L = 1, K
                  WRITE( NOUT, FMT = 99988 ) L
                  DO 40 I = 1, N
                     WRITE( NOUT, FMT = 99995 ) ( Q( I, J, L ), J = 1, N
     $                                          )
   40             CONTINUE
   50          CONTINUE
            ELSE IF( LSAME( COMPQ, 'P' ) ) THEN
               WRITE( NOUT, FMT = 99994 )
               DO 70 L = 1, K
                  IF( QIND( L ).GT.0 ) THEN
                     WRITE( NOUT, FMT = 99988 ) QIND( L )
                     DO 60 I = 1, N
                        WRITE( NOUT, FMT = 99995 )
     $                       ( Q( I, J, QIND( L ) ), J = 1, N )
   60                CONTINUE
                  END IF
   70          CONTINUE
            END IF
            WRITE( NOUT, FMT = 99993 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAR( I ), I = 1, N )
            WRITE( NOUT, FMT = 99992 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAI( I ), I = 1, N )
            WRITE( NOUT, FMT = 99991 )
            WRITE( NOUT, FMT = 99995 ) (   BETA( I ), I = 1, N )
            WRITE( NOUT, FMT = 99990 )
            WRITE( NOUT, FMT = 99989 ) (   SCAL( I ), I = 1, N )
         ELSE
            WRITE( NOUT, FMT = 99987 ) IWARN
         END IF
      END IF
      STOP
*
99999 FORMAT( 'MB03BD EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT( 'INFO on exit from MB03BD = ', I2 )
99996 FORMAT( 'The matrix A on exit is ' )
99995 FORMAT( 50( 1X, F8.4 ) )
99994 FORMAT( 'The matrix Q on exit is ' )
99993 FORMAT( 'The vector ALPHAR is ' )
99992 FORMAT( 'The vector ALPHAI is ' )
99991 FORMAT( 'The vector BETA is ' )
99990 FORMAT( 'The vector SCAL is ' )
99989 FORMAT( 50( 1X, I8 ) )
99988 FORMAT( 'The factor ', I2, ' is ' )
99987 FORMAT( 'IWARN on exit from MB03BD = ', I2 )
      END
