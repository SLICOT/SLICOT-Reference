*     MB03BZ EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            KMAX, NMAX
      PARAMETER          ( KMAX = 6, NMAX = 50 )
      INTEGER            LDA1, LDA2, LDQ1, LDQ2, LDWORK, LZWORK
      PARAMETER          ( LDA1 = NMAX, LDA2 = NMAX, LDQ1 = NMAX,
     $                     LDQ2 = NMAX, LDWORK = NMAX, LZWORK = NMAX )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ, JOB
      INTEGER            I, IHI, ILO, INFO, J, K, L, N
*
*     .. Local Arrays ..
      COMPLEX*16         A( LDA1, LDA2, KMAX ), ALPHA( NMAX ),
     $                   BETA( NMAX ), Q( LDQ1, LDQ2, KMAX ),
     $                   ZWORK( LZWORK )
      DOUBLE PRECISION   DWORK( LDWORK)
      INTEGER            S( KMAX ), SCAL( NMAX )
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           MB03BZ
*
*     .. Executable Statements ..
*
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) JOB, COMPQ, K, N, ILO, IHI
      IF( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
         READ( NIN, FMT = * ) ( S( I ), I = 1, K )
         READ( NIN, FMT = * ) ( ( ( A( I, J, L ), J = 1, N ),
     $                                I = 1, N ), L = 1, K )
         IF( LSAME( COMPQ, 'V' ) )
     $      READ( NIN, FMT = * ) ( ( ( Q( I, J, L ), J = 1, N ),
     $                                   I = 1, N ), L = 1, K )
*        Compute the eigenvalues and the transformed matrices, if
*        required.
         CALL MB03BZ( JOB, COMPQ, K, N, ILO, IHI, S, A, LDA1, LDA2,
     $                Q, LDQ1, LDQ2, ALPHA, BETA, SCAL, DWORK, LDWORK,
     $                ZWORK, LZWORK, INFO )
*
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE
            IF( LSAME( JOB, 'S' ) ) THEN
               WRITE( NOUT, FMT = 99996 )
               DO 20 L = 1, K
                  WRITE( NOUT, FMT = 99995 ) L
                  DO 10 I = 1, N
                     WRITE( NOUT, FMT = 99994 ) ( A( I, J, L ), J = 1, N
     $                                          )
   10             CONTINUE
   20          CONTINUE
            END IF
            IF( .NOT.LSAME( COMPQ, 'N' ) ) THEN
               WRITE( NOUT, FMT = 99993 )
               DO 40 L = 1, K
                  WRITE( NOUT, FMT = 99995 ) L
                  DO 30 I = 1, N
                     WRITE( NOUT, FMT = 99994 ) ( Q( I, J, L ), J = 1, N
     $                                          )
   30             CONTINUE
   40          CONTINUE
            END IF
            WRITE( NOUT, FMT = 99992 )
            WRITE( NOUT, FMT = 99994 ) ( ALPHA( I ), I = 1, N )
            WRITE( NOUT, FMT = 99991 )
            WRITE( NOUT, FMT = 99994 ) (  BETA( I ), I = 1, N )
            WRITE( NOUT, FMT = 99990 )
            WRITE( NOUT, FMT = 99989 ) (  SCAL( I ), I = 1, N )
         END IF
      END IF
      STOP
*
99999 FORMAT( 'MB03BZ EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT( 'INFO on exit from MB03BZ = ', I2 )
99996 FORMAT(/'The matrix A on exit is ' )
99995 FORMAT( 'The factor ', I2, ' is ' )
99994 FORMAT( 50( 1X, F9.4, SP, F9.4, S, 'i ') )
99993 FORMAT(/'The matrix Q on exit is ' )
99992 FORMAT(/'The vector ALPHA is ' )
99991 FORMAT( 'The vector BETA is ' )
99990 FORMAT( 'The vector SCAL is ' )
99989 FORMAT( 50( 1X, I8 ) )
      END
