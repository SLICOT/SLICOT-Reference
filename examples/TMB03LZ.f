*     MB03LZ EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 50 )
      INTEGER            LDA, LDB, LDDE, LDFG, LDQ, LDWORK, LZWORK
      PARAMETER          ( LDA = NMAX, LDB = NMAX, LDDE = NMAX,
     $                     LDFG = NMAX, LDQ = 2*NMAX,
     $                     LDWORK = 11*NMAX*NMAX + 2*NMAX,
     $                     LZWORK =  8*NMAX + 4 )
*
*     .. Local Scalars ..
      CHARACTER*1        COMPQ, ORTH
      INTEGER            I, INFO, J, N, NEIG
*
*     .. Local Arrays ..
      COMPLEX*16         A( LDA, NMAX ), B( LDB, NMAX ),
     $                   DE( LDDE, NMAX ), FG( LDFG, NMAX ),
     $                   Q( LDQ, 2*NMAX ), ZWORK( LZWORK )
      DOUBLE PRECISION   ALPHAI( NMAX ), ALPHAR( NMAX ), BETA( NMAX ),
     $                   DWORK( LDWORK )
      INTEGER            IWORK( NMAX + 1 )
      LOGICAL            BWORK( NMAX )
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           MB03LZ
*
*     .. Intrinsic Functions ..
      INTRINSIC          MOD
*
*     .. Executable statements ..
*
      WRITE( NOUT, FMT = 99999 )
*
*     Skip first line in data file.
*
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) COMPQ, ORTH, N
      READ( NIN, FMT = * ) ( (  A( I, J ), J = 1, N/2 ),   I = 1, N/2 )
      READ( NIN, FMT = * ) ( ( DE( I, J ), J = 1, N/2+1 ), I = 1, N/2 )
      READ( NIN, FMT = * ) ( (  B( I, J ), J = 1, N/2 ),   I = 1, N/2 )
      READ( NIN, FMT = * ) ( ( FG( I, J ), J = 1, N/2+1 ), I = 1, N/2 )
      IF( N.LT.0 .OR. N.GT.NMAX .OR. MOD( N, 2 ).NE.0 ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
*        Compute the eigenvalues and an orthogonal basis of the right
*        deflating subspace of a complex skew-Hamiltonian/Hamiltonian
*        pencil, corresponding to the eigenvalues with strictly negative
*        real part.
         CALL MB03LZ( COMPQ, ORTH, N, A, LDA, DE, LDDE, B, LDB, FG,
     $                LDFG, NEIG, Q, LDQ, ALPHAR, ALPHAI, BETA, IWORK,
     $                DWORK, LDWORK, ZWORK, LZWORK, BWORK, INFO )
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE
            IF( LSAME( COMPQ, 'C' ) ) THEN
               WRITE( NOUT, FMT = 99996 )
               DO 10 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( A( I, J ), J = 1, N )
   10          CONTINUE
               WRITE( NOUT, FMT = 99994 )
               DO 20 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( DE( I, J ), J = 1, N )
   20          CONTINUE
               WRITE( NOUT, FMT = 99993 )
               DO 30 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( B( I, J ), J = 1, N )
   30          CONTINUE
               WRITE( NOUT, FMT = 99992 )
               DO 40 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( FG( I, J ), J = 1, N )
   40          CONTINUE
            END IF
            WRITE( NOUT, FMT = 99991 )
            WRITE( NOUT, FMT = 99990 ) ( ALPHAR( I ), I = 1, N )
            WRITE( NOUT, FMT = 99989 )
            WRITE( NOUT, FMT = 99990 ) ( ALPHAI( I ), I = 1, N )
            WRITE( NOUT, FMT = 99988 )
            WRITE( NOUT, FMT = 99990 ) (   BETA( I ), I = 1, N )
            IF( LSAME( COMPQ, 'C' ) .AND. NEIG.GT.0 ) THEN
               WRITE( NOUT, FMT = 99987 )
               DO 50 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( Q( I, J ), J = 1, NEIG )
   50          CONTINUE
            END IF
            IF( LSAME( COMPQ, 'C' ) )
     $         WRITE( NOUT, FMT = 99986 ) NEIG
         END IF
      END IF
      STOP
99999 FORMAT ( 'MB03LZ EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT ( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT ( 'INFO on exit from MB03LZ = ', I2 )
99996 FORMAT (/'The matrix A on exit is ' )
99995 FORMAT ( 20( 1X, F9.4, SP, F9.4, S, 'i ') )
99994 FORMAT (/'The matrix D on exit is ' )
99993 FORMAT (/'The matrix B on exit is ' )
99992 FORMAT (/'The matrix F on exit is ' )
99991 FORMAT ( 'The vector ALPHAR is ' )
99990 FORMAT ( 50( 1X, F8.4 ) )
99989 FORMAT (/'The vector ALPHAI is ' )
99988 FORMAT (/'The vector BETA is ' )
99987 FORMAT (/'The deflating subspace corresponding to the ',
     $         'eigenvalues with negative real part is ' )
99986 FORMAT (/'The number of eigenvalues in the initial pencil with ',
     $         'negative real part is ', I2 )
      END
