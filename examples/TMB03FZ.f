*     MB03FZ EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 50 )
      INTEGER            LDB, LDC, LDD, LDFG, LDQ, LDU, LDWORK, LDZ,
     $                   LIWORK, LZWORK
      PARAMETER          ( LDB  = NMAX, LDC =   NMAX, LDD = NMAX,
     $                     LDFG = NMAX, LDQ = 2*NMAX, LDU = NMAX,
     $                     LDWORK = 18*NMAX*NMAX + NMAX + 3 +
     $                              MAX( 2*NMAX, 24 ), LDZ  = NMAX,
     $                     LIWORK = 2*NMAX + 9, LZWORK = 8*NMAX + 28 )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ, COMPU, ORTH
      INTEGER            I, INFO, J, M, N, NEIG
*
*     .. Local Arrays ..
      COMPLEX*16         B( LDB, NMAX ), C( LDC, NMAX ), D( LDD, NMAX ),
     $                   FG( LDFG, NMAX ), Q( LDQ, 2*NMAX ),
     $                   U( LDU, 2*NMAX ), Z( LDZ, NMAX ),
     $                   ZWORK( LZWORK )
      DOUBLE PRECISION   ALPHAI( NMAX ),  ALPHAR( NMAX ), BETA( NMAX ),
     $                   DWORK( LDWORK )
      INTEGER            IWORK( LIWORK )
      LOGICAL            BWORK( NMAX )
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           MB03FZ
*
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MOD
*
*     .. Executable Statements ..
*
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) COMPQ, COMPU, ORTH, N
      IF( N.LT.0 .OR. N.GT.NMAX .OR. MOD( N, 2 ).NE.0 ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
         M = N/2
         READ( NIN, FMT = * ) ( (  Z( I, J ), J = 1, N   ), I = 1, N )
         READ( NIN, FMT = * ) ( (  B( I, J ), J = 1, M   ), I = 1, M )
         READ( NIN, FMT = * ) ( ( FG( I, J ), J = 1, M+1 ), I = 1, M )
*        Compute the eigenvalues and orthogonal bases of the right
*        deflating subspace and companion subspace of a complex
*        skew-Hamiltonian/Hamiltonian pencil, corresponding to the
*        eigenvalues with strictly negative real part.
         CALL MB03FZ( COMPQ, COMPU, ORTH, N, Z, LDZ, B, LDB, FG, LDFG,
     $                NEIG, D, LDD, C, LDC, Q, LDQ, U, LDU, ALPHAR,
     $                ALPHAI, BETA, IWORK, LIWORK, DWORK, LDWORK, ZWORK,
     $                LZWORK, BWORK, INFO )
*
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE
            WRITE( NOUT, FMT = 99996 )
            DO 10 I = 1, N
               WRITE( NOUT, FMT = 99995 ) ( Z( I, J ), J = 1, N )
   10       CONTINUE
            IF( LSAME( COMPQ, 'C' ) .OR. LSAME( COMPU, 'C' ) ) THEN
               WRITE( NOUT, FMT = 99994 )
               DO 20 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( D( I, J ), J = 1, N )
   20          CONTINUE
               WRITE( NOUT, FMT = 99993 )
               DO 30 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( C( I, J ), J = 1, N )
   30          CONTINUE
               WRITE( NOUT, FMT = 99992 )
               DO 40 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( B( I, J ), J = 1, N )
   40          CONTINUE
               WRITE( NOUT, FMT = 99991 )
               DO 50 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( FG( I, J ), J = 1, N )
   50          CONTINUE
            END IF
            WRITE( NOUT, FMT = 99990 )
            WRITE( NOUT, FMT = 99989 ) ( ALPHAR( I ), I = 1, N )
            WRITE( NOUT, FMT = 99988 )
            WRITE( NOUT, FMT = 99989 ) ( ALPHAI( I ), I = 1, N )
            WRITE( NOUT, FMT = 99987 )
            WRITE( NOUT, FMT = 99989 ) (   BETA( I ), I = 1, N )
            IF( LSAME( COMPQ, 'C' ) .AND. NEIG.GT.0 ) THEN
               WRITE( NOUT, FMT = 99986 )
               DO 60 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( Q( I, J ), J = 1, NEIG )
   60          CONTINUE
            END IF
            IF( LSAME( COMPU, 'C' ) .AND. NEIG.GT.0 ) THEN
               WRITE( NOUT, FMT = 99985 )
               DO 70 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( U( I, J ), J = 1, NEIG )
   70          CONTINUE
            END IF
            IF( LSAME( COMPQ, 'C' ) .OR. LSAME( COMPU, 'C' ) )
     $         WRITE( NOUT, FMT = 99984 ) NEIG
         END IF
      END IF
      STOP
*
99999 FORMAT ( 'MB03FZ EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT ( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT ( 'INFO on exit from MB03FZ = ', I2 )
99996 FORMAT (/'The matrix Z on exit is ' )
99995 FORMAT ( 20( 1X, F9.4, SP, F9.4, S, 'i ') )
99994 FORMAT (/'The matrix D is ' )
99993 FORMAT (/'The matrix C is ' )
99992 FORMAT (/'The matrix B on exit is ' )
99991 FORMAT (/'The matrix F on exit is ' )
99990 FORMAT (/'The vector ALPHAR is ' )
99989 FORMAT ( 50( 1X, F8.4 ) )
99988 FORMAT (/'The vector ALPHAI is ' )
99987 FORMAT (/'The vector BETA is ' )
99986 FORMAT (/'The deflating subspace corresponding to the ',
     $         'eigenvalues with negative real part is ' )
99985 FORMAT (/'The companion subspace corresponding to the ',
     $         'eigenvalues with negative real part is ' )
99984 FORMAT (/'The number of eigenvalues in the initial pencil with ',
     $         'negative real part is ', I2 )
      END
