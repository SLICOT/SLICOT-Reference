*     MB03LD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 50 )
      INTEGER            LDA, LDB, LDDE, LDFG, LDQ, LDWORK, LIWORK
      PARAMETER          (  LDA = NMAX/2, LDB = NMAX/2, LDDE = NMAX/2,
     $                     LDFG = NMAX/2, LDQ = 2*NMAX,
     $                     LDWORK = 8*NMAX*NMAX +
     $                              MAX( 8*NMAX + 32, NMAX/2 + 168,
     $                                   272 ),
     $                     LIWORK = MAX( 32, NMAX + 12, NMAX*2 + 3 ) )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ, ORTH
      INTEGER            I, INFO, J, M, N, NEIG
*
*     .. Local Arrays ..
      LOGICAL            BWORK( NMAX/2 )
      INTEGER            IWORK( LIWORK )
      DOUBLE PRECISION   A( LDA, NMAX/2 ),  ALPHAI( NMAX/2 ),
     $                   ALPHAR( NMAX/2 ),  B( LDB, NMAX/2 ),
     $                   BETA( NMAX/2 ),  DE( LDDE, NMAX/2+1 ),
     $                   DWORK( LDWORK ), FG( LDFG, NMAX/2+1 ),
     $                   Q( LDQ, 2*NMAX )
*
*     .. External Subroutines ..
      EXTERNAL           MB03LD
*
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*
*     .. Executable Statements ..
*
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) COMPQ, ORTH, N
      IF( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
         M = N/2
         READ( NIN, FMT = * ) ( (  A( I, J ), J = 1, M   ), I = 1, M )
         READ( NIN, FMT = * ) ( ( DE( I, J ), J = 1, M+1 ), I = 1, M )
         READ( NIN, FMT = * ) ( (  B( I, J ), J = 1, M   ), I = 1, M )
         READ( NIN, FMT = * ) ( ( FG( I, J ), J = 1, M+1 ), I = 1, M )
*        Compute the eigenvalues and an orthogonal basis of the right
*        deflating subspace of a real skew-Hamiltonian/Hamiltonian
*        pencil, corresponding to the eigenvalues with strictly negative
*        real part.
         CALL MB03LD( COMPQ, ORTH, N, A, LDA, DE, LDDE, B, LDB, FG,
     $                LDFG, NEIG, Q, LDQ, ALPHAR, ALPHAI, BETA, IWORK,
     $                LIWORK, DWORK, LDWORK, BWORK, INFO )
*
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE
            WRITE( NOUT, FMT = 99996 )
            DO 10 I = 1, M
               WRITE( NOUT, FMT = 99995 ) ( A( I, J ), J = 1, M )
   10       CONTINUE
            WRITE( NOUT, FMT = 99994 )
            DO 20 I = 1, M
               WRITE( NOUT, FMT = 99995 ) ( DE( I, J ), J = 1, M+1 )
   20       CONTINUE
            WRITE( NOUT, FMT = 99993 )
            DO 30 I = 1, M
               WRITE( NOUT, FMT = 99995 ) ( B( I, J ), J = 1, M )
   30       CONTINUE
            WRITE( NOUT, FMT = 99992 )
            DO 40 I = 1, M
               WRITE( NOUT, FMT = 99995 ) ( FG( I, J ), J = 2, M+1 )
   40       CONTINUE
            WRITE( NOUT, FMT = 99991 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAR( I ), I = 1, M )
            WRITE( NOUT, FMT = 99990 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAI( I ), I = 1, M )
            WRITE( NOUT, FMT = 99989 )
            WRITE( NOUT, FMT = 99995 ) (   BETA( I ), I = 1, M )
            IF( LSAME( COMPQ, 'C' ) .AND. NEIG.GT.0 ) THEN
               WRITE( NOUT, FMT = 99988 )
               DO 50 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( Q( I, J ), J = 1, NEIG )
   50          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT( 'MB03LD EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT( 'INFO on exit from MB03LD = ', I2 )
99996 FORMAT( 'The matrix A on exit is ' )
99995 FORMAT( 50( 1X, F8.4 ) )
99994 FORMAT( 'The matrix DE on exit is ' )
99993 FORMAT( 'The matrix C1 on exit is ' )
99992 FORMAT( 'The matrix V on exit is ' )
99991 FORMAT( 'The vector ALPHAR is ' )
99990 FORMAT( 'The vector ALPHAI is ' )
99989 FORMAT( 'The vector BETA is ' )
99988 FORMAT( 'The matrix Q is ' )
      END
