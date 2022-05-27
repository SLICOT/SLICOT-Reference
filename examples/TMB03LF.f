*     MB03LF EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 50 )
      INTEGER            LDB, LDFG, LDQ, LDU, LDWORK, LDZ, LIWORK
      PARAMETER          ( LDB = NMAX/2, LDFG = NMAX/2, LDQ = 2*NMAX,
     $                     LDU = NMAX,   LDZ  = NMAX,
     $                     LDWORK = 10*NMAX*NMAX +
     $                              MAX( NMAX*NMAX +
     $                                   MAX( NMAX/2 + 252, 432 ),
     $                                   MAX( 8*NMAX +  48, 171 ) ),
     $                     LIWORK = MAX( NMAX + 18, NMAX/2 + 48,
     $                                   5*NMAX/2 + 1 ) )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ, COMPU, ORTH
      INTEGER            I, INFO, IWARN, J, M, N, NEIG
*
*     .. Local Arrays ..
      LOGICAL            BWORK( NMAX/2 )
      INTEGER            IWORK( LIWORK )
      DOUBLE PRECISION   ALPHAI( NMAX/2 ),  ALPHAR( NMAX/2 ),
     $                   B( LDB, NMAX/2 ),    BETA( NMAX/2 ),
     $                   DWORK( LDWORK ), FG( LDFG, NMAX/2+1 ),
     $                   Q( LDQ, 2*NMAX ),  U( LDU, 2*NMAX ),
     $                   Z( LDZ, NMAX )
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           MB03LF
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
*        deflating subspace and companion subspace of a real
*        skew-Hamiltonian/Hamiltonian pencil, corresponding to the
*        eigenvalues with strictly negative real part.
         CALL MB03LF( COMPQ, COMPU, ORTH, N, Z, LDZ, B, LDB, FG, LDFG,
     $                NEIG, Q, LDQ, U, LDU, ALPHAR, ALPHAI, BETA, IWORK,
     $                LIWORK, DWORK, LDWORK, BWORK, IWARN, INFO )
*
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE
            WRITE( NOUT, FMT = 99996 )
            DO 10 I = 1, N
               WRITE( NOUT, FMT = 99995 ) ( Z( I, J ), J = 1, N )
   10       CONTINUE
            WRITE( NOUT, FMT = 99994 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAR( I ), I = 1, M )
            WRITE( NOUT, FMT = 99993 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAI( I ), I = 1, M )
            WRITE( NOUT, FMT = 99992 )
            WRITE( NOUT, FMT = 99995 ) (   BETA( I ), I = 1, M )
            IF( LSAME( COMPQ, 'C' ) .AND. NEIG.GT.0 ) THEN
               WRITE( NOUT, FMT = 99991 )
               DO 20 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( Q( I, J ), J = 1, NEIG )
   20          CONTINUE
            END IF
            IF( LSAME( COMPU, 'C' ) .AND. NEIG.GT.0 ) THEN
               WRITE( NOUT, FMT = 99990 )
               DO 30 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( U( I, J ), J = 1, NEIG )
   30          CONTINUE
            END IF
            IF( LSAME( COMPQ, 'C' ) .OR. LSAME( COMPU, 'C' ) )
     $         WRITE( NOUT, FMT = 99989 ) NEIG
         END IF
      END IF
      STOP
*
99999 FORMAT ( 'MB03LF EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT ( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT ( 'INFO on exit from MB03LF = ', I2 )
99996 FORMAT (/'The matrix Z on exit is ' )
99995 FORMAT ( 50( 1X, F8.4 ) )
99994 FORMAT (/'The vector ALPHAR is ' )
99993 FORMAT (/'The vector ALPHAI is ' )
99992 FORMAT (/'The vector BETA is ' )
99991 FORMAT (/'The deflating subspace corresponding to the ',
     $         'eigenvalues with negative real part is ' )
99990 FORMAT (/'The companion subspace corresponding to the ',
     $         'eigenvalues with negative real part is ' )
99989 FORMAT (/'The number of eigenvalues in the initial pencil with ',
     $         'negative real part is ', I2 )
      END
