*     MB04ED EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 60 )
      INTEGER            LDB, LDFG, LDQ, LDU1, LDU2, LDWORK, LDZ,
     $                   LIWORK
      PARAMETER          ( LDB = NMAX/2, LDFG = NMAX/2,
     $                     LDQ = NMAX,   LDU1 = NMAX/2, LDU2 = NMAX/2,
     $                     LDWORK = 3*NMAX**2/2 + MAX( NMAX, 24 ) + 3,
     $                     LDZ = NMAX, LIWORK = NMAX + 9 )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ, COMPU, JOB
      INTEGER            I, INFO, J, N
*
*     .. Local Arrays ..
      INTEGER            IWORK( LIWORK )
      DOUBLE PRECISION   ALPHAI( NMAX/2 ),   ALPHAR( NMAX/2 ),
     $                   B( LDB, NMAX/2 ),     BETA( NMAX/2 ),
     $                   DWORK( LDWORK ),  FG( LDFG, NMAX/2+1 ),
     $                   Q( LDQ, NMAX ),   U1( LDU1, NMAX/2 ),
     $                   U2( LDU2, NMAX/2 ), Z( LDZ, NMAX )
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           MB04ED
*
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MOD
*
*     .. Executable statements ..
*
      WRITE( NOUT, FMT = 99999 )
*
*     Skip first line in data file.
*
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) JOB, COMPQ, COMPU, N
      READ( NIN, FMT = * ) ( (  Z( I, J ), J = 1, N ),     I = 1, N )
      READ( NIN, FMT = * ) ( (  B( I, J ), J = 1, N/2 ),   I = 1, N/2 )
      READ( NIN, FMT = * ) ( ( FG( I, J ), J = 1, N/2+1 ), I = 1, N/2 )
      IF( LSAME( COMPU, 'U' ) ) THEN
         READ( NIN, FMT = * ) ( ( U1( I, J ), J = 1, N/2 ), I = 1, N/2 )
         READ( NIN, FMT = * ) ( ( U2( I, J ), J = 1, N/2 ), I = 1, N/2 )
      END IF
      IF( N.LT.0 .OR. N.GT.NMAX .OR. MOD( N, 2 ).NE.0 ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
*
*        Test of MB04ED.
*
         CALL MB04ED( JOB, COMPQ, COMPU, N, Z, LDZ, B, LDB, FG, LDFG, Q,
     $                LDQ, U1, LDU1, U2, LDU2, ALPHAR, ALPHAI, BETA,
     $                IWORK, LIWORK, DWORK, LDWORK, INFO )
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE
            WRITE( NOUT, FMT = 99996 )
            DO 10 I = 1, N
               WRITE( NOUT, FMT = 99995 ) ( Z(I,J), J = 1, N )
   10       CONTINUE
            WRITE( NOUT, FMT = 99994 )
            DO 20 I = 1, N/2
               WRITE( NOUT, FMT = 99995 ) ( B(I,J), J = 1, N/2 )
   20       CONTINUE
            WRITE( NOUT, FMT = 99993 )
            DO 30 I = 1, N/2
               WRITE( NOUT, FMT = 99995 ) ( FG(I,J), J = 1, N/2+1 )
   30       CONTINUE
            WRITE( NOUT, FMT = 99992 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAR(I), I = 1, N/2 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAI(I), I = 1, N/2 )
            WRITE( NOUT, FMT = 99995 ) (   BETA(I), I = 1, N/2 )
            WRITE( NOUT, FMT = 99991 )
            DO 40 I = 1, N
               WRITE( NOUT, FMT = 99995 ) ( Q( I, J ), J = 1, N )
   40       CONTINUE
            IF ( .NOT.LSAME( COMPU, 'N' ) ) THEN
               WRITE( NOUT, FMT = 99990 )
               DO 50 I = 1, N/2
                  WRITE( NOUT, FMT = 99995 ) ( U1( I, J ), J = 1, N/2 )
   50          CONTINUE
               WRITE( NOUT, FMT = 99989 )
               DO 60 I = 1, N/2
                  WRITE( NOUT, FMT = 99995 ) ( U2( I, J ), J = 1, N/2 )
   60          CONTINUE
            END IF
         END IF
      END IF
      STOP
99999 FORMAT ( 'MB04ED EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT ( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT ( 'INFO on exit from MB04ED = ', I2 )
99996 FORMAT (/' The transformed matrix Z is' )
99995 FORMAT ( 60( 1X, F8.4 ) )
99994 FORMAT (/' The transformed matrix B is' )
99993 FORMAT (/' The transformed matrix FG is' )
99992 FORMAT (/' The real, imaginary, and beta parts of eigenvalues are'
     $       )
99991 FORMAT (/' The matrix Q is ' )
99990 FORMAT (/' The upper left block of the matrix U is ' )
99989 FORMAT (/' The upper right block of the matrix U is ' )
      END
 
 
