*     MB04FD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 50 )
      INTEGER            LDA, LDB, LDDE, LDFG, LDQ, LDWORK
      PARAMETER          ( LDA  = NMAX/2, LDB = NMAX/2, LDDE = NMAX/2,
     $                     LDFG = NMAX/2, LDQ = NMAX, LDWORK =
     $                     3*NMAX*NMAX/4 )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ, JOB
      INTEGER            I, INFO, J, N
*
*     .. Local Arrays ..
      DOUBLE PRECISION   A( LDA, NMAX/2 ),  ALPHAI( NMAX/2 ),
     $                   ALPHAR( NMAX/2 ),  B( LDB, NMAX/2 ),
     $                   BETA( NMAX/2 ),  DE( LDDE, NMAX/2+1 ),
     $                   DWORK( LDWORK ), FG( LDFG, NMAX/2+1 ),
     $                   Q( LDQ, NMAX )
      INTEGER            IWORK( NMAX/2+1 )
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           MB04FD
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
      READ( NIN, FMT = * ) JOB, COMPQ, N
      READ( NIN, FMT = * ) ( (  A( I, J ), J = 1, N/2 ),   I = 1, N/2 )
      READ( NIN, FMT = * ) ( ( DE( I, J ), J = 1, N/2+1 ), I = 1, N/2 )
      READ( NIN, FMT = * ) ( (  B( I, J ), J = 1, N/2 ),   I = 1, N/2 )
      READ( NIN, FMT = * ) ( ( FG( I, J ), J = 1, N/2+1 ), I = 1, N/2 )
      IF( LSAME( COMPQ, 'U' ) )
     $   READ( NIN, FMT = * ) ( ( Q( I, J ), J = 1, N ), I = 1, N )
      IF( N.LT.0 .OR. N.GT.NMAX .OR. MOD( N, 2 ).NE.0 ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
*
*        Test of MB04FD.
*
         CALL MB04FD( JOB, COMPQ, N, A, LDA, DE, LDDE, B, LDB, FG, LDFG,
     $                Q, LDQ, ALPHAR, ALPHAI, BETA, IWORK, DWORK,
     $                LDWORK, INFO )
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE
            IF( LSAME( JOB, 'T' ) ) THEN
               WRITE( NOUT, FMT = 99996 )
               DO 10 I = 1, N/2
                  WRITE( NOUT, FMT = 99995 ) ( A( I, J ), J = 1, N/2 )
   10          CONTINUE
            END IF
            WRITE( NOUT, FMT = 99994 )
            DO 20 I = 1, N/2
               WRITE( NOUT, FMT = 99995 ) ( DE( I, J ), J = 1, N/2+1 )
   20       CONTINUE
            IF( LSAME( JOB, 'T' ) ) THEN
               WRITE( NOUT, FMT = 99993 )
               DO 30 I = 1, N/2
                  WRITE( NOUT, FMT = 99995 ) ( B( I, J ), J = 1, N/2 )
   30          CONTINUE
            END IF
            WRITE( NOUT, FMT = 99992 )
            DO 40 I = 1, N/2
               WRITE( NOUT, FMT = 99995 ) ( FG( I, J ), J = 1, N/2+1 )
   40       CONTINUE
            IF( .NOT.LSAME( COMPQ, 'N' ) ) THEN
               WRITE( NOUT, FMT = 99991 )
               DO 50 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( Q( I, J ), J = 1, N )
   50          CONTINUE
            END IF
            WRITE( NOUT, FMT = 99990 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAR(I), I = 1, N/2 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAI(I), I = 1, N/2 )
            WRITE( NOUT, FMT = 99995 ) (   BETA(I), I = 1, N/2 )
         END IF
      END IF
      STOP
99999 FORMAT ( 'MB04FD EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT ( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT ( 'INFO on exit from MB04FD = ', I2 )
99996 FORMAT (/' The transformed matrix A is' )
99995 FORMAT ( 51( 1X, F8.4 ) )
99994 FORMAT (/' The transformed matrix DE is' )
99993 FORMAT (/' The transformed matrix B is' )
99992 FORMAT (/' The transformed matrix FG is' )
99991 FORMAT (/' The transformed matrix Q is ' )
99990 FORMAT (/' The real, imaginary, and beta parts of eigenvalues are'
     $       )
      END
 
