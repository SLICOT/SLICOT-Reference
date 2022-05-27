*     MB04BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 50 )
      INTEGER            LDA, LDB, LDC1, LDC2, LDDE, LDF, LDQ1, LDQ2,
     $                   LDVW, LDWORK, LIWORK
      PARAMETER          (  LDA = NMAX/2,  LDB = NMAX/2, LDC1 = NMAX/2,
     $                     LDC2 = NMAX/2, LDDE = NMAX/2,  LDF = NMAX/2,
     $                     LDQ1 = NMAX, LDQ2 = NMAX, LDVW = NMAX/2,
     $                     LDWORK = 2*NMAX*NMAX + MAX( 4*NMAX, 36 ),
     $                     LIWORK = MAX( NMAX + 12, 2*NMAX + 3 ) )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ1, COMPQ2, JOB
      INTEGER            I, INFO, J, M, N
*
*     .. Local Arrays ..
      INTEGER            IWORK( LIWORK )
      DOUBLE PRECISION   A( LDA, NMAX/2 ), ALPHAI( NMAX/2 ),
     $                   ALPHAR( NMAX/2 ), B( LDB, NMAX/2 ),
     $                   BETA( NMAX/2 ), C1( LDC1, NMAX/2 ),
     $                   C2( LDC2, NMAX/2 ), DE( LDDE, NMAX/2+1 ),
     $                   DWORK( LDWORK ),  F( LDF, NMAX/2 ),
     $                   Q1( LDQ1, NMAX ), Q2( LDQ2, NMAX ),
     $                   VW( LDVW, NMAX/2+1 )
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           MB04BD
*
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*
*     .. Executable Statements ..
*
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) JOB, COMPQ1, COMPQ2, N
      IF( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
         M = N/2
         READ( NIN, FMT = * ) ( (  A( I, J ), J = 1, M   ), I = 1, M )
         READ( NIN, FMT = * ) ( ( DE( I, J ), J = 1, M+1 ), I = 1, M )
         READ( NIN, FMT = * ) ( ( C1( I, J ), J = 1, M   ), I = 1, M )
         READ( NIN, FMT = * ) ( ( VW( I, J ), J = 1, M+1 ), I = 1, M )
*        Compute the eigenvalues of a real skew-Hamiltonian/Hamiltonian
*        pencil.
         CALL MB04BD( JOB, COMPQ1, COMPQ2, N, A, LDA, DE, LDDE, C1,
     $                LDC1, VW, LDVW, Q1, LDQ1, Q2, LDQ2, B, LDB, F,
     $                LDF, C2, LDC2, ALPHAR, ALPHAI, BETA, IWORK,
     $                LIWORK, DWORK, LDWORK, INFO )
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
               WRITE( NOUT, FMT = 99995 ) ( DE( I, J ), J = 2, M+1 )
   20       CONTINUE
            WRITE( NOUT, FMT = 99993 )
            DO 30 I = 1, M
               WRITE( NOUT, FMT = 99995 ) ( B( I, J ), J = 1, M )
   30       CONTINUE
            WRITE( NOUT, FMT = 99992 )
            DO 40 I = 1, M
               WRITE( NOUT, FMT = 99995 ) ( F( I, J ), J = 1, M )
   40       CONTINUE
            WRITE( NOUT, FMT = 99991 )
            DO 50 I = 1, M
               WRITE( NOUT, FMT = 99995 ) ( C1( I, J ), J = 1, M )
   50       CONTINUE
            WRITE( NOUT, FMT = 99990 )
            DO 60 I = 1, M
               WRITE( NOUT, FMT = 99995 ) ( C2( I, J ), J = 1, M )
   60       CONTINUE
            WRITE( NOUT, FMT = 99989 )
            DO 70 I = 1, M
               WRITE( NOUT, FMT = 99995 ) ( VW( I, J ), J = 2, M+1 )
   70       CONTINUE
            WRITE( NOUT, FMT = 99988 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAR( I ), I = 1, M )
            WRITE( NOUT, FMT = 99987 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAI( I ), I = 1, M )
            WRITE( NOUT, FMT = 99986 )
            WRITE( NOUT, FMT = 99995 ) (   BETA( I ), I = 1, M )
            WRITE( NOUT, FMT = 99985 )
            IF( .NOT.LSAME( COMPQ1, 'N' ) ) THEN
               DO 80 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( Q1( I, J ), J = 1, N )
   80          CONTINUE
            END IF
            IF( .NOT.LSAME( COMPQ2, 'N' ) ) THEN
               WRITE( NOUT, FMT = 99984 )
               DO 90 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( Q2( I, J ), J = 1, N )
   90          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT( 'MB04BD EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT( 'INFO on exit from MB04BD = ', I2 )
99996 FORMAT( 'The matrix A on exit is ' )
99995 FORMAT( 50( 1X, F8.4 ) )
99994 FORMAT( 'The matrix D on exit is ' )
99993 FORMAT( 'The matrix B on exit is ' )
99992 FORMAT( 'The matrix F on exit is ' )
99991 FORMAT( 'The matrix C1 on exit is ' )
99990 FORMAT( 'The matrix C2 on exit is ' )
99989 FORMAT( 'The matrix V on exit is ' )
99988 FORMAT( 'The vector ALPHAR is ' )
99987 FORMAT( 'The vector ALPHAI is ' )
99986 FORMAT( 'The vector BETA is ' )
99985 FORMAT( 'The matrix Q1 is ' )
99984 FORMAT( 'The matrix Q2 is ' )
      END
