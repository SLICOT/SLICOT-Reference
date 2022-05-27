*     MB04AZ EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 50 )
      INTEGER            LDB, LDC, LDD, LDFG, LDQ, LDU, LDWORK, LDZ,
     $                   LIWORK, LZWORK
      PARAMETER          ( LDB = NMAX,  LDC =   NMAX, LDD = NMAX,
     $                     LDFG = NMAX, LDQ = 2*NMAX, LDU = NMAX,
     $                     LDWORK = 18*NMAX*NMAX + NMAX + MAX( 2*NMAX,
     $                                                         24 ) + 3,
     $                     LDZ = NMAX, LIWORK = 2*NMAX + 9,
     $                     LZWORK = 8*NMAX + 28 )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ, COMPU, JOB
      INTEGER            I, INFO, J, M, N
*
*     .. Local Arrays ..
      COMPLEX*16         B( LDB,   NMAX ),  C( LDC,   NMAX ),
     $                   D( LDD,   NMAX ), FG( LDFG,  NMAX ),
     $                   Q( LDQ, 2*NMAX ),  U( LDU, 2*NMAX ),
     $                   Z( LDZ,   NMAX ), ZWORK( LZWORK )
      DOUBLE PRECISION   ALPHAI( NMAX ), ALPHAR( NMAX ),
     $                   BETA(   NMAX ), DWORK(LDWORK )
      INTEGER            IWORK( LIWORK )
      LOGICAL            BWORK( NMAX )
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           MB04AZ
*
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MOD
*
*     .. Executable Statements ..
*
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) JOB, COMPQ, COMPU, N
      IF( N.LT.0 .OR. N.GT.NMAX .OR. MOD( N, 2 ).NE.0 ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
         READ( NIN, FMT = * ) ( (  Z( I, J ), J = 1, N ),     I=1, N )
         READ( NIN, FMT = * ) ( (  B( I, J ), J = 1, N/2 ),   I=1, N/2 )
         READ( NIN, FMT = * ) ( ( FG( I, J ), J = 1, N/2+1 ), I=1, N/2 )
*        Compute the eigenvalues of a complex skew-Hamiltonian/
*        Hamiltonian pencil (factored version).
         CALL MB04AZ( JOB, COMPQ, COMPU, N, Z, LDZ, B, LDB, FG, LDFG,
     $                D, LDD, C, LDC, Q, LDQ, U, LDU, ALPHAR, ALPHAI,
     $                BETA, IWORK, LIWORK, DWORK, LDWORK, ZWORK, LZWORK,
     $                BWORK, INFO )
*
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE
            M = N/2
            IF( LSAME( JOB, 'T' ) ) THEN
               WRITE( NOUT, FMT = 99996 )
               DO 10 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) (  Z( I, J ), J = 1, N )
   10          CONTINUE
               WRITE( NOUT, FMT = 99994 )
               DO 20 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) (  B( I, J ), J = 1, N )
   20          CONTINUE
               WRITE( NOUT, FMT = 99993 )
               DO 30 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( FG( I, J ), J = 1, N )
   30          CONTINUE
               WRITE( NOUT, FMT = 99992 )
               DO 40 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) (  D( I, J ), J = 1, N )
   40          CONTINUE
               WRITE( NOUT, FMT = 99991 )
               DO 50 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) (  C( I, J ), J = 1, N )
   50          CONTINUE
            END IF
            IF( LSAME( COMPQ, 'C' ) ) THEN
               WRITE( NOUT, FMT = 99990 )
               DO 60 I = 1, 2*N
                  WRITE( NOUT, FMT = 99995 ) ( Q( I, J ), J = 1, 2*N )
   60          CONTINUE
            END IF
            IF( LSAME( COMPU, 'C' ) ) THEN
               WRITE( NOUT, FMT = 99989 )
               DO 70 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( U( I, J ), J = 1, 2*N )
   70          CONTINUE
            END IF
            WRITE( NOUT, FMT = 99988 )
            WRITE( NOUT, FMT = 99987 ) ( ALPHAR( I ), I = 1, N )
            WRITE( NOUT, FMT = 99986 )
            WRITE( NOUT, FMT = 99987 ) ( ALPHAI( I ), I = 1, N )
            WRITE( NOUT, FMT = 99985 )
            WRITE( NOUT, FMT = 99987 ) (   BETA( I ), I = 1, N )
         END IF
      END IF
      STOP
* 
99999 FORMAT ( 'MB04AZ EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT ( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT ( 'INFO on exit from MB04AZ = ', I2 )
99996 FORMAT (/' The transformed matrix Z is' )
99995 FORMAT (20(1X,F9.4,SP,F9.4,S,'i '))
99994 FORMAT (/' The transformed matrix B is' )
99993 FORMAT (/' The transformed matrix FG is' )
99992 FORMAT (/' The matrix D is' )
99991 FORMAT (/' The matrix C is' )
99990 FORMAT (/' The matrix Q is' )
99989 FORMAT (/' The upper part of the matrix U is' )
99988 FORMAT (/' The vector ALPHAR is ' )
99987 FORMAT ( 50( 1X, F8.4 ) )
99986 FORMAT (/' The vector ALPHAI is ' )
99985 FORMAT (/' The vector BETA is ' )
      END
