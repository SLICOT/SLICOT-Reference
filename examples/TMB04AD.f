*     MB04AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
      INTEGER            NMAX
      PARAMETER          ( NMAX = 50 )
      INTEGER            LDH, LDQ1, LDQ2, LDT, LDU11, LDU12, LDU21,
     $                   LDU22, LDWORK, LDZ, LIWORK
      PARAMETER          ( LDH = NMAX, LDQ1  = NMAX,   LDQ2 = NMAX,
     $                     LDT = NMAX, LDU11 = NMAX/2, LDU12 = NMAX/2,
     $                     LDU21  = NMAX/2, LDU22 = NMAX/2,
     $                     LDWORK = 3*NMAX*NMAX + MAX( NMAX, 48 ) + 6,
     $                     LDZ = NMAX,  LIWORK = NMAX + 18 )
      DOUBLE PRECISION  ZERO
      PARAMETER         ( ZERO = 0.0D0 )
*
*     .. Local Scalars ..
      CHARACTER          COMPQ1, COMPQ2, COMPU1, COMPU2, JOB
      INTEGER            I, INFO, J, M, N
*
*     .. Local Arrays ..
      INTEGER            IWORK( LIWORK )
      DOUBLE PRECISION   ALPHAI( NMAX/2 ), ALPHAR( NMAX/2 ),
     $                   BETA( NMAX/2 ), DWORK( LDWORK ),
     $                   H( LDH, NMAX ), Q1( LDQ1, NMAX ),
     $                   Q2( LDQ2, NMAX ), T( LDT, NMAX ),
     $                   U11( LDU11, NMAX/2 ), U12( LDU12, NMAX/2 ),
     $                   U21( LDU21, NMAX/2 ), U22( LDU22, NMAX/2 ),
     $                   Z( LDZ, NMAX )
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*
*     .. External Subroutines ..
      EXTERNAL           DLASET, MB04AD
*
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*
*     .. Executable Statements ..
*
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ( NIN, FMT = * )
      READ( NIN, FMT = * ) JOB, COMPQ1, COMPQ2, COMPU1, COMPU2, N
      IF( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE( NOUT, FMT = 99998 ) N
      ELSE
         READ( NIN, FMT = * ) ( ( Z( I, J ), J = 1, N ), I = 1, N )
         READ( NIN, FMT = * ) ( ( H( I, J ), J = 1, N ), I = 1, N )
*        Compute the eigenvalues of a real skew-Hamiltonian/Hamiltonian
*        pencil (factored version).
         CALL MB04AD( JOB, COMPQ1, COMPQ2, COMPU1, COMPU2, N, Z, LDZ, H,
     $                LDH, Q1, LDQ1, Q2, LDQ2, U11, LDU11, U12, LDU12,
     $                U21, LDU21, U22, LDU22, T, LDT, ALPHAR, ALPHAI,
     $                BETA, IWORK, LIWORK, DWORK, LDWORK, INFO )
*
         IF( INFO.NE.0 ) THEN
            WRITE( NOUT, FMT = 99997 ) INFO
         ELSE
            M = N/2
            CALL DLASET( 'Full', M, M, ZERO, ZERO, Z( M+1, 1 ), LDZ )
            WRITE( NOUT, FMT = 99996 )
            DO 10 I = 1, N
               WRITE( NOUT, FMT = 99995 ) ( T( I, J ), J = 1, N )
   10       CONTINUE
            WRITE( NOUT, FMT = 99994 )
            DO 20 I = 1, N
               WRITE( NOUT, FMT = 99995 ) ( Z( I, J ), J = 1, N )
   20       CONTINUE
            WRITE( NOUT, FMT = 99993 )
            DO 30 I = 1, N
               WRITE( NOUT, FMT = 99995 ) ( H( I, J ), J = 1, N )
   30       CONTINUE
            IF( LSAME( COMPQ1, 'I' ) ) THEN
               WRITE( NOUT, FMT = 99992 )
               DO 40 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( Q1( I, J ), J = 1, N )
   40          CONTINUE
            END IF
            IF( LSAME( COMPQ2, 'I' ) ) THEN
               WRITE( NOUT, FMT = 99991 )
               DO 50 I = 1, N
                  WRITE( NOUT, FMT = 99995 ) ( Q2( I, J ), J = 1, N )
   50          CONTINUE
            END IF
            IF( LSAME( COMPU1, 'I' ) ) THEN
               WRITE( NOUT, FMT = 99990 )
               DO 60 I = 1, M
                  WRITE( NOUT, FMT = 99995 ) ( U11( I, J ), J = 1, M )
   60          CONTINUE
               WRITE( NOUT, FMT = 99989 )
               DO 70 I = 1, M
                  WRITE( NOUT, FMT = 99995 ) ( U12( I, J ), J = 1, M )
   70          CONTINUE
            END IF
            IF( LSAME( COMPU2, 'I' ) ) THEN
               WRITE( NOUT, FMT = 99988 )
               DO 80 I = 1, M
                  WRITE( NOUT, FMT = 99995 ) ( U21( I, J ), J = 1, M )
   80          CONTINUE
               WRITE( NOUT, FMT = 99987 )
               DO 90 I = 1, M
                  WRITE( NOUT, FMT = 99995 ) ( U22( I, J ), J = 1, M )
   90          CONTINUE
            END IF
            WRITE( NOUT, FMT = 99986 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAR( I ), I = 1, M )
            WRITE( NOUT, FMT = 99985 )
            WRITE( NOUT, FMT = 99995 ) ( ALPHAI( I ), I = 1, M )
            WRITE( NOUT, FMT = 99984 )
            WRITE( NOUT, FMT = 99995 ) (   BETA( I ), I = 1, M )
         END IF
      END IF
      STOP
* 
99999 FORMAT( 'MB04AD EXAMPLE PROGRAM RESULTS', 1X )
99998 FORMAT( 'N is out of range.', /, 'N = ', I5 )
99997 FORMAT( 'INFO on exit from MB04AD = ', I2 )
99996 FORMAT( 'The matrix T on exit is ' )
99995 FORMAT( 50( 1X, F8.4 ) )
99994 FORMAT( 'The matrix Z on exit is ' )
99993 FORMAT( 'The matrix H is ' )
99992 FORMAT( 'The matrix Q1 is ' )
99991 FORMAT( 'The matrix Q2 is ' )
99990 FORMAT( 'The upper left block of the matrix U1 is ' )
99989 FORMAT( 'The upper right block of the matrix U1 is ' )
99988 FORMAT( 'The upper left block of the matrix U2 is ' )
99987 FORMAT( 'The upper right block of the matrix U2 is ' )
99986 FORMAT( 'The vector ALPHAR is ' )
99985 FORMAT( 'The vector ALPHAI is ' )
99984 FORMAT( 'The vector BETA is ' )
      END
