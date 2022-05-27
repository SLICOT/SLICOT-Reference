*     MB03SD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDQG
      PARAMETER        ( LDA = NMAX, LDQG = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX*( NMAX+1 ) )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, N
      CHARACTER*1      JOBSCL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), QG(LDQG,NMAX+1),
     $                 WI(NMAX), WR(NMAX)
*     .. External Subroutines ..
      EXTERNAL         MB03SD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
*     NOTE: input must define a square-reduced Hamiltonian matrix.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, JOBSCL
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99998 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J),    J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( QG(J,I+1), I = J,N ), J = 1,N )
         READ ( NIN, FMT = * ) ( ( QG(I,J),   I = J,N ), J = 1,N )
*        Compute the eigenvalues.
         CALL MB03SD( JOBSCL, N, A, LDA, QG, LDQG, WR, WI, DWORK,
     $                LDWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99997 ) INFO
         ELSE
*           Show the computed eigenvalues.
            WRITE ( NOUT, FMT = 99996 )
            DO 10 I = 1, N
               WRITE ( NOUT, FMT = 99995 )  WR(I), ' + (',  WI(I), ')i'
10          CONTINUE
            DO 20 I = N, 1, -1
               WRITE ( NOUT, FMT = 99995 ) -WR(I), ' + (', -WI(I), ')i'
20          CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB03SD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (/' N is out of range.',/' N = ',I5)
99997 FORMAT (' INFO on exit from MB03SD = ',I2)
99996 FORMAT (/' The eigenvalues are ')
99995 FORMAT (1X,F8.4,A,F8.4,A)
      END
