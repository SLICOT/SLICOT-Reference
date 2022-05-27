*     SB01MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDZ
      PARAMETER        ( LDA = NMAX, LDZ = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 3*NMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO1, INFO2, J, N, NCONT
      CHARACTER*1      JOBZ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(NMAX), DWORK(LDWORK), G(NMAX),
     $                 WI(NMAX), WR(NMAX), Z(LDZ,NMAX)
*     .. External Subroutines ..
      EXTERNAL         AB01MD, SB01MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, TOL, JOBZ
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( B(I), I = 1,N )
         READ ( NIN, FMT = * ) ( WR(I), I = 1,N )
         READ ( NIN, FMT = * ) ( WI(I), I = 1,N )
*        First reduce the given system to canonical form.
         CALL AB01MD( JOBZ, N, A, LDA, B, NCONT, Z, LDZ, DWORK, TOL,
     $                DWORK(N+1), LDWORK-N, INFO1 )
*
         IF ( INFO1.EQ.0 ) THEN
*           Find the one-dimensional state feedback matrix G.
            CALL SB01MD( NCONT, N, A, LDA, B, WR, WI, Z, LDZ, G, DWORK,
     $                   INFO2 )
*
            IF ( INFO2.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) INFO2
            ELSE
               WRITE ( NOUT, FMT = 99996 ) ( G(I), I = 1,NCONT )
            END IF
         ELSE
            WRITE ( NOUT, FMT = 99998 ) INFO1
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB01MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB01MD =',I2)
99997 FORMAT (' INFO on exit from SB01MD =',I2)
99996 FORMAT (' The one-dimensional state feedback matrix G is',
     $       /20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
      END
