*     SB01DD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDG, LDZ
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDG = MMAX,
     $                   LDZ = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( 3*NMAX, MMAX*NMAX,
     $                                 MMAX*MMAX + 2*NMAX + 4*MMAX + 1 )
     $                 )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          COUNT, I, INDCON, INFO1, INFO2, J, M, N, NCONT
      CHARACTER*1      JOBZ
*     .. Local Arrays ..
      INTEGER          IWORK(MMAX), NBLK(NMAX)
      DOUBLE PRECISION A(LDA,NMAX), B(NMAX,MMAX), DWORK(LDWORK),
     $                 G(LDG,NMAX), WI(NMAX), WR(NMAX), Y(MMAX*NMAX),
     $                 Z(LDZ,NMAX)
*     .. External Subroutines ..
      EXTERNAL         AB01ND, SB01DD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, TOL, JOBZ
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            READ ( NIN, FMT = * ) ( WR(I), I = 1,N )
            READ ( NIN, FMT = * ) ( WI(I), I = 1,N )
            READ ( NIN, FMT = * ) ( Y(I),  I = 1,M*N )
*           First reduce the given system to canonical form.
            CALL AB01ND( JOBZ, N, M, A, LDA, B, LDB, NCONT, INDCON,
     $                   NBLK, Z, LDZ, DWORK, TOL, IWORK, DWORK(N+1),
     $                   LDWORK-N, INFO1 )
*
            IF ( INFO1.EQ.0 ) THEN
*              Find the state feedback matrix G.
               CALL SB01DD( N, M, INDCON, A, LDA, B, LDB, NBLK, WR, WI,
     $                      Z, LDZ, Y, COUNT, G, LDG, TOL, IWORK, DWORK,
     $                      LDWORK, INFO2 )
*
               IF ( INFO2.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99997 ) INFO2
               ELSE
                  WRITE ( NOUT, FMT = 99996 )
                  DO 10 I = 1, M
                     WRITE ( NOUT, FMT = 99995 ) ( G(I,J), J = 1,N )
   10             CONTINUE
               END IF
            ELSE
               WRITE ( NOUT, FMT = 99998 ) INFO1
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB01DD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB01ND =',I2)
99997 FORMAT (' INFO on exit from SB01DD =',I2)
99996 FORMAT (' The state feedback matrix G is')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' M is out of range.',/' M = ',I5)
      END
