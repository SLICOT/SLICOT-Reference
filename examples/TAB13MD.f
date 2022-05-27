*     AB13MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 10, MMAX = 10 )
      INTEGER          LDZ
      PARAMETER        ( LDZ = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MAX( 4*MMAX-2, NMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 2*NMAX*NMAX*MMAX - NMAX*NMAX +
     $                            9*MMAX*MMAX + NMAX*MMAX + 11*NMAX +
     $                            33*MMAX - 11 )
      INTEGER          LZWORK
      PARAMETER        ( LZWORK = 6*NMAX*NMAX*MMAX + 12*NMAX*NMAX +
     $                            6*MMAX + 6*NMAX - 3 )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, M, N
      DOUBLE PRECISION BOUND
*     .. Local Arrays ..
      INTEGER          ITYPE(MMAX), IWORK(LIWORK), NBLOCK(MMAX)
      DOUBLE PRECISION D(NMAX), DWORK(LDWORK), G(NMAX), X(2*MMAX-1)
      COMPLEX*16       Z(LDZ,NMAX), ZWORK(LZWORK)
*     .. External Subroutines ..
      EXTERNAL         AB13MD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) M
      ELSE
         READ ( NIN, FMT = * ) ( NBLOCK(I), I = 1, M )
         READ ( NIN, FMT = * ) ( ITYPE(I), I = 1, M )
         READ ( NIN, FMT = * ) ( ( Z(I,J), J = 1,N ), I = 1,N )
*        Computing mu.
         CALL AB13MD( 'N', N, Z, LDZ, M, NBLOCK, ITYPE, X, BOUND, D, G,
     $               IWORK, DWORK, LDWORK, ZWORK, LZWORK, INFO )
*
         IF ( INFO.EQ.0 ) THEN
            WRITE ( NOUT, FMT = 99997 )
            WRITE ( NOUT, FMT = 99991 ) BOUND
         ELSE
            WRITE( NOUT, FMT = 99998 ) INFO
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB13MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB13MD =',I2)
99997 FORMAT (' The value of the structured singular value is'/)
99991 FORMAT (D17.10)
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
      END
