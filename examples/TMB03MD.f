*     MB03MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
*     .. Local Scalars ..
      DOUBLE PRECISION PIVMIN, RELTOL, SAFMIN, THETA, TOL
      INTEGER          I, INFO, IWARN, L, N
*     .. Local Arrays ..
      DOUBLE PRECISION E(NMAX-1), E2(NMAX-1), Q(NMAX), Q2(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION DLAMCH
      EXTERNAL         DLAMCH
*     .. External Subroutines ..
      EXTERNAL         MB03MD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, THETA, L, TOL, RELTOL
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) N
      ELSE IF ( L.LT.0 .OR. L.GT.N ) THEN
         WRITE ( NOUT, FMT = 99990 ) L
      ELSE
         READ ( NIN, FMT = * ) ( Q(I), I = 1,N )
         READ ( NIN, FMT = * ) ( E(I), I = 1,N-1 )
*        Print out the bidiagonal matrix J.
         WRITE ( NOUT, FMT = 99997 )
         DO 20 I = 1, N - 1
            WRITE ( NOUT, FMT = 99996 ) I, I, Q(I), I, (I+1), E(I)
   20    CONTINUE
         WRITE ( NOUT, FMT = 99995 ) N, N, Q(N)
*        Compute Q**2, E**2, and PIVMIN.
         Q2(N) = Q(N)**2
         PIVMIN = Q2(N)
         DO 40 I = 1, N - 1
            Q2(I) = Q(I)**2
            E2(I) = E(I)**2
            PIVMIN = MAX( PIVMIN, Q2(I), E2(I) )
   40    CONTINUE
         SAFMIN = DLAMCH( 'Safe minimum' )
         PIVMIN = MAX( PIVMIN*SAFMIN, SAFMIN )
         TOL = MAX( TOL, ZERO )
         IF ( RELTOL.LE.ZERO )
     $      RELTOL = DLAMCH( 'Base' )*DLAMCH( 'Epsilon' )
*        Compute an upper bound THETA such that J has 3 singular values
*        < =  THETA.
         CALL MB03MD( N, L, THETA, Q, E, Q2, E2, PIVMIN, TOL, RELTOL,
     $                IWARN, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( IWARN.NE.0 ) WRITE ( NOUT, FMT = 99994 ) IWARN
            WRITE ( NOUT, FMT = 99993 ) THETA
            WRITE ( NOUT, FMT = 99992 ) L
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB03MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB03MD = ',I2)
99997 FORMAT (' The Bidiagonal Matrix J is',/)
99996 FORMAT (2(' (',I1,',',I1,') = ',F7.4,2X))
99995 FORMAT (' (',I1,',',I1,') = ',F7.4)
99994 FORMAT (' IWARN on exit from MB03MD = ',I2,/)
99993 FORMAT (/' The computed value of THETA is ',F7.4)
99992 FORMAT (/' J has ',I2,' singular values < =  THETA')
99991 FORMAT (/' N is out of range.',/' N = ',I5)
99990 FORMAT (/' L is out of range.',/' L = ',I5)
      END
