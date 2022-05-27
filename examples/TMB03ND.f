*     MB03ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
*     .. Local Scalars ..
      DOUBLE PRECISION PIVMIN, SAFMIN, THETA
      INTEGER          I, INFO, N, NUMSV
*     .. Local Arrays ..
      DOUBLE PRECISION E(NMAX-1), E2(NMAX-1), Q(NMAX), Q2(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION DLAMCH
      EXTERNAL         DLAMCH
*     .. External Functions ..
      INTEGER          MB03ND
      EXTERNAL         MB03ND
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, THETA
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
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
*        Compute the number of singular values of J < =  THETA.
         NUMSV = MB03ND( N, THETA, Q2, E2, PIVMIN, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99994 ) NUMSV, THETA
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB03ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB03ND = ',I2)
99997 FORMAT (' The Bidiagonal Matrix J is',/)
99996 FORMAT (2(' (',I1,',',I1,') = ',F7.4,2X))
99995 FORMAT (' (',I1,',',I1,') = ',F7.4)
99994 FORMAT (/' J has ',I2,' singular values < =  ',F7.4)
99993 FORMAT (/' N is out of range.',/' N = ',I5)
      END
