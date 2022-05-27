*     FD01AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT, NOUT1
      PARAMETER        ( NIN = 5, NOUT = 6, NOUT1 = 7 )
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
      INTEGER          IMAX, LMAX
      PARAMETER        ( IMAX = 500, LMAX = 10 )
      DOUBLE PRECISION LAMBDA
      PARAMETER        ( LAMBDA = 0.99D0 )
*     .. Local Scalars ..
      CHARACTER        JP
      INTEGER          I, INFO, IWARN, L
      DOUBLE PRECISION DELTA, EFOR, EOUT, EPOS, XIN, YIN
*     .. Local Arrays ..
      DOUBLE PRECISION CTETA(LMAX), EPSBCK(LMAX+1), SALPH(LMAX),
     $                 STETA(LMAX), XF(LMAX), YQ(LMAX)
*     .. External Functions ..
      DOUBLE PRECISION XFCN, YFCN
      EXTERNAL         XFCN, YFCN
*     NOTE: XFCN() generates at each iteration the next sample of the
*           input sequence. YFCN() generates at each iteration the next
*           sample of the reference sequence. These functions are user
*           defined (obtained from data acquisition devices, for
*           example).
*     .. External Subroutines ..
      EXTERNAL         FD01AD
*
*     .. File for the output error sequence ..
      OPEN ( UNIT = NOUT1, FILE = 'ERR.OUT', STATUS = 'REPLACE' )
*     ..  Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) L, DELTA, JP
      IF ( L.LE.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) L
      ELSE
         IF ( DELTA.LT.ZERO ) THEN
            WRITE ( NOUT, FMT = 99991 )
         ELSE
*
            DO 10 I = 1, L
               CTETA(I)  = ONE
               STETA(I)  = ZERO
               EPSBCK(I) = ZERO
               XF(I) = ZERO
               YQ(I) = ZERO
   10       CONTINUE
            EPSBCK(L+1) = ONE
            EFOR = DELTA
*           .. Run least squares filter.
            DO 20 I = 1, IMAX
               XIN = XFCN(I)
               YIN = YFCN(I)
               CALL FD01AD( JP, L, LAMBDA, XIN, YIN, EFOR, XF, EPSBCK,
     $                      CTETA, STETA, YQ, EPOS, EOUT, SALPH, IWARN,
     $                      INFO)
               WRITE(NOUT1,*) EOUT
   20       CONTINUE
            CLOSE(NOUT1)
*           NOTE:  File 'ERR.OUT' now contains the output error
*                  sequence.
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 )
               DO 30 I = 1, L
                  WRITE ( NOUT, FMT = 99996 ) I, XF(I), YQ(I), EPSBCK(I)
   30          CONTINUE
               WRITE ( NOUT, FMT = 99995 ) L+1, EPSBCK(L+1)
               WRITE ( NOUT, FMT = 99994 ) EFOR
               IF ( IWARN.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99993 ) IWARN
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' FD01AD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' INFO on exit from FD01AD = ', I2)
99997 FORMAT ('  i', 7X, 'XF(i)', 7X, 'YQ(i)', 6X, 'EPSBCK(i)', /1X)
99996 FORMAT ( I3, 2X, 3(2X, F10.6))
99995 FORMAT ( I3, 28X, F10.6, /1X)
99994 FORMAT (' EFOR = ', D10.3)
99993 FORMAT (' IWARN on exit from FD01AD = ', I2)
99992 FORMAT (/' L is out of range.',/' L = ',I5)
99991 FORMAT (/' The exponentially weighted forward prediction error',
     $         '  energy must be non-negative.' )
*
      END
*
*     .. Example functions ..
*
      DOUBLE PRECISION FUNCTION XFCN( I )
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE, SIN
*     .. Local Scalar ..
      INTEGER          I
*     .. Executable Statements ..
      XFCN = SIN( 0.3D0*DBLE( I ) )
* *** Last line of XFCN ***
      END
*
      DOUBLE PRECISION FUNCTION YFCN( I )
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE, SIN
*     .. Local Scalar ..
      INTEGER          I
*     .. Executable Statements ..
      YFCN = 0.5D0 * SIN( 0.3D0*DBLE( I ) ) +
     $       2.0D0 * SIN( 0.3D0*DBLE( I-1 ) )
* *** Last line of YFCN ***
      END
