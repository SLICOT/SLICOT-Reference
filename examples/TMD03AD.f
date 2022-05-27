*     MD03AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER           NIN, NOUT
      PARAMETER         ( NIN = 5, NOUT = 6 )
      INTEGER           MMAX, NMAX
      PARAMETER         ( MMAX = 20, NMAX = 20 )
      INTEGER           LDWORK
      PARAMETER         ( LDWORK = MMAX + 2*NMAX + MMAX*NMAX +
     $                             MAX( NMAX*NMAX, 3*NMAX + MMAX ) )
*     .. The lengths of DPAR1, DPAR2, IPAR are set to 1, 1, and 5 ..
      INTEGER           LDPAR1, LDPAR2, LIPAR
      PARAMETER         ( LDPAR1 = 1, LDPAR2 = 1, LIPAR = 5 )
*     .. Local Scalars ..
      CHARACTER*1       ALG, STOR, UPLO, XINIT
      INTEGER           I, INFO, ITMAX, IWARN, M, N, NFEV, NJEV, NPRINT
      DOUBLE PRECISION  CGTOL, TOL
*     .. Array Arguments ..
      INTEGER           IPAR(LIPAR)
      DOUBLE PRECISION  DPAR1(LDPAR1), DPAR2(LDPAR2), DWORK(LDWORK),
     $                  X(NMAX)
*     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
*     .. External Subroutines ..
      EXTERNAL          MD03AD, MD03AF, NF01BV, NF01BX
*     .. Intrinsic Functions ..
      INTRINSIC         MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, ITMAX, NPRINT, TOL, CGTOL, XINIT,
     $                      ALG, STOR, UPLO
      IF( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) M
      ELSE
         IF( N.LE.0 .OR. N.GT.NMAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) N
         ELSE
            IF ( LSAME( XINIT, 'G' ) )
     $         READ ( NIN, FMT = * ) ( X(I), I = 1,N )
*           Solve a standard nonlinear least squares problem.
            IPAR(1) = M
            IF ( LSAME( ALG, 'D' ) ) THEN
               CALL MD03AD( XINIT, ALG, STOR, UPLO, MD03AF, NF01BV, M,
     $                      N, ITMAX, NPRINT, IPAR, LIPAR, DPAR1,
     $                      LDPAR1, DPAR2, LDPAR2, X, NFEV, NJEV, TOL,
     $                      CGTOL, DWORK, LDWORK, IWARN, INFO )
            ELSE
               CALL MD03AD( XINIT, ALG, STOR, UPLO, MD03AF, NF01BX, M,
     $                      N, ITMAX, NPRINT, IPAR, LIPAR, DPAR1,
     $                      LDPAR1, DPAR2, LDPAR2, X, NFEV, NJEV, TOL,
     $                      CGTOL, DWORK, LDWORK, IWARN, INFO )
            END IF
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               IF( IWARN.NE.0 ) WRITE ( NOUT, FMT = 99991 ) IWARN
               WRITE ( NOUT, FMT = 99997 ) DWORK(2)
               WRITE ( NOUT, FMT = 99996 ) NFEV, NJEV
               WRITE ( NOUT, FMT = 99994 )
               WRITE ( NOUT, FMT = 99995 ) ( X(I), I = 1, N )
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MD03AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MD03AD = ',I2)
99997 FORMAT (/' Final 2-norm of the residuals = ',D15.7)
99996 FORMAT (/' The number of function and Jacobian evaluations = ',
     $           2I7)
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' Final approximate solution is ' )
99993 FORMAT (/' M is out of range.',/' M = ',I5)
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (' IWARN on exit from MD03AD = ',I2)
      END
C
      SUBROUTINE MD03AF( IFLAG, M, N, IPAR, LIPAR, DPAR1, LDPAR1, DPAR2,
     $                   LDPAR2, X, NFEVL, E, J, LDJ, JTE, DWORK,
     $                   LDWORK, INFO )
C
C     This is the FCN routine for solving a standard nonlinear least
C     squares problem using SLICOT Library routine MD03AD. See the
C     argument FCN in the routine MD03AD for the description of
C     parameters.
C
C     The example programmed in this routine is adapted from that
C     accompanying the MINPACK routine LMDER.
C
C     ******************************************************************
C
C     .. Parameters ..
C     .. NOUT is the unit number for printing intermediate results ..
      INTEGER           NOUT
      PARAMETER         ( NOUT = 6 )
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           IFLAG, INFO, LDJ, LDPAR1, LDPAR2, LDWORK, LIPAR,
     $                  M, N, NFEVL
C     .. Array Arguments ..
      INTEGER           IPAR(*)
      DOUBLE PRECISION  DPAR1(*), DPAR2(*), DWORK(*), E(*), J(LDJ,*),
     $                  JTE(*), X(*)
C     .. Local Scalars ..
      INTEGER           I
      DOUBLE PRECISION  ERR, TMP1, TMP2, TMP3, TMP4
C     .. External Functions ..
      DOUBLE PRECISION  DNRM2
      EXTERNAL          DNRM2
C     .. External Subroutines ..
      EXTERNAL          DGEMV
C     .. DATA Statements ..
      DOUBLE PRECISION  Y(15)
      DATA              Y(1), Y(2), Y(3), Y(4), Y(5), Y(6), Y(7), Y(8),
     $                  Y(9), Y(10), Y(11), Y(12), Y(13), Y(14), Y(15)
     $                  / 1.4D-1, 1.8D-1, 2.2D-1, 2.5D-1, 2.9D-1,
     $                    3.2D-1, 3.5D-1, 3.9D-1, 3.7D-1, 5.8D-1,
     $                    7.3D-1, 9.6D-1, 1.34D0, 2.1D0,  4.39D0 /
C
C     .. Executable Statements ..
C
      INFO = 0
      IF ( IFLAG.EQ.1 ) THEN
C
C        Compute the error function values, e.
C
         DO 10 I = 1, 15
            TMP1 = I
            TMP2 = 16 - I
            IF ( I.GT.8 ) THEN
               TMP3 = TMP2
            ELSE
               TMP3 = TMP1
            END IF
            E(I) = Y(I) - ( X(1) + TMP1/( X(2)*TMP2 + X(3)*TMP3 ) )
   10    CONTINUE
C
      ELSE IF ( IFLAG.EQ.2 ) THEN
C
C        Compute the Jacobian.
C
         DO 30 I = 1, 15
            TMP1 = I
            TMP2 = 16 - I
            IF ( I.GT.8 ) THEN
               TMP3 = TMP2
            ELSE
               TMP3 = TMP1
            END IF
            TMP4 = ( X(2)*TMP2 + X(3)*TMP3 )**2
            J(I,1) = -ONE
            J(I,2) = TMP1*TMP2/TMP4
            J(I,3) = TMP1*TMP3/TMP4
   30    CONTINUE
C
C        Compute the product J'*e (the error e was computed in array E).
C
         CALL DGEMV( 'Transpose', M, N, ONE, J, LDJ, E, 1, ZERO, JTE,
     $               1 )
C
         NFEVL = 0
C
      ELSE IF ( IFLAG.EQ.3 ) THEN
C
C        Set the parameter LDJ, the length of the array J, and the sizes
C        of the workspace for MD03AF (IFLAG = 1 or 2), NF01BV and
C        NF01BX.
C
         LDJ = M
         IPAR(1) = M*N
         IPAR(2) = 0
         IPAR(3) = 0
         IPAR(4) = M
      ELSE IF ( IFLAG.EQ.0 ) THEN
C
C        Special call for printing intermediate results.
C
         ERR = DNRM2( M, E, 1 )
         WRITE( NOUT, '('' Norm of current error = '', D15.6)') ERR
C
      END IF
C
      DWORK(1) = ZERO
      RETURN
C
C *** Last line of MD03AF ***
      END
