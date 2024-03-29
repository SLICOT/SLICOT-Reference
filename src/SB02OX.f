      LOGICAL FUNCTION SB02OX( ALPHAR, ALPHAI, BETA )
C
C     PURPOSE
C
C     To select the stable generalized eigenvalues for solving the
C     discrete-time algebraic Riccati equation.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     ALPHAR  (input) DOUBLE PRECISION
C             The real part of the numerator of the current eigenvalue
C             considered.
C
C     ALPHAI  (input) DOUBLE PRECISION
C             The imaginary part of the numerator of the current
C             eigenvalue considered.
C
C     BETA    (input) DOUBLE PRECISION
C             The (real) denominator of the current eigenvalue
C             considered.
C
C     METHOD
C
C     The function value SB02OX is set to .TRUE. for a stable eigenvalue
C     (i.e., with modulus less than one) and to .FALSE., otherwise.
C
C     REFERENCES
C
C     None.
C
C     NUMERICAL ASPECTS
C
C     None.
C
C     CONTRIBUTOR
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Sep. 1997.
C     Supersedes Release 2.0 routine SB02CX by P. Van Dooren, Philips
C     Research Laboratory, Brussels, Belgium.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Algebraic Riccati equation, closed loop system, continuous-time
C     system, optimal regulator, Schur form.
C
C     ******************************************************************
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHAR, ALPHAI, BETA
C     .. External Functions ..
      DOUBLE PRECISION   DLAPY2
      EXTERNAL           DLAPY2
C     .. Intrinsic Functions ..
      INTRINSIC          ABS
C     .. Executable Statements ..
C
      SB02OX = DLAPY2( ALPHAR, ALPHAI ).LT.ABS( BETA )
C
      RETURN
C *** Last line of SB02OX ***
      END
