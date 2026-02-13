C
C SPDX-License-Identifier: BSD-3-Clause
C
      SUBROUTINE NF01BF( IFLAG, NFUN, LX, IPAR, LIPAR, U, LDU, Y, LDY,
     $                   X, NFEVL, E, J, LDJ, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     This is the FCN routine for optimizing all parameters of a Wiener
C     system using SLICOT Library routine MD03BD. See the argument FCN
C     in the routine MD03BD for the description of parameters.
C
C     Arguments
C
C     IFLAG   (input/output) INTEGER
C             On entry, this parameter must contain a value
C             defining the computations to be performed:
C             = 0 :  Optionally, print the current iterate X,
C                    function values E, and Jacobian matrix J,
C                    or other results defined in terms of these
C                    values. See the argument NPRINT of MD03BD.
C                    Do not alter E and J.
C             = 1 :  Calculate the functions at X and return
C                    this vector in E. Do not alter J.
C             = 2 :  Calculate the Jacobian at X and return
C                    this matrix in J. Also return NFEVL
C                    (see below). Do not alter E.
C             = 3 :  Do not compute neither the functions nor
C                    the Jacobian, but return in LDJ and
C                    IPAR/DPAR1,DPAR2 (some of) the integer/real
C                    parameters needed.
C             On exit, the value of this parameter should not be
C             changed by FCN unless the user wants to terminate
C             execution of MD03BD, in which case IFLAG must be
C             set to a negative integer.
C
C     NFUN    (input) INTEGER
C             The number of functions.  NFUN >= 0.
C
C     LX      (input) INTEGER
C             The number of variables.  NFUN >= LX >= 0.
C
C     IPAR    (input/output) INTEGER array, dimension (LIPAR)
C             The integer parameters describing the structure of
C             the Jacobian matrix or needed for problem solving.
C             IPAR is an input parameter, except for IFLAG = 3
C             on entry, when it is also an output parameter.
C             On exit, if IFLAG = 3, IPAR(1) contains the length
C             of the array J, for storing the Jacobian matrix,
C             and the entries IPAR(2:5) contain the workspace
C             required by FCN for IFLAG = 1, FCN for IFLAG = 2,
C             QRFACT, and LMPARM, respectively.
C
C     LIPAR   (input) INTEGER
C             The length of the array IPAR.  LIPAR >= 5.
C
C     U       (input/output) DOUBLE PRECISION array, dimension
C             (LDU,*) or (LDU)
C             A first set of real parameters needed for
C             describing or solving the problem.
C             U can also be used as an additional array for
C             intermediate results when computing the functions
C             or the Jacobian. For control problems, U could
C             store the input trajectory of a system.
C
C     LDU     (input) INTEGER
C             The leading dimension or the length of the array
C             U, as convenient.  LDU >= 0.  (LDU >= 1,
C             if leading dimension.)
C
C     Y       (input/output) DOUBLE PRECISION array, dimension
C             (LDY,*) or (LDY)
C             A second set of real parameters needed for
C             describing or solving the problem.
C             Y can also be used as an additional array for
C             intermediate results when computing the functions
C             or the Jacobian. For control problems, Y could
C             store the output trajectory of a system.
C
C     LDY     (input) INTEGER
C             The leading dimension or the length of the array
C             Y, as convenient.  LDY >= 0.  (LDY >= 1,
C             if leading dimension.)
C
C     X       (input) DOUBLE PRECISION array, dimension (N)
C             This array must contain the value of the
C             variables x where the functions or the Jacobian
C             must be evaluated.
C
C     NFEVL   (input/output) INTEGER
C             The number of function evaluations needed to
C             compute the Jacobian by a finite difference
C             approximation.
C             NFEVL is an input parameter if IFLAG = 0, or an
C             output parameter if IFLAG = 2. If the Jacobian is
C             computed analytically, NFEVL should be set to a
C             non-positive value.
C
C     E       (input/output) DOUBLE PRECISION array,
C             dimension (M)
C             This array contains the value of the (error)
C             functions e evaluated at X.
C             E is an input parameter if IFLAG = 0 or 2, or an
C             output parameter if IFLAG = 1.
C
C     J       (input/output) DOUBLE PRECISION array, dimension
C             (LDJ,NC), where NC is the number of columns
C             needed.
C             This array contains a possibly compressed
C             representation of the Jacobian matrix evaluated
C             at X. If full Jacobian is stored, then NC = N.
C             J is an input parameter if IFLAG = 0, or an output
C             parameter if IFLAG = 2.
C
C     LDJ     (input/output) INTEGER
C             The leading dimension of array J.  LDJ >= 1.
C             LDJ is essentially used inside the routines FCN,
C             QRFACT and LMPARM.
C             LDJ is an input parameter, except for IFLAG = 3
C             on entry, when it is an output parameter.
C             It is assumed in MD03BD that LDJ is not larger
C             than needed.
C
C     DWORK   (input/output) DOUBLE PRECISION array, dimension (LDWORK)
C             The workspace array for subroutine FCN.
C             On exit, if INFO = 0, DWORK(1) returns the optimal
C             value of LDWORK.
C
C     LDWORK  (input) INTEGER
C             The size of the array DWORK (as large as needed
C             in the subroutine FCN).  LDWORK >= 1.
C
C     INFO    (output) INTEGER
C             Error indicator, set to a negative value if an
C             input (scalar) argument is erroneous, and to
C             positive values for other possible errors in the
C             subroutine FCN. The LAPACK Library routine XERBLA
C             should be used in conjunction with negative INFO.
C             INFO must be zero if the subroutine finished
C             successfully.
C

C     ******************************************************************
C
C     .. Parameters ..
C     .. CJTE is initialized to avoid the calculation of J'*e ..
C     .. NOUT is the unit number for printing intermediate results ..
      CHARACTER         CJTE
      PARAMETER         ( CJTE = 'N' )
      INTEGER           NOUT
      PARAMETER         ( NOUT = 6 )
      DOUBLE PRECISION  ONE
      PARAMETER         ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           IFLAG, INFO, LDJ, LDU, LDWORK, LDY, LIPAR, LX,
     $                  NFEVL, NFUN
C     .. Array Arguments ..
      INTEGER           IPAR(*)
      DOUBLE PRECISION  DWORK(*), E(*), J(LDJ,*), U(LDU,*), X(*),
     $                  Y(LDY,*)
C     .. Local Scalars ..
      LOGICAL           FULL
      INTEGER           BSN, I, JWORK, L, M, N, NN, NSMP, ST
      DOUBLE PRECISION  ERR
C     .. External Functions ..
      DOUBLE PRECISION  DNRM2
      EXTERNAL          DNRM2
C     .. External Subroutines ..
      EXTERNAL          DAXPY, NF01AD, NF01BD
C
C     .. Executable Statements ..
C
      L = IPAR(2)
      M = IPAR(5)
      N = IPAR(6)
      IF ( L.EQ.0 ) THEN
         NSMP = NFUN
      ELSE
         NSMP = NFUN/L
      END IF
C
      INFO = 0
      IF ( IFLAG.EQ.1 ) THEN
C
C        Call NF01AD to compute the output y of the Wiener system (in E)
C        and then the error functions (also in E). The array U must
C        contain the input to the linear part of the Wiener system, and
C        Y must contain the original output Y of the Wiener system.
C        IPAR(6) must contain the number of states of the linear part, n.
C        Workspace: need:    NFUN + MAX( 2*NN, (N + L)*(N + M) + 2*N +
C                                        MAX( N*(N + L), N + M + L ) ),
C                                                               if M>0,
C                            NFUN + MAX( 2*NN, (N + L)*N + 2*N +
C                                        MAX( N*(N + L), L ) ), if M=0,
C                            where NN = IPAR(7) (number of neurons);
C                   prefer:  larger.
C
         CALL NF01AD( NSMP, M, L, IPAR(6), LIPAR-2, X, LX, U, LDU, E,
     $                NSMP, DWORK, LDWORK, INFO )
C
         DO 10 I = 1, L
            CALL DAXPY( NSMP, -ONE, Y(1,I), 1, E((I-1)*NSMP+1), 1 )
   10    CONTINUE
C
         DWORK(1) = NFUN + MAX( 2*IPAR(7), (N + L)*(N + M) + 2*N +
     $                          MAX( N*(N + L), N + M + L ) )
C
      ELSE IF ( IFLAG.EQ.2 ) THEN
C
C        Call NF01BD to compute the Jacobian in a compressed form.
C        Workspace: need:    2*NFUN + MAX( 2*NN, (N + L)*(N + M) + 2*N +
C                                          MAX( N*(N + L), N + M + L )),
C                                                              if M > 0,
C                            2*NFUN + MAX( 2*NN, (N + L)*N + 2*N +
C                                          MAX( N*(N + L), L ) ),
C                                                              if M > 0;
C                   prefer:  larger.
C
         CALL NF01BD( CJTE, NSMP, M, L, IPAR(6), LIPAR-2, X, LX, U,
     $                LDU, E, J, LDJ, DWORK, DWORK, LDWORK, INFO )
         NFEVL = IPAR(6)*( M + L + 1 ) + L*M
         DWORK(1) = 2*NFUN + MAX( 2*IPAR(7), (N + L)*(N + M) + 2*N +
     $                            MAX( N*(N + L), N + M + L ) )
C
      ELSE IF ( IFLAG.EQ.3 ) THEN
C
C        Set the parameter LDJ, the length of the array J, and the sizes
C        of the workspace for FCN (IFLAG = 1 or 2), QRFACT and LMPARM.
C        Condition estimation (COND = 'E') is assumed in these routines.
C
         ST   = IPAR(1)
         BSN  = IPAR(4)
         NN   = IPAR(7)
         FULL = L.LE.1 .OR. BSN.EQ.0
C
         LDJ     = NFUN
         IPAR(1) = LDJ*( BSN + ST )
         IF ( M.GT.0 ) THEN
            JWORK = MAX( N*( N + L ), N + M + L )
         ELSE
            JWORK = MAX( N*( N + L ), L )
         END IF
         IPAR(2) = LDJ + MAX( (N + L)*(N + M) + 2*N + JWORK, 2*NN )
         IPAR(3) = LDJ + IPAR(2)
         JWORK   = 1
         IF ( FULL ) THEN
            JWORK = 4*LX + 1
         ELSEIF ( BSN.GT.0 ) THEN
            JWORK = BSN + MAX( 3*BSN + 1, ST )
            IF ( NSMP.GT.BSN ) THEN
               JWORK = MAX( JWORK, 4*ST + 1 )
               IF ( NSMP.LT.2*BSN )
     $            JWORK = MAX( JWORK, ( NSMP - BSN )*( L - 1 ) )
            END IF
         END IF
         IPAR(4) = JWORK
         IF ( FULL ) THEN
            JWORK = 4*LX
         ELSE
            JWORK = ST*( LX - ST ) + 2*LX + 2*MAX( BSN, ST )
         END IF
         IPAR(5) = JWORK
C
      ELSE IF ( IFLAG.EQ.0 ) THEN
C
C        Special call for printing intermediate results.
C
         ERR = DNRM2( NFUN, E, 1 )
         WRITE( NOUT, '('' Norm of current error = '', D15.6)') ERR
      END IF
      RETURN
C
C *** Last line of NF01BF ***
      END
