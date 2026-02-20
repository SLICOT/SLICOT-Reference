C
C SPDX-License-Identifier: BSD-3-Clause
C
      DOUBLE PRECISION FUNCTION MB03NY( N, OMEGA, A, LDA, S, DWORK,
     $                                  LDWORK, ZWORK, LZWORK, INFO )
C
C     PURPOSE
C
C     To compute the smallest singular value of A - jwI.
C
C     FUNCTION VALUE
C
C     MB03NY  DOUBLE PRECISION
C             The smallest singular value of A - jwI (if INFO = 0).
C             If N = 0, the function value is set to zero.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     OMEGA   (input) DOUBLE PRECISION
C             The constant factor of A - jwI.
C
C     A       (input/workspace) DOUBLE PRECISION array, dimension
C             (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A.
C             On exit, if OMEGA = 0, the contents of this array are
C             destroyed. Otherwise, this array is unchanged.
C
C     LDA     (input) INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,N).
C
C     S       (output) DOUBLE PRECISION array, dimension (N)
C             The singular values of A - jwI in decreasing order.
C
C     Workspace
C
C     DWORK   (input/output) DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  (input) INTEGER
C             The length of the array DWORK.  LDWORK >= MAX( 1, 5*N ).
C             For optimum performance LDWORK should be larger.
C
C     ZWORK   (input/output) COMPLEX*16 array, dimension (LZWORK)
C             On exit, if INFO = 0 and OMEGA <> 0, ZWORK(1) returns the
C             optimal value of LZWORK.
C             If OMEGA is zero, this array is not referenced.
C
C     LZWORK  (input) INTEGER
C             The length of the array ZWORK.
C             LZWORK >= 1,                 if OMEGA =  0;
C             LZWORK >= MAX( 1, N*N+3*N ), if OMEGA <> 0.
C             For optimum performance LZWORK should be larger.
C
C     Error Indicator
C
C     INFO    (output) INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 2:  The SVD algorithm (in either LAPACK Library routine
C                   DGESVD or ZGESVD) fails to converge; this error is
C                   very rare.
C
C     METHOD
C
C     This procedure simply constructs the matrix A - jwI, and calls
C     ZGESVD if w is not zero, or DGESVD if w = 0.
C
C     FURTHER COMMENTS
C
C     This routine is not very efficient because it computes all
C     singular values, but it is very accurate. The routine is intended
C     to be called only from the SLICOT Library routine AB13FD.
C
C     CONTRIBUTOR
C
C     R. Byers, the routine SIGMIN (January, 1995).
C
C     REVISIONS
C
C     Release 4.0: V. Sima, Katholieke Univ. Leuven, Belgium, Dec. 1999.
C
C     REVISIONS
C
C     Oct. 2001, V. Sima, Research Institute for Informatics, Bucharest.
C     Apr. 2002, V. Sima.
C
C     KEYWORDS
C
C     singular values.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION       ZERO, ONE
      PARAMETER              ( ZERO = 0.0D0, ONE = 1.0D0 )
      COMPLEX*16             CONE, RTMONE
      PARAMETER              ( CONE   = ( 1.0D0, 0.0D0 ),
     $                         RTMONE = ( 0.0D0, 1.0D0 ) )
C     .. Scalar Arguments ..
      INTEGER                INFO, LZWORK, LDA, LDWORK, N
      DOUBLE PRECISION       OMEGA
C     .. Array Arguments ..
      DOUBLE PRECISION       A(LDA,*), DWORK(*), S(*)
      COMPLEX*16             ZWORK(*)
C     .. Local Scalars ..
      INTEGER                I, IC, J
C     .. Local Arrays ..
      DOUBLE PRECISION       DUMMY(1,1)
      COMPLEX*16             ZDUMMY(1,1)
C     .. External Subroutines ..
      EXTERNAL               DGESVD, XERBLA, ZGESVD
C     .. Intrinsic Functions ..
      INTRINSIC              DBLE, MAX
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO = 0
C
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LDWORK.LT.MAX( 1, 5*N ) ) THEN
         INFO = -7
      ELSE IF( LZWORK.LT.1 .OR. ( OMEGA.NE.ZERO .AND.
     $         LZWORK.LT.N*N + 3*N ) ) THEN
         INFO = -9
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB03NY', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 ) THEN
         MB03NY   = ZERO
         DWORK(1) = ONE
         IF ( OMEGA.NE.ZERO )
     $      ZWORK(1) = CONE
         RETURN
      END IF
C
      IF ( OMEGA.EQ.ZERO ) THEN
C
C        OMEGA = 0 allows real SVD.
C
         CALL DGESVD( 'No vectors', 'No vectors', N, N, A, N, S, DUMMY,
     $                1, DUMMY, 1, DWORK, LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            INFO = 2
            RETURN
         END IF
      ELSE
C
C        General case, that is complex SVD.
C
         IC = 1
         DO 20 J = 1, N
            DO 10 I = 1, N
               ZWORK(IC) = A(I,J)
               IC = IC + 1
   10       CONTINUE
            ZWORK((J-1)*N+J) = ZWORK((J-1)*N+J) - OMEGA * RTMONE
   20    CONTINUE
         CALL ZGESVD( 'No vectors', 'No vectors', N, N, ZWORK, N, S,
     $                ZDUMMY, 1, ZDUMMY, 1, ZWORK(N*N+1), LZWORK-N*N,
     $                DWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            INFO = 2
            RETURN
         END IF
         ZWORK(1) = ZWORK(N*N+1) + DBLE( N*N ) * CONE
         DWORK(1) = DBLE( 5*N )
      END IF
C
      MB03NY = S(N)
C
C *** Last line of MB03NY ***
      END
