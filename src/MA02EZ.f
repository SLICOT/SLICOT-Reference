      SUBROUTINE MA02EZ( UPLO, TRANS, SKEW, N, A, LDA )
C
C     SLICOT RELEASE 5.7.
C
C     Copyright (c) 2002-2020 NICONET e.V.
C
C     PURPOSE
C
C     To store by (skew-)symmetry the upper or lower triangle of a
C     (skew-)symmetric/Hermitian complex matrix, given the other
C     triangle.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies which part of the matrix is given as follows:
C             = 'U':  Upper triangular part;
C             = 'L':  Lower triangular part.
C             For all other values, the array A is not referenced.
C
C     TRANS   CHARACTER*1
C             Specifies whether to use transposition or conjugate
C             transposition as follows:
C             = 'T':  Use transposition;
C             = 'C':  Use conjugate transposition.
C
C     SKEW    CHARACTER*1
C             Specifies whether the matrix is symmetric/Hermitian or
C             skew-symmetric/Hermitian as follows:
C             = 'N':  The matrix is symmetric/Hermitian;
C             = 'S':  The matrix is skew-symmetric/Hermitian.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA,N)
C             On entry, the leading N-by-N upper triangular part
C             (if UPLO = 'U'), or lower triangular part (if UPLO = 'L'),
C             of this array must contain the corresponding upper or
C             lower triangle of the (skew-)symmetric/Hermitian matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the (skew-)symmetric/Hermitian matrix A with all elements
C             stored. If the resulted matrix should be Hermitian, the
C             imaginary parts of the diagonal entries are set to zero.
C             If the resulted matrix should be skew-Hermitian, the real
C             parts of the diagonal entries are set to zero.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,N).
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Sep. 2012. Based on SLICOT Library routine MA02ED.
C
C     REVISIONS
C
C     V. Sima, Jan. 2016.
C
C     ******************************************************************
C
C     .. Scalar Arguments ..
      CHARACTER          SKEW, TRANS, UPLO
      INTEGER            LDA, N
C     .. Array Arguments ..
      COMPLEX*16         A(LDA,*)
C     .. Local Scalars ..
      INTEGER            I, J
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..Intrinsic Functions..
      INTRINSIC          DBLE, DCONJG, DIMAG
C
C     .. Executable Statements ..
C
C     For efficiency reasons, the parameters are not checked for errors.
C
      IF( LSAME( UPLO, 'L' ) ) THEN
C
C        Construct the upper triangle of A.
C
         IF( LSAME( TRANS, 'T' ) ) THEN
C
            IF( LSAME( SKEW, 'N' ) ) THEN
C
               DO 20 I = 1, N
                  DO 10 J = 2, N
                     A(I,J) = A(J,I)
   10             CONTINUE
   20          CONTINUE
C
            ELSE
C
               DO 40 I = 1, N
                  DO 30 J = 2, N
                     A(I,J) = -A(J,I)
   30             CONTINUE
   40          CONTINUE
C
            END IF
C
         ELSE
C
            IF( LSAME( SKEW, 'N' ) ) THEN
C
               DO 60 I = 1, N
                  A(I,I) = DBLE( A(I,I) )
                  DO 50 J = 2, N
                     A(I,J) = DCONJG( A(J,I) )
   50             CONTINUE
   60          CONTINUE
C
            ELSE
C
               DO 80 I = 1, N
                  A(I,I) = DIMAG( A(I,I) )
                  DO 70 J = 2, N
                     A(I,J) = -DCONJG( A(J,I) )
   70             CONTINUE
   80          CONTINUE
C
            END IF
C
         END IF
C
      ELSE IF( LSAME( UPLO, 'U' ) ) THEN
C
C        Construct the lower triangle of A.
C
         IF( LSAME( TRANS, 'T' ) ) THEN
C
            IF( LSAME( SKEW, 'N' ) ) THEN
C
               DO 100 I = 1, N
                  DO 90 J = 2, N
                     A(J,I) = A(I,J)
   90             CONTINUE
  100          CONTINUE
C
            ELSE
C
               DO 120 I = 1, N
                  DO 110 J = 2, N
                     A(J,I) = -A(I,J)
  110             CONTINUE
  120          CONTINUE
C
            END IF
C
         ELSE
C
            IF( LSAME( SKEW, 'N' ) ) THEN
C
               DO 140 I = 1, N
                  A(I,I) = DBLE( A(I,I) )
                  DO 130 J = 2, N
                     A(J,I) = DCONJG( A(I,J) )
  130             CONTINUE
  140          CONTINUE
C
            ELSE
C
               DO 160 I = 1, N
                  A(I,I) = DIMAG( A(I,I) )
                  DO 150 J = 2, N
                     A(J,I) = -DCONJG( A(I,J) )
  150             CONTINUE
  160          CONTINUE
C
            END IF
C
         END IF
C
      END IF
      RETURN
C *** Last line of MA02EZ ***
      END
