!
! SPDX-License-Identifier: BSD-3-Clause
!
SUBROUTINE TEST_CONTINOUS(TRANS, AI, EI, BI, OK)
    IMPLICIT NONE
    DOUBLE PRECISION AI(2,2), EI(2,2), BI(2,2)
    LOGICAL OK
    CHARACTER*1 TRANS

    ! Locals
    DOUBLE PRECISION A(2,2), E(2,2), B(2,2), U(2,2)
    DOUBLE PRECISION M1(2,2), M2(2,2), SCALE
    DOUBLE PRECISION X(2,2), TMP1(2,2), RES(2,2), V
    INTEGER          INFO, I, J
    DOUBLE PRECISION RNORM, M1NORM,M2NORM
    EXTERNAL DGEMM, DTRSM, SG03BX, LSAME
    LOGICAL LSAME
    CHARACTER*1 TRANSA, TRANSB

    A = AI
    B = BI
    E = EI
    OK = .TRUE.
    IF (LSAME(TRANS, 'N')) THEN
        TRANSA = 'N'
        TRANSB = 'T'
    ELSE
        TRANSA = 'T'
        TRANSB = 'N'
    END IF

    CALL SG03BX( 'C', TRANS, A, 2, E, 2, B, 2, U, 2, SCALE, M1, 2, M2, 2, INFO )

    !
    !
    !     X = U**T * U, then residual = A**T*X*E + E**T*X*A + scale**2*B**T*B
    !
    CALL DGEMM(TRANSB,TRANSA,2,2,2,1D0,U,2,U,2,0D0,X,2)
    CALL DGEMM(TRANSB,'N',2,2,2,1D0,A,2,X,2,0D0,TMP1,2)
    CALL DGEMM('N',TRANSA,2,2,2,1D0,TMP1,2,E,2,0D0,RES,2)
    CALL DGEMM(TRANSB,'N',2,2,2,1D0,E,2,X,2,0D0,TMP1,2)
    CALL DGEMM('N',TRANSA,2,2,2,1D0,TMP1,2,A,2,1D0,RES,2)
    CALL DGEMM(TRANSB,TRANSA,2,2,2,SCALE**2,B,2,B,2,1D0,RES,2)

    RNORM = 0.0D0
    DO J = 1, 2
        DO I = 1, 2
            IF (ABS(RES(I,J)).GT.RNORM) RNORM = ABS(RES(I,J))
        END DO
    END DO

    ! Check M1 and M2
    !                               -1        -1
    !    op(M1) := op(U) * op(A) * op(E)   * op(U)
    !
    !                       -1        -1
    !    op(M2) := op(B) * op(E)   * op(U)
    !
    CALL DGEMM(TRANS, TRANS, 2, 2, 2, 1.0D0, U, 2, A, 2, 0.0D0, TMP1, 2)
    CALL DTRSM("R", "U", TRANS, "N", 2, 2, 1.0D0, E, 2, TMP1, 2)
    CALL DTRSM("R", "U", TRANS, "N", 2, 2, 1.0D0, U, 2, TMP1, 2)
    IF ( LSAME(TRANS,"T" )) THEN
        V = TMP1(1,2)
        TMP1(1,2) = TMP1(2,1)
        TMP1(2,1) = V
    END IF

    TMP1 = TMP1 - M1
    M1NORM = 0.0D0
    DO J = 1, 2
        DO I = 1, 2
            IF (ABS(TMP1(I,J)).GT.M1NORM) M1NORM = ABS(TMP1(I,J))
        END DO
    END DO

    ! M2
    TMP1 = B
    IF ( LSAME(TRANS,"T" )) THEN
        V = TMP1(1,2)
        TMP1(1,2) = TMP1(2,1)
        TMP1(2,1) = V
    END IF
    CALL DTRSM("R", "U", TRANS, "N", 2, 2, 1.0D0, E, 2, TMP1, 2)
    CALL DTRSM("R", "U", TRANS, "N", 2, 2, 1.0D0, U, 2, TMP1, 2)
    IF ( LSAME(TRANS,"T" )) THEN
        V = TMP1(1,2)
        TMP1(1,2) = TMP1(2,1)
        TMP1(2,1) = V
    END IF

    TMP1 = TMP1 - M2

    M2NORM = 0.0D0
    DO J = 1, 2
        DO I = 1, 2
            IF (ABS(TMP1(I,J)).GT.M2NORM) M2NORM = ABS(TMP1(I,J))
        END DO
    END DO




    IF ( RNORM.GT.1.0D-10 .OR. M1NORM .GT. 1.0D-10 .OR. M2NORM .GT. 1.0D-10 ) THEN
        OK = .FALSE.
        WRITE(*,'("TEST: SG03BX( C, ",A1," E12 = ", E8.3, ") - RNORM = ", E20.15, " M1NORM = ", E20.4 , &
            & " M2NORM = ", E20.14, " ", A4)') &
            & TRANS, E(1,2), RNORM, M1NORM, M2NORM, "FAIL"
    ELSE
        OK = .TRUE.
        WRITE(*,'("TEST: SG03BX( C, ",A1," E12 = ", E8.3, ") - RNORM = ", E20.15, " M1NORM = ", E20.4 , &
            & " M2NORM = ", E20.14, " ", A4)') &
            & TRANS, E(1,2), RNORM, M1NORM, M2NORM, "PASS"
    END IF

END SUBROUTINE

SUBROUTINE TEST_DISCRETE(TRANS, AI, EI, BI, OK)
    IMPLICIT NONE
    DOUBLE PRECISION AI(2,2), EI(2,2), BI(2,2)
    LOGICAL OK
    CHARACTER*1 TRANS

    ! Locals
    DOUBLE PRECISION A(2,2), E(2,2), B(2,2), U(2,2)
    DOUBLE PRECISION M1(2,2), M2(2,2), SCALE
    DOUBLE PRECISION X(2,2), TMP1(2,2), RES(2,2)
    INTEGER          INFO, I, J
    DOUBLE PRECISION RNORM, M1NORM, M2NORM, V
    EXTERNAL DGEMM, DTRSM, SG03BX, LSAME
    LOGICAL LSAME
    CHARACTER*1 TRANSA, TRANSB

    A = AI
    B = BI
    E = EI
    OK = .TRUE.
    IF (LSAME(TRANS, 'N')) THEN
        TRANSA = 'N'
        TRANSB = 'T'
    ELSE
        TRANSA = 'T'
        TRANSB = 'N'
    END IF

    CALL SG03BX( 'D', TRANS, A, 2, E, 2, B, 2, U, 2, SCALE, M1, 2, M2, 2, INFO )

    !            T                    T
    !       op(A)  * X * op(A) - op(E)  * X * op(E)
    !
    !                2        T
    !       = - SCALE  * op(B)  * op(B),                                (2)
    !
    CALL DGEMM(TRANSB,TRANSA,2,2,2,1D0,U,2,U,2,0D0,X,2)
    CALL DGEMM(TRANSB,'N',2,2,2,1D0,A,2,X,2,0D0,TMP1,2)
    CALL DGEMM('N',TRANSA,2,2,2,1D0,TMP1,2,A,2,0D0,RES,2)
    CALL DGEMM(TRANSB,'N',2,2,2,1D0,E,2,X,2,0D0,TMP1,2)
    CALL DGEMM('N',TRANSA,2,2,2,-1D0,TMP1,2,E,2,1D0,RES,2)
    CALL DGEMM(TRANSB,TRANSA,2,2,2,SCALE**2,B,2,B,2,1D0,RES,2)
    !
    RNORM = 0.0D0
    DO J = 1, 2
        DO I = 1, 2
            IF (ABS(RES(I,J)).GT.RNORM) RNORM = ABS(RES(I,J))
        END DO
    END DO

    ! Check M1 and M2
    !                               -1        -1
    !    op(M1) := op(U) * op(A) * op(E)   * op(U)
    !
    !                       -1        -1
    !    op(M2) := op(B) * op(E)   * op(U)
    CALL DGEMM(TRANS, TRANS, 2, 2, 2, 1.0D0, U, 2, A, 2, 0.0D0, TMP1, 2)
    CALL DTRSM("R", "U", TRANS, "N", 2, 2, 1.0D0, E, 2, TMP1, 2)
    CALL DTRSM("R", "U", TRANS, "N", 2, 2, 1.0D0, U, 2, TMP1, 2)
    IF ( LSAME(TRANS,"T" )) THEN
        V = TMP1(1,2)
        TMP1(1,2) = TMP1(2,1)
        TMP1(2,1) = V
    END IF
    TMP1 = TMP1 - M1
    M1NORM = 0.0D0
    DO J = 1, 2
        DO I = 1, 2
            IF (ABS(TMP1(I,J)).GT.M1NORM) M1NORM = ABS(TMP1(I,J))
        END DO
    END DO

    ! M2
    TMP1 = B
    IF ( LSAME(TRANS,"T" )) THEN
        V = TMP1(1,2)
        TMP1(1,2) = TMP1(2,1)
        TMP1(2,1) = V
    END IF
    CALL DTRSM("R", "U", TRANS, "N", 2, 2, 1.0D0, E, 2, TMP1, 2)
    CALL DTRSM("R", "U", TRANS, "N", 2, 2, 1.0D0, U, 2, TMP1, 2)
    IF ( LSAME(TRANS,"T" )) THEN
        V = TMP1(1,2)
        TMP1(1,2) = TMP1(2,1)
        TMP1(2,1) = V
    END IF

    TMP1 = TMP1 - M2

    M2NORM = 0.0D0
    DO J = 1, 2
        DO I = 1, 2
            IF (ABS(TMP1(I,J)).GT.M2NORM) M2NORM = ABS(TMP1(I,J))
        END DO
    END DO

    IF ( RNORM.GT.1.0D-10 .OR. M1NORM .GT. 1.0D-10 .OR. M2NORM .GT. 1.0D-10 ) THEN
        OK = .FALSE.
        WRITE(*,'("TEST: SG03BX( D, ",A1," E12 = ", E8.3, ") - RNORM = ", E20.15, " M1NORM = ", E20.4 , &
            & " M2NORM = ", E20.14, " ", A4)') &
            & TRANS, E(1,2), RNORM, M1NORM, M2NORM, "FAIL"
    ELSE
        OK = .TRUE.
        WRITE(*,'("TEST: SG03BX( D, ",A1," E12 = ", E8.3, ") - RNORM = ", E20.15, " M1NORM = ", E20.4 , &
            & " M2NORM = ", E20.14, " ", A4)') &
            & TRANS, E(1,2), RNORM, M1NORM, M2NORM, "PASS"
    END IF

END SUBROUTINE



PROGRAM TEST_SG03BX
    IMPLICIT NONE
    DOUBLE PRECISION A(2,2), E(2,2), B(2,2)
    LOGICAL OK
    INTEGER CNT
    EXTERNAL TEST_DISCRETE, TEST_CONTINOUS

    CNT = 0
    !
    !     A has complex conjugate eigenvalues -1 +/- 2i (c-stable).
    A(1,1) = -1.0D0;  A(1,2) =  2.0D0
    A(2,1) = -2.0D0;  A(2,2) = -1.0D0
    !     E upper triangular with E(1,2) /= 0.
    E(1,1) =  1.0D0;  E(1,2) =  0.5D0
    E(2,1) =  0.0D0;  E(2,2) =  1.0D0
    !     B upper triangular.
    B(1,1) =  1.0D0;  B(1,2) =  0.3D0
    B(2,1) =  0.0D0;  B(2,2) =  1.0D0
    !
    CALL TEST_CONTINOUS("N", A, E, B, OK )
    IF (.NOT. OK) CNT = CNT +1
    CALL TEST_CONTINOUS("T", A, E, B, OK )
    IF (.NOT. OK) CNT = CNT +1

    E(1,2) = 0.0D0
    CALL TEST_CONTINOUS("N", A, E, B, OK )
    IF (.NOT. OK) CNT = CNT +1
    CALL TEST_CONTINOUS("T", A, E, B, OK )
    IF (.NOT. OK) CNT = CNT +1


    !
    !     (A,E) is d-stable  -0.0950 +/- 0.2024i
    A(1,1) = -1.0D0;  A(1,2) =  2.0D0
    A(2,1) = -2.0D0;  A(2,2) = -1.0D0
    !     E upper triangular with E(1,2) /= 0.
    E(1,1) =  1.0D1;  E(1,2) =  0.5D0
    E(2,1) =  0.0D0;  E(2,2) =  1.0D1
    !     B upper triangular.
    B(1,1) =  1.0D0;  B(1,2) =  0.3D0
    B(2,1) =  0.0D0;  B(2,2) =  1.0D0

    CALL TEST_DISCRETE("N", A, E, B, OK )
    IF (.NOT. OK) CNT = CNT +1
    CALL TEST_DISCRETE("T", A, E, B, OK )
    IF (.NOT. OK) CNT = CNT +1

    E(1,2) = 0.0D0

    CALL TEST_DISCRETE("N", A, E, B, OK )
    IF (.NOT. OK) CNT = CNT +1
    CALL TEST_DISCRETE("T", A, E, B, OK )
    IF (.NOT. OK) CNT = CNT +1

    IF ( CNT .GT. 0) THEN
        STOP 1
    END IF

END
