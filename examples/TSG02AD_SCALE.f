C
C SPDX-License-Identifier: BSD-3-Clause
C
      PROGRAM TSG02AD_SCALE

      INTEGER          NOUT
      PARAMETER        ( NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          NMAX2M, NMAX2, NMMAX
      PARAMETER        ( NMAX2M = 2*NMAX+MMAX, NMAX2 = 2*NMAX,
     $                   NMMAX  = MAX(NMAX,MMAX) )
      INTEGER          LDA, LDB, LDE, LDL, LDQ, LDR, LDS, LDT, LDU, LDX
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDE = NMAX, LDL = NMAX,
     $                   LDQ = MAX(NMAX,PMAX), LDR = MAX(MMAX,PMAX),
     $                   LDS = NMAX2M, LDT = NMAX2M, LDU = NMAX2,
     $                   LDX = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MAX(MMAX,NMAX2) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX(14*NMAX+23,16*NMAX,2*NMAX+MMAX,
     $                                3*MMAX) )
      INTEGER          LBWORK
      PARAMETER        ( LBWORK = NMAX2 )
*     .. Local Scalars ..
      DOUBLE PRECISION RCONDU, TOL
      INTEGER          INFO, IWARN, M, N, P
      CHARACTER(1)     ACC, DICO, FACT, JOBB, JOBL, SCAL, SORT, UPLO
      CHARACTER(1)     FACTX
      INTEGER          FLOOP, NERRORS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX),  ALFAI(NMAX2),  ALFAR(NMAX2),
     $                 B(LDB,NMMAX), BETA(NMAX2),   DWORK(LDWORK),
     $                 E(LDE,NMAX),  L(LDL,MMAX),   Q(LDQ,NMAX),
     $                 R(LDR,MMAX),  S(LDS,NMAX2M), T(LDT,NMAX2),
     $                 U(LDU,NMAX2), X1(LDX,NMAX), X2(LDX, NMAX)
      INTEGER          IWORK(LIWORK)
      LOGICAL          BWORK(LBWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SG02AD, DCOPY, DSYRK
*     .. Intrinsic Functions ..
      INTRINSIC        MAX

      NERRORS = 0
      DO FLOOP =  1, 4
          SELECT CASE(FLOOP)
              CASE (1)
                  FACTX = 'N'
              CASE (2)
                  FACTX = 'C'
              CASE (3)
                  FACTX = 'D'
              CASE (4)
                  FACTX = 'B'
          END SELECT
      N = 2
      M = 2
      P = 2
      A(1,1) = 1
      A(2,2) = 1
      B(1,1) = 1
      B(2,2) = 2
      E(2,2) = 1
      E(1,1) = 1
      Q(1,1) = 1
      Q(1,2) = 0.5
      Q(2,2) = 2
      R(1,1) = 1
      R(2,2) = 2

      DICO = 'C'
      JOBB = 'B'
      FACT = FACTX
      UPLO = 'U'
      JOBL = 'Z'
      SCAL = 'N'
      SORT = 'S'
      ACC = 'R'


      CALL SG02AD(DICO, JOBB, FACT, UPLO, JOBL, SCAL, SORT, ACC, N, M,
     $     P, A, LDA, E, LDE, B, LDB, Q, LDQ, R, LDR, L, LDL, RCONDU,
     $     X1, LDX, ALFAR, ALFAI, BETA, S, LDS, T, LDT, U, LDU, TOL,
     $     IWORK, DWORK, LDWORK, BWORK, IWARN, INFO)

      WRITE ( NOUT, FMT = 99998 ) SCAL, FACTX, INFO
      WRITE ( NOUT, FMT = 99996 ) ( X1(1:2,1:2) )

      IF (INFO .NE. 0) NERRORS = NERRORS + 1


      N = 2
      M = 2
      P = 2
      A(1,1) = 1
      A(2,2) = 1
      B(1,1) = 1
      B(2,2) = 2
      E(2,2) = 1
      E(1,1) = 1
      Q(1,1) = 1
      Q(1,2) = 0.5
      Q(2,2) = 2
      R(1,1) = 1
      R(2,2) = 2

!      N = 1
!      M = 1
!      P = 1
!      A(1,1) = 1
!      B(1,1) = 1
!      E(1,1) = 1
!      Q(1,1) = 1
!      R(1,1) = 1

      DICO = 'C'
      JOBB = 'B'
      FACT = FACTX
      UPLO = 'U'
      JOBL = 'Z'
      SCAL = 'G'
      SORT = 'S'
      ACC = 'R'

      CALL SG02AD(DICO, JOBB, FACT, UPLO, JOBL, SCAL, SORT, ACC, N, M,
     $     P, A, LDA, E, LDE, B, LDB, Q, LDQ, R, LDR, L, LDL, RCONDU,
     $     X2, LDX, ALFAR, ALFAI, BETA, S, LDS, T, LDT, U, LDU, TOL,
     $     IWORK, DWORK, LDWORK, BWORK, IWARN, INFO)

      WRITE ( NOUT, FMT = 99998 ) SCAL, FACTX, INFO
      WRITE ( NOUT, FMT = 99996 ) ( X2(1:2,1:2) )

      IF (INFO .NE. 0) NERRORS = NERRORS + 1

      X1(1:2, 1:2) = X1(1:2, 1:2) - X2(1:2,1:2)
      TOL = NORM2(X1(1:2,1:2))

      IF ( TOL .GT. 1.0D-7) THEN
        NERRORS = NERRORS + 1
        WRITE(NOUT, 99995) TOL
      ELSE
        WRITE(NOUT, 99994) TOL
      END IF

      END DO
      STOP NERRORS

99994 FORMAT ('PASSED: ||X1-X2||_2 = ', F8.4)
99995 FORMAT ('FAILED: ||X1-X2||_2 = ', F8.4)
99996 FORMAT (20(1X,F8.4))
99998 FORMAT ('SCAL = ', A1, ' FACT = ', A1 ,
     $        ' INFO on exit from SG02AD = ',I2)

      END PROGRAM
