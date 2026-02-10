C
C SPDX-License-Identifier: BSD-3-Clause
C
C Reproducer for Github #34

      PROGRAM REPRODUCE_ISSUE_34
      IMPLICIT NONE
C     .. Parameters ..
      INTEGER          N, LDA, LDG, LDQ, LDS, LDWORK, LIWORK
      INTEGER          LDT, LDV, LDX
      PARAMETER        ( N = 3, LDA = N, LDG = N, LDQ = N, LDS = 2*N,
     $                   LDT = N, LDV = N, LDX = N,
     $                   LDWORK = 100, LIWORK = 2*N )
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Local Scalars ..
      INTEGER          INFO, I, J
      DOUBLE PRECISION SEP, RCOND, FERR
      CHARACTER        JOB, DICO, HINV, TRANA, UPLO, SCAL, SORT, FACT,
     $                 LYAPUN
C     .. Local Arrays ..
      INTEGER          IWORK(LIWORK)
      LOGICAL          BWORK(2*N)
      DOUBLE PRECISION A(LDA,N), G(LDG,N), Q(LDQ,N), S(LDS,2*N),
     $                 X(LDX,N), WR(2*N), WI(2*N), DWORK(LDWORK)
      DOUBLE PRECISION T(LDT,N), V(LDV,N)
C     .. External Subroutines ..
      EXTERNAL         SB02RD, DLASET
C
C     .. Executable Statements ..
C
      JOB    = 'X'
      DICO   = 'D'
      HINV   = 'I'
      TRANA  = 'N'
      UPLO   = 'U'
      SCAL   = 'N'
      SORT   = 'U'
      FACT   = 'N'
      LYAPUN = 'O'

C     Initialize A with values known to trigger the issue
      A(1,1) =  0.10723198D0
      A(2,1) =  0.35266465D0
      A(3,1) = -0.16273185D0
      A(1,2) =  0.11326152D0
      A(2,2) = -0.28178271D0
      A(3,2) = -0.16461242D0
      A(1,3) =  0.41470137D0
      A(2,3) = -0.34294504D0
      A(3,3) =  0.06255592D0

C     Q = I
      CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDQ )

C     G = 0.1 * I
      CALL DLASET( 'Full', N, N, ZERO, 0.1D0, G, LDG )

      WRITE(*,*) 'Running SB02RD with DICO=D, HINV=I...'

C     Call SB02RD
      CALL SB02RD( JOB, DICO, HINV, TRANA, UPLO, SCAL, SORT, FACT,
     $             LYAPUN, N, A, LDA, T, LDT, V, LDV, G, LDG, Q, LDQ,
     $             X, LDX, SEP, RCOND, FERR, WR, WI, S, LDS, IWORK,
     $             DWORK, LDWORK, BWORK, INFO )

      WRITE(*,*) 'INFO = ', INFO

      IF ( INFO .NE. 0 ) THEN
         WRITE(*,*) 'FAILURE: SB02RD returned INFO = ', INFO
         IF ( INFO .EQ. 4 ) THEN
             WRITE(*,*) 'Reproduced Issue #61: INFO=4',
     $                  ' (Unstable eigenvalues)'
         END IF
      ELSE
         WRITE(*,*) 'SUCCESS: SB02RD returned INFO = 0'
      END IF

      END
