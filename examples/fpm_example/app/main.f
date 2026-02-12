C
C SPDX-License-Identifier: BSD-3-Clause
C
C
C
C     Test program to reproduce DLACPY overlap bug in NF01BS
C
C     Bug location: NF01BS.f line 519
C     CALL DLACPY( 'Full', N, ST, J(LDJ*BSN+1), LDJ, J(IBSN), N )
C
C     The source and destination regions overlap when LDJ > N
C     See Github #35
C
      PROGRAM TEST_NF01BS_35
      IMPLICIT NONE
C
C     Parameters to trigger overlap:
C     BN = 2, BSM = 4, BSN = 2, ST = 2
C     N = BN*BSN + ST = 6
C     M = BN*BSM = 8
C     LDJ = M = 8
C
      EXTERNAL NF01BS
      INTEGER BN, BSM, BSN, ST, N, M, LDJ_INIT
      PARAMETER ( BN = 2, BSM = 4, BSN = 2, ST = 2 )
      PARAMETER ( N = BN*BSN + ST )
      PARAMETER ( M = BN*BSM )
      PARAMETER ( LDJ_INIT = M )
C
C     Array dimensions
      INTEGER NC
      PARAMETER ( NC = BSN + ST )
C
C     Local variables
      INTEGER INFO, LDWORK, LIPAR, I, K, LDJ
      DOUBLE PRECISION FNORM, GNORM
C
C     Arrays
      INTEGER IPAR(4), IPVT(N)
      DOUBLE PRECISION J(LDJ_INIT, NC), J_ORIG(LDJ_INIT, NC)
      DOUBLE PRECISION E(M), JNORMS(N), DWORK(1000)
C
C     Initialize LDJ (must be variable since NF01BS modifies it)
      LDJ = LDJ_INIT
C
C     Initialize IPAR
      IPAR(1) = ST
      IPAR(2) = BN
      IPAR(3) = BSM
      IPAR(4) = BSN
      LIPAR = 4
C
C     Initialize J matrix with recognizable pattern
C     Column-major: J(i,j) stored at position i + (j-1)*LDJ
      DO K = 1, NC
         DO I = 1, M
            J(I,K) = DBLE(I + (K-1)*100)
            J_ORIG(I,K) = J(I,K)
         END DO
      END DO
C
C     Initialize error vector
      DO I = 1, M
         E(I) = 1.0D0
      END DO
      FNORM = SQRT(DBLE(M))
C
C     Workspace
      LDWORK = 1000
C
C     Print test setup
      WRITE(*,*) '=================================================='
      WRITE(*,*) 'NF01BS DLACPY Overlap Bug Reproduction Test'
      WRITE(*,*) '=================================================='
      WRITE(*,*)
      WRITE(*,*) 'Parameters:'
      WRITE(*,*) '  BN  =', BN,  ' (number of blocks)'
      WRITE(*,*) '  BSM =', BSM, ' (block rows)'
      WRITE(*,*) '  BSN =', BSN, ' (block cols)'
      WRITE(*,*) '  ST  =', ST,  ' (linear part size)'
      WRITE(*,*) '  N   =', N,   ' (total columns)'
      WRITE(*,*) '  M   =', M,   ' (total rows)'
      WRITE(*,*) '  LDJ =', LDJ, ' (leading dimension)'
      WRITE(*,*)
      WRITE(*,*) 'Memory layout analysis:'
      WRITE(*,*) '  Source offset  = LDJ*BSN+1 =', LDJ*BSN+1
      WRITE(*,*) '  Dest offset    = N*BSN+1   =', N*BSN+1
      WRITE(*,*) '  LDJ > N:', LDJ, '>', N, '=> OVERLAP!'
      WRITE(*,*)
C
C     Print original J matrix (last block column - linear part)
      WRITE(*,*) 'Original J matrix (cols BSN+1 to NC, linear part):'
      DO I = 1, M
         WRITE(*,'(A,I2,A,10F8.1)') '  Row ', I, ':',
     $        (J_ORIG(I,K), K=BSN+1, NC)
      END DO
      WRITE(*,*)
C
C     Call NF01BS
      WRITE(*,*) 'Calling NF01BS...'
      CALL NF01BS( N, IPAR, LIPAR, FNORM, J, LDJ, E, JNORMS,
     $             GNORM, IPVT, DWORK, LDWORK, INFO )
C
      WRITE(*,*)
      WRITE(*,*) 'Results:'
      WRITE(*,*) '  INFO  =', INFO
      WRITE(*,*) '  GNORM =', GNORM
      WRITE(*,*) '  LDJ (after) =', LDJ
      WRITE(*,*)
C
C     Print pivot indices
      WRITE(*,*) 'Pivot indices:'
      WRITE(*,'(A,20I4)') '  IPVT:', (IPVT(I), I=1,N)
      WRITE(*,*)
C
C     Print column norms
      WRITE(*,*) 'Column norms:'
      WRITE(*,'(A,6F10.4)') '  JNORMS:', (JNORMS(I), I=1,N)
      WRITE(*,*)
C
      WRITE(*,*) '=================================================='
      WRITE(*,*) 'Test completed. Run with asan to see overlap:'
      WRITE(*,*) '=================================================='
C
      END
