*     MB02FD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          ITMAX, KMAX, NMAX
      PARAMETER        ( ITMAX = 10, KMAX = 20, NMAX = 20 )
      INTEGER          LDR, LDT, LDWORK
      PARAMETER        ( LDR = NMAX*KMAX, LDT = KMAX,
     $                   LDWORK = ( NMAX + 1 )*KMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, IT, J, K, LEN, M, N, P, PIT, POS, POSR,
     $                 S1, SCIT
      CHARACTER        TYPET
      DOUBLE PRECISION NNRM
*     .. Local Arrays .. (Dimensioned for TYPET = 'R'.)
      INTEGER          S(ITMAX)
      DOUBLE PRECISION DWORK(LDWORK), R(LDR, NMAX*KMAX),
     $                 T(LDT, NMAX*KMAX), V(NMAX*KMAX), W(NMAX*KMAX),
     $                 Z(NMAX*KMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      DOUBLE PRECISION DNRM2
      EXTERNAL         DNRM2, LSAME
*     .. External Subroutines ..
      EXTERNAL         DAXPY, DCOPY, DGEMV, DLASET, DSCAL, DTRMV, MB02FD
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, K, IT
      TYPET = 'R'
      M = N*K
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE IF( K.LE.0 .OR. K.GT.KMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) K
      ELSE IF( IT.LE.0 .OR. IT.GT.ITMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) IT
      ELSE
         READ ( NIN, FMT = * ) ( S(I), I = 1, IT )
         READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,M ), I = 1,K )
         P   = 0
         POS = 1
         WRITE ( NOUT, FMT = 99997 )
         DO 90  SCIT = 1, IT
            CALL MB02FD( TYPET, K, N, P, S(SCIT), T(1,POS), LDT,
     $                   R(POS,POS), LDR, DWORK, LDWORK, INFO )
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
               STOP
            END IF
            S1 = S(SCIT) + P
            IF ( S1.EQ.0 ) THEN
*              Estimate the 2-norm of the Toeplitz matrix with 5 power
*              iterations.
               LEN = N*K
               CALL DLASET( 'All', LEN, 1, ONE, ONE, V, 1 )
               DO 30  PIT = 1, 5
                  DO 10  I = 1, N
                     CALL DGEMV( 'NoTranspose', K, LEN-(I-1)*K, ONE, T,
     $                           LDT, V((I-1)*K+1), 1, ZERO,
     $                           W((I-1)*K+1), 1 )
   10             CONTINUE
                  DO 20 I = 1, N-1
                     CALL DGEMV( 'Transpose', K, (N-I)*K, ONE,
     $                           T(1,K+1), LDT, V((I-1)*K+1), 1,
     $                           ONE, W(I*K+1), 1 )
   20             CONTINUE
                  CALL DCOPY( LEN, W, 1, V, 1 )
                  NNRM = DNRM2( LEN, V, 1 )
                  CALL DSCAL( LEN, ONE/NNRM, V, 1 )
   30          CONTINUE
            ELSE
*              Estimate the 2-norm of the Schur complement with 5 power
*              iterations.
               LEN = ( N - S1 )*K
               CALL DLASET( 'All', LEN, 1, ONE, ONE, V, 1 )
               DO 80  PIT = 1, 5
                  POSR = ( S1 - 1 )*K + 1
                  DO 40  I = 1, N - S1
                     CALL DGEMV( 'NoTranspose', K, LEN-(I-1)*K, ONE,
     $                           T(1,POSR+K), LDT, V((I-1)*K+1), 1,
     $                           ZERO, W((I-1)*K+1), 1 )
   40             CONTINUE
                  DO 50  I = 1, N - S1
                     CALL DTRMV( 'Upper', 'NoTranspose', 'NonUnit', K,
     $                           R(POSR,POSR), LDR, V((I-1)*K+1), 1 )
                     CALL DGEMV( 'NoTranspose', K, LEN-I*K, ONE,
     $                           R(POSR,POSR+K), LDR, V(I*K+1), 1, ONE,
     $                           V((I-1)*K+1), 1 )
   50             CONTINUE
                  CALL DLASET( 'All', LEN, 1, ZERO, ZERO, Z, 1 )
                  DO 60  I = 1, N - S1
                     CALL DGEMV( 'Transpose', K, LEN-I*K, ONE,
     $                           R(POSR,POSR+K), LDR, V((I-1)*K+1), 1,
     $                           ONE, Z(I*K+1), 1 )
                     CALL DTRMV( 'Upper', 'Transpose', 'NonUnit', K,
     $                           R(POSR,POSR), LDR, V((I-1)*K+1), 1 )
                     CALL DAXPY( K, ONE, V((I-1)*K+1), 1, Z((I-1)*K+1),
     $                           1 )
   60             CONTINUE
                  CALL DLASET( 'All', LEN, 1, ZERO, ZERO, V, 1 )
                  DO 70  I = 1, N - S1
                     CALL DGEMV( 'Transpose', K, LEN-(I-1)*K, ONE,
     $                           T(1,POSR+K), LDT, W((I-1)*K+1), 1,
     $                           ONE, V((I-1)*K+1), 1 )
   70             CONTINUE
                  CALL DAXPY( LEN, -ONE, Z, 1, V, 1 )
                  NNRM = DNRM2( LEN, V, 1 )
                  CALL DSCAL( LEN, -ONE/NNRM, V, 1 )
   80          CONTINUE
               POS = ( S1 - 1 )*K + 1
               P   = S1
            END IF
            WRITE ( NOUT, FMT = 99995 ) P*K, NNRM
   90    CONTINUE
         WRITE ( NOUT, FMT = 99996 )
         DO 100  I = 1, P*K
            WRITE ( NOUT, FMT = 99994 ) ( R(I,J), J = 1, M )
  100    CONTINUE
      END IF
      STOP
*
99999 FORMAT (' MB02FD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02FD = ',I2)
99997 FORMAT ('   Incomplete Cholesky factorization ',
     $         //'   rows    norm(Schur complement)',/)
99996 FORMAT (/' The upper ICC factor of the block Toeplitz matrix is '
     $       )
99995 FORMAT (I4,5X,F8.4)
99994 FORMAT (20(1X,F8.4))
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' K is out of range.',/' K = ',I5)
99991 FORMAT (/' IT is out of range.',/' IT = ',I5)
      END
