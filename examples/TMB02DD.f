*     MB02DD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX, MMAX, NMAX
      PARAMETER        ( KMAX = 20, MMAX = 20, NMAX = 20 )
      INTEGER          LCS, LDG, LDL, LDR, LDT, LDWORK
      PARAMETER        ( LDG = KMAX*( MMAX + NMAX ),
     $                   LDL = KMAX*( MMAX + NMAX ),
     $                   LDR = KMAX*( MMAX + NMAX ),
     $                   LDT = KMAX*( MMAX + NMAX ),
     $                   LDWORK = ( MMAX + NMAX - 1 )*KMAX )
      PARAMETER        ( LCS = 3*LDWORK )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, K, M, N, S
      CHARACTER        JOB, TYPET
*     .. Local Arrays ..
*     The arrays are dimensioned for both TYPET = 'R' and TYPET = 'C'.
*     Arrays G and T could be smaller.
*     For array G, it is assumed that MMAX + NMAX >= 2.
*     The matrix TA is also stored in the array T.
      DOUBLE PRECISION CS(LCS), DWORK(LDWORK),
     $                 G(LDG, KMAX*( MMAX + NMAX )),
     $                 L(LDL, KMAX*( MMAX + NMAX )),
     $                 R(LDR, KMAX*( MMAX + NMAX )),
     $                 T(LDT, KMAX*( MMAX + NMAX ))
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         DLACPY, MB02CD, MB02DD
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, K, M, JOB, TYPET
      S = ( N + M )*K
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) N
      ELSE
         IF ( K.LE.0 .OR. K.GT.KMAX ) THEN
            WRITE ( NOUT, FMT = 99988 ) K
         ELSE
            IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
               WRITE ( NOUT, FMT = 99987 ) M
            ELSE
               IF ( LSAME( TYPET, 'R' ) ) THEN
                  READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,S ), I = 1,K )
               ELSE
                  READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,K ), I = 1,S )
               END IF
*              Compute the Cholesky factors.
               CALL MB02CD( JOB, TYPET, K, N, T, LDT, G, LDG, R, LDR, L,
     $                      LDL, CS, LCS, DWORK, LDWORK, INFO )
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99996 )
                  DO 10  I = 1, N*K
                     WRITE ( NOUT, FMT = 99990 ) ( R(I,J), J = 1, N*K )
   10             CONTINUE
                  IF ( LSAME( JOB, 'R' ) .OR. LSAME( JOB, 'A' ) ) THEN
                     WRITE ( NOUT, FMT = 99995 )
                     IF ( LSAME( TYPET, 'R' ) ) THEN
                        DO 20  I = 1, 2*K
                           WRITE ( NOUT, FMT = 99990 )
     $                           ( G(I,J), J = 1, N*K )
   20                   CONTINUE
                     ELSE
                        DO 30  I = 1, N*K
                           WRITE ( NOUT, FMT = 99990 )
     $                           ( G(I,J), J = 1, 2*K )
   30                   CONTINUE
                     END IF
                  END IF
                  IF ( LSAME( JOB, 'A' ) ) THEN
                     WRITE ( NOUT, FMT = 99994 )
                     DO 40  I = 1, N*K
                        WRITE ( NOUT, FMT = 99990 )
     $                        ( L(I,J), J = 1, N*K )
   40                CONTINUE
                  END IF
*                 Update the Cholesky factors.
                  IF ( LSAME( TYPET, 'R' ) ) THEN
*                    Copy the last block column of R.
                     CALL DLACPY( 'All', N*K, K, R(1,(N-1)*K+1), LDR,
     $                            R(K+1,N*K+1), LDR )
                     CALL MB02DD( JOB, TYPET, K, M, N, T(1,N*K+1), LDT,
     $                            T, LDT, G, LDG, R(1,N*K+1), LDR,
     $                            L(N*K+1,1), LDL, CS, LCS, DWORK,
     $                            LDWORK, INFO )
                  ELSE
*                    Copy the last block row of R.
                     CALL DLACPY( 'All', K, N*K, R((N-1)*K+1,1), LDR,
     $                            R(N*K+1,K+1), LDR )
                     CALL MB02DD( JOB, TYPET, K, M, N, T(N*K+1,1), LDT,
     $                            T, LDT, G, LDG, R(N*K+1,1), LDR,
     $                            L(1,N*K+1), LDL, CS, LCS, DWORK,
     $                            LDWORK, INFO )
                  END IF
                  IF ( INFO.NE.0 ) THEN
                     WRITE ( NOUT, FMT = 99997 ) INFO
                  ELSE
                     WRITE ( NOUT, FMT = 99993 )
                     DO 50  I = 1, S
                        WRITE ( NOUT, FMT = 99990 ) ( R(I,J), J = 1, S )
   50                CONTINUE
                     IF ( LSAME( JOB, 'R' ) .OR. LSAME( JOB, 'A' ) )
     $                       THEN
                        WRITE ( NOUT, FMT = 99992 )
                        IF ( LSAME( TYPET, 'R' ) ) THEN
                           DO 60  I = 1, 2*K
                              WRITE ( NOUT, FMT = 99990 )
     $                              ( G(I,J), J = 1, S )
   60                      CONTINUE
                        ELSE
                           DO 70  I = 1, S
                              WRITE ( NOUT, FMT = 99990 )
     $                              ( G(I,J), J = 1, 2*K )
   70                      CONTINUE
                        END IF
                     END IF
                     IF ( LSAME( JOB, 'A' ) ) THEN
                        WRITE ( NOUT, FMT = 99991 )
                        DO 80  I = 1, S
                           WRITE ( NOUT, FMT = 99990 )
     $                           ( L(I,J), J = 1, S )
   80                   CONTINUE
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT ( ' MB02DD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT ( ' INFO on exit from MB02CD = ',I2)
99997 FORMAT ( ' INFO on exit from MB02DD = ',I2)
99996 FORMAT ( ' The Cholesky factor is ')
99995 FORMAT (/' The inverse generator is ')
99994 FORMAT (/' The inverse Cholesky factor is ')
99993 FORMAT (/' The updated Cholesky factor is ')
99992 FORMAT (/' The updated inverse generator is ')
99991 FORMAT (/' The updated inverse Cholesky factor is ')
99990 FORMAT (20(1X,F8.4))
99989 FORMAT (/' N is out of range.',/' N = ',I5)
99988 FORMAT (/' K is out of range.',/' K = ',I5)
99987 FORMAT (/' M is out of range.',/' M = ',I5)
      END
