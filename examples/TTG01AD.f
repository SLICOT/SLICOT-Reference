*     TG01AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          LMAX, NMAX, MMAX, PMAX
      PARAMETER        ( LMAX = 20, NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDE
      PARAMETER        ( LDA = LMAX, LDB = LMAX, LDC = PMAX,
     $                   LDE = LMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( 1, 3*(LMAX+NMAX ) ) )
*     .. Local Scalars ..
      CHARACTER*1      JOBS
      INTEGER          I, INFO, J, L, M, N, P
      DOUBLE PRECISION ABCNRM, ENORM, SABCNM, SENORM, THRESH
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 DWORK(LDWORK), E(LDE,NMAX), LSCALE(LMAX),
     $                 RSCALE(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION DLANGE
      EXTERNAL         DLANGE
*     .. External Subroutines ..
      EXTERNAL         TG01AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) L, N, M, P, JOBS, THRESH
      IF ( L.LT.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) L
      ELSE
         IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
            WRITE ( NOUT, FMT = 99988 ) N
         ELSE
            READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,L )
            READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,L )
            IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
               WRITE ( NOUT, FMT = 99987 ) M
            ELSE
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,L )
               IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
                  WRITE ( NOUT, FMT = 99986 ) P
               ELSE
                  READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
*                 Compute norms before scaling
                  ABCNRM = MAX( DLANGE( '1', L, N, A, LDA, DWORK ),
     $                          DLANGE( '1', L, M, B, LDB, DWORK ),
     $                          DLANGE( '1', P, N, C, LDC, DWORK ) )
                  ENORM = DLANGE( '1', L, N, E, LDE, DWORK )
*                 Find the transformed descriptor system
*                 (A-lambda E,B,C).
                  CALL TG01AD( JOBS, L, N, M, P, THRESH, A, LDA, E, LDE,
     $                         B, LDB, C, LDC, LSCALE, RSCALE, DWORK,
     $                         INFO )
*
                  IF ( INFO.NE.0 ) THEN
                     WRITE ( NOUT, FMT = 99998 ) INFO
                  ELSE
                     SABCNM = MAX( DLANGE( '1', L, N, A, LDA, DWORK ),
     $                             DLANGE( '1', L, M, B, LDB, DWORK ),
     $                             DLANGE( '1', P, N, C, LDC, DWORK ) )
                     SENORM = DLANGE( '1', L, N, E, LDE, DWORK )
                     WRITE ( NOUT, FMT = 99997 )
                     DO 10 I = 1, L
                        WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,N )
   10                CONTINUE
                     WRITE ( NOUT, FMT = 99996 )
                     DO 20 I = 1, L
                        WRITE ( NOUT, FMT = 99995 ) ( E(I,J), J = 1,N )
   20                CONTINUE
                     WRITE ( NOUT, FMT = 99993 )
                     DO 30 I = 1, L
                        WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   30                CONTINUE
                     WRITE ( NOUT, FMT = 99992 )
                     DO 40 I = 1, P
                        WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,N )
   40                CONTINUE
                     WRITE ( NOUT, FMT = 99991 )
                     WRITE ( NOUT, FMT = 99995 ) ( LSCALE(I), I = 1,L )
                     WRITE ( NOUT, FMT = 99990 )
                     WRITE ( NOUT, FMT = 99995 ) ( RSCALE(J), J = 1,N )
                     WRITE ( NOUT, FMT = 99994 )
     $                       ABCNRM, SABCNM, ENORM, SENORM
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TG01AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TG01AD = ',I2)
99997 FORMAT (/' The transformed state dynamics matrix Dl*A*Dr is ')
99996 FORMAT (/' The transformed descriptor matrix Dl*E*Dr is ')
99995 FORMAT (20(1X,F9.4))
99994 FORMAT (/' Norm of [ A B; C 0]         =', 1PD10.3/
     $         ' Norm of scaled [ A B; C 0]  =', 1PD10.3/
     $         ' Norm of E                   =', 1PD10.3/
     $         ' Norm of scaled E            =', 1PD10.3)
99993 FORMAT (/' The transformed input/state matrix Dl*B is ')
99992 FORMAT (/' The transformed state/output matrix C*Dr is ')
99991 FORMAT (/' The diagonal of left scaling matrix Dl is ')
99990 FORMAT (/' The diagonal of right scaling matrix Dr is ')
99989 FORMAT (/' L is out of range.',/' L = ',I5)
99988 FORMAT (/' N is out of range.',/' N = ',I5)
99987 FORMAT (/' M is out of range.',/' M = ',I5)
99986 FORMAT (/' P is out of range.',/' P = ',I5)
      END
