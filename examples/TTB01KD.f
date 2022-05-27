*     TB01KD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDU
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDU = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 3*NMAX )
*     .. Local Scalars ..
      CHARACTER*1      DICO, JOBA, STDOM
      INTEGER          I, INFO, J, M, N, NDIM, P
      DOUBLE PRECISION ALPHA
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 DWORK(LDWORK), U(LDU,NMAX), WI(NMAX), WR(NMAX)
*     .. External Subroutines ..
      EXTERNAL         TB01KD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, ALPHA, DICO, STDOM, JOBA
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1, N )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99988 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
*              Find the transformed ssr for (A,B,C).
               CALL TB01KD( DICO, STDOM, JOBA, N, M, P, ALPHA, A, LDA,
     $                      B, LDB, C, LDC, NDIM, U, LDU, WR, WI, DWORK,
     $                      LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99987 ) NDIM
                  WRITE ( NOUT, FMT = 99997 )
                  DO 10 I = 1, N
                     WRITE ( NOUT, FMT = 99994 ) WR(I), WI(I)
   10             CONTINUE
                  WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, N
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,N )
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99992 )
                  DO 60 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,N )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99991 )
                  DO 70 I = 1, N
                     WRITE ( NOUT, FMT = 99995 ) ( U(I,J), J = 1,N )
   70             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB01KD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB01KD = ',I2)
99997 FORMAT (/' The eigenvalues of state dynamics matrix A are ')
99996 FORMAT (/' The transformed state dynamics matrix inv(U)*A*U is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT ( ' (',F8.4,', ',F8.4,' )')
99993 FORMAT (/' The transformed input/state matrix inv(U)*B is ')
99992 FORMAT (/' The transformed state/output matrix C*U is ')
99991 FORMAT (/' The similarity transformation matrix U is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
99987 FORMAT (' The number of eigenvalues in the domain of interest =',
     $        I5 )
      END
