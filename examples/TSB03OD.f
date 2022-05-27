*     SB03OD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDQ, LDX, LDWORK
      PARAMETER        ( LDA = NMAX, LDB = MAX( MMAX,NMAX ),
     $                   LDQ = NMAX, LDX = NMAX )
      PARAMETER        ( LDWORK = 4*NMAX+MIN(MMAX,NMAX) )
*     .. Local Scalars ..
      DOUBLE PRECISION SCALE, TEMP
      INTEGER          I, INFO, J, K, M, N
      CHARACTER*1      DICO, FACT, TRANS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,LDB), DWORK(LDWORK),
     $                 Q(LDQ,NMAX), WR(NMAX), WI(NMAX), X(LDX,NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SB03OD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, DICO, FACT, TRANS
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( FACT, 'F' ) ) READ ( NIN, FMT = * )
     $                         ( ( Q(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) M
         ELSE
            IF ( LSAME( TRANS, 'N' ) ) THEN
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,M )
            ELSE
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            END IF
*           Find the Cholesky factor U.
            CALL SB03OD( DICO, FACT, TRANS, N, M, A, LDA, Q, LDQ, B,
     $                   LDB, SCALE, WR, WI, DWORK, LDWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 )
               DO 20 J = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( B(I,J), I = 1,J )
   20          CONTINUE
*              Form the solution matrix X = op(U)'*op(U).
               IF ( LSAME( TRANS, 'N' ) ) THEN
                  DO 80 I = 1, N
                     DO 60 J = I, N
                        TEMP = ZERO
                        DO 40 K = 1, I
                           TEMP = TEMP + B(K,I)*B(K,J)
   40                   CONTINUE
                        X(I,J) = TEMP
                        X(J,I) = TEMP
   60                CONTINUE
   80             CONTINUE
               ELSE
                  DO 140 I = 1, N
                     DO 120 J = I, N
                        TEMP = ZERO
                        DO 100 K = J, N
                           TEMP = TEMP + B(I,K)*B(J,K)
  100                   CONTINUE
                        X(I,J) = TEMP
                        X(J,I) = TEMP
  120                CONTINUE
  140             CONTINUE
               END IF
               WRITE ( NOUT, FMT = 99995 )
               DO 160 J = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( X(I,J), I = 1,N )
  160          CONTINUE
               WRITE ( NOUT, FMT = 99992 ) SCALE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB03OD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB03OD = ',I2)
99997 FORMAT (' The transpose of the Cholesky factor U is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The solution matrix X = op(U)''*op(U) is ')
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' M is out of range.',/' M = ',I5)
99992 FORMAT (/' Scaling factor = ',F8.4)
      END
