*     SG03BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER           NIN, NOUT
      PARAMETER         ( NIN = 5, NOUT = 6 )
      INTEGER           NMAX
      PARAMETER         ( NMAX = 20 )
      INTEGER           LDA, LDB, LDE, LDQ, LDZ
      PARAMETER         ( LDA = NMAX, LDB = NMAX, LDE = NMAX,
     $                    LDQ = NMAX, LDZ = NMAX )
      INTEGER           LDWORK
      PARAMETER         ( LDWORK = MAX( 1, 4*NMAX, 6*NMAX-6 ) )
*     .. Local Scalars ..
      CHARACTER*1       DICO, FACT, TRANS
      DOUBLE PRECISION  SCALE
      INTEGER           I, INFO, J, N, M
*     .. Local Arrays ..
      DOUBLE PRECISION  A(LDA,NMAX), ALPHAI(NMAX), ALPHAR(NMAX),
     $                  B(LDB,NMAX), BETA(NMAX), DWORK(LDWORK),
     $                  E(LDE,NMAX), Q(LDQ,NMAX), Z(LDZ,NMAX)
*     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
*     .. External Subroutines ..
      EXTERNAL          SG03BD
*     .. Intrinsic Functions ..
      INTRINSIC         MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, DICO, FACT, TRANS
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE IF ( M.LT.0 .OR. M.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) M
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( FACT, 'F' ) ) THEN
            READ ( NIN, FMT = * ) ( ( Q(I,J), J = 1,N ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( Z(I,J), J = 1,N ), I = 1,N )
         END IF
         IF ( LSAME( FACT, 'T' ) ) THEN
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,M )
         END IF
*        Find the Cholesky factor U of the solution matrix.
         CALL SG03BD( DICO, FACT, TRANS, N, M, A, LDA, E, LDE, Q, LDQ,
     $                Z, LDZ, B, LDB, SCALE, ALPHAR, ALPHAI, BETA,
     $                DWORK, LDWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 ) SCALE
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,N )
   20       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' SG03BD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SG03BD = ',I2)
99997 FORMAT (' SCALE = ',F8.4,//' The Cholesky factor U of the solution
     $ matrix is')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
99994 FORMAT (/' M is out of range.',/' M = ',I5)
      END
