*     SG03AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER           NIN, NOUT
      PARAMETER         ( NIN = 5, NOUT = 6 )
      INTEGER           NMAX
      PARAMETER         ( NMAX = 20 )
      INTEGER           LDA, LDE, LDQ, LDX, LDZ
      PARAMETER         ( LDA = NMAX, LDE = NMAX, LDQ = NMAX,
     $                    LDX = NMAX, LDZ = NMAX )
      INTEGER           LIWORK, LDWORK
      PARAMETER         ( LIWORK = NMAX**2,
     $                    LDWORK = MAX( 2*NMAX**2, 4*NMAX ) )
*     .. Local Scalars ..
      CHARACTER*1       DICO, FACT, JOB, TRANS, UPLO
      DOUBLE PRECISION  FERR, SCALE, SEP
      INTEGER           I, INFO, J, N
*     .. Local Arrays ..
      INTEGER           IWORK(LIWORK)
      DOUBLE PRECISION  A(LDA,NMAX), ALPHAI(NMAX), ALPHAR(NMAX),
     $                  BETA(NMAX), DWORK(LDWORK), E(LDE,NMAX),
     $                  Q(LDQ,NMAX), X(LDX,NMAX), Z(LDZ,NMAX)
*     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
*     .. External Subroutines ..
      EXTERNAL          SG03AD
*     .. Intrinsic Functions ..
      INTRINSIC         MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, JOB, DICO, FACT, TRANS, UPLO
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME ( FACT, 'F' ) ) THEN
            READ ( NIN, FMT = * ) ( ( Q(I,J), J = 1,N ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( Z(I,J), J = 1,N ), I = 1,N )
         END IF
         IF ( .NOT.LSAME ( JOB, 'S' ) )
     $      READ ( NIN, FMT = * ) ( ( X(I,J), J = 1,N ), I = 1,N )
*        Find the solution matrix X and the scalar SEP.
         CALL SG03AD( DICO, JOB, FACT, TRANS, UPLO, N, A, LDA, E, LDE,
     $                Q, LDQ, Z, LDZ, X, LDX, SCALE, SEP, FERR, ALPHAR,
     $                ALPHAI, BETA, IWORK, DWORK, LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( LSAME ( JOB, 'B' ) .OR. LSAME ( JOB, 'S' ) ) THEN
               WRITE ( NOUT, FMT = 99997 ) SEP
               WRITE ( NOUT, FMT = 99996 ) FERR
            END IF
            IF ( LSAME ( JOB, 'B' ) .OR. LSAME ( JOB, 'X' ) ) THEN
               WRITE ( NOUT, FMT = 99995 ) SCALE
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99994 ) ( X(I,J), J = 1,N )
   20          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SG03AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SG03AD = ',I2)
99997 FORMAT (' SEP =   ',D8.2)
99996 FORMAT (' FERR =  ',D8.2)
99995 FORMAT (' SCALE = ',D8.2,//' The solution matrix X is ')
99994 FORMAT (20(1X,F8.4))
99993 FORMAT (/' N is out of range.',/' N = ',I5)
      END
