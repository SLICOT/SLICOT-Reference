*     SB02MD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDG, LDQ, LDS, LDU
      PARAMETER        ( LDA = NMAX, LDG = NMAX, LDQ = NMAX,
     $                   LDS = 2*NMAX, LDU = 2*NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = 2*NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 6*NMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION RCOND
      INTEGER          I, INFO, J, N
      CHARACTER        DICO, HINV, SCAL, SORT, UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), G(LDG,NMAX),
     $                 Q(LDQ,NMAX), S(LDS,2*NMAX), U(LDU,2*NMAX),
     $                 WI(2*NMAX), WR(2*NMAX)
      INTEGER          IWORK(LIWORK)
      LOGICAL          BWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         SB02MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, DICO, HINV, UPLO, SCAL, SORT
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( Q(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( G(I,J), J = 1,N ), I = 1,N )
*        Find the solution matrix X.
         CALL SB02MD( DICO, HINV, UPLO, SCAL, SORT, N, A, LDA, G, LDG,
     $                Q, LDQ, RCOND, WR, WI, S, LDS, U, LDU, IWORK,
     $                DWORK, LDWORK, BWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 ) RCOND
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( Q(I,J), J = 1,N )
   20       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB02MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB02MD = ',I2)
99997 FORMAT (' RCOND = ',F4.2,//' The solution matrix X is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
      END
