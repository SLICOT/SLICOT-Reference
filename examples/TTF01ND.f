*     TF01ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX, NYMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20, NYMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDD, LDU, LDY
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX, LDU = MMAX, LDY = PMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX )
*     .. Local Scalars ..
      CHARACTER*1      UPLO
      INTEGER          I, INFO, J, K, M, N, NY, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK), U(LDU,NYMAX),
     $                 X(NMAX), Y(LDY,NYMAX)
*     .. External Subroutines ..
      EXTERNAL         TF01ND
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, NY, UPLO
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), I = 1,N ), J = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
            IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99992 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), I = 1,P ), J = 1,N )
               READ ( NIN, FMT = * ) ( ( D(I,J), I = 1,P ), J = 1,M )
               READ ( NIN, FMT = * ) ( X(I), I = 1,N )
               IF ( NY.LE.0 .OR. NY.GT.NYMAX ) THEN
                  WRITE ( NOUT, FMT = 99991 ) NY
               ELSE
                  READ ( NIN, FMT = * )
     $                 ( ( U(I,J), I = 1,M ), J = 1,NY )
*                 Compute y(1),...,y(NY) of the given system.
                  CALL TF01ND( UPLO, N, M, P, NY, A, LDA, B, LDB, C,
     $                         LDC, D, LDD, U, LDU, X, Y, LDY, DWORK,
     $                         INFO )
*
                  IF ( INFO.NE.0 ) THEN
                     WRITE ( NOUT, FMT = 99998 ) INFO
                  ELSE
                     WRITE ( NOUT, FMT = 99997 ) NY
                     DO 20 K = 1, NY
                        WRITE ( NOUT, FMT = 99996 ) K, Y(1,K)
                        WRITE ( NOUT, FMT = 99995 ) ( Y(J,K), J = 2,P )
   20                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TF01ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TF01ND = ',I2)
99997 FORMAT (' The output sequence Y(1),...,Y(',I2,') is',/)
99996 FORMAT (' Y(',I2,') : ',F8.4)
99995 FORMAT (9X,F8.4,/)
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' M is out of range.',/' M = ',I5)
99992 FORMAT (/' P is out of range.',/' P = ',I5)
99991 FORMAT (/' NY is out of range.',/' NY = ',I5)
      END
