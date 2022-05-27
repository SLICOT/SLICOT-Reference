*     MB04OD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO  = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX, PMAX
      PARAMETER        ( MMAX = 20, NMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDR
      PARAMETER        ( LDA = PMAX, LDB = NMAX, LDC = PMAX,
     $                   LDR = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX-1,MMAX ) )
*     .. Local Scalars ..
      CHARACTER*1      UPLO
      INTEGER          I, J, M, N, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,MMAX),
     $                 DWORK(LDWORK), R(LDR,NMAX), TAU(NMAX)
*     .. External Subroutines ..
      EXTERNAL         MB04OD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, UPLO
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) M
         ELSE
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99991 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( R(I,J), J = 1,N ), I = 1,N )
               READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,M ), I = 1,P )
*              Compute and apply QR factorization.
               CALL MB04OD( UPLO, N, M, P, R, LDR, A, LDA, B, LDB, C,
     $                      LDC,  TAU, DWORK )
*
               WRITE ( NOUT, FMT = 99997 )
               DO 40 I = 1, N
                  DO 20 J = 1, I-1
                     R(I,J) = ZERO
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99996 ) ( R(I,J), J = 1,N )
   40          CONTINUE
               IF ( M.GT.0 ) THEN
                  WRITE ( NOUT, FMT = 99995 )
                  DO 60 I = 1, N
                     WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,M )
   60             CONTINUE
                  IF ( P.GT.0 ) THEN
                     WRITE ( NOUT, FMT = 99994 )
                     DO 80 I = 1, P
                        WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,M )
   80                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB04OD EXAMPLE PROGRAM RESULTS',/1X)
99997 FORMAT (' The updated matrix R is ')
99996 FORMAT (20(1X,F10.4))
99995 FORMAT (' The updated matrix B is ')
99994 FORMAT (' The updated matrix C is ')
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' P is out of range.',/' P = ',I5)
      END
