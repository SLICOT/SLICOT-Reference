*     TB01MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDU, LDWORK
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDU = NMAX,
     $                   LDWORK = NMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, M, N
      CHARACTER*1      JOBU, UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), U(LDU,NMAX),
     $                 DWORK(LDWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TB01MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, JOBU, UPLO
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), I = 1,N ), J = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
            IF ( LSAME( JOBU, 'U' ) )
     $         READ ( NIN, FMT = * ) ( ( U(I,J), J = 1,N ), I = 1,N )
*           Reduce the pair (B,A) to controller Hessenberg form.
            CALL TB01MD( JOBU, UPLO, N, M, A, LDA, B, LDB, U, LDU,
     $                   DWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 )
               DO 60 I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,N )
   60          CONTINUE
               WRITE ( NOUT, FMT = 99995 )
               DO 80 I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,M )
   80          CONTINUE
               IF ( LSAME( JOBU, 'I' ).OR.LSAME( JOBU, 'U' ) ) THEN
                  WRITE ( NOUT, FMT = 99994 )
                  DO 100 I = 1, N
                     WRITE ( NOUT, FMT = 99996 ) ( U(I,J), J = 1,N )
  100             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB01MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB01MD = ',I2)
99997 FORMAT (' The transformed state transition matrix is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The transformed input matrix is ')
99994 FORMAT (/' The transformation matrix that reduces (B,A) to contr',
     $       'oller Hessenberg form is ')
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
      END
