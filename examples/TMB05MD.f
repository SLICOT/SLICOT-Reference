*     MB05MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDV, LDY
      PARAMETER        ( LDA = NMAX, LDV = NMAX, LDY = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 4*NMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION DELTA
      INTEGER          I, INFO, J, N
      CHARACTER*1      BALANC
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), V(LDV,NMAX),
     $                 VALI(NMAX), VALR(NMAX), Y(LDY,NMAX)
      INTEGER          IWORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         MB05MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      BALANC = 'N'
      READ ( NIN, FMT = * ) N, DELTA
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
*        Find the exponential of the real non-defective matrix A*DELTA.
         CALL MB05MD( BALANC, N, DELTA, A, LDA, V, LDV, Y, LDY, VALR,
     $                VALI, IWORK, DWORK, LDWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,N )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99995 ) ( VALR(I), VALI(I), I = 1,N )
            WRITE ( NOUT, FMT = 99994 )
            DO 40 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( V(I,J), J = 1,N )
   40       CONTINUE
            WRITE ( NOUT, FMT = 99993 )
            DO 60 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( Y(I,J), J = 1,N )
   60       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB05MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB05MD = ',I2)
99997 FORMAT (' The solution matrix exp(A*DELTA) is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The eigenvalues of A are ',/20(2F5.1,'*j  '))
99994 FORMAT (/' The eigenvector matrix for A is ')
99993 FORMAT (/' The inverse eigenvector matrix for A (premultiplied by'
     $        ,' exp(Lambda*DELTA)) is ')
99992 FORMAT (/' N is out of range.',/' N = ',I5)
      END
