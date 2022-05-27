*     MB05OD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA
      PARAMETER        ( LDA = NMAX )
      INTEGER          NDIAG
      PARAMETER        ( NDIAG = 9 )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX*( 2*NMAX+NDIAG+1 )+NDIAG )
*     .. Local Scalars ..
      DOUBLE PRECISION DELTA
      INTEGER          I, IDIG, INFO, IWARN, J, MDIG, N
      CHARACTER*1      BALANC
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK)
      INTEGER          IWORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         MB05OD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, DELTA, BALANC
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
*        Find the exponential of the real defective matrix A*DELTA.
         CALL MB05OD( BALANC, N, NDIAG, DELTA, A, LDA, MDIG, IDIG,
     $                IWORK, DWORK, LDWORK, IWARN, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( IWARN.NE.0 )
     $         WRITE ( NOUT, FMT = 99993 ) IWARN
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,N )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99995 ) MDIG, IDIG
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB05OD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB05OD = ',I2)
99997 FORMAT (' The solution matrix E = exp(A*DELTA) is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' Minimal number of accurate digits in the norm of E =',
     $       I4,/' Number of accurate digits in the norm of E',/'     ',
     $       '            at 95 per cent confidence interval =',I4)
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (' IWARN on exit from MB05OD = ',I2)
      END
