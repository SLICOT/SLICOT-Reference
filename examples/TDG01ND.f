*     DG01ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 128 )
*     .. Local Scalars ..
      INTEGER          I, IEND, INFO, N
      CHARACTER*1      INDI
*     .. Local Arrays ..
      DOUBLE PRECISION A(2*NMAX), XI(NMAX+1), XR(NMAX+1)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         DG01ND
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, INDI
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( A(I), I = 1,2*N )
*        Copy the odd and even parts of A into XR and XI respectively.
         DO 20 I = 1, N
            XR(I) = A(2*I-1)
            XI(I) = A(2*I)
   20    CONTINUE
*        Find the Fourier transform of the given real signal.
         CALL DG01ND( INDI, N, XR, XI, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            IEND = N
            IF ( LSAME( INDI, 'D' ) ) IEND = N + 1
            DO 40 I = 1, IEND
               WRITE ( NOUT, FMT = 99996 ) I, XR(I), XI(I)
   40       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' DG01ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from DG01ND = ',I2)
99997 FORMAT (' Components of Fourier transform are',//'   i',6X,
     $       'XR(i)',6X,'XI(i)',/)
99996 FORMAT (I4,3X,F8.4,3X,F8.4)
99995 FORMAT (/' N is out of range.',/' N = ',I5)
      END
