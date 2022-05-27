*     SB03MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDC, LDU
      PARAMETER        ( LDA = NMAX, LDC = NMAX, LDU = NMAX )
      INTEGER          LDWORK, LIWORK
      PARAMETER        ( LDWORK = 2*NMAX*NMAX + 3*NMAX,
     $                   LIWORK = NMAX*NMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, N
      CHARACTER*1      DICO, FACT, JOB, TRANA
      DOUBLE PRECISION FERR, SCALE, SEP
*     .. Local Arrays ..
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), C(LDC,NMAX), DWORK(LDWORK),
     $                 U(LDU,NMAX), WI(NMAX), WR(NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SB03MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, DICO, FACT, JOB, TRANA
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( FACT, 'F' ) ) READ ( NIN, FMT = * )
     $                         ( ( U(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,N )
*        Find the solution matrix X.
         CALL SB03MD( DICO, JOB, FACT, TRANA, N, A, LDA, U, LDU, C, LDC,
     $                SCALE, SEP, FERR, WR, WI, IWORK, DWORK, LDWORK,
     $                INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,N )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99994 ) SCALE
            IF ( .NOT.LSAME( JOB, 'X' ) )
     $         WRITE ( NOUT, FMT = 99993 ) SEP
            IF ( LSAME( JOB, 'B' ) )
     $         WRITE ( NOUT, FMT = 99992 ) FERR
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB03MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB03MD = ',I2)
99997 FORMAT (' The solution matrix X is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
99994 FORMAT (/' Scaling factor = ',F8.4)
99993 FORMAT (/' Estimated separation = ',F8.4)
99992 FORMAT (/' Estimated forward error bound = ',F8.4)
      END
