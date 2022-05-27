*     MB04DY EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDQG
      PARAMETER        ( LDA = NMAX, LDQG = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, N
      CHARACTER*1      JOBSCL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), D(NMAX), DWORK(LDWORK),
     $                 QG(LDQG,NMAX+1)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB04DY
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, JOBSCL
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99998 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J),    J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( QG(J,I+1), I = J,N ), J = 1,N )
         READ ( NIN, FMT = * ) ( ( QG(I,J),   I = J,N ), J = 1,N )
*        Scale the Hamiltonian matrix.
         CALL MB04DY( JOBSCL, N, A, LDA, QG, LDQG, D, DWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99997 ) INFO
         ELSE
*           Show the scaled Hamiltonian matrix.
            WRITE ( NOUT, FMT = 99996 )
            DO 10 I = 1, N
              WRITE ( NOUT, FMT = 99993 )  ( A(I,J),    J = 1,N ),
     $           ( QG(J,I+1), J = 1,I-1 ), ( QG(I,J+1), J = I,N )
10          CONTINUE
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99993 ) (  QG(I,J), J = 1,I-1 ),
     $               ( QG(J,I), J = I,N ), ( -A(J,I),  J = 1,N )
20          CONTINUE
*           Show the scaling factors.
            IF ( LSAME( JOBSCL, 'S' ) ) THEN
               WRITE ( NOUT, FMT = 99995 )
               WRITE ( NOUT, FMT = 99993 ) ( D(I), I = 1,N )
            ELSE IF ( LSAME( JOBSCL, '1' ) .OR. LSAME( JOBSCL, 'O' ) )
     $            THEN
               WRITE ( NOUT, FMT = 99994 )
               WRITE ( NOUT, FMT = 99993 ) D(1)
            END IF
         ENDIF
      END IF
      STOP
*
99999 FORMAT (' MB04DY EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (/' N is out of range.',/' N = ',I5)
99997 FORMAT (' INFO on exit from MB04DY = ',I2)
99996 FORMAT (/' The scaled Hamiltonian is ')
99995 format (/' The scaling factors are ')
99994 format (/' The scaling factor tau is ')
99993 FORMAT (1X,8(F10.4))
      END
