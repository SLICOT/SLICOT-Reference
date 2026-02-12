*     AB01MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDZ
      PARAMETER        ( LDA = NMAX, LDZ = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, J, N, NCONT
      CHARACTER*1      JOBZ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(NMAX), DWORK(LDWORK), TAU(NMAX),
     $                 Z(LDZ,NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         AB01MD, DORGQR
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, TOL, JOBZ
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( B(I), I = 1,N )
*        Find a controllable realization for the given system.
         CALL AB01MD( JOBZ, N, A, LDA, B, NCONT, Z, LDZ, TAU, TOL,
     $                DWORK, LDWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 ) NCONT
            DO 20 I = 1, NCONT
               WRITE ( NOUT, FMT = 99994 ) ( A(I,J), J = 1,NCONT )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99996 ) ( B(I), I = 1,NCONT )
            IF ( LSAME( JOBZ, 'F' ) )
     $         CALL DORGQR( N, N, N, Z, LDZ, TAU, DWORK, LDWORK, INFO )
            IF ( LSAME( JOBZ, 'F' ).OR.LSAME( JOBZ, 'I' ) ) THEN
               WRITE ( NOUT, FMT = 99995 )
               DO 40 I = 1, N
                  WRITE ( NOUT, FMT = 99994 ) ( Z(I,J), J = 1,N )
   40          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB01MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB01MD = ',I2)
99997 FORMAT (' The order of the controllable state-space representati',
     $       'on = ',I2,//' The state dynamics matrix A of a controlla',
     $       'ble realization is ')
99996 FORMAT (/' The input/state vector B of a controllable realizatio',
     $       'n is ',/(1X,F8.4))
99995 FORMAT (/' The similarity transformation matrix Z is ')
99994 FORMAT (20(1X,F8.4))
99993 FORMAT (/' N is out of range.',/' N = ',I5)
      END
