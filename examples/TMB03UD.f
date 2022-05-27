*     MB03UD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 10 )
      INTEGER          LDA, LDQ
      PARAMETER        ( LDA = NMAX, LDQ = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( 1, 5*NMAX ) )
*     .. Local Scalars ..
      CHARACTER*1      JOBQ, JOBP
      INTEGER          I, INFO, J, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), Q(LDQ,NMAX),
     $                 SV(NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
*     .. External Subroutines ..
      EXTERNAL         MB03UD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, JOBQ, JOBP
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
*        Compute the singular values and vectors.
         CALL MB03UD( JOBQ, JOBP, N, A, LDA, Q, LDQ, SV, DWORK,
     $                LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            WRITE ( NOUT, FMT = 99995 ) ( SV(I), I = 1,N )
            IF ( LSAME( JOBP, 'V' ) ) THEN
               WRITE ( NOUT, FMT = 99996 )
               DO 10 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,N )
   10          CONTINUE
            END IF
            IF ( LSAME( JOBQ, 'V' ) ) THEN
               WRITE ( NOUT, FMT = 99994 )
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( Q(I,J), J = 1,N )
   20          CONTINUE
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MB03UD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB03UD = ',I2)
99997 FORMAT (' Singular values are ',I5)
99996 FORMAT (/' The transpose of the right singular vectors matrix is '
     $       )
99995 FORMAT (8X,20(1X,F8.4))
99994 FORMAT (/' The left singular vectors matrix is ')
99993 FORMAT (/' N is out of range.',/' N = ',I5)
      END
