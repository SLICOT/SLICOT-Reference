*     MB03QD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 10 )
      INTEGER          LDA, LDU
      PARAMETER        ( LDA = NMAX, LDU = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 3*NMAX )
*     .. Local Scalars ..
      CHARACTER*1      DICO, JOBU, STDOM
      INTEGER          I, INFO, J, N, NDIM, NLOW, NSUP
      DOUBLE PRECISION ALPHA
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), U(LDU,NMAX),
     $                 WI(NMAX), WR(NMAX)
      LOGICAL          BWORK(NMAX)
*     .. External Functions ..
      LOGICAL          SELECT
*     .. External Subroutines ..
      EXTERNAL         DGEES, MB03QD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, NLOW, NSUP, ALPHA, DICO, STDOM, JOBU
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
*        Compute Schur form, eigenvalues and Schur vectors.
         CALL DGEES( 'Vectors', 'Not sorted', SELECT, N, A, LDA, NDIM,
     $               WR, WI, U, LDU, DWORK, LDWORK, BWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
*           Block reordering.
            CALL MB03QD( DICO, STDOM, JOBU, N, NLOW, NSUP, ALPHA,
     $                   A, LDA, U, LDU, NDIM, DWORK, INFO )
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99996 ) NDIM
               WRITE ( NOUT, FMT = 99994 )
               DO 10 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,N )
   10          CONTINUE
               WRITE ( NOUT, FMT = 99993 )
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( U(I,J), J = 1,N )
   20          CONTINUE
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MB03QD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from DGEES  = ',I2)
99997 FORMAT (' INFO on exit from MB03QD = ',I2)
99996 FORMAT (' The number of eigenvalues in the domain is ',I5)
99995 FORMAT (8X,20(1X,F8.4))
99994 FORMAT (/' The ordered Schur form matrix is ')
99993 FORMAT (/' The transformation matrix is ')
99992 FORMAT (/' N is out of range.',/' N = ',I5)
      END
