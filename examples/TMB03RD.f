*     MB03RD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 10 )
      INTEGER          LDA, LDX
      PARAMETER        ( LDA = NMAX, LDX = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 3*NMAX )
*     .. Local Scalars ..
      CHARACTER*1      JOBX, SORT
      INTEGER          I, INFO, J, N, NBLCKS, SDIM
      DOUBLE PRECISION PMAX, TOL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), WI(NMAX), WR(NMAX),
     $                 X(LDX,NMAX)
      INTEGER          BLSIZE(NMAX)
      LOGICAL          BWORK(NMAX)
*     .. External Functions ..
      LOGICAL          SELECT
*     .. External Subroutines ..
      EXTERNAL         DGEES, MB03RD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, PMAX, TOL, JOBX, SORT
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99972 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
*        Compute Schur form, eigenvalues and Schur vectors.
         CALL DGEES( 'Vectors', 'Not sorted', SELECT, N, A, LDA, SDIM,
     $               WR, WI, X, LDX, DWORK, LDWORK, BWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
*           Block-diagonalization.
            CALL MB03RD( JOBX, SORT, N, PMAX, A, LDA, X, LDX, NBLCKS,
     $                   BLSIZE, WR, WI, TOL, DWORK, INFO )
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99997 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99995 ) NBLCKS
               WRITE ( NOUT, FMT = 99994 ) ( BLSIZE(I), I = 1,NBLCKS )
               WRITE ( NOUT, FMT = 99993 )
               DO 10 I = 1, N
                  WRITE ( NOUT, FMT = 99992 ) ( A(I,J), J = 1,N )
   10          CONTINUE
               WRITE ( NOUT, FMT = 99991 )
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99992 ) ( X(I,J), J = 1,N )
   20          CONTINUE
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MB03RD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from DGEES  = ',I2)
99997 FORMAT (' INFO on exit from MB03RD = ',I2)
99995 FORMAT (' The number of blocks is ',I5)
99994 FORMAT (' The orders of blocks are ',/(20(I3,2X)))
99993 FORMAT (' The block-diagonal matrix is ')
99992 FORMAT (8X,20(1X,F8.4))
99991 FORMAT (' The transformation matrix is ')
99972 FORMAT (/' N is out of range.',/' N = ',I5)
      END
