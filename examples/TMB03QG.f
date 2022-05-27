*     MB03QG EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 10 )
      INTEGER          LDA, LDE, LDU, LDV
      PARAMETER        ( LDA = NMAX, LDE = NMAX, LDU = NMAX, LDV = NMAX)
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 8*NMAX + 16 )
*     .. Local Scalars ..
      CHARACTER*1      DICO, JOBU, JOBV, STDOM
      INTEGER          I, INFO, J, N, NDIM, NLOW, NSUP
      DOUBLE PRECISION ALPHA
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), BETA(NMAX), DWORK(LDWORK),
     $                 E(LDE,NMAX), U(LDU,NMAX), V(LDV,NMAX), WI(NMAX),
     $                 WR(NMAX)
      LOGICAL          BWORK(NMAX)
*     .. External Functions ..
      LOGICAL          DELCTG
*     .. External Subroutines ..
      EXTERNAL         DGGES, MB03QG
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, NLOW, NSUP, ALPHA, DICO, STDOM, JOBU,
     $                      JOBV
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,N )
*        Compute Schur form, eigenvalues and Schur vectors.
         CALL DGGES( 'Vectors', 'Vectors', 'Not sorted', DELCTG, N,
     $               A, LDA, E, LDE, NDIM, WR, WI, BETA, U, LDU, V, LDV,
     $               DWORK, LDWORK, BWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
*           Block reordering.
            CALL MB03QG( DICO, STDOM, JOBU, JOBV, N, NLOW, NSUP, ALPHA,
     $                   A, LDA, E, LDE, U, LDU, V, LDV, NDIM, DWORK,
     $                   LDWORK, INFO )
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
                  WRITE ( NOUT, FMT = 99995 ) ( E(I,J), J = 1,N )
   20          CONTINUE
               WRITE ( NOUT, FMT = 99992 )
               DO 30 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( U(I,J), J = 1,N )
   30          CONTINUE
               WRITE ( NOUT, FMT = 99991 )
               DO 40 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( V(I,J), J = 1,N )
   40          CONTINUE
           END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MB03QG EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from DGEES  = ',I2)
99997 FORMAT (' INFO on exit from MB03QG = ',I2)
99996 FORMAT (' The number of eigenvalues in the domain is ',I5)
99995 FORMAT (8X,20(1X,F8.4))
99994 FORMAT (/' The ordered Schur form matrix is ')
99993 FORMAT (/' The ordered triangular matrix is ')
99992 FORMAT (/' The transformation matrix U is ')
99991 FORMAT (/' The transformation matrix V is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
      END
