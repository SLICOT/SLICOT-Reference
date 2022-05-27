*     MB04UD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX
      PARAMETER        ( MMAX = 20, NMAX = 20 )
      INTEGER          LDA, LDE, LDQ, LDZ
      PARAMETER        ( LDA = MMAX, LDE = MMAX, LDQ = MMAX,
     $                   LDZ = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX,MMAX ) )
*     PARAMETER        ( LDWORK = NMAX+MMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, J, M, N, RANKE
      CHARACTER*1      JOBQ, JOBZ
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), E(LDE,NMAX),
     $                 Q(LDQ,MMAX), Z(LDZ,NMAX)
      INTEGER          ISTAIR(MMAX)
*     .. External Subroutines ..
      EXTERNAL         MB04UD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, TOL
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) M
      ELSE IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,M )
         READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,M )
         JOBQ = 'N'
         JOBZ = 'N'
*        Reduce E to column echelon form and compute Q'*A*Z.
         CALL MB04UD( JOBQ, JOBZ, M, N, A, LDA, E, LDE, Q, LDQ, Z, LDZ,
     $                RANKE, ISTAIR, TOL, DWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99991 )
            DO 10 I = 1, M
               WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,N )
   10       CONTINUE
            WRITE ( NOUT, FMT = 99997 )
            DO 100 I = 1, M
               WRITE ( NOUT, FMT = 99996 ) ( E(I,J), J = 1,N )
  100       CONTINUE
            WRITE ( NOUT, FMT = 99995 ) RANKE
            WRITE ( NOUT, FMT = 99994 ) ( ISTAIR(I), I = 1,M )
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB04UD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04UD = ',I2)
99997 FORMAT (' The transformed matrix E is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The computed rank of E = ',I2)
99994 FORMAT (/' ISTAIR is ',/20(1X,I5))
99993 FORMAT (/' M is out of range.',/' M = ',I5)
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (' The transformed matrix A is ')
      END
