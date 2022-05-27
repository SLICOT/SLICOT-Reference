*     MB04VD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX
      PARAMETER        ( MMAX = 20, NMAX = 20 )
      INTEGER          LDA, LDE, LDQ, LDZ
      PARAMETER        ( LDA  = MMAX, LDE = MMAX, LDQ = MMAX,
     $                   LDZ = NMAX )
      INTEGER          LINUK
      PARAMETER        ( LINUK = MAX( NMAX,MMAX+1 ) )
*     PARAMETER        ( LINUK = NMAX+MMAX+1 )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX,MMAX ) )
*     PARAMETER        ( LDWORK = NMAX+MMAX )
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, J, M, N, NBLCKI, NBLCKS, RANKE
      CHARACTER*1      JOBQ, JOBZ, MODE
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), E(LDE,NMAX),
     $                 Q(LDQ,MMAX), Z(LDZ,NMAX)
      INTEGER          IMUK(LINUK), IMUK0(NMAX), INUK(LINUK),
     $                 ISTAIR(MMAX), IWORK(LIWORK), MNEI(3)
C     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB04UD, MB04VD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, TOL, MODE
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99984 ) M
      ELSE IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99983 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,M )
         READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,M )
         JOBQ = 'I'
         JOBZ = 'I'
*        Reduce E to column echelon form and compute Q'*A*Z.
         CALL MB04UD( JOBQ, JOBZ, M, N, A, LDA, E, LDE, Q, LDQ, Z, LDZ,
     $                RANKE, ISTAIR, TOL, DWORK, INFO )
         JOBQ = 'U'
         JOBZ = 'U'
*
         IF ( INFO.EQ.0 ) THEN
*           Compute a unitary transformed pencil Q'*(s*E-A)*Z.
            CALL MB04VD( MODE, JOBQ, JOBZ, M, N, RANKE, A, LDA, E, LDE,
     $                   Q, LDQ, Z, LDZ, ISTAIR, NBLCKS, NBLCKI, IMUK,
     $                   INUK, IMUK0, MNEI, TOL, IWORK, INFO )
*
            IF ( INFO.EQ.0 ) THEN
               WRITE ( NOUT, FMT = 99996 )
               WRITE ( NOUT, FMT = 99995 )
               DO 140 I = 1, M
                  WRITE ( NOUT, FMT = 99994 ) ( Q(I,J), J = 1,M )
  140          CONTINUE
               WRITE ( NOUT, FMT = 99993 )
               DO 160 I = 1, M
                  WRITE ( NOUT, FMT = 99994 ) ( E(I,J), J = 1,N )
  160          CONTINUE
               WRITE ( NOUT, FMT = 99992 )
               DO 180 I = 1, M
                  WRITE ( NOUT, FMT = 99994 ) ( A(I,J), J = 1,N )
  180          CONTINUE
               WRITE ( NOUT, FMT = 99991 )
               DO 200 I = 1, N
                  WRITE ( NOUT, FMT = 99994 ) ( Z(I,J), J = 1,N )
  200          CONTINUE
               WRITE ( NOUT, FMT = 99990 ) NBLCKS
               IF ( .NOT. LSAME( MODE, 'S' ) ) THEN
                  WRITE ( NOUT, FMT = 99989 ) ( IMUK(I),  I = 1,NBLCKS )
                  WRITE ( NOUT, FMT = 99988 ) ( INUK(I),  I = 1,NBLCKS )
               ELSE
                  WRITE ( NOUT, FMT = 99987 ) ( IMUK(I),  I = 1,NBLCKS )
                  WRITE ( NOUT, FMT = 99986 ) ( INUK(I),  I = 1,NBLCKS )
                  WRITE ( NOUT, FMT = 99982 ) ( IMUK0(I), I = 1,NBLCKI )
                  WRITE ( NOUT, FMT = 99985 ) ( MNEI(I),  I = 1,3 )
               END IF
            ELSE
               WRITE ( NOUT, FMT = 99998 ) INFO
            END IF
         ELSE
            WRITE ( NOUT, FMT = 99997 ) INFO
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB04VD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04VD = ',I2)
99997 FORMAT (' INFO on exit from MB04UD = ',I2)
99996 FORMAT (' The unitary transformed pencil is Q''*(s*E-A)*Z, where',
     $          /)
99995 FORMAT (' Matrix Q',/)
99994 FORMAT (20(1X,F8.4))
99993 FORMAT (/' Matrix E',/)
99992 FORMAT (/' Matrix A',/)
99991 FORMAT (/' Matrix Z',/)
99990 FORMAT (/' The number of submatrices having full row rank detect',
     $       'ed in matrix A = ',I3)
99989 FORMAT (/' The column dimensions of the submatrices having full ',
     $       'column rank in the pencil',/' sE(eps,inf) - A(eps,inf) a',
     $       're',/20(1X,I5))
99988 FORMAT (/' The row dimensions of the submatrices having full row',
     $       ' rank in the pencil',/' sE(eps,inf) - A(eps,inf) are',
     $       /20(1X,I5))
99987 FORMAT (/' The column dimensions of the submatrices having full ',
     $       'column rank in the pencil',/' sE(eps) - A(eps) are',
     $       /20(1X,I5))
99986 FORMAT (/' The row dimensions of the submatrices having full row',
     $       ' rank in the pencil',/' sE(eps) - A(eps) are',/20(1X,I5))
99985 FORMAT (/' MNEI is ',/20(1X,I5))
99984 FORMAT (/' M is out of range.',/' M = ',I5)
99983 FORMAT (/' N is out of range.',/' N = ',I5)
99982 FORMAT (/' The orders of the diagonal submatrices in the pencil ',
     $       'sE(inf) - A(inf) are',/20(1X,I5))
      END
