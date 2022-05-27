*     TB01PD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          MAXMP
      PARAMETER        ( MAXMP = MAX( MMAX, PMAX ) )
      INTEGER          LDA, LDB, LDC
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = MAXMP )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX+MAXMP )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX+MAX( NMAX, 3*MAXMP ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, J, M, N, NR, P
      CHARACTER        JOB, EQUIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MAXMP), C(LDC,NMAX),
     $                 DWORK(LDWORK)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         TB01PD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL, JOB, EQUIL
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99988 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
*              Find a minimal ssr for (A,B,C).
               CALL TB01PD( JOB, EQUIL, N, M, P, A, LDA, B, LDB, C, LDC,
     $                      NR, TOL, IWORK, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 ) NR
                  WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,NR )
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99992 )
                  DO 60 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,NR )
   60             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB01PD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB01PD = ',I2)
99997 FORMAT (' The order of the minimal realization = ',I2)
99996 FORMAT (/' The transformed state dynamics matrix of a minimal re',
     $       'alization is ')
99995 FORMAT (20(1X,F8.4))
99993 FORMAT (/' The transformed input/state matrix of a minimal reali',
     $       'zation is ')
99992 FORMAT (/' The transformed state/output matrix of a minimal real',
     $       'ization is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
      END
