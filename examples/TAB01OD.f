*     AB01OD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDU, LDV
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDU = NMAX,
     $                   LDV = MMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX + MAX( NMAX, 3*MMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INDCON, INFO, J, M, N, NCONT
      CHARACTER*1      JOBU, JOBV, STAGES
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), DWORK(LDWORK),
     $                 U(LDU,NMAX), V(LDV,MMAX)
      INTEGER          IWORK(LIWORK), KSTAIR(NMAX)
*     .. External Subroutines ..
      EXTERNAL         AB01OD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, TOL, STAGES, JOBU, JOBV
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), I = 1,N ), J = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
*           Reduce the matrices A and B to upper "staircase" form.
            CALL AB01OD( STAGES, JOBU, JOBV, N, M, A, LDA, B, LDB, U,
     $                   LDU, V, LDV, NCONT, INDCON, KSTAIR, TOL, IWORK,
     $                   DWORK, LDWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 )
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,N )
   20          CONTINUE
               WRITE ( NOUT, FMT = 99996 )
               DO 40 I = 1, N
                  WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   40          CONTINUE
               WRITE ( NOUT, FMT = 99994 ) INDCON
               WRITE ( NOUT, FMT = 99993 ) ( KSTAIR(I), I = 1,INDCON )
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB01OD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB01OD = ',I2)
99997 FORMAT (' The transformed state transition matrix is ')
99996 FORMAT (/' The transformed input matrix is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' The number of stairs in the staircase form = ',I3,/)
99993 FORMAT (' The dimensions of the stairs are ',/(20(I3,2X)))
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (/' M is out of range.',/' M = ',I5)
      END
