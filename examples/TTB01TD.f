*     TB01TD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDD
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, IGH, J, LOW, M, N, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(NMAX), SCIN(MMAX),
     $                 SCOUT(PMAX), SCSTAT(NMAX)
*     .. External Subroutines ..
      EXTERNAL         TB01TD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99990 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
            IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99989 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*              Balance the state-space representation (A,B,C,D).
               CALL TB01TD( N, M, P, A, LDA, B, LDB, C, LDC, D, LDD,
     $                      LOW, IGH, SCSTAT, SCIN, SCOUT, DWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 ) LOW, IGH
                  WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, N
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,N )
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99994 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99993 )
                  DO 60 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,N )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99992 )
                  DO 80 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( D(I,J), J = 1,M )
   80             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB01TD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB01TD = ',I2)
99997 FORMAT (' LOW = ',I2,'   IGH = ',I2,/)
99996 FORMAT (' The balanced state dynamics matrix A is ')
99995 FORMAT (20(1X,F9.4))
99994 FORMAT (/' The balanced input/state matrix B is ')
99993 FORMAT (/' The balanced state/output matrix C is ')
99992 FORMAT (/' The scaled direct transmission matrix D is ')
99991 FORMAT (/' N is out of range.',/' N = ',I5)
99990 FORMAT (/' M is out of range.',/' M = ',I5)
99989 FORMAT (/' P is out of range.',/' P = ',I5)
      END
