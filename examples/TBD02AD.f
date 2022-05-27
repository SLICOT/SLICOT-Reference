C     BD02AD EXAMPLE PROGRAM TEXT
C
C     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN = 5, NOUT = 6)
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        (NMAX = 421, MMAX = 211, PMAX = 211)
      INTEGER          LDA, LDB, LDC, LDD, LDE, LDWORK
      PARAMETER        (LDA = NMAX, LDB = NMAX, LDC = PMAX, LDD = PMAX,
     1                  LDE = NMAX, LDWORK = 1)
C     .. Local Scalars ..
      CHARACTER        DEF
      INTEGER          I, INFO, J, LDPAR, LIPAR, M, N, P
      CHARACTER*70     NOTE
C     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     1                 D(LDD,MMAX), DPAR(7), DWORK(LDWORK), E(LDE,NMAX)
      INTEGER          NR(2), IPAR(7)
      LOGICAL          VEC(8)
C     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
C     .. External Subroutines ..
      EXTERNAL         BD02AD
C     .. Executable Statements ..
      WRITE (NOUT, FMT = 99999)
C     Skip the heading in the data file and read the data.
      READ (NIN, FMT = '()')
      READ (NIN, FMT = *) DEF
      READ (NIN, FMT = *) (NR(I), I = 1, 2)
      IF (LSAME(DEF,'N')) THEN
        READ (NIN, FMT = *) LDPAR
        IF (LDPAR .GT. 0)  READ (NIN, FMT = *) (DPAR(I), I = 1, LDPAR)
        READ (NIN, FMT = *) LIPAR
        IF (LIPAR .GT. 0)  READ (NIN, FMT = *) (IPAR(I), I = 1, LIPAR)
      END IF
C     Generate benchmark example
      CALL BD02AD(DEF, NR, DPAR, IPAR, VEC, N, M, P, E, LDE, A, LDA,
     1            B, LDB, C, LDC, D, LDD, NOTE, DWORK, LDWORK, INFO)
C
      IF (INFO .NE. 0) THEN
        WRITE (NOUT, FMT = 99998) INFO
      ELSE
        WRITE (NOUT, FMT = *) NOTE
        WRITE (NOUT, FMT = 99997) N
        WRITE (NOUT, FMT = 99996) M
        WRITE (NOUT, FMT = 99995) P
        IF (VEC(4)) THEN
          WRITE (NOUT, FMT = 99994)
          DO 10  I = 1, N
            WRITE (NOUT, FMT = 99987) (E(I,J), J = 1, N)
10        CONTINUE
        ELSE
          WRITE (NOUT, FMT = 99993)
        END IF
        WRITE (NOUT,FMT = 99992)
        DO 20  I = 1, N
          WRITE (NOUT, FMT = 99987) (A(I,J), J = 1, N)
20      CONTINUE
        WRITE (NOUT,FMT = 99991)
        DO 30  I = 1, N
          WRITE (NOUT, FMT = 99987) (B(I,J), J = 1, M)
30      CONTINUE
        WRITE (NOUT,FMT = 99990)
        DO 40  I = 1, P
          WRITE (NOUT, FMT = 99987) (C(I,J), J = 1, N)
40      CONTINUE
        IF (VEC(8)) THEN
          WRITE (NOUT,FMT = 99989)
          DO 50  I = 1, P
            WRITE (NOUT, FMT = 99987) (D(I,J), J = 1, M)
50        CONTINUE
        ELSE
          WRITE (NOUT, FMT = 99988)
        END IF
      END IF
C
99999 FORMAT (' BD02AD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' INFO on exit from BD02AD = ', I3)
99997 FORMAT (/' Order of matrix A:               N  = ', I3)
99996 FORMAT (' Number of columns in matrix B:   M  = ', I3)
99995 FORMAT (' Number of rows in matrix C:      P  = ', I3)
99994 FORMAT (/' E  = ')
99993 FORMAT (/' E is the identity matrix.')
99992 FORMAT (' A  = ')
99991 FORMAT (' B  = ')
99990 FORMAT (' C  = ')
99989 FORMAT (' D  = ')
99988 FORMAT (' D is of zeros.')
99987 FORMAT (20(1X,F8.4))
C
      END
