C     BB03AD EXAMPLE PROGRAM TEXT
C
C     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN = 5, NOUT = 6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX = 100, MMAX = 100)
      INTEGER          LDE, LDA, LDY, LDB, LDX, LDU, LDWORK
      PARAMETER        (LDE = NMAX, LDA = NMAX, LDY = NMAX, LDB = MMAX,
     1                  LDX = NMAX, LDU = NMAX, LDWORK = 2*NMAX)
C     .. Local Scalars ..
      CHARACTER        DEF
      INTEGER          INFO, N, M, I, J, LDPAR, LIPAR
      CHARACTER*70     NOTE
C     .. Local Arrays ..
      DOUBLE PRECISION E(LDE,NMAX), A(LDA, NMAX), Y(LDY, NMAX),
     1                 B(LDB,NMAX), X(LDX, NMAX), U(LDU, NMAX),
     2                 DPAR(2), DWORK(LDWORK)
      INTEGER          NR(2), IPAR(1)
      LOGICAL          VEC(8)
C     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
C     .. External Subroutines ..
      EXTERNAL         BB03AD
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
      CALL BB03AD(DEF, NR, DPAR, IPAR, VEC, N, M, E, LDE, A, LDA, Y,
     1            LDY, B, LDB, X, LDX, U, LDU, NOTE, DWORK, LDWORK,
     2            INFO)
C
      IF (INFO .NE. 0) THEN
        WRITE (NOUT, FMT = 99998) INFO
      ELSE
        WRITE (NOUT, FMT = *) NOTE
        WRITE (NOUT, FMT = 99997) N
        WRITE (NOUT, FMT = 99996) M
        IF (VEC(3)) THEN
          WRITE (NOUT, FMT = 99995)
          DO 10  I = 1, N
            WRITE (NOUT, FMT = 99985) (E(I,J), J = 1, N)
10        CONTINUE
        ELSE
          WRITE (NOUT, FMT = 99994)
        END IF
        WRITE (NOUT,FMT = 99993)
        DO 20  I = 1, N
          WRITE (NOUT, FMT = 99985) (A(I,J), J = 1, N)
20      CONTINUE
        IF (VEC(6)) THEN
          WRITE (NOUT,FMT = 99992)
          DO 30  I = 1, M
            WRITE (NOUT, FMT = 99985) (B(I,J), J = 1, N)
30        CONTINUE
        ELSE
          WRITE (NOUT, FMT = 99991)
        END IF
        WRITE (NOUT,FMT = 99990)
        DO 40  I = 1, N
          WRITE (NOUT, FMT = 99985) (Y(I,J), J = 1, N)
40      CONTINUE
        IF (VEC(7)) THEN
          WRITE (NOUT, FMT = 99989)
          DO 50  I = 1, N
            WRITE (NOUT, FMT = 99985) (X(I,J), J = 1, N)
50        CONTINUE
        ELSE
          WRITE (NOUT, FMT = 99988)
        END IF
        IF (VEC(8)) THEN
          WRITE (NOUT, FMT = 99987)
          DO 60  I = 1, N
            WRITE (NOUT, FMT = 99985) (U(I,J), J = 1, N)
60        CONTINUE
        ELSE
          WRITE (NOUT, FMT = 99986)
        END IF
      END IF
C
99999 FORMAT (' BB03AD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' INFO on exit from BB03AD = ', I3)
99997 FORMAT (/' Order of matrix A:            N  = ', I3)
99996 FORMAT (' Number of rows in matrix B:   M  = ', I3)
99995 FORMAT (/' E  = ')
99994 FORMAT (/' E is the identity matrix.')
99993 FORMAT (' A  = ')
99992 FORMAT (' B  = ')
99991 FORMAT (' B is not provided.')
99990 FORMAT (' Y  = ')
99989 FORMAT (' X  = ')
99988 FORMAT (' X is not provided.')
99987 FORMAT (' U  = ')
99986 FORMAT (' U is not provided.')
99985 FORMAT (20(1X,F8.4))
C
      END
