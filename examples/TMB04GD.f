*     MB04GD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 10, MMAX = 10 )
      INTEGER          LDA
      PARAMETER        ( LDA = MMAX )
      INTEGER          LDTAU
      PARAMETER        ( LDTAU = MIN(MMAX,NMAX) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 3*MMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), TAU(LDTAU)
      INTEGER          JPVT(MMAX)
*     .. External Subroutines ..
      EXTERNAL         DLASET, MB04GD
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99972 ) N
      ELSE
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99971 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,M )
            READ ( NIN, FMT = * ) ( JPVT(I), I = 1,M )
*           RQ with row pivoting.
            CALL MB04GD( M, N, A, LDA, JPVT, TAU, DWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99994 ) ( JPVT(I), I = 1,M )
               WRITE ( NOUT, FMT = 99990 )
               IF ( M.GE.N ) THEN
                  IF ( N.GT.1 )
     $               CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO,
     $                            A(M-N+2,1), LDA )
                ELSE
                   CALL DLASET( 'Full', M, N-M-1, ZERO, ZERO, A, LDA )
                   CALL DLASET( 'Lower', M, M, ZERO, ZERO, A(1,N-M),
     $                          LDA )
               END IF
               DO 20 I = 1, M
                  WRITE ( NOUT, FMT = 99989 ) ( A(I,J), J = 1,N )
   20          CONTINUE
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MB04GD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB04GD = ',I2)
99994 FORMAT (' Row permutations are ',/(20(I3,2X)))
99990 FORMAT (/' The matrix A is ')
99989 FORMAT (20(1X,F8.4))
99972 FORMAT (/' N is out of range.',/' N = ',I5)
99971 FORMAT (/' M is out of range.',/' M = ',I5)
      END
