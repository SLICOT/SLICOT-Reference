*     TG01CD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          LMAX, NMAX, MMAX
      PARAMETER        ( LMAX = 20, NMAX = 20, MMAX = 20)
      INTEGER          LDA, LDB, LDE, LDQ
      PARAMETER        ( LDA = LMAX, LDB = LMAX,
     $                   LDE = LMAX, LDQ = LMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MIN(LMAX,NMAX)+MAX(LMAX,NMAX,MMAX) )
*     .. Local Scalars ..
      CHARACTER*1      COMPQ
      INTEGER          I, INFO, J, L, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX),
     $                 DWORK(LDWORK), E(LDE,NMAX), Q(LDQ,LMAX)
*     .. External Subroutines ..
      EXTERNAL         TG01CD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) L, N, M
      COMPQ = 'I'
      IF ( L.LT.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) L
      ELSE
         IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) N
         ELSE
            READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,L )
            READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,L )
            IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
               WRITE ( NOUT, FMT = 99990 ) M
            ELSE
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,L )
*              Find the transformed descriptor system pair
*              (A-lambda E,B).
               CALL TG01CD( COMPQ, L, N, M, A, LDA, E, LDE, B, LDB,
     $                      Q, LDQ, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 )
                  DO 10 I = 1, L
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,N )
   10             CONTINUE
                  WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, L
                     WRITE ( NOUT, FMT = 99995 ) ( E(I,J), J = 1,N )
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99994 )
                  DO 30 I = 1, L
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   30             CONTINUE
                  WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, L
                     WRITE ( NOUT, FMT = 99995 ) ( Q(I,J), J = 1,L )
   40             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TG01CD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TG01CD = ',I2)
99997 FORMAT (/' The transformed state dynamics matrix Q''*A is ')
99996 FORMAT (/' The transformed descriptor matrix Q''*E is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' The transformed input/state matrix Q''*B is ')
99993 FORMAT (/' The left transformation matrix Q is ')
99992 FORMAT (/' L is out of range.',/' L = ',I5)
99991 FORMAT (/' N is out of range.',/' N = ',I5)
99990 FORMAT (/' M is out of range.',/' M = ',I5)
      END
