*     TG01DD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          LMAX, NMAX, PMAX
      PARAMETER        ( LMAX = 20, NMAX = 20, PMAX = 20)
      INTEGER          LDA, LDC, LDE, LDZ
      PARAMETER        ( LDA = LMAX, LDC = PMAX,
     $                   LDE = LMAX, LDZ = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MIN(LMAX,NMAX)+MAX(LMAX,NMAX,PMAX) )
*     .. Local Scalars ..
      CHARACTER*1      COMPZ
      INTEGER          I, INFO, J, L, N, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), C(LDC,NMAX),
     $                 DWORK(LDWORK), E(LDE,NMAX), Z(LDZ,NMAX)
*     .. External Subroutines ..
      EXTERNAL         TG01DD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) L, N, P
      COMPZ = 'I'
      IF ( L.LT.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) L
      ELSE
         IF( N.LT.0 .OR. N.GT.NMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) N
         ELSE
            READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,L )
            READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,L )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99990 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
*              Find the transformed descriptor system pair
*              (A-lambda E,B).
               CALL TG01DD( COMPZ, L, N, P, A, LDA, E, LDE, C, LDC,
     $                      Z, LDZ, DWORK, LDWORK, INFO )
*
               IF( INFO.NE.0 ) THEN
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
                  DO 30 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,N )
   30             CONTINUE
                  WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99995 ) ( Z(I,J), J = 1,N )
   40             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TG01DD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TG01DD = ',I2)
99997 FORMAT (/' The transformed state dynamics matrix A*Z is ')
99996 FORMAT (/' The transformed descriptor matrix E*Z is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' The transformed input/state matrix C*Z is ')
99993 FORMAT (/' The right transformation matrix Z is ')
99992 FORMAT (/' L is out of range.',/' L = ',I5)
99991 FORMAT (/' N is out of range.',/' N = ',I5)
99990 FORMAT (/' P is out of range.',/' P = ',I5)
      END
