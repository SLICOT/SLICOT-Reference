*     AB13AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX*( MAX( NMAX, MMAX, PMAX ) + 5 )
     $                        + ( NMAX*( NMAX + 1 ) )/2 )
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, SHNORM
      INTEGER          I, INFO, J, M, N, NS, P
      CHARACTER*1      DICO, EQUIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 DWORK(LDWORK), HSV(NMAX)
*     .. External Functions ..
      DOUBLE PRECISION AB13AD
      EXTERNAL         AB13AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, ALPHA, DICO, EQUIL
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1, N )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99988 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
*              Compute the Hankel-norm of the ALPHA-stable projection of
*              (A,B,C).
               SHNORM = AB13AD( DICO, EQUIL, N, M, P, ALPHA, A, LDA, B,
     $                          LDB, C, LDC, NS, HSV, DWORK, LDWORK,
     $                          INFO)
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 ) SHNORM
                  WRITE ( NOUT, FMT = 99987 )
                  WRITE ( NOUT, FMT = 99995 ) ( HSV(J), J = 1,NS )
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB13AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB13AD = ',I2)
99997 FORMAT (' The Hankel-norm of the ALPHA-projection = ',1PD14.5)
99995 FORMAT (20(1X,F8.4))
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
99987 FORMAT (/' The Hankel singular values of ALPHA-projection are')
      END
