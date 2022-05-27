*     AB13BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDD
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( MMAX*( NMAX + MMAX ) +
     $                                 MAX( NMAX*( NMAX + 5 ),
     $                                      MMAX*( MMAX + 2 ), 4*PMAX ),
     $                                 NMAX*( MAX( NMAX, PMAX ) + 4 ) +
     $                                 MIN( NMAX, PMAX ) ) )
*     .. Local Scalars ..
      DOUBLE PRECISION S2NORM, TOL
      INTEGER          I, INFO, IWARN, J, M, N, NQ, P
      CHARACTER*1      DICO, JOBN
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      DOUBLE PRECISION AB13BD
      EXTERNAL         AB13BD, LSAME
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL, DICO, JOBN
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1, N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1, M ), I = 1, N )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99988 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1, N ), I = 1, P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1, M ), I = 1, P )
*              Compute the H2 or L2 norm of (A,B,C,D).
               S2NORM = AB13BD( DICO, JOBN, N, M, P, A, LDA, B, LDB,
     *                          C, LDC, D, LDD, NQ, TOL, DWORK, LDWORK,
     *                          IWARN, INFO)
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF( LSAME( JOBN, 'H' ) ) THEN
                     WRITE ( NOUT, FMT = 99997 ) S2NORM
                  ELSE
                     WRITE ( NOUT, FMT = 99996 ) S2NORM
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB13BD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB13BD = ',I2)
99997 FORMAT (' The H2-norm of the system = ',1PD14.5)
99996 FORMAT (' The L2-norm of the system = ',1PD14.5)
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
      END
