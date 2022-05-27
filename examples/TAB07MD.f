*     AB07MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          MAXMP
      PARAMETER        ( MAXMP = MAX( MMAX, PMAX ) )
      INTEGER          LDA, LDB, LDC, LDD
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = MAXMP,
     $                   LDD = MAXMP )
*     .. Local Scalars ..
      CHARACTER*1      JOBD
      INTEGER          I, INFO, J, M, N, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MAXMP), C(LDC,NMAX),
     $                 D(LDD,MAXMP)
*     .. External functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         AB07MD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, JOBD
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99990 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*              Find the dual of the ssr (A,B,C,D).
               CALL AB07MD( JOBD, N, M, P, A, LDA, B, LDB, C, LDC, D,
     $                      LDD, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 )
                  DO 20 I = 1, N
                     WRITE ( NOUT, FMT = 99996 ) ( A(I,J), J = 1,N )
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99995 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,P )
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99994 )
                  DO 60 I = 1, M
                     WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,N )
   60             CONTINUE
                  IF ( LSAME( JOBD, 'D' ) ) THEN
                     WRITE ( NOUT, FMT = 99993 )
                     DO 80 I = 1, M
                        WRITE ( NOUT, FMT = 99996 ) ( D(I,J), J = 1,P )
   80                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB07MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB07MD = ',I2)
99997 FORMAT (' The dual state dynamics matrix is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The dual input/state matrix is ')
99994 FORMAT (/' The dual state/output matrix is ')
99993 FORMAT (/' The dual direct transmission matrix is ')
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (/' M is out of range.',/' M = ',I5)
99990 FORMAT (/' P is out of range.',/' P = ',I5)
      END
