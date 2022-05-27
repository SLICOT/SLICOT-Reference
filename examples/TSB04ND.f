*     SB04ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDC
      PARAMETER        ( LDA = NMAX, LDB = MMAX, LDC = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 2*( MAX( NMAX,MMAX ) )*
     $                        ( 4+2*( MAX( NMAX,MMAX ) ) ) )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = 2*MAX( NMAX,MMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, J, M, N
      CHARACTER*1      ABSCHU, ULA, ULB
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,MMAX),
     $                 DWORK(LDWORK)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         SB04ND
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, TOL, ULA, ULB, ABSCHU
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99994 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,M )
            READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,M ), I = 1,N )
*           Find the solution matrix X.
            CALL SB04ND( ABSCHU, ULA, ULB, N, M, A, LDA, B, LDB, C,
     $                   LDC, TOL, IWORK, DWORK, LDWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 )
               DO 20 I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( C(I,J), J = 1,M )
   20          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB04ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB04ND = ',I2)
99997 FORMAT (' The solution matrix X is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
99994 FORMAT (/' M is out of range.',/' M = ',I5)
      END
