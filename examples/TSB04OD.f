*     SB04OD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ONE
      PARAMETER        ( ONE = 1.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX
      PARAMETER        ( MMAX = 10, NMAX = 10 )
      INTEGER          LDA, LDB, LDC, LDD, LDE, LDF, LDP, LDQ, LDU, LDV
      PARAMETER        ( LDA = MMAX, LDB = NMAX, LDC = MMAX, LDD = MMAX,
     $                   LDE = NMAX, LDF = MMAX, LDP = MMAX, LDQ = MMAX,
     $                   LDU = NMAX, LDV = NMAX )
      INTEGER          LDWORK, LIWORK
      PARAMETER        ( LDWORK = MAX(11*MAX(MMAX,NMAX),
     $                                10*MAX(MMAX,NMAX)+23,2*MMAX*NMAX),
     $                   LIWORK = MMAX+NMAX+6 )
*     .. Local Scalars ..
      DOUBLE PRECISION DIF, SCALE
      INTEGER          I, INFO, J, M, N
      CHARACTER*1      JOBD, REDUCE, TRANS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,MMAX), B(LDB,NMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK), E(LDE,NMAX),
     $                 F(LDF,NMAX), P(LDP,MMAX), Q(LDQ,MMAX),
     $                 U(LDU,NMAX), V(LDV,NMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SB04OD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) M, N, REDUCE, TRANS, JOBD
      IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) M
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,M ), I = 1,M )
         IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
            WRITE ( NOUT, FMT = 99988 ) N
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,N ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,M )
            READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,M )
            READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( F(I,J), J = 1,N ), I = 1,M )
*           Find the solution matrices L and R.
            CALL SB04OD( REDUCE, TRANS, JOBD, M, N, A, LDA, B, LDB, C,
     $                   LDC, D, LDD, E, LDE, F, LDF, SCALE, DIF, P,
     $                   LDP, Q, LDQ, U, LDU, V, LDV, IWORK, DWORK,
     $                   LDWORK, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 )
               DO 20 I = 1, M
                  WRITE ( NOUT, FMT = 99991 ) ( F(I,J), J = 1,N )
   20          CONTINUE
               WRITE ( NOUT, FMT = 99996 )
               DO 40 I = 1, M
                  WRITE ( NOUT, FMT = 99991 ) ( C(I,J), J = 1,N )
   40          CONTINUE
               IF ( LSAME( REDUCE, 'R' ).OR.LSAME( REDUCE, 'A' ) ) THEN
                  WRITE ( NOUT, FMT = 99995 )
                  DO 60 I = 1, M
                     WRITE ( NOUT, FMT = 99991 ) ( P(I,J), J = 1,M )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99994 )
                  DO 80 I = 1, M
                     WRITE ( NOUT, FMT = 99991 ) ( Q(I,J), J = 1,M )
   80             CONTINUE
               END IF
               IF ( LSAME( REDUCE, 'R' ).OR.LSAME( REDUCE, 'B' ) ) THEN
                  WRITE ( NOUT, FMT = 99993 )
                  DO 100 I = 1, N
                     WRITE ( NOUT, FMT = 99991 ) ( U(I,J), J = 1,N )
  100             CONTINUE
                  WRITE ( NOUT, FMT = 99992 )
                  DO 120 I = 1, N
                     WRITE ( NOUT, FMT = 99991 ) ( V(I,J), J = 1,N )
  120             CONTINUE
               END IF
               IF ( SCALE.NE.ONE ) WRITE ( NOUT, FMT = 99987 ) SCALE
               IF ( .NOT.LSAME( JOBD, 'N' ) )
     $            WRITE ( NOUT, FMT = 99990 ) DIF
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' SB04OD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB04OD = ',I2)
99997 FORMAT (' The solution matrix L is ')
99996 FORMAT (/' The solution matrix R is ')
99995 FORMAT (/' The left transformation matrix P is ')
99994 FORMAT (/' The right transformation matrix Q is ')
99993 FORMAT (/' The left transformation matrix U is ')
99992 FORMAT (/' The right transformation matrix V is ')
99991 FORMAT (20(1X,F8.4))
99990 FORMAT (/' DIF = ',F8.4)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' N is out of range.',/' N = ',I5)
99987 FORMAT (/' SCALE = ',F8.4)
      END
