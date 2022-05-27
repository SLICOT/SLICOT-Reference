*     SB10ZD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX, PMAX
      PARAMETER        ( MMAX = 10, NMAX = 10, PMAX = 10 )
      INTEGER          LDA, LDAK, LDB, LDBK, LDC, LDCK, LDD, LDDK
      PARAMETER        ( LDA  = NMAX, LDAK = NMAX, LDB  = NMAX,
     $                   LDBK = NMAX, LDC  = PMAX, LDCK = MMAX,
     $                   LDD  = PMAX, LDDK = MMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = 2*MAX( NMAX, MMAX + PMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 16*NMAX*NMAX + 5*MMAX*MMAX +
     $                            7*PMAX*PMAX + 6*MMAX*NMAX +
     $                            7*MMAX*PMAX + 7*NMAX*PMAX + 6*NMAX +
     $                            2*( MMAX + PMAX ) +
     $                            MAX( 14*NMAX + 23, 16*NMAX,
     $                                  2*MMAX - 1, 2*PMAX - 1 ) )
*     .. Local Scalars ..
      DOUBLE PRECISION FACTOR, TOL
      INTEGER          I, INFO, J, M, N, NP
*     .. Local Arrays ..
      LOGICAL          BWORK(2*NMAX)
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX),   AK(LDAK,NMAX), B(LDB,MMAX),
     $                 BK(LDBK,PMAX), C(LDC,NMAX),   CK(LDCK,NMAX),
     $                 D(LDD,MMAX),   DK(LDDK,PMAX), DWORK(LDWORK),
     $                 RCOND( 6 )
*     .. External Subroutines ..
      EXTERNAL         SB10ZD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, NP
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99989 ) M
      ELSE IF ( NP.LT.0 .OR. NP.GT.PMAX ) THEN
         WRITE ( NOUT, FMT = 99988 ) NP
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,NP )
         READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,NP )
         READ ( NIN, FMT = * ) FACTOR, TOL
         CALL SB10ZD( N, M, NP, A, LDA, B, LDB, C, LDC, D, LDD, FACTOR,
     $                AK, LDAK, BK, LDBK, CK, LDCK, DK, LDDK, RCOND,
     $                TOL, IWORK, DWORK, LDWORK, BWORK, INFO )
         IF ( INFO.EQ.0 ) THEN
            WRITE ( NOUT, FMT = 99997 )
            DO 10 I = 1, N
               WRITE ( NOUT, FMT = 99992 ) ( AK(I,J), J = 1,N )
   10       CONTINUE
            WRITE ( NOUT, FMT = 99996 )
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99992 ) ( BK(I,J), J = 1,NP )
   20       CONTINUE
            WRITE ( NOUT, FMT = 99995 )
            DO 30 I = 1, M
               WRITE ( NOUT, FMT = 99992 ) ( CK(I,J), J = 1,N )
   30       CONTINUE
            WRITE ( NOUT, FMT = 99994 )
            DO 40 I = 1, M
               WRITE ( NOUT, FMT = 99992 ) ( DK(I,J), J = 1,NP )
   40       CONTINUE
            WRITE( NOUT, FMT = 99993 )
            WRITE( NOUT, FMT = 99991 ) ( RCOND(I), I = 1,6 )
         ELSE
            WRITE( NOUT, FMT = 99998 ) INFO
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB10ZD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (/' INFO on exit from SB10ZD =',I2)
99997 FORMAT (/' The controller state matrix AK is'/)
99996 FORMAT (/' The controller input matrix BK is'/)
99995 FORMAT (/' The controller output matrix CK is'/)
99994 FORMAT (/' The controller matrix DK is'/)
99993 FORMAT (/' The estimated condition numbers are'/)
99992 FORMAT (10(1X,F8.4))
99991 FORMAT ( 5(1X,D12.5))
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' NP is out of range.',/' NP = ',I5)
      END
