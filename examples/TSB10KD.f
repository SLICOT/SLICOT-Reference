*     SB10KD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 10, MMAX = 10, PMAX = 10 )
      INTEGER          LDA, LDAK, LDB, LDBK, LDC, LDCK, LDDK
      PARAMETER        ( LDA  = NMAX, LDAK = NMAX, LDB  = NMAX,
     $                   LDBK = NMAX, LDC  = PMAX, LDCK = MMAX,
     $                   LDDK = MMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = 2*MAX( NMAX, MMAX + PMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 15*NMAX*NMAX + 6*NMAX +
     $                            MAX( 14*NMAX + 23, 16*NMAX,
     $                                 2*NMAX+PMAX+MMAX,
     $                                 3*(PMAX+MMAX) ) +
     $                            MAX( NMAX*NMAX,
     $                                 11*NMAX*PMAX + 2*MMAX*MMAX +
     $                                 8*PMAX*PMAX + 8*MMAX*NMAX +
     $                                 4*MMAX*PMAX + PMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION FACTOR
      INTEGER          I, INFO, J, M, N, NP
*     .. Local Arrays ..
      LOGICAL          BWORK(2*NMAX)
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), AK(LDA,NMAX), B(LDB,MMAX),
     $                 BK(LDBK,PMAX), C(LDC,NMAX), CK(LDCK,NMAX),
     $                 DK(LDDK,PMAX), DWORK(LDWORK), RCOND(4)
*     .. External Subroutines ..
      EXTERNAL         SB10KD
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
         READ ( NIN, FMT = * ) FACTOR
         CALL SB10KD( N, M, NP, A, LDA, B, LDB, C, LDC, FACTOR, AK,
     $                LDAK, BK, LDBK, CK, LDCK, DK, LDDK, RCOND,
     $                IWORK, DWORK, LDWORK, BWORK, INFO )
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
            WRITE( NOUT, FMT = 99991 ) ( RCOND(I), I = 1, 4 )
         ELSE
            WRITE( NOUT, FMT = 99998 ) INFO
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB10KD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (/' INFO on exit from SB10KD =',I2)
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
