*     MC03ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          DPMAX, MPMAX, NPMAX
*     PARAMETER        ( DPMAX = 5, MPMAX = 5, NPMAX = 5 )
      PARAMETER        ( DPMAX = 2, MPMAX = 5, NPMAX = 4 )
      INTEGER          LDP1, LDP2, LDNULL, LDKER1, LDKER2
      PARAMETER        ( LDP1 = MPMAX, LDP2 = NPMAX, LDNULL = NPMAX,
     $                   LDKER1 = NPMAX, LDKER2 = NPMAX )
      INTEGER          M, N
      PARAMETER        ( M = DPMAX*MPMAX, N = ( DPMAX-1 )*MPMAX+NPMAX )
      INTEGER          LIWORK, LDWORK
*     PARAMETER        ( LIWORK = 3*( N+M )+2,
      PARAMETER        ( LIWORK = M+2*MAX( N,M+1 )+N,
     $                   LDWORK = M*N**2+2*M*N+2*N**2 )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          DK, DP, I, INFO, J, K, M1, MP, NK, NP
*     .. Local Arrays ..
      DOUBLE PRECISION DWORK(LDWORK), KER(LDKER1,LDKER2,M+1),
     $                 NULLSP(LDNULL,(M+1)*NPMAX), P(LDP1,LDP2,DPMAX+1)
      INTEGER          GAM(M+1), IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         MC03ND
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) MP, NP, DP, TOL
      IF ( MP.LT.0 .OR. MP.GT.MPMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) MP
      ELSE IF ( NP.LT.0 .OR. NP.GT.NPMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) NP
      ELSE IF ( DP.LE.0 .OR. DP.GT.DPMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) DP
      ELSE
         DO 40 K = 1, DP + 1
            DO 20 I = 1, MP
               READ ( NIN, FMT = * ) ( P(I,J,K), J = 1,NP )
   20       CONTINUE
   40    CONTINUE
*        Compute a minimal polynomial basis K(s) of the given P(s).
         CALL MC03ND( MP, NP, DP, P, LDP1, LDP2, DK, GAM, NULLSP,
     $                LDNULL, KER, LDKER1, LDKER2, TOL, IWORK, DWORK,
     $                LDWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE IF ( DK.LT.0 ) THEN
            WRITE ( NOUT, FMT = 99997 )
         ELSE
            NK = 0
            M1 = 0
            DO 60 I = 1, DK + 1
               NK = NK + GAM(I)
               M1 = M1 + GAM(I)*I
   60       CONTINUE
            WRITE ( NOUT, FMT = 99996 )
            DO 80 I = 1, NP
               WRITE ( NOUT, FMT = 99995 ) ( NULLSP(I,J), J = 1,M1 )
   80       CONTINUE
            WRITE ( NOUT, FMT = 99994 ) DK, ( I-1, I = 1,DK+1 )
            DO 120 I = 1, NP
               DO 100 J = 1, NK
                  WRITE ( NOUT, FMT = 99993 )
     $                  I, J, ( KER(I,J,K), K = 1,DK+1 )
  100          CONTINUE
  120       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' MC03ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC03ND = ',I2)
99997 FORMAT (' The polynomial matrix P(s) has no right nullspace')
99996 FORMAT (' The right nullspace vectors of P(s) are ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' The minimal polynomial basis K(s) (of degree ',I2,') ',
     $       'for the right nullspace is ',//' power of s         ',
     $       20I8)
99993 FORMAT (/' element (',I2,',',I2,') is ',20(1X,F7.2))
99992 FORMAT (/' DP is out of range.',/' DP = ',I5)
99991 FORMAT (/' NP is out of range.',/' NP = ',I5)
99990 FORMAT (/' MP is out of range.',/' MP = ',I5)
      END
