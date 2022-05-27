*     IB01AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          LDR, LDU, LDWORK, LDY, LIWORK, LMAX, MMAX,
     $                 NOBRMX, NSMPMX
      PARAMETER        ( LMAX = 5, MMAX = 5, NOBRMX = 20, NSMPMX = 2000,
     $                   LDR = MAX( 2*( MMAX + LMAX )*NOBRMX,
     $                              3*MMAX*NOBRMX ), LDU = NSMPMX,
     $                   LDWORK = MAX( 6*( MMAX + LMAX )*NOBRMX,
     $                                 ( MMAX + LMAX )*( 4*NOBRMX*
     $                                 ( MMAX + LMAX + 1 ) + 2*NOBRMX ),
     $                                 ( MMAX + LMAX )*4*NOBRMX*
     $                                 ( NOBRMX + 1 ) ),
     $                   LDY = NSMPMX, LIWORK = ( MMAX + LMAX )*NOBRMX )
*     .. Local Scalars ..
      LOGICAL          NGIVEN
      CHARACTER        ALG, BATCH, CONCT, CTRL, JOBD, METH
      INTEGER          I, ICYCLE, II, INFO, IWARN, J, L, M, N, NCYCLE,
     $                 NGIV, NOBR, NSAMPL, NSMP
      DOUBLE PRECISION RCOND, TOL
*     .. Local Arrays ..
      DOUBLE PRECISION DWORK(LDWORK), R(LDR, 2*(MMAX+LMAX)*NOBRMX),
     $                 SV(LMAX*NOBRMX), U(LDU, MMAX), Y(LDY, LMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         IB01AD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
*     If the value of N is positive, it will be taken as system order.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) NOBR, N, M, L, NSMP, RCOND, TOL, METH, ALG,
     $                      JOBD, BATCH, CONCT, CTRL
      IF ( LSAME( BATCH, 'F' ) ) THEN
         READ ( NIN, FMT = * ) NCYCLE
      ELSE
         NCYCLE = 1
      END IF
      NSAMPL = NCYCLE*NSMP
*
      NGIVEN = N.GT.0
      IF( NGIVEN )
     $   NGIV = N
      IF ( NOBR.LE.0 .OR. NOBR.GT.NOBRMX ) THEN
         WRITE ( NOUT, FMT = 99997 ) NOBR
      ELSE IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99996 ) M
      ELSE IF ( L.LE.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) L
      ELSE IF ( NSMP.LT.0 .OR. NSMP.GT.NSMPMX .OR.
     $        ( NSMP.LT.2*( M + L + 1 )*NOBR - 1 .AND.
     $          LSAME( BATCH, 'O' ) ) .OR.
     $        ( NSAMPL.LT.2*( M + L + 1 )*NOBR - 1 .AND.
     $          LSAME( BATCH, 'L' ) ) .OR.
     $          NSMP.LT.2*NOBR .AND. ( LSAME( BATCH, 'F' ) .OR.
     $                                 LSAME( BATCH, 'I' ) ) ) THEN
         WRITE ( NOUT, FMT = 99994 ) NSMP
      ELSE IF ( NCYCLE.LE.0 .OR. NSAMPL.GT.NSMPMX ) THEN
         WRITE ( NOUT, FMT = 99993 ) NCYCLE
      ELSE
*        Read the matrices U and Y from the input file.
         IF ( M.GT.0 )
     $      READ ( NIN, FMT = * )
     $                         ( ( U(I,J), J = 1, M ), I = 1, NSAMPL )
         READ ( NIN, FMT = * ) ( ( Y(I,J), J = 1, L ), I = 1, NSAMPL )
*        Compute the  R  factor from a QR (or Cholesky) factorization
*        of the Hankel-like matrix (or correlation matrix).
         DO 10 ICYCLE = 1, NCYCLE
            II = ( ICYCLE - 1 )*NSMP + 1
            IF ( NCYCLE.GT.1 ) THEN
               IF ( ICYCLE.GT.1 )      BATCH = 'I'
               IF ( ICYCLE.EQ.NCYCLE ) BATCH = 'L'
            END IF
            CALL IB01AD( METH, ALG, JOBD, BATCH, CONCT, CTRL, NOBR, M,
     $                   L, NSMP, U(II,1), LDU, Y(II,1), LDY, N, R, LDR,
     $                   SV, RCOND, TOL, IWORK, DWORK, LDWORK, IWARN,
     $                   INFO )
   10    CONTINUE
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( IWARN.NE.0 )
     $         WRITE ( NOUT, FMT = 99990 ) IWARN
            IF( NGIVEN )
     $         N = NGIV
            WRITE ( NOUT, FMT = 99992 ) N
            WRITE ( NOUT, FMT = 99991 ) ( SV(I), I = 1,L*NOBR )
         END IF
      END IF
      STOP
99999 FORMAT ( ' IB01AD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT ( ' INFO on exit from IB01AD = ',I2)
99997 FORMAT (/' NOBR is out of range.',/' NOBR = ', I5)
99996 FORMAT (/' M is out of range.',/' M = ', I5)
99995 FORMAT (/' L is out of range.',/' L = ', I5)
99994 FORMAT (/' NSMP is out of range.',/' NSMP = ', I5)
99993 FORMAT (/' NCYCLE is out of range.',/' NCYCLE = ', I5)
99992 FORMAT ( ' The order of the system is ', I5)
99991 FORMAT ( ' The singular values are ',/ (8(1X,F8.4)))
99990 FORMAT ( ' IWARN on exit from IB01AD = ',I2)
      END
