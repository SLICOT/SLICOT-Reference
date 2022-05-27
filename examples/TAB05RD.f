*     AB05RD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, MVMAX, PMAX, PZMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, MVMAX = 20,
     $                   PMAX = 20, PZMAX = 20 )
      INTEGER          LDA, LDB, LDBC, LDC, LDCC, LDD, LDDC, LDF, LDG,
     $                 LDH, LDK, LDWORK, LIWORK
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDBC = NMAX,
     $                   LDC = PMAX, LDCC = PZMAX,
     $                   LDD = PMAX, LDDC = PZMAX, LDF = MMAX,
     $                   LDG = MMAX, LDH  = PZMAX, LDK = MMAX,
     $                   LDWORK = MAX( MMAX, PMAX*MVMAX,
     $                   PMAX*PMAX + 4*PMAX ), LIWORK = 2*PMAX )
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
*     .. Local Scalars ..
      LOGICAL          LJOBD, OUTPF
      CHARACTER*1      FBTYPE, JOBD
      INTEGER          I, INFO, J, M, MV, N, P, PZ
      DOUBLE PRECISION ALPHA, BETA, RCOND
*     .. Local Arrays ..
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), BC(LDBC,MVMAX),
     $                 C(LDC,NMAX), CC(LDCC,NMAX),
     $                 D(LDD,MMAX), DC(LDDC,MVMAX),  DWORK(LDWORK),
     $                 F(LDF,PMAX), G(LDG,MVMAX), H(LDH,PMAX),
     $                 K(LDK,NMAX)
*     .. External functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         AB05RD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, MV, PZ, ALPHA, BETA, FBTYPE, JOBD
      OUTPF = LSAME( FBTYPE, 'O' )
      LJOBD = LSAME( JOBD, 'D' )
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
            IF ( BETA.NE.ZERO )
     $         READ ( NIN, FMT = * ) ( ( K(I,J), J = 1,N ), I = 1,M )
            IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99990 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               IF ( LJOBD )
     $            READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
               IF ( OUTPF.AND.ALPHA.NE.ZERO )
     $            READ ( NIN, FMT = * ) ( ( F(I,J), J = 1,P ), I = 1,M )
               IF ( MV.LE.0 .OR. MV.GT.MVMAX ) THEN
                  WRITE ( NOUT, FMT = 99989 ) MV
               ELSE
                  READ ( NIN, FMT = * )
     $                 ( ( G(I,J), J = 1,MV ), I = 1,M )
                  IF ( PZ.LE.0 .OR. PZ.GT.PZMAX ) THEN
                     WRITE ( NOUT, FMT = 99988 ) PZ
                  ELSE
                     READ ( NIN, FMT = * )
     $                    ( ( H(I,J), J = 1,P ), I = 1,PZ )
*                       Find the state-space model (A,B,C,D).
                     CALL AB05RD( FBTYPE, JOBD, N, M, P, MV, PZ, ALPHA,
     $                            BETA, A, LDA, B, LDB, C, LDC, D, LDD,
     $                            F, LDF, K, LDK, G, LDG, H, LDH, RCOND,
     $                            BC, LDBC, CC, LDCC, DC, LDDC, IWORK,
     $                            DWORK, LDWORK, INFO )
*
                     WRITE ( NOUT, FMT = 99987 ) RCOND
                     IF ( INFO.NE.0 ) THEN
                        WRITE ( NOUT, FMT = 99998 ) INFO
                     ELSE
                        WRITE ( NOUT, FMT = 99997 )
                        DO 20 I = 1, N
                           WRITE ( NOUT, FMT = 99996 )
     $                           ( A(I,J), J = 1,N )
   20                   CONTINUE
                        WRITE ( NOUT, FMT = 99995 )
                        DO 40 I = 1, N
                           WRITE ( NOUT, FMT = 99996 )
     $                           ( BC(I,J), J = 1,MV )
   40                   CONTINUE
                        WRITE ( NOUT, FMT = 99994 )
                        DO 60 I = 1, PZ
                           WRITE ( NOUT, FMT = 99996 )
     $                           ( CC(I,J), J = 1,N )
   60                   CONTINUE
                        IF ( LJOBD ) THEN
                           WRITE ( NOUT, FMT = 99993 )
                           DO 80 I = 1, PZ
                              WRITE ( NOUT, FMT = 99996 )
     $                              ( DC(I,J), J = 1,MV )
   80                      CONTINUE
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB05RD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB05RD = ',I2)
99997 FORMAT (' The state transition matrix of the closed-loop system is
     $')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The input/state matrix of the closed-loop system is ')
99994 FORMAT (/' The state/output matrix of the closed-loop system is ')
99993 FORMAT (/' The input/output matrix of the closed-loop system is ')
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (/' M is out of range.',/' M = ',I5)
99990 FORMAT (/' P is out of range.',/' P = ',I5)
99989 FORMAT (/' MV is out of range.',/' MV = ',I5)
99988 FORMAT (/' PZ is out of range.',/' PZ = ',I5)
99987 FORMAT ( ' The reciprocal condition number of the matrix ',
     $         ' I - alpha*D*F is',F8.4,/1X)
      END
