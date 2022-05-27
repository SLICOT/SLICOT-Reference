*     SB01BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDF, LDZ
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDF = MMAX,
     $                   LDZ = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( 5*MMAX,5*NMAX,2*NMAX+4*MMAX ) )
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, ANORM, NRM, TOL
      INTEGER          I, INFO, IWARN, J, M, N, NAP, NFP, NP, NUP
      CHARACTER*1      DICO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), AIN(LDA,NMAX), B(LDB,MMAX),
     $                 DWORK(LDWORK), F(LDF,NMAX), WI(NMAX), WR(NMAX),
     $                 Z(LDZ,NMAX), ZTA(LDZ,NMAX)
C     .. External Functions ..
      LOGICAL          LSAME
      DOUBLE PRECISION DLAMCH, DLANGE
      EXTERNAL         DLAMCH, DLANGE, LSAME
*     .. External Subroutines ..
      EXTERNAL         DGEMM, DLACPY, MB03QX, SB01BD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, NP, ALPHA, TOL, DICO
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF( NP.LT.0 .OR. NP.GT.NMAX ) THEN
               WRITE ( NOUT, FMT = 99992 ) NP
            ELSE
               DO 10 I = 1, NP
                  READ ( NIN, FMT = * ) WR(I), WI(I)
   10          CONTINUE
*              Perform "eigenvalue assignment" to compute F.
               CALL DLACPY( 'G', N, N, A, LDA, AIN, LDA )
               CALL SB01BD( DICO, N, M, NP, ALPHA, A, LDA, B, LDB,
     $                      WR, WI, NFP, NAP, NUP, F, LDF, Z, LDZ,
     $                      TOL, DWORK, LDWORK, IWARN, INFO )
*
               IF ( INFO.NE.0 .AND. INFO.LT.3 ) THEN
                  WRITE ( NOUT, FMT = 99997 ) INFO
               ELSE
                  IF ( INFO  .NE. 0 ) WRITE ( NOUT, FMT = 99997 ) INFO
                  IF ( IWARN .NE. 0 ) WRITE ( NOUT, FMT = 99991 ) IWARN
                  WRITE ( NOUT, FMT = 99990 ) NAP
                  WRITE ( NOUT, FMT = 99989 ) NFP
                  WRITE ( NOUT, FMT = 99988 ) NUP
                  WRITE ( NOUT, FMT = 99996 )
                  DO 60 I = 1, M
                     WRITE ( NOUT, FMT = 99995 ) ( F(I,J), J = 1,N )
   60             CONTINUE
                  CALL MB03QX( N, A, LDA, WR, WI, INFO )
                  WRITE ( NOUT, FMT = 99998 ) ( WR(I), WI(I), I = 1,N )
*                 Compute NORM (Z*Aout*Z'-(A+B*F)) / (eps*NORM(A))
                  ANORM = DLANGE( 'F', N, N, AIN, LDA, DWORK )
                  CALL DGEMM( 'N', 'N', N, N, M, ONE, B, LDB, F, LDF,
     $                        ONE, AIN, LDA )
                  CALL DGEMM( 'N', 'N', N, N, N, ONE, Z, LDZ, A, LDA,
     $                        ZERO, ZTA, LDZ )
                  CALL DGEMM( 'N', 'T', N, N, N, ONE, ZTA, LDZ, Z, LDZ,
     $                        -ONE, AIN, LDA )
                  NRM = DLANGE( 'F', N, N, AIN, LDA, DWORK ) /
     $                  ( DLAMCH( 'E' )*ANORM )
                  WRITE ( NOUT, FMT = 99987 ) NRM
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB01BD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (/,' The eigenvalues of closed-loop matrix A+B*F',/
     $          ( ' ( ',F8.4,',',F8.4,' )' ) )
99997 FORMAT (' INFO on exit from SB01BD = ',I2)
99996 FORMAT (/,' The state feedback matrix F is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' M is out of range.',/' M = ',I5)
99992 FORMAT (/' NP is out of range.',/' NP = ',I5)
99991 FORMAT (/' IWARN on exit from SB01BD = ', I2)
99990 FORMAT ( ' Number of assigned eigenvalues: NAP = ', I2 )
99989 FORMAT ( ' Number of fixed eigenvalues:    NFP = ', I2)
99988 FORMAT ( ' Number of uncontrollable poles: NUP = ', I2)
99987 FORMAT (/,' NORM(A+B*F - Z*Aout*Z'') / (eps*NORM(A)) =',1PD12.5)
      END
