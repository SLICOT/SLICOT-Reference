*     SB02PD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDG, LDQ, LDX
      PARAMETER        ( LDA = NMAX, LDG = NMAX, LDQ = NMAX,
     $                   LDX = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MAX( 2*NMAX, NMAX*NMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( 4*NMAX*NMAX + 8*NMAX,
     $                                 6*NMAX*NMAX ) + 1 )
*     .. Local Scalars ..
      DOUBLE PRECISION FERR, RCOND
      INTEGER          I, INFO, J, N
      CHARACTER        JOB, TRANA, UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), G(LDG,NMAX),
     $                 Q(LDQ,NMAX), WI(NMAX), WR(NMAX),
     $                 X(LDX,NMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         SB02PD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, JOB, TRANA, UPLO
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( Q(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( G(I,J), J = 1,N ), I = 1,N )
*        Find the solution matrix X.
         CALL SB02PD( JOB, TRANA, UPLO, N, A, LDA, G, LDG, Q, LDQ, X,
     $                LDX, RCOND, FERR, WR, WI, IWORK, DWORK, LDWORK,
     $                INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         END IF
         IF ( INFO.EQ.0 .OR. INFO.EQ.2 .OR. INFO.EQ.4 ) THEN
             WRITE ( NOUT, FMT = 99997 )
             DO 20 I = 1, N
                WRITE ( NOUT, FMT = 99996 ) ( X(I,J), J = 1,N )
   20        CONTINUE
             IF ( LSAME( JOB, 'A' ) .AND. INFO.NE.4 ) THEN
                WRITE ( NOUT, FMT = 99994 ) RCOND
                WRITE ( NOUT, FMT = 99993 ) FERR
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB02PD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB02PD = ',I2)
99997 FORMAT (' The solution matrix X is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' N is out of range.',/' N = ',I5)
99994 FORMAT (/' Estimated reciprocal condition number = ',F8.4)
99993 FORMAT (/' Estimated error bound = ',F20.16)
      END
