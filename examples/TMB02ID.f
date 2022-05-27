*     MB02ID EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX, LMAX, MMAX, NMAX, RBMAX, RCMAX
      PARAMETER        ( KMAX  = 20, LMAX  = 20, MMAX = 20, NMAX = 20,
     $                   RBMAX = 20, RCMAX = 20 )
      INTEGER          LDB, LDC, LDTC, LDTR, LDWORK
      PARAMETER        ( LDB  = KMAX*MMAX, LDC  = KMAX*MMAX,
     $                   LDTC = MMAX*KMAX, LDTR = KMAX,
     $                   LDWORK = 2*NMAX*LMAX*( LMAX + KMAX ) +
     $                            ( 6 + NMAX )*LMAX +
     $                            MMAX*KMAX*( LMAX + 1 ) +
     $                            RBMAX + RCMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, K, L, M, N, RB, RC
      CHARACTER        JOB
      DOUBLE PRECISION B(LDB,RBMAX),  C(LDC,RCMAX), DWORK(LDWORK),
     $                 TC(LDTC,LMAX), TR(LDTR,NMAX*LMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB02ID
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  K, L, M, N, RB, RC, JOB
      IF( K.LE.0 .OR. K.GT.KMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) K
      ELSE IF( L.LE.0 .OR. L.GT.LMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) L
      ELSE IF( M.LE.0 .OR. M.GT.MMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) M
      ELSE IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) N
      ELSE IF ( ( LSAME( JOB, 'O' ) .OR. LSAME( JOB, 'A' ) )
     $          .AND. ( ( RB.LE.0 ) .OR. ( RB.GT.RBMAX ) ) ) THEN
         WRITE ( NOUT, FMT = 99990 ) RB
      ELSE IF ( ( LSAME( JOB, 'U' ) .OR. LSAME( JOB, 'A' ) )
     $          .AND. ( ( RC.LE.0 ) .OR. ( RC.GT.RCMAX ) ) ) THEN
         WRITE ( NOUT, FMT = 99989 ) RC
      ELSE
         READ ( NIN, FMT = * ) ( ( TC(I,J), J = 1,L ), I = 1,M*K )
         READ ( NIN, FMT = * ) ( ( TR(I,J), J = 1,(N-1)*L ), I = 1,K )
         IF ( LSAME( JOB, 'O' ) .OR. LSAME( JOB, 'A' ) ) THEN
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,RB ), I = 1,M*K )
         END IF
         IF ( LSAME( JOB, 'U' ) .OR. LSAME( JOB, 'A' ) ) THEN
            READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,RC ), I = 1,N*L )
         END IF
         CALL MB02ID( JOB, K, L, M, N, RB, RC, TC, LDTC, TR, LDTR, B,
     $                LDB, C, LDC, DWORK, LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( LSAME( JOB, 'O' ) .OR. LSAME( JOB, 'A' ) ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 10  I = 1, N*L
                  WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1, RB )
   10          CONTINUE
            END IF
            IF ( LSAME( JOB, 'U' ) .OR. LSAME( JOB, 'A' ) ) THEN
               WRITE ( NOUT, FMT = 99996 )
               DO 20  I = 1, M*K
                  WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1, RC )
   20          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02ID EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02ID = ',I2)
99997 FORMAT (' The least squares solution of T * X = B is ')
99996 FORMAT (' The minimum norm solution of T^T * X = C is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' K is out of range.',/' K = ',I5)
99993 FORMAT (/' L is out of range.',/' L = ',I5)
99992 FORMAT (/' M is out of range.',/' M = ',I5)
99991 FORMAT (/' N is out of range.',/' N = ',I5)
99990 FORMAT (/' RB is out of range.',/' RB = ',I5)
99989 FORMAT (/' RC is out of range.',/' RC = ',I5)
      END
