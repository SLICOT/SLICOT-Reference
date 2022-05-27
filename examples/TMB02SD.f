*     MB02SD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, NRHMAX
      PARAMETER        ( NMAX = 20, NRHMAX = 20 )
      INTEGER          LDB, LDH
      PARAMETER        ( LDB = NMAX, LDH = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 3*NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX )
*     .. Local Scalars ..
      DOUBLE PRECISION HNORM, RCOND
      INTEGER          I, INFO, INFO1, J, N, NRHS
      CHARACTER*1      NORM, TRANS
*     .. Local Arrays ..
      DOUBLE PRECISION H(LDH,NMAX), B(LDB,NRHMAX), DWORK(LDWORK)
      INTEGER          IPIV(NMAX), IWORK(LIWORK)
*     .. External Functions ..
      DOUBLE PRECISION DLAMCH, DLANHS
      EXTERNAL         DLAMCH, DLANHS
*     .. External Subroutines ..
      EXTERNAL         DLASET, MB02RD, MB02SD, MB02TD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read in the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, NRHS, NORM, TRANS
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( H(I,J), J = 1,N ), I = 1,N )
         IF ( NRHS.LT.0 .OR. NRHS.GT.NRHMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) NRHS
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,NRHS ), I = 1,N )
            IF ( N.GT.2 )
     $         CALL DLASET( 'Lower', N-2, N-2, ZERO, ZERO, H(3,1), LDH )
*           Compute the LU factorization of the upper Hessenberg matrix.
            CALL MB02SD( N, H, LDH, IPIV, INFO )
*           Estimate the reciprocal condition number of the matrix.
            HNORM = DLANHS( NORM, N, H, LDH, DWORK )
            CALL MB02TD( NORM, N, HNORM, H, LDH, IPIV, RCOND, IWORK,
     $                   DWORK, INFO1 )
            IF ( INFO.EQ.0 .AND. RCOND.GT.DLAMCH( 'Epsilon' ) ) THEN
*              Solve the linear system.
               CALL MB02RD( TRANS, N, NRHS, H, LDH, IPIV, B, LDB, INFO )
*
               WRITE ( NOUT, FMT = 99997 )
            ELSE
               WRITE ( NOUT, FMT = 99998 ) INFO
            END IF
               DO 10 I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) ( B(I,J), J = 1,NRHS )
   10          CONTINUE
            WRITE ( NOUT, FMT = 99995 ) RCOND
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02SD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02SD = ',I2)
99997 FORMAT (' The solution matrix is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' Reciprocal condition number = ',D12.4)
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' NRHS is out of range.',/' NRHS = ',I5)
      END
