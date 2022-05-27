*     MB02CD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX, NMAX
      PARAMETER        ( KMAX = 20, NMAX = 20 )
      INTEGER          LCS, LDG, LDL, LDR, LDT, LDWORK
      PARAMETER        ( LDG = 2*KMAX, LDL = NMAX*KMAX, LDR = NMAX*KMAX,
     $                   LDT = KMAX, LDWORK = ( NMAX - 1 )*KMAX )
      PARAMETER        ( LCS = 3*LDWORK )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, K, M, N
      CHARACTER        JOB, TYPET
*     .. Local Arrays .. (Dimensioned for TYPET = 'R'.)
      DOUBLE PRECISION CS(LCS), DWORK(LDWORK), G(LDG, NMAX*KMAX),
     $                 L(LDL, NMAX*KMAX), R(LDR, NMAX*KMAX),
     $                 T(LDT, NMAX*KMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         DLASET, MB02CD
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, K, JOB
      TYPET = 'R'
      M = N*K
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         IF( K.LE.0 .OR. K.GT.KMAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) K
         ELSE
            READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,M ), I = 1,K )
*           Compute the Cholesky factor(s) and/or the generator.
            CALL MB02CD( JOB, TYPET, K, N, T, LDT, G, LDG, R, LDR, L,
     $                   LDL, CS, LCS, DWORK, LDWORK, INFO )
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               IF ( LSAME( JOB, 'G' ) .OR. LSAME( JOB, 'A' ) .OR.
     $              LSAME( JOB, 'L' ) .OR. LSAME( JOB, 'R' ) ) THEN
                  WRITE ( NOUT, FMT = 99997 )
                  CALL DLASET( 'Full', K, K, ZERO, ZERO, G(K+1,1), LDG )
                  DO 10  I = 1, 2*K
                     WRITE ( NOUT, FMT = 99994 ) ( G(I,J), J = 1, M )
   10             CONTINUE
               END IF
               IF ( LSAME( JOB, 'L' ) .OR. LSAME( JOB, 'A' ) ) THEN
                  WRITE ( NOUT, FMT = 99996 )
                  DO 20  I = 1, M
                     WRITE ( NOUT, FMT = 99994 ) ( L(I,J), J = 1, M )
   20             CONTINUE
               END IF
               IF ( LSAME( JOB, 'R' ) .OR. LSAME( JOB, 'A' )
     $                                .OR. LSAME( JOB, 'O' ) ) THEN
                  WRITE ( NOUT, FMT = 99995 )
                  DO 30  I = 1, M
                     WRITE ( NOUT, FMT = 99994 ) ( R(I,J), J = 1, M )
   30             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02CD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02CD = ',I2)
99997 FORMAT (' The generator of the inverse of block Toeplitz matrix',
     $        ' is ')
99996 FORMAT (/' The lower Cholesky factor of the inverse is ')
99995 FORMAT (/' The upper Cholesky factor of block Toeplitz matrix is '
     $       )
99994 FORMAT (20(1X,F8.4))
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' K is out of range.',/' K = ',I5)
      END
