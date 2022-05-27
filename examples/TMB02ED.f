*     MB02ED EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          KMAX, NMAX
      PARAMETER        ( KMAX = 20, NMAX = 20 )
      INTEGER          LDB, LDT, LDWORK
      PARAMETER        ( LDB = KMAX*NMAX, LDT = KMAX*NMAX,
     $                   LDWORK = NMAX*KMAX*KMAX + ( NMAX+2 )*KMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, K, M, N, NRHS
      CHARACTER        TYPET
*     .. Local Arrays ..
*     The arrays B and T are dimensioned for both TYPET = 'R' and
*     TYPET = 'C'.
*     NRHS is assumed to be not larger than KMAX*NMAX.
      DOUBLE PRECISION B(LDB, KMAX*NMAX), DWORK(LDWORK),
     $                 T(LDT, KMAX*NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         MB02ED
*
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, K, NRHS, TYPET
      M = N*K
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         IF ( K.LE.0 .OR. K.GT.KMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) K
         ELSE
            IF ( NRHS.LE.0 .OR. NRHS.GT.KMAX*NMAX ) THEN
               WRITE ( NOUT, FMT = 99992 ) NRHS
            ELSE
               IF ( LSAME( TYPET, 'R' ) ) THEN
                  READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,M ), I = 1,K )
               ELSE
                  READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,K ), I = 1,M )
               END IF
               IF ( LSAME( TYPET, 'R' ) ) THEN
                  READ (NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,
     $                                   NRHS )
               ELSE
                  READ (NIN, FMT = * ) ( ( B(I,J), J = 1,NRHS ), I = 1,
     $                                   M )
               END IF
*              Compute the solution of X T = B or T X = B.
               CALL MB02ED( TYPET, K, N, NRHS, T, LDT, B, LDB, DWORK,
     $                      LDWORK, INFO )
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF ( LSAME( TYPET, 'R' ) ) THEN
                     WRITE ( NOUT, FMT = 99997 )
                     DO 10  I = 1, NRHS
                        WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1, M )
   10                CONTINUE
                  ELSE
                     WRITE ( NOUT, FMT = 99996 )
                     DO 20  I = 1, M
                        WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,
     $                                                NRHS )
   20                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' MB02ED EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB02ED = ',I2)
99997 FORMAT (' The solution of X*T = B is ')
99996 FORMAT (' The solution of T*X = B is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' K is out of range.',/' K = ',I5)
99992 FORMAT (/' NRHS is out of range.',/' NRHS = ',I5)
      END
