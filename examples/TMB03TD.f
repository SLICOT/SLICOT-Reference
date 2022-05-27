*     MB03TD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 100 )
      INTEGER          LDA, LDG, LDRES, LDU1, LDU2, LDWORK
      PARAMETER        ( LDA  = NMAX, LDG  = NMAX, LDRES  = NMAX,
     $                   LDU1 = NMAX, LDU2 = NMAX, LDWORK = 8*NMAX )
*     .. Local Scalars ..
      CHARACTER*1      COMPU, TYP
      INTEGER          I, INFO, J, N, M
*     .. Local Arrays ..
      LOGICAL          LOWER(NMAX), SELECT(NMAX)
      DOUBLE PRECISION A(LDA, NMAX), DWORK(LDWORK), G(LDG, NMAX),
     $                 RES(LDRES,NMAX), U1(LDU1,NMAX), U2(LDU2,NMAX),
     $                 WR(NMAX), WI(NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      DOUBLE PRECISION MA02JD
      EXTERNAL         LSAME, MA02JD
*     .. External Subroutines ..
      EXTERNAL         MB03TD
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  N, TYP, COMPU
      IF( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) N
      ELSE
         READ ( NIN, FMT = * ) ( SELECT(J), J = 1,N )
         READ ( NIN, FMT = * ) ( LOWER(J), J = 1,N )
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( G(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( COMPU, 'U' ) ) THEN
            READ ( NIN, FMT = * ) ( ( U1(I,J), J = 1,N ), I = 1,N )
            READ ( NIN, FMT = * ) ( ( U2(I,J), J = 1,N ), I = 1,N )
         END IF
         CALL MB03TD( TYP, COMPU, SELECT, LOWER, N, A, LDA, G, LDG, U1,
     $                LDU1, U2, LDU2, WR, WI, M, DWORK, LDWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            IF ( LSAME( COMPU, 'U' ) ) THEN
               WRITE ( NOUT, FMT = 99997 )
               DO 10  I = 1, N
                  WRITE ( NOUT, FMT = 99994 )
     $                  ( U1(I,J), J = 1,N ), ( U2(I,J), J = 1,N )
10             CONTINUE
               DO 20  I = 1, N
                  WRITE ( NOUT, FMT = 99994 )
     $                  ( -U2(I,J), J = 1,N ), ( U1(I,J), J = 1,N )
20             CONTINUE
               WRITE ( NOUT, FMT = 99992 ) MA02JD( .FALSE., .FALSE., N,
     $                 U1, LDU1, U2, LDU2, RES, LDRES )
            END IF
*
            WRITE ( NOUT, FMT = 99996 )
            DO 30  I = 1, N
               WRITE ( NOUT, FMT = 99994 ) ( A(I,J), J = 1,N )
30          CONTINUE
*
            WRITE ( NOUT, FMT = 99995 )
            IF ( LSAME( TYP, 'S' ) ) THEN
               DO 40  I = 1, N
                  WRITE ( NOUT, FMT = 99994 )
     $               ( -G(J,I), J = 1,I-1 ), ZERO, ( G(I,J), J = I+1,N )
40             CONTINUE
            ELSE
               DO 50  I = 1, N
                  WRITE ( NOUT, FMT = 99994 )
     $               ( G(J,I), J = 1,I-1 ), ( G(I,J), J = I,N )
50             CONTINUE
           END IF
         END IF
      END IF
*
99999 FORMAT (' MB03TD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB03TD = ',I2)
99997 FORMAT (' The orthogonal symplectic factor U is ')
99996 FORMAT (/' The matrix A in reordered Schur canonical form is ')
99995 FORMAT (/' The matrix G is ')
99994 FORMAT (20(1X,F9.4))
99993 FORMAT (/' N is out of range.',/' N = ',I5)
99992 FORMAT (/' Orthogonality of U: || U''*U - I ||_F = ',G7.2)
      END
