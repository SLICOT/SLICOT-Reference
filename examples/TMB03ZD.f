*     MB03ZD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 200 )
      INTEGER          LDG, LDRES, LDS, LDT, LDU1, LDU2, LDUS, LDUU,
     $                 LDV1, LDV2, LDWORK
      PARAMETER        ( LDG = NMAX, LDRES = 2*NMAX, LDS = NMAX,
     $                   LDT = NMAX, LDU1 = NMAX, LDU2 = NMAX,
     $                   LDUS = 2*NMAX, LDUU = 2*NMAX, LDV1 = NMAX,
     $                   LDV2 = NMAX, LDWORK = 3*NMAX*NMAX + 7*NMAX )
*     .. Local Scalars ..
      CHARACTER*1      BALANC, METH, ORTBAL, STAB, WHICH
      INTEGER          I, ILO, INFO, J, M, N
*     .. Local Arrays ..
      LOGICAL          LWORK(2*NMAX), SELECT(NMAX)
      INTEGER          IWORK(2*NMAX)
      DOUBLE PRECISION DWORK(LDWORK), G(LDG, NMAX), RES(LDRES,NMAX),
     $                 S(LDS, NMAX), SCALE(NMAX), T(LDT,NMAX),
     $                 U1(LDU1,NMAX), U2(LDU2, NMAX), US(LDUS,2*NMAX),
     $                 UU(LDUU,2*NMAX), V1(LDV1,NMAX), V2(LDV2, NMAX),
     $                 WI(NMAX), WR(NMAX)
*     .. External Functions ..
      EXTERNAL         DLANGE, LSAME
      LOGICAL          LSAME
      DOUBLE PRECISION DLANGE
*     .. External Subroutines ..
      EXTERNAL         MB03ZD
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  N, ILO, WHICH, METH, STAB, BALANC, ORTBAL
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
*
         IF ( LSAME( WHICH, 'S' ) )
     $      READ ( NIN, FMT = * ) ( SELECT(I), I = 1,N )
         READ ( NIN, FMT = * ) ( ( S(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( T(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( WHICH, 'A' ).AND.LSAME( METH, 'L' ) )
     $      READ ( NIN, FMT = * ) ( ( G(I,J), J = 1,N ), I = 1,N )
         IF ( LSAME( BALANC, 'P' ).OR.LSAME( BALANC, 'S' ).OR.
     $        LSAME( BALANC, 'B' ) )
     $      READ ( NIN, FMT = * ) ( SCALE(I), I = 1,N )
         READ ( NIN, FMT = * ) ( ( U1(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( U2(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( V1(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( V2(I,J), J = 1,N ), I = 1,N )
*
         CALL MB03ZD( WHICH, METH, STAB, BALANC, ORTBAL, SELECT, N, 2*N,
     $                ILO, SCALE, S, LDS, T, LDT, G, LDG, U1, LDU1, U2,
     $                LDU2, V1, LDV1, V2, LDV2, M, WR, WI, US, LDUS,
     $                UU, LDUU, LWORK, IWORK, DWORK, LDWORK, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 20  I = 1, N
               WRITE ( NOUT, FMT = 99996 ) I, WR(I), WI(I)
20          CONTINUE
*
            IF ( LSAME( STAB, 'S' ).OR.LSAME( STAB, 'B' ) ) THEN
               WRITE ( NOUT, FMT = 99995 )
               DO 30  I = 1, 2*N
                  WRITE ( NOUT, FMT = 99993 ) ( US(I,J), J = 1,M )
30             CONTINUE
               IF ( LSAME( ORTBAL, 'B' ).OR.LSAME( BALANC, 'N' ).OR.
     $            LSAME( BALANC, 'P' ) ) THEN
                  CALL DGEMM( 'Transpose', 'No Transpose', M, M, 2*N,
     $                        ONE, US, LDUS, US, LDUS, ZERO, RES,
     $                        LDRES )
                  DO 40  I = 1, M
                     RES(I,I) = RES(I,I) - ONE
40                CONTINUE
                  WRITE ( NOUT, FMT = 99991 ) DLANGE( 'Frobenius', M, M,
     $                    RES, LDRES, DWORK )
               END IF
               CALL DGEMM( 'Transpose', 'No Transpose', M, M, N, ONE,
     $                     US, LDUS, US(N+1,1), LDUS, ZERO, RES, LDRES )
               CALL DGEMM( 'Transpose', 'No Transpose', M, M, N, -ONE,
     $                     US(N+1,1), LDUS, US, LDUS, ONE, RES, LDRES )
               WRITE ( NOUT, FMT = 99990 ) DLANGE( 'Frobenius', M, M,
     $                 RES, LDRES, DWORK )
            END IF
*
            IF ( LSAME( STAB, 'U' ).OR.LSAME( STAB, 'B' ) ) THEN
               WRITE ( NOUT, FMT = 99994 )
               DO 50  I = 1, 2*N
                  WRITE ( NOUT, FMT = 99993 ) ( UU(I,J), J = 1,M )
50             CONTINUE
               IF ( LSAME( ORTBAL, 'B' ).OR.LSAME( BALANC, 'N' ).OR.
     $            LSAME( BALANC, 'P' ) ) THEN
                  CALL DGEMM( 'Transpose', 'No Transpose', M, M, 2*N,
     $                        ONE, UU, LDUU, UU, LDUU, ZERO, RES,
     $                        LDRES )
                  DO 60  I = 1, M
                     RES(I,I) = RES(I,I) - ONE
60                CONTINUE
                  WRITE ( NOUT, FMT = 99989 ) DLANGE( 'Frobenius', M, M,
     $                    RES, LDRES, DWORK )
               END IF
               CALL DGEMM( 'Transpose', 'No Transpose', M, M, N, ONE,
     $                     UU, LDUU, UU(N+1,1), LDUU, ZERO, RES, LDRES )
               CALL DGEMM( 'Transpose', 'No Transpose', M, M, N, -ONE,
     $                     UU(N+1,1), LDUU, UU, LDUU, ONE, RES, LDRES )
               WRITE ( NOUT, FMT = 99988 ) DLANGE( 'Frobenius', M, M,
     $                 RES, LDRES, DWORK )
            END IF
         END IF
      END IF
*
99999 FORMAT (' MB03ZD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB03ZD = ',I2)
99997 FORMAT (' The stable eigenvalues are',//'   i',6X,
     $        'WR(i)',6X,'WI(i)',/)
99996 FORMAT (I4,3X,F8.4,3X,F8.4)
99995 FORMAT (/' A basis for the stable invariant subspace is')
99994 FORMAT (/' A basis for the unstable invariant subspace is')
99993 FORMAT (20(1X,F9.3))
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (/' Orthogonality of US: || US''*US - I ||_F = ',G7.2)
99990 FORMAT (/' Symplecticity of US: || US''*J*US   ||_F = ',G7.2)
99989 FORMAT (/' Orthogonality of UU: || UU''*UU - I ||_F = ',G7.2)
99988 FORMAT (/' Symplecticity of UU: || UU''*J*UU   ||_F = ',G7.2)

      END
