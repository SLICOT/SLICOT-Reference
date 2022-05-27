*     MB03XZ EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZER
      PARAMETER        ( ZER = 0.0D0 )
      COMPLEX*16       ZERO, ONE
      PARAMETER        ( ZERO = (0.0D0,0.0D0), ONE = (1.0D0,0.0D0) )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 100 )
      INTEGER          LDA, LDAE, LDAS, LDGE, LDQG, LDQGE, LDQGS, LDRES,
     $                 LDU1, LDU2, LDWORK, LZWORK
      PARAMETER        ( LDA   = 2*NMAX, LDAE  = 2*NMAX, LDAS  = NMAX,
     $                   LDGE  = 2*NMAX, LDQG  = 2*NMAX, LDQGE = 2*NMAX,
     $                   LDQGS  = NMAX,  LDRES = 2*NMAX, LDU1  = 2*NMAX,
     $                   LDU2   = 2*NMAX,
     $                   LDWORK = 20*NMAX*NMAX + 12*NMAX + 2,
     $                   LZWORK = 12*NMAX - 2 )
*     .. Local Scalars ..
      CHARACTER*1      BALANC, JOB, JOBU
      INTEGER          I, ILO, INFO, J, M, N
      DOUBLE PRECISION TEMP
*     .. Local Arrays ..
      COMPLEX*16       A(LDA, 2*NMAX), AE(LDAE, 2*NMAX), AS(LDAS, NMAX),
     $                 GE(LDGE, 2*NMAX), QG(LDQG, 2*NMAX+1),
     $                 QGE(LDQGE, 2*NMAX+1), QGS(LDQGS, NMAX+1),
     $                 RES(LDRES,2*NMAX), U1(LDU1,2*NMAX),
     $                 U2(LDU2,  2*NMAX), ZWORK(LZWORK)
      DOUBLE PRECISION DWORK(LDWORK), SCALE(NMAX), WI(2*NMAX),
     $                 WR(2*NMAX)
      LOGICAL          BWORK(2*NMAX)
*     .. External Functions ..
      LOGICAL          LSAME
      DOUBLE PRECISION DLAPY2, MA02JZ, ZLANGE
      EXTERNAL         DLAPY2, LSAME, MA02JZ, ZLANGE
*     .. External Subroutines ..
      EXTERNAL         MA02EZ, MB03XZ, MB04DZ, ZCOPY, ZGEMM, ZLACPY,
     $                 ZLASET
*     ..Intrinsic Functions..
      INTRINSIC        DBLE, DCMPLX, DIMAG
*     .. Executable Statements ..
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * )  N, BALANC, JOB, JOBU
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         M = 2*N
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         CALL ZLACPY( 'All', N, N, A, LDA, AS, LDAS )
         READ ( NIN, FMT = * ) ( ( QG(I,J), J = 1,N+1 ), I = 1,N )
         CALL ZLACPY( 'All', N, N+1, QG, LDQG, QGS, LDQGS )
*        Compute the eigenvalues and the transformed Hamiltonian matrix.
         CALL MB03XZ( BALANC, JOB, JOBU, N, A, LDA, QG, LDQG, U1, LDU1,
     $                U2, LDU2, WR, WI, ILO, SCALE, DWORK, LDWORK,
     $                ZWORK, LZWORK, BWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            WRITE ( NOUT, FMT = 99997 )
            DO 10  I = 1, M
               WRITE ( NOUT, FMT = 99996 ) I, WR(I), WI(I)
   10       CONTINUE
            IF ( LSAME( JOB, 'S' ).OR.LSAME( JOB, 'G' ) ) THEN
               WRITE ( NOUT, FMT = 99995 )
               DO 20  I = 1, M
                  WRITE ( NOUT, FMT = 99992 ) ( A(I,J), J = 1,M )
   20          CONTINUE
            END IF
            IF ( LSAME( JOB, 'G' ) ) THEN
               WRITE ( NOUT, FMT = 99994 )
               DO 30  I = 1, M
                  WRITE ( NOUT, FMT = 99992 ) ( QG(I,J), J = 1,M )
   30          CONTINUE
            END IF
*
            IF ( LSAME( JOB, 'G' ).AND.LSAME( JOBU, 'U' ) ) THEN
*              Compute the residual of the formula (4) in MB03XZ.
               CALL MB04DZ( BALANC, N, AS, LDAS, QGS, LDQGS, I, DWORK,
     $                      INFO )
               CALL ZLASET( 'Lower', M-1, M-1, ZERO, ZERO, A(2,1), LDA )
               CALL MA02EZ( 'Upper', 'Conjugate', 'Not skew', M, QG,
     $                      LDQG )
*              Compute Ae, Ge, and Qe.
               DO 60  J = 1, N
                  DO 40  I = 1, N
                     AE(I,J) = DCMPLX( ZER, DIMAG( AS(I,J) ) )
   40             CONTINUE
                  DO 50  I = 1, N
                     AE(I+N,J) = -DCMPLX( ZER, DBLE( AS(I,J) ) )
   50             CONTINUE
   60          CONTINUE
*
               DO 90  J = 1, N
                  DO 70  I = 1, N
                     AE(I,J+N) = -AE(I+N,J)
   70             CONTINUE
                  DO 80  I = 1, N
                     AE(I+N,J+N) = AE(I,J)
   80             CONTINUE
   90          CONTINUE
*
               DO 120  J = 1, N+1
                  DO 100  I = 1, N
                     QGE(I,J) = DCMPLX( ZER, DIMAG( QGS(I,J) ) )
  100             CONTINUE
                  DO 110  I = J, N
                     QGE(I+N,J) = -DCMPLX( ZER, DBLE( QGS(I,J) ) )
  110             CONTINUE
  120          CONTINUE
*
               DO 150  J = 1, N
                  DO 130  I = 1, J
                     QGE(I,J+N+1) = DCMPLX( ZER, DBLE( QGS(I,J+1) ) )
  130             CONTINUE
                  DO 140  I = 1, N
                     QGE(I+N,J+N+1) = QGE(I,J+1)
  140             CONTINUE
  150          CONTINUE
               CALL ZCOPY( N, QGE, 1, QGE(N+1,N+1), 1 )
               CALL MA02EZ( 'Lower', 'Transpose', 'Not Skew', N,
     $                      QGE(N+1,1), LDQGE )
               CALL MA02EZ( 'Upper', 'Transpose', 'Not Skew', N,
     $                      QGE(1,N+2), LDQGE )
*
               CALL ZLACPY( 'Upper', M, M, QGE(1,2), LDQGE, GE, LDGE )
               CALL MA02EZ( 'Upper', 'Transpose', 'Skew', M, GE, LDGE )
               CALL MA02EZ( 'Lower', 'Transpose', 'Skew', M, QGE,
     $                      LDQGE )
*              Compute the residual of the (1,1) block in (4).
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                     AE, LDAE, U1, LDU1, ZERO, RES, LDRES )
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M,
     $                     -ONE, GE, LDGE, U2, LDU2, ONE, RES, LDRES )
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M,
     $                     -ONE, U1, LDU1, A, LDA, ONE, RES, LDRES )
               TEMP = ZLANGE( 'Frobenius', M, M, RES, LDRES, DWORK )
*              Compute the residual of the (1,2) block in (4).
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                     AE, LDAE, U2, LDU2, ZERO, RES, LDRES )
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                     GE, LDGE, U1, LDU1, ONE, RES, LDRES )
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M,
     $                     -ONE, U1, LDU1, QG, LDQG, ONE, RES, LDRES )
               CALL ZGEMM( 'No Transpose', 'Conj Transpose', M, M, M,
     $                     ONE, U2, LDU2, A, LDA, ONE, RES, LDRES )
               TEMP = DLAPY2( TEMP, ZLANGE( 'Frobenius', M, M, RES,
     $                                      LDRES, DWORK ) )
*              Compute the residual of the (2,1) block in (4).
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                     QGE, LDQGE, U1, LDU1, ZERO, RES, LDRES )
               CALL ZGEMM( 'Transpose', 'No Transpose', M, M, M,
     $                     -ONE, AE, LDAE, U2, LDU2, ONE, RES, LDRES )
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                     U2, LDU2, A, LDA, ONE, RES, LDRES )
               TEMP = DLAPY2( TEMP, ZLANGE( 'Frobenius', M, M, RES,
     $                                      LDRES, DWORK ) )
*              Compute the residual of the (2,2) block in (4).
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                     QGE, LDQGE, U2, LDU2, ZERO, RES, LDRES )
               CALL ZGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $                     AE, LDAE, U1, LDU1, ONE, RES, LDRES )
               CALL ZGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                     U2, LDU2, QG, LDQG, ONE, RES, LDRES )
               CALL ZGEMM( 'No Transpose', 'Conj Transpose', M, M, M,
     $                     ONE, U1, LDU1, A, LDA, ONE, RES, LDRES )
               TEMP = DLAPY2( TEMP, ZLANGE( 'Frobenius', M, M, RES,
     $                                      LDRES, DWORK ) )
               WRITE ( NOUT, FMT = 99989 ) TEMP
            END IF
*
            IF ( .NOT.LSAME( JOB, 'E' ).AND.LSAME( JOBU, 'U' ) ) THEN
               WRITE ( NOUT, FMT = 99993 )
               DO 160  I = 1, M
                  WRITE ( NOUT, FMT = 99992 )
     $               ( U1(I,J), J = 1,M ), ( U2(I,J), J = 1,M )
  160          CONTINUE
               DO 170  I = 1, M
                  WRITE ( NOUT, FMT = 99992 )
     $               ( -U2(I,J), J = 1,M ), ( U1(I,J), J = 1,M )
  170          CONTINUE
               WRITE ( NOUT, FMT = 99988 ) MA02JZ( .FALSE., .FALSE., M,
     $                 U1, LDU1, U2, LDU2, RES, LDRES )
            END IF
            IF ( LSAME( BALANC, 'S' ).OR.LSAME( BALANC, 'B' ) ) THEN
               WRITE ( NOUT, FMT = 99991 )
               DO 180  I = 1, N
                  WRITE ( NOUT, FMT = 99996 ) I, SCALE(I)
  180          CONTINUE
            END IF
         END IF
      END IF
*
99999 FORMAT (' MB03XZ EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MB03XZ = ',I2)
99997 FORMAT (' The eigenvalues are',//'   i',6X,
     $        'WR(i)',6X,'WI(i)',/)
99996 FORMAT (I4,3X,F8.4,3X,F8.4)
99995 FORMAT (/' The transformed matrix S is')
99994 FORMAT (/' The transformed matrix G is')
99993 FORMAT (/' The unitary symplectic factor U is')
99992 FORMAT (20(1X,F9.4,SP,F9.4,S,'i '))
99991 FORMAT (/' The diagonal scaling factors are ',//'   i',6X,
     $        'SCALE(i)',/)
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' Residual: || i*He*U - U*Hc ||_F = ',G9.2)
99988 FORMAT (/' Orthogonality of U: || U^H U - I ||_F = ',G9.2)
      END
