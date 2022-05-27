*     MB04ZD EXAMPLE PROGRAM TEXT.
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX
      PARAMETER        ( NMAX = 20 )
      INTEGER          LDA, LDQG, LDU
      PARAMETER        ( LDA = NMAX, LDQG = NMAX, LDU = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = ( NMAX+NMAX )*( NMAX+NMAX+1 ) )
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
*     .. Local Scalars ..
      INTEGER          I, INFO, IJ, J, JI, N, POS, WPOS
      CHARACTER*1      COMPU
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DWORK(LDWORK), QG(LDQG,NMAX+1),
     $                 U(LDU,NMAX)
*     .. External Subroutines ..
      EXTERNAL         DCOPY, DGEMM, DSYMV, MB04ZD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, COMPU
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99998 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J),    J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( QG(J,I+1), I = J,N ), J = 1,N )
         READ ( NIN, FMT = * ) ( ( QG(I,J),   I = J,N ), J = 1,N )
*        Square-reduce by symplectic orthogonal similarity.
         CALL MB04ZD( COMPU, N, A, LDA, QG, LDQG, U, LDU, DWORK, INFO )
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99997 ) INFO
         ELSE
*           Show the square-reduced Hamiltonian.
            WRITE ( NOUT, FMT = 99996 )
            DO 10 I = 1, N
               WRITE ( NOUT, FMT = 99994 )  ( A(I,J),    J = 1,N ),
     $            ( QG(J,I+1), J = 1,I-1 ), ( QG(I,J+1), J = I,N )
10          CONTINUE
            DO 20 I = 1, N
               WRITE ( NOUT, FMT = 99994 ) ( QG(I,J), J = 1,I-1 ),
     $               ( QG(J,I), J = I,N ), ( -A(J,I), J = 1,N )
20          CONTINUE
*           Show the square of H.
            WRITE ( NOUT, FMT = 99995 )
            WPOS = ( NMAX+NMAX )*( NMAX+NMAX )
*                                                    T
*           Compute N11 = A*A + G*Q and set N22 = N11 .
            CALL DGEMM( 'N', 'N', N, N, N, ONE, A, LDA, A, LDA, ZERO,
     $                  DWORK, N+N )
            DO 30 I = 1, N
               CALL DCOPY( N-I+1, QG(I,I), 1, DWORK(WPOS+I), 1 )
               CALL DCOPY( I-1, QG(I,1), LDQG, DWORK(WPOS+1), 1 )
               CALL DSYMV( 'U', N, ONE, QG(1,2), LDQG, DWORK(WPOS+1), 1,
     $                     ONE, DWORK((I-1)*(N+N)+1), 1 )
               POS = N*( N+N ) + N + I
               CALL DCOPY( N, DWORK((I-1)*(N+N)+1), 1, DWORK(POS), N+N )
30          CONTINUE
            DO 40 I = 1, N
               CALL DSYMV( 'U', N, -ONE, QG(1,2), LDQG, A(I,1), LDA,
     $                     ZERO, DWORK((N+I-1)*(N+N)+1), 1 )
               CALL DSYMV( 'L', N, ONE, QG, LDQG, A(1,I), 1, ZERO,
     $                     DWORK((I-1)*(N+N)+N+1), 1 )
40          CONTINUE
            DO 60 J = 1, N
               DO 50 I = J, N
                  IJ = ( N+J-1 )*( N+N ) + I
                  JI = ( N+I-1 )*( N+N ) + J
                  DWORK(IJ) =  DWORK(IJ) - DWORK(JI)
                  DWORK(JI) = -DWORK(IJ)
                  IJ = N + I + ( J-1 )*( N+N )
                  JI = N + J + ( I-1 )*( N+N )
                  DWORK(IJ) =  DWORK(IJ) - DWORK(JI)
                  DWORK(JI) = -DWORK(IJ)
50             CONTINUE
60          CONTINUE
            DO 70 I = 1, N+N
               WRITE ( NOUT, FMT = 99994 )
     $               ( DWORK(I+(J-1)*(N+N) ), J = 1,N+N )
70          CONTINUE
         ENDIF
      END IF
      STOP
*
99999 FORMAT (' MB04ZD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (/' N is out of range.',/' N = ',I5)
99997 FORMAT (' INFO on exit from MB04ZD = ',I2)
99996 FORMAT (/' The square-reduced Hamiltonian is ')
99995 FORMAT (/' The square of the square-reduced Hamiltonian is ')
99994 FORMAT (1X,8(F10.4))
      END
