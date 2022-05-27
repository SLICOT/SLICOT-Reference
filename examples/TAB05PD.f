*     AB05PD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          N1MAX, N2MAX, NMAX, MMAX, PMAX
      PARAMETER        ( N1MAX = 20, N2MAX = 20, NMAX = N1MAX+N2MAX,
     $                   MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDA1, LDA2, LDB, LDB1, LDB2, LDC, LDC1,
     $                 LDC2, LDD, LDD1, LDD2
      PARAMETER        ( LDA = NMAX, LDA1 = N1MAX, LDA2 = N2MAX,
     $                   LDB = NMAX, LDB1 = N1MAX, LDB2 = N2MAX,
     $                   LDC = PMAX, LDC1 = PMAX,  LDC2 = PMAX,
     $                   LDD = PMAX, LDD1 = PMAX,  LDD2 = PMAX )
*     .. Local Scalars ..
      CHARACTER*1      OVER
      INTEGER          I, INFO, J, M, N, N1, N2, P
      DOUBLE PRECISION ALPHA
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), A1(LDA1,N1MAX), A2(LDA2,N2MAX),
     $                 B(LDB,MMAX), B1(LDB1,MMAX), B2(LDB2,MMAX),
     $                 C(LDC,NMAX), C1(LDC1,N1MAX), C2(LDC2,N2MAX),
     $                 D(LDD,MMAX), D1(LDD1,MMAX), D2(LDD2,MMAX)
*     .. External Subroutines ..
      EXTERNAL         AB05PD
*     .. Executable Statements ..
*
      OVER = 'N'
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N1, M, P, N2, ALPHA
      IF ( N1.LE.0 .OR. N1.GT.N1MAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N1
      ELSE
         READ ( NIN, FMT = * ) ( ( A1(I,J), J = 1,N1 ), I = 1,N1 )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B1(I,J), I = 1,N1 ), J = 1,M )
            IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99990 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C1(I,J), J = 1,N1 ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D1(I,J), J = 1,M ), I = 1,P )
               IF ( N2.LE.0 .OR. N2.GT.N2MAX ) THEN
                  WRITE ( NOUT, FMT = 99989 ) N2
               ELSE
                  READ ( NIN, FMT = * )
     $                 ( ( A2(I,J), J = 1,N2 ), I = 1,N2 )
                  READ ( NIN, FMT = * )
     $                 ( ( B2(I,J), I = 1,N2 ), J = 1,M )
                  READ ( NIN, FMT = * )
     $                 ( ( C2(I,J), J = 1,N2 ), I = 1,P )
                  READ ( NIN, FMT = * )
     $                 ( ( D2(I,J), J = 1,M ), I = 1,P )
*                    Find the state-space model (A,B,C,D).
                  CALL AB05PD( OVER, N1, M, P, N2, ALPHA, A1, LDA1, B1,
     $                         LDB1, C1, LDC1, D1, LDD1, A2, LDA2, B2,
     $                         LDB2, C2, LDC2, D2, LDD2, N, A, LDA, B,
     $                         LDB, C, LDC, D, LDD, INFO )
*
                  IF ( INFO.NE.0 ) THEN
                     WRITE ( NOUT, FMT = 99998 ) INFO
                  ELSE
                     WRITE ( NOUT, FMT = 99997 )
                     DO 20 I = 1, N
                        WRITE ( NOUT, FMT = 99996 )
     $                        ( A(I,J), J = 1,N )
   20                CONTINUE
                     WRITE ( NOUT, FMT = 99995 )
                     DO 40 I = 1, N
                        WRITE ( NOUT, FMT = 99996 )
     $                        ( B(I,J), J = 1,M )
   40                CONTINUE
                     WRITE ( NOUT, FMT = 99994 )
                     DO 60 I = 1, P
                        WRITE ( NOUT, FMT = 99996 )
     $                        ( C(I,J), J = 1,N )
   60                CONTINUE
                     WRITE ( NOUT, FMT = 99993 )
                     DO 80 I = 1, P
                        WRITE ( NOUT, FMT = 99996 )
     $                        ( D(I,J), J = 1,M )
   80                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB05PD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB05PD = ',I2)
99997 FORMAT (' The state transition matrix of the connected system is')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The input/state matrix of the connected system is ')
99994 FORMAT (/' The state/output matrix of the connected system is ')
99993 FORMAT (/' The input/output matrix of the connected system is ')
99992 FORMAT (/' N1 is out of range.',/' N1 = ',I5)
99991 FORMAT (/' M is out of range.',/' M = ',I5)
99990 FORMAT (/' P is out of range.',/' P = ',I5)
99989 FORMAT (/' N2 is out of range.',/' N2 = ',I5)
      END
