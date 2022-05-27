*     AB05QD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          N1MAX, N2MAX, NMAX, M1MAX, M2MAX, MMAX, P1MAX,
     $                 P2MAX, PMAX
      PARAMETER        ( N1MAX = 20, N2MAX = 20, NMAX = N1MAX+N2MAX,
     $                   M1MAX = 20, M2MAX = 20, MMAX = M1MAX+M2MAX,
     $                   P1MAX = 20, P2MAX = 20, PMAX = P1MAX+P2MAX )
      INTEGER          LDA, LDA1, LDA2, LDB, LDB1, LDB2, LDC, LDC1,
     $                 LDC2, LDD, LDD1, LDD2
      PARAMETER        ( LDA = NMAX, LDA1 = N1MAX, LDA2 = N2MAX,
     $                   LDB = NMAX, LDB1 = N1MAX, LDB2 = N2MAX,
     $                   LDC = PMAX, LDC1 = P1MAX, LDC2 = P1MAX,
     $                   LDD = PMAX, LDD1 = P1MAX, LDD2 = P1MAX )
      DOUBLE PRECISION ONE
      PARAMETER        ( ONE=1.0D0 )
*     .. Local Scalars ..
      CHARACTER*1      OVER
      INTEGER          I, INFO, J, M, M1, M2, N, N1, N2, P, P1, P2
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), A1(LDA1,N1MAX), A2(LDA2,N2MAX),
     $                 B(LDB,MMAX), B1(LDB1,M1MAX), B2(LDB2,M2MAX),
     $                 C(LDC,NMAX), C1(LDC1,N1MAX), C2(LDC2,N2MAX),
     $                 D(LDD,MMAX), D1(LDD1,M1MAX), D2(LDD2,M2MAX)
*     .. External Subroutines ..
      EXTERNAL         AB05QD
*     .. Executable Statements ..
*
      OVER = 'N'
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N1, M1, P1, N2, M2, P2
      IF ( N1.LE.0 .OR. N1.GT.N1MAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N1
      ELSE
         READ ( NIN, FMT = * ) ( ( A1(I,J), J = 1,N1 ), I = 1,N1 )
         IF ( M1.LE.0 .OR. M1.GT.M1MAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) M1
         ELSE
            READ ( NIN, FMT = * ) ( ( B1(I,J), I = 1,N1 ), J = 1,M1 )
            IF ( P1.LE.0 .OR. P1.GT.P1MAX ) THEN
               WRITE ( NOUT, FMT = 99990 ) P1
            ELSE
               READ ( NIN, FMT = * ) ( ( C1(I,J), J = 1,N1 ), I = 1,P1 )
               READ ( NIN, FMT = * ) ( ( D1(I,J), J = 1,M1 ), I = 1,P1 )
               IF ( N2.LE.0 .OR. N2.GT.N2MAX ) THEN
                  WRITE ( NOUT, FMT = 99989 ) N2
               ELSE
                  READ ( NIN, FMT = * )
     $                 ( ( A2(I,J), J = 1,N2 ), I = 1,N2 )
                  IF ( M2.LE.0 .OR. M2.GT.M2MAX ) THEN
                     WRITE ( NOUT, FMT = 99988 ) M2
                  ELSE
                     READ ( NIN, FMT = * )
     $                    ( ( B2(I,J), I = 1,N2 ), J = 1,M2 )
                     IF ( P2.LE.0 .OR. P2.GT.P2MAX ) THEN
                        WRITE ( NOUT, FMT = 99987 ) P2
                     ELSE
                        READ ( NIN, FMT = * )
     $                       ( ( C2(I,J), J = 1,N2 ), I = 1,P2 )
                        READ ( NIN, FMT = * )
     $                       ( ( D2(I,J), J = 1,M2 ), I = 1,P2 )
*                          Find the state-space model (A,B,C,D).
                        CALL AB05QD( OVER, N1, M1, P1, N2, M2, P2, A1,
     $                               LDA1, B1, LDB1, C1, LDC1, D1, LDD1,
     $                               A2, LDA2, B2, LDB2, C2, LDC2, D2,
     $                               LDD2, N, M, P, A, LDA, B, LDB, C,
     $                               LDC, D, LDD, INFO )
*
                        IF ( INFO.NE.0 ) THEN
                           WRITE ( NOUT, FMT = 99998 ) INFO
                        ELSE
                           WRITE ( NOUT, FMT = 99997 )
                           DO 20 I = 1, N
                              WRITE ( NOUT, FMT = 99996 )
     $                              ( A(I,J), J = 1,N )
   20                      CONTINUE
                           WRITE ( NOUT, FMT = 99995 )
                           DO 40 I = 1, N
                              WRITE ( NOUT, FMT = 99996 )
     $                              ( B(I,J), J = 1,M )
   40                      CONTINUE
                           WRITE ( NOUT, FMT = 99994 )
                           DO 60 I = 1, P
                              WRITE ( NOUT, FMT = 99996 )
     $                              ( C(I,J), J = 1,N )
   60                      CONTINUE
                           WRITE ( NOUT, FMT = 99993 )
                           DO 80 I = 1, P
                              WRITE ( NOUT, FMT = 99996 )
     $                              ( D(I,J), J = 1,M )
   80                      CONTINUE
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB05QD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB05QD = ',I2)
99997 FORMAT (' The state transition matrix of the connected system is')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' The input/state matrix of the connected system is ')
99994 FORMAT (/' The state/output matrix of the connected system is ')
99993 FORMAT (/' The input/output matrix of the connected system is ')
99992 FORMAT (/' N1 is out of range.',/' N1 = ',I5)
99991 FORMAT (/' M1 is out of range.',/' M1 = ',I5)
99990 FORMAT (/' P1 is out of range.',/' P1 = ',I5)
99989 FORMAT (/' N2 is out of range.',/' N2 = ',I5)
99988 FORMAT (/' M2 is out of range.',/' M2 = ',I5)
99987 FORMAT (/' P2 is out of range.',/' P2 = ',I5)
      END
