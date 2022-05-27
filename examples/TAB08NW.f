*     AB08NW EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          MPMAX
      PARAMETER        ( MPMAX = MAX( MMAX, PMAX ) )
      INTEGER          LDA, LDB, LDC, LDD, LDE, LDQ, LDZ
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX, LDE = NMAX, LDQ = 1, LDZ = 1 )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( MAX( MIN( PMAX, MMAX ) + MMAX +
     $                                      MAX( 2*MMAX, NMAX ) - 1,
     $                                      MIN( PMAX, NMAX ) +
     $                                      MAX( NMAX + MPMAX,
     $                                           3*PMAX - 1 ) ) +
     $                                 ( NMAX + MPMAX )**2, 8*NMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          DINFZ, I, INFO, J, M, N, NFZ, NINFE, NIZ, NKROL,
     $                 NKROR, NRANK, P
      CHARACTER*1      EQUIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), ALFI(NMAX), ALFR(NMAX),
     $                 AS(LDA,NMAX), B(LDB,MMAX), BS(LDB,MMAX),
     $                 BETA(NMAX), C(LDC,NMAX), CS(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK), E(LDE,NMAX),
     $                 Q(LDQ,1), Z(LDZ,1)
      INTEGER          INFE(NMAX+1), INFZ(NMAX+1), IWORK(MPMAX),
     $                 KRONL(NMAX+1), KRONR(NMAX+1)
*     .. External Subroutines ..
      EXTERNAL         AB08NW, DLACPY, DGEGV
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL, EQUIL
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99972 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99971 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99970 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*              Save the matrices A, B, and C.
               CALL DLACPY( 'Full', N, N, A, LDA, AS, LDA )
               CALL DLACPY( 'Full', N, M, B, LDB, BS, LDB )
               CALL DLACPY( 'Full', P, N, C, LDC, CS, LDC )
*              Check the observability and compute the ordered set of
*              the observability indices (call the routine with M = 0).
               CALL AB08NW( EQUIL, N, 0, P, A, LDA, B, LDB, C, LDC, D,
     $                      LDD, NFZ, NRANK, NIZ, DINFZ, NKROR, NINFE,
     $                      NKROL, INFZ, KRONR, INFE, KRONL, E, LDE,
     $                      TOL, IWORK, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99994 ) ( KRONL(I), I = 1,P )
                  IF ( NFZ.EQ.0 ) THEN
                     WRITE ( NOUT, FMT = 99993 )
                  ELSE
                     WRITE ( NOUT, FMT = 99992 ) N - NFZ
                     WRITE ( NOUT, FMT = 99991 )
                     WRITE ( NOUT, FMT = 99990 )
                     DO 20 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( A(I,J), J = 1,NFZ )
   20                CONTINUE
                  END IF
               END IF
*              Restore the matrices A and C.
               CALL DLACPY( 'Full', N, N, AS, LDA, A, LDA )
               CALL DLACPY( 'Full', P, N, CS, LDC, C, LDC )
*              Check the controllability and compute the ordered set of
*              the controllability indices (call the routine with P = 0)
               CALL AB08NW( EQUIL, N, M, 0, A, LDA, B, LDB, C, LDC, D,
     $                      LDD, NFZ, NRANK, NIZ, DINFZ, NKROR, NINFE,
     $                      NKROL, INFZ, KRONR, INFE, KRONL, E, LDE,
     $                      TOL, IWORK, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99988 ) ( KRONR(I), I = 1,M )
                  IF ( NFZ.EQ.0 ) THEN
                     WRITE ( NOUT, FMT = 99987 )
                  ELSE
                     WRITE ( NOUT, FMT = 99986 ) N - NFZ
                     WRITE ( NOUT, FMT = 99985 )
                     WRITE ( NOUT, FMT = 99990 )
                     DO 40 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( A(I,J), J = 1,NFZ )
   40                CONTINUE
                  END IF
               END IF
*              Restore the matrices A and B.
               CALL DLACPY( 'Full', N, N, AS, LDA, A, LDA )
               CALL DLACPY( 'Full', N, M, BS, LDB, B, LDB )
*              Compute the structural invariants of the given system.
               CALL AB08NW( EQUIL, N, M, P, A, LDA, B, LDB, C, LDC, D,
     $                      LDD, NFZ, NRANK, NIZ, DINFZ, NKROR, NINFE,
     $                      NKROL, INFZ, KRONR, INFE, KRONL, E, LDE,
     $                      TOL, IWORK, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99984 ) NFZ
                  IF ( NFZ.GT.0 ) THEN
*                    Compute the invariant zeros of the given system.
*                    Workspace: need 8*NFZ.
                     WRITE ( NOUT, FMT = 99983 )
                     CALL DGEGV( 'No vectors', 'No vectors', NFZ, A,
     $                           LDA, E, LDE, ALFR, ALFI, BETA, Q,
     $                           LDQ, Z, LDZ, DWORK, LDWORK, INFO )
*
                     IF ( INFO.NE.0 ) THEN
                        WRITE ( NOUT, FMT = 99997 ) INFO
                     ELSE
                        WRITE ( NOUT, FMT = 99981 )
                        DO 60 I = 1, NFZ
                           IF ( ALFI(I).EQ.ZERO ) THEN
                              WRITE ( NOUT, FMT = 99980 )
     $                                ALFR(I)/BETA(I)
                           ELSE
                              WRITE ( NOUT, FMT = 99979 )
     $                                ALFR(I)/BETA(I),
     $                                ALFI(I)/BETA(I)
                           END IF
   60                   CONTINUE
                        WRITE ( NOUT, FMT = 99982 )
                     END IF
                  END IF
                  WRITE ( NOUT, FMT = 99978 ) NIZ
                  IF ( NIZ.GT.0 ) THEN
                     DO 100 I = 1, DINFZ
                        WRITE ( NOUT, FMT = 99977 ) INFZ(I), I
  100                CONTINUE
                  END IF
                  WRITE ( NOUT, FMT = 99976 ) NKROR
                  IF ( NKROR.GT.0 ) WRITE ( NOUT, FMT = 99975 )
     $                                      ( KRONR(I), I = 1,NKROR )
                  WRITE ( NOUT, FMT = 99974 ) NKROL
                  IF ( NKROL.GT.0 ) WRITE ( NOUT, FMT = 99973 )
     $                                      ( KRONL(I), I = 1,NKROL )
                  IF ( NINFE.GT.0 ) WRITE ( NOUT, FMT = 99969 )
     $                                      ( INFE(I), I = 1,NINFE )
               END IF
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' AB08NW EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB08NW = ',I2)
99997 FORMAT (' INFO on exit from DGEGV = ',I2)
99994 FORMAT (' The left Kronecker indices of (A,C) are ',/(20(I3,2X)))
99993 FORMAT (/' The system (A,C) is completely observable ')
99992 FORMAT (/' The dimension of the observable subspace = ',I3)
99991 FORMAT (/' The output decoupling zeros are the eigenvalues of th',
     $       'e matrix AF. ')
99990 FORMAT (/' The matrix AF is ')
99989 FORMAT (20(1X,F8.4))
99988 FORMAT (//' The right Kronecker indices of (A,B) are ',/(20(I3,2X)
     $       ))
99987 FORMAT (/' The system (A,B) is completely controllable ')
99986 FORMAT (/' The dimension of the controllable subspace = ',I3)
99985 FORMAT (/' The input decoupling zeros are the eigenvalues of the',
     $       ' matrix AF. ')
99984 FORMAT (//' The number of finite invariant zeros = ',I3)
99983 FORMAT (/' The finite invariant zeros are ')
99982 FORMAT (/' which correspond to the generalized eigenvalues of (l',
     $       'ambda*EF - AF).')
99981 FORMAT (/' real  part     imag  part ')
99980 FORMAT (1X,F9.4)
99979 FORMAT (1X,F9.4,6X,F9.4)
99978 FORMAT (//' The number of infinite zeros = ',I3)
99977 FORMAT ( I4,' infinite zero(s) of order ',I3)
99976 FORMAT (/' The number of right Kronecker indices = ',I3)
99975 FORMAT (/' Right Kronecker (column) indices of (A,B,C,D) are ',
     $       /(20(I3,2X)))
99974 FORMAT (/' The number of left Kronecker indices = ',I3)
99973 FORMAT (/' The left Kronecker (row) indices of (A,B,C,D) are ',
     $       /(20(I3,2X)))
99972 FORMAT (/' N is out of range.',/' N = ',I5)
99971 FORMAT (/' M is out of range.',/' M = ',I5)
99970 FORMAT (/' P is out of range.',/' P = ',I5)
99969 FORMAT (/' Multiplicities of infinite eigenvalues',/(20(I3,2X))) 
      END
