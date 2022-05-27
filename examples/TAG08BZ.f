*     AG08BZ EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          LMAX, MMAX, NMAX, PMAX
      PARAMETER        ( LMAX = 20, MMAX = 20, NMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDAEMX, LDB, LDC, LDD, LDE, LDQ, LDZ
      PARAMETER        ( LDA = LMAX, LDB = LMAX, LDC = PMAX,
     $                   LDD = PMAX, LDE = LMAX, LDQ = 1, LDZ = 1,
     $                   LDAEMX = MAX( PMAX + LMAX, NMAX + MMAX ) )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( 4*( LMAX + NMAX ), 2*LDAEMX,
     $                                 8*NMAX ) )
      INTEGER          LZWORK
      PARAMETER        ( LZWORK = MAX( 1, LDAEMX*LDAEMX +
     $                                 MAX( MIN( LMAX+PMAX, MMAX+NMAX )+
     $                                      MAX( MIN( LMAX, NMAX ),
     $                                           3*( MMAX+NMAX )-1 ),
     $                                      3*( LMAX+PMAX ) ) ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          DINFZ, I, INFO, J, L, M, N, NFZ, NINFE, NIZ,
     $                 NKROL, NKROR, NRANK, P
      CHARACTER*1      EQUIL
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), ALPHA(NMAX), ASAVE(LDA,NMAX),
     $                 B(LDB,MMAX),  BETA(NMAX), BSAVE(LDB,MMAX),
     $                 C(LDC,NMAX), CSAVE(LDC,NMAX),
     $                 D(LDD,MMAX), DSAVE(LDD,MMAX),
     $                 E(LDE,NMAX), ESAVE(LDE,NMAX), Q(LDQ,1), Z(LDZ,1),
     $                 ZWORK(LZWORK)
      DOUBLE PRECISION DWORK(LDWORK)
      INTEGER          INFE(1+LMAX+PMAX), INFZ(NMAX+1),
     $                 IWORK(NMAX+MMAX), KRONL(LMAX+PMAX+1),
     $                 KRONR(NMAX+MMAX+1)
*     .. External Subroutines ..
      EXTERNAL         AG08BZ, ZGEGV, ZLACPY
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) L, N, M, P, TOL, EQUIL
      IF( ( L.LT.0 .OR. L.GT.LMAX ) .OR. ( N.LT.0 .OR. N.GT.NMAX ) )
     $   THEN
         WRITE ( NOUT, FMT = 99972 ) L, N
      ELSE
         IF( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99971 ) M
         ELSE
            IF( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99970 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,L )
               READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,L )
               READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,L )
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
               CALL ZLACPY( 'F', L, N, A, LDA, ASAVE, LDA )
               CALL ZLACPY( 'F', L, N, E, LDE, ESAVE, LDE )
               CALL ZLACPY( 'F', L, M, B, LDB, BSAVE, LDB )
               CALL ZLACPY( 'F', P, N, C, LDC, CSAVE, LDC )
               CALL ZLACPY( 'F', P, M, D, LDD, DSAVE, LDD )
*              Compute poles (call the routine with M = 0, P = 0).
               CALL AG08BZ( EQUIL, L, N, 0, 0, A, LDA, E, LDE, B, LDB,
     $                      C, LDC, D, LDD, NFZ, NRANK, NIZ, DINFZ,
     $                      NKROR, NINFE, NKROL, INFZ, KRONR, INFE,
     $                      KRONL, TOL, IWORK, DWORK, ZWORK, LZWORK,
     $                      INFO )
*
               IF( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99968 ) NIZ
                  DO 10 I = 1, DINFZ
                     WRITE ( NOUT, FMT = 99967 ) INFZ(I), I
   10             CONTINUE
                  WRITE ( NOUT, FMT = 99962 ) NINFE
                  IF( NINFE.GT.0 ) WRITE ( NOUT, FMT = 99958 )
     $                                      ( INFE(I), I = 1,NINFE )
                  IF( NFZ.EQ.0 ) THEN
                     WRITE ( NOUT, FMT = 99965 )
                  ELSE
                     WRITE ( NOUT, FMT = 99966 )
                     WRITE ( NOUT, FMT = 99990 )
                     DO 20 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( A(I,J), J = 1,NFZ )
   20                CONTINUE
                     WRITE ( NOUT, FMT = 99995 )
                     DO 30 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( E(I,J), J = 1,NFZ )
   30                CONTINUE
                     CALL ZGEGV( 'No vectors', 'No vectors', NFZ, A,
     $                           LDA, E, LDE, ALPHA, BETA, Q, LDQ,
     $                           Z, LDZ, ZWORK, LZWORK, DWORK, INFO )
*
                     IF( INFO.NE.0 ) THEN
                        WRITE ( NOUT, FMT = 99997 ) INFO
                     ELSE
                        WRITE ( NOUT, FMT = 99996 )
                        DO 40 I = 1, NFZ
                           WRITE ( NOUT, FMT = 99979 ) ALPHA(I)/BETA(I)
   40                   CONTINUE
                     END IF
                  END IF
               END IF
               CALL ZLACPY( 'F', L, N, ASAVE, LDA, A, LDA )
               CALL ZLACPY( 'F', L, N, ESAVE, LDE, E, LDE )
*              Check the observability and compute the ordered set of
*              the observability indices (call the routine with M = 0).
               CALL AG08BZ( EQUIL, L, N, 0, P, A, LDA, E, LDE, B, LDB,
     $                      C, LDC, D, LDD, NFZ, NRANK, NIZ, DINFZ,
     $                      NKROR, NINFE, NKROL, INFZ, KRONR, INFE,
     $                      KRONL, TOL, IWORK, DWORK, ZWORK, LZWORK,
     $                      INFO )
*
               IF( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99964 ) NIZ
                  DO 50 I = 1, DINFZ
                     WRITE ( NOUT, FMT = 99967 ) INFZ(I), I
   50             CONTINUE
                  WRITE ( NOUT, FMT = 99962 ) NINFE
                  IF( NINFE.GT.0 ) WRITE ( NOUT, FMT = 99960 )
     $                                     ( INFE(I), I = 1,NINFE )
                  WRITE ( NOUT, FMT = 99994 ) ( KRONL(I), I = 1,NKROL )
                  IF( NFZ+NINFE.EQ.0 ) WRITE ( NOUT, FMT = 99993 )
                  IF( NFZ.EQ.0 ) THEN
                     WRITE ( NOUT, FMT = 99957 )
                  ELSE
                     WRITE ( NOUT, FMT = 99991 )
                     WRITE ( NOUT, FMT = 99990 )
                     DO 60 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( A(I,J), J = 1,NFZ )
   60                CONTINUE
                     WRITE ( NOUT, FMT = 99995 )
                     DO 70 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( E(I,J), J = 1,NFZ )
   70                CONTINUE
                     CALL ZGEGV( 'No vectors', 'No vectors', NFZ, A,
     $                           LDA, E, LDE, ALPHA, BETA, Q, LDQ,
     $                           Z, LDZ, ZWORK, LZWORK, DWORK, INFO )
*
                     IF( INFO.NE.0 ) THEN
                        WRITE ( NOUT, FMT = 99997 ) INFO
                     ELSE
                        WRITE ( NOUT, FMT = 99996 )
                        DO 80 I = 1, NFZ
                           WRITE ( NOUT, FMT = 99979 ) ALPHA(I)/BETA(I)
   80                   CONTINUE
                     END IF
                  END IF
               END IF
               CALL ZLACPY( 'F', L, N, ASAVE, LDA, A, LDA )
               CALL ZLACPY( 'F', L, N, ESAVE, LDE, E, LDE )
               CALL ZLACPY( 'F', P, N, CSAVE, LDC, C, LDC )
*              Check the controllability and compute the ordered set of
*              the controllability indices (call the routine with P = 0)
               CALL AG08BZ( EQUIL, L, N, M, 0, A, LDA, E, LDE, B, LDB,
     $                      C, LDC, D, LDD, NFZ, NRANK, NIZ, DINFZ,
     $                      NKROR, NINFE, NKROL, INFZ, KRONR, INFE,
     $                      KRONL, TOL, IWORK, DWORK, ZWORK, LZWORK,
     $                      INFO )
*
               IF( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99963 ) NIZ
                  DO 90  I = 1, DINFZ
                     WRITE ( NOUT, FMT = 99967 ) INFZ(I), I
   90             CONTINUE
                  WRITE ( NOUT, FMT = 99962 ) NINFE
                  IF( NINFE.GT.0 ) WRITE ( NOUT, FMT = 99959 )
     $                                     ( INFE(I), I = 1,NINFE )
                  WRITE ( NOUT, FMT = 99988 ) ( KRONR(I), I = 1,NKROR )
                  IF( NFZ+NINFE.EQ.0 ) WRITE ( NOUT, FMT = 99987 )
                  IF( NFZ.EQ.0 ) THEN
                     WRITE ( NOUT, FMT = 99956 )
                  ELSE
                     WRITE ( NOUT, FMT = 99985 )
                     WRITE ( NOUT, FMT = 99990 )
                     DO 100 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( A(I,J), J = 1,NFZ )
  100                CONTINUE
                     WRITE ( NOUT, FMT = 99995 )
                     DO 110 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( E(I,J), J = 1,NFZ )
  110                CONTINUE
                     CALL ZGEGV( 'No vectors', 'No vectors', NFZ, A,
     $                           LDA, E, LDE, ALPHA, BETA, Q, LDQ,
     $                           Z, LDZ, ZWORK, LZWORK, DWORK, INFO )
*
                     IF( INFO.NE.0 ) THEN
                        WRITE ( NOUT, FMT = 99997 ) INFO
                     ELSE
                        WRITE ( NOUT, FMT = 99982 )
                        DO 120 I = 1, NFZ
                           WRITE ( NOUT, FMT = 99979 ) ALPHA(I)/BETA(I)
  120                   CONTINUE
                     END IF
                  END IF
               END IF
               CALL ZLACPY( 'F', L, N, ASAVE, LDA, A, LDA )
               CALL ZLACPY( 'F', L, N, ESAVE, LDE, E, LDE )
               CALL ZLACPY( 'F', L, M, BSAVE, LDB, B, LDB )
               CALL ZLACPY( 'F', P, N, CSAVE, LDC, C, LDC )
               CALL ZLACPY( 'F', P, M, DSAVE, LDD, D, LDD )
*              Compute the structural invariants of the given system.
               CALL AG08BZ( EQUIL, L, N, M, P, A, LDA, E, LDE, B, LDB,
     $                      C, LDC, D, LDD, NFZ, NRANK, NIZ, DINFZ,
     $                      NKROR, NINFE, NKROL, INFZ, KRONR, INFE,
     $                      KRONL, TOL, IWORK, DWORK, ZWORK, LZWORK,
     $                      INFO )
*
               IF( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF( L.EQ.N ) THEN
                     WRITE ( NOUT, FMT = 99969 ) NRANK - N
                  ELSE
                     WRITE ( NOUT, FMT = 99955 ) NRANK
                  END IF
                  WRITE ( NOUT, FMT = 99984 ) NFZ
                  IF( NFZ.GT.0 ) THEN
*                    Compute the finite zeros of the given system.
*                    Workspace: need 8*NFZ.
                     WRITE ( NOUT, FMT = 99983 )
                     WRITE ( NOUT, FMT = 99990 )
                     DO 130 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( A(I,J), J = 1,NFZ )
  130                CONTINUE
                     WRITE ( NOUT, FMT = 99995 )
                     DO 140 I = 1, NFZ
                        WRITE ( NOUT, FMT = 99989 )
     $                        ( E(I,J), J = 1,NFZ )
  140                CONTINUE
                     CALL ZGEGV( 'No vectors', 'No vectors', NFZ, A,
     $                           LDA, E, LDE, ALPHA, BETA, Q, LDQ,
     $                           Z, LDZ, ZWORK, LZWORK, DWORK, INFO )
*
                     IF( INFO.NE.0 ) THEN
                        WRITE ( NOUT, FMT = 99997 ) INFO
                     ELSE
                        WRITE ( NOUT, FMT = 99981 )
                        DO 150 I = 1, NFZ
                           WRITE ( NOUT, FMT = 99979 ) ALPHA(I)/BETA(I)
  150                   CONTINUE
                     END IF
                  END IF
                  WRITE ( NOUT, FMT = 99978 ) NIZ
                  DO 160 I = 1, DINFZ
                     WRITE ( NOUT, FMT = 99977 ) INFZ(I), I
  160             CONTINUE
                  WRITE ( NOUT, FMT = 99962 ) NINFE
                  IF( NINFE.GT.0 ) WRITE ( NOUT, FMT = 99961 )
     $                                     ( INFE(I), I = 1,NINFE )
                  WRITE ( NOUT, FMT = 99976 ) NKROR
                  IF( NKROR.GT.0 ) WRITE ( NOUT, FMT = 99975 )
     $                                     ( KRONR(I), I = 1,NKROR )
                  WRITE ( NOUT, FMT = 99974 ) NKROL
                  IF( NKROL.GT.0 ) WRITE ( NOUT, FMT = 99973 )
     $                                     ( KRONL(I), I = 1,NKROL )
               END IF
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' AG08BZ EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AG08BZ = ',I2)
99997 FORMAT (' INFO on exit from ZGEGV = ',I2)
99996 FORMAT (/' Unobservable finite eigenvalues'/
     $         ' real  part     imag  part ')
99995 FORMAT (/' The matrix Ef is ')
99994 FORMAT (/' The left Kronecker indices of [A-lambda*E;C] are ',
     $          /(20(I3,2X)))
99993 FORMAT (/' The system (A-lambda*E,C) is completely observable ')
99991 FORMAT (/' The finite output decoupling zeros are the eigenvalues'
     $       , ' of the pair (Af,Ef). ')
99990 FORMAT (/' The matrix Af is ')
99989 FORMAT (20(1X,F9.4,SP,F9.4,S,'i '))
99988 FORMAT (/' The right Kronecker indices of [A-lambda*E,B] are ',
     $        /( 20(I3,2X) ) )
99987 FORMAT (/' The system (A-lambda*E,B) is completely controllable ')
99985 FORMAT (/' The input decoupling zeros are the eigenvalues of the',
     $         ' pair (Af,Ef). ')
99984 FORMAT (/' The number of finite zeros = ',I3)
99983 FORMAT (/' The finite zeros are the eigenvalues ',
     $         'of the pair (Af,Ef)')
99982 FORMAT (/' Uncontrollable finite eigenvalues'/
     $         ' real  part     imag  part ')
99981 FORMAT (/' Finite zeros'/' real  part     imag  part ')
99979 FORMAT (1X,F9.4,SP,F9.4,S,'i ')
99978 FORMAT (//' The number of infinite zeros = ',I3)
99977 FORMAT ( I4,' infinite zero(s) of order ',I3)
99976 FORMAT (/' The number of right Kronecker indices = ',I3)
99975 FORMAT (/' Right Kronecker indices of [A-lambda*E,B;C,D]'
     $         ,' are ', /(20(I3,2X)))
99974 FORMAT (/' The number of left Kronecker indices = ',I3)
99973 FORMAT (/' The left Kronecker indices of [A-lambda*E,B;C,D]'
     $         ,' are ',  /(20(I3,2X)))
99972 FORMAT (/' L or N is out of range.',/' L = ', I5, '  N = ',I5)
99971 FORMAT (/' M is out of range.',/' M = ',I5)
99970 FORMAT (/' P is out of range.',/' P = ',I5)
99969 FORMAT (/' Normal rank  of transfer function matrix = ',I3)
99968 FORMAT (//' The number of infinite poles = ',I3)
99967 FORMAT ( I4,' infinite pole(s) of order ',I3)
99966 FORMAT (/' The finite poles are the eigenvalues',
     $         ' of the pair (Af,Ef). ')
99965 FORMAT (/' The system has no finite poles ')
99964 FORMAT (//' The number of unobservable infinite poles = ',I3)
99963 FORMAT (//' The number of uncontrollable infinite poles = ',I3)
99962 FORMAT (/' The number of infinite Kronecker blocks = ',I3)
99961 FORMAT (/' Multiplicities of infinite eigenvalues of '
     $         ,'[A-lambda*E,B;C,D] are ', /(20(I3,2X)))
99960 FORMAT (/' Multiplicities of infinite eigenvalues of '
     $         ,'[A-lambda*E;C] are ', /(20(I3,2X)))
99959 FORMAT (/' Multiplicities of infinite eigenvalues of '
     $         ,'[A-lambda*E,B] are ', /(20(I3,2X)))
99958 FORMAT (/' Multiplicities of infinite eigenvalues of A-lambda*E'
     $         ,' are ', /(20(I3,2X)))
99957 FORMAT (/' The system (A-lambda*E,C) has no finite output',
     $         ' decoupling zeros ')
99956 FORMAT (/' The system (A-lambda*E,B) has no finite input',
     $         ' decoupling zeros ')
99955 FORMAT (/' Normal rank  of system pencil = ',I3)
      END
