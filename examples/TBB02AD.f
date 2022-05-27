*     BB02AD EXAMPLE PROGRAM TEXT
*
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX, PMAX
      PARAMETER        ( MMAX = 100, NMAX = 100, PMAX = 100 )
      INTEGER          LDA, LDB, LDC, LDQ, LDR, LDS, LDX
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDQ = NMAX, LDR = NMAX, LDS = NMAX,
     $                   LDX = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX*NMAX )
*     .. Local Scalars ..
      CHARACTER        DEF
      INTEGER          I, INFO, ISYMM, J, LBPAR, LDPAR, LIPAR, M, N, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA, NMAX), B(LDB,MMAX), C(LDC, NMAX),
     $                 DPAR(4), DWORK(LDWORK), Q(LDQ, NMAX),
     $                 R(LDR, NMAX), S(LDS, NMAX), X(LDX, NMAX)
      INTEGER          IPAR(3), NR(2)
      LOGICAL          BPAR(7), VEC(10)
      CHARACTER        CHPAR(255)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         BB02AD, MA02DD
*     .. Executable Statements ..
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ( NIN, FMT = '()' )
      READ( NIN, FMT = * ) DEF
      READ( NIN, FMT = * ) ( NR(I), I = 1, 2 )
      IF( LSAME( DEF, 'N' ) ) THEN
        READ( NIN, FMT = * ) LBPAR
        IF( LBPAR.GT.0 ) READ( NIN, FMT = * ) ( BPAR(I), I = 1, LBPAR )
        READ( NIN, FMT = * ) LDPAR
        IF( LDPAR.GT.0 ) READ( NIN, FMT = * ) ( DPAR(I), I = 1, LDPAR )
        READ( NIN, FMT = * ) LIPAR
        IF( LIPAR.GT.0 ) READ( NIN, FMT = * ) ( IPAR(I), I = 1, LIPAR )
      END IF
*     Generate benchmark example
      CALL BB02AD( DEF, NR, DPAR, IPAR, BPAR, CHPAR, VEC, N, M, P, A,
     $             LDA, B, LDB, C, LDC, Q, LDQ, R, LDR, S, LDS, X, LDX,
     $             DWORK, LDWORK, INFO )
*
      IF( INFO.NE.0 ) THEN
        WRITE( NOUT, FMT = 99998 ) INFO
      ELSE
        WRITE( NOUT, FMT = * ) CHPAR(1:70)
        WRITE( NOUT, FMT = 99997 ) N
        WRITE( NOUT, FMT = 99996 ) M
        WRITE( NOUT, FMT = 99995 ) P
        WRITE( NOUT, FMT = 99994 )
        DO 10  I = 1, N
          WRITE( NOUT, FMT = 99977 ) ( A(I,J), J = 1, N )
   10   CONTINUE
        IF( VEC(5) ) THEN
          WRITE( NOUT, FMT = 99993 )
          DO 20  I = 1, N
            WRITE( NOUT, FMT = 99977 ) ( B(I,J), J = 1, M )
   20     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99992 )
        END IF
        IF( VEC(6) ) THEN
          WRITE( NOUT,FMT = 99991 )
          DO 30  I = 1, P
            WRITE( NOUT, FMT = 99977 ) ( C(I,J), J = 1, N )
   30     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99990 )
        END IF
        IF( .NOT.VEC(5) ) THEN
          WRITE( NOUT, FMT = 99989 )
          IF( .NOT.BPAR(2) ) THEN
            ISYMM = ( N * ( N + 1 ) ) / 2
            CALL DCOPY( ISYMM, R, 1, DWORK, 1 )
            IF( BPAR(3) ) THEN
              CALL MA02DD( 'Unpack', 'Upper', N, R, LDR, DWORK )
            ELSE
              CALL MA02DD( 'Unpack', 'Lower', N, R, LDR, DWORK )
            END IF
          END IF
          DO 40  I = 1, N
            WRITE( NOUT, FMT = 99977 ) ( R(I,J), J = 1, N )
   40     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99988 )
        END IF
        IF( .NOT.VEC(6) ) THEN
          IF( .NOT.BPAR(5) ) THEN
            ISYMM = ( N * ( N + 1 ) ) / 2
            CALL DCOPY( ISYMM, Q, 1, DWORK, 1 )
            IF( BPAR(6) ) THEN
              CALL MA02DD( 'Unpack', 'Upper', N, Q, LDQ, DWORK )
            ELSE
              CALL MA02DD( 'Unpack', 'Lower', N, Q, LDQ, DWORK )
            END IF
          END IF
          WRITE( NOUT, FMT = 99987 )
          DO 50  I = 1, N
            WRITE( NOUT, FMT = 99977 ) ( Q(I,J), J = 1, N )
   50     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99986 )
        END IF
        IF( VEC(6) ) THEN
          IF( .NOT.BPAR(5) ) THEN
            ISYMM = ( P * ( P + 1 ) ) / 2
            CALL DCOPY( ISYMM, Q, 1, DWORK, 1 )
            IF( BPAR(6) ) THEN
              CALL MA02DD( 'Unpack', 'Upper', P, Q, LDQ, DWORK )
            ELSE
              CALL MA02DD( 'Unpack', 'Lower', P, Q, LDQ, DWORK )
            END IF
          END IF
          WRITE( NOUT, FMT = 99985 )
          DO 60  I = 1, P
            WRITE( NOUT, FMT = 99977 ) ( Q(I,J), J = 1, P )
   60     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99984 )
        END IF
        IF( VEC(5) ) THEN
          IF( .NOT.BPAR(2) ) THEN
            ISYMM = ( M * ( M + 1 ) ) / 2
            CALL DCOPY( ISYMM, R, 1, DWORK, 1 )
            IF( BPAR(3) ) THEN
              CALL MA02DD( 'Unpack', 'Upper', M, R, LDR, DWORK )
            ELSE
              CALL MA02DD( 'Unpack', 'Lower', M, R, LDR, DWORK )
            END IF
          END IF
          WRITE( NOUT, FMT = 99983 )
          DO 70  I = 1, M
            WRITE( NOUT, FMT = 99977 ) ( R(I,J), J = 1, M )
   70     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99982 )
        END IF
        IF( VEC(9) ) THEN
          WRITE( NOUT, FMT = 99981 )
          DO 80  I = 1, N
            WRITE( NOUT, FMT = 99977 ) ( S(I,J), J = 1, M )
   80     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99980 )
        END IF
        IF( VEC(10) ) THEN
          WRITE( NOUT, FMT = 99979 )
          DO 90  I = 1, N
            WRITE( NOUT, FMT = 99977 ) ( X(I,J), J = 1, N )
   90     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99978 )
        END IF
      END IF
      STOP
*
99999 FORMAT (' BB02AD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' INFO on exit from BB02AD = ', I3)
99997 FORMAT (/' Order of matrix A:              N  = ', I3)
99996 FORMAT (' Number of columns in matrix B:  M  = ', I3)
99995 FORMAT (' Number of rows in matrix C:     P  = ', I3)
99994 FORMAT (' A  = ')
99993 FORMAT (' B  = ')
99992 FORMAT (' B is not provided.')
99991 FORMAT (' C  = ')
99990 FORMAT (' C is not provided.')
99989 FORMAT (' G  = ')
99988 FORMAT (' G is not provided.')
99987 FORMAT (' Q  = ')
99986 FORMAT (' Q is not provided.')
99985 FORMAT (' Q0  = ')
99984 FORMAT (' Q0 is not provided.')
99983 FORMAT (' R  = ')
99982 FORMAT (' R is not provided.')
99981 FORMAT (' S  = ')
99980 FORMAT (' S is not provided.')
99979 FORMAT (' X  = ')
99978 FORMAT (' X is not provided.')
99977 FORMAT (20(1X,F8.4))
*
      END
