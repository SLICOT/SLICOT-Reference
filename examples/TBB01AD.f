*     BB01AD EXAMPLE PROGRAM TEXT
*
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          MMAX, NMAX, PMAX
      PARAMETER        ( MMAX = 100, NMAX = 100, PMAX = 100 )
      INTEGER          LDA, LDB, LDC, LDG, LDQ, LDX
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDG = NMAX, LDQ = NMAX, LDX = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX*MAX( 4, NMAX ) )
*     .. Local Scalars ..
      CHARACTER        DEF
      INTEGER          I, INFO, ISYMM, J, LBPAR, LDPAR, LIPAR, M, N, P
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA, NMAX), B(LDB,MMAX), C(LDC, NMAX),
     $                 DPAR(7), DWORK(LDWORK), G(LDG, NMAX),
     $                 Q(LDQ, NMAX), X(LDX, NMAX)
      INTEGER          IPAR(4), NR(2)
      LOGICAL          BPAR(6), VEC(9)
      CHARACTER        CHPAR*255
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         BB01AD, MA02DD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
      WRITE( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ( NIN, FMT = '()' )
      READ( NIN, FMT = * ) DEF
      READ( NIN, FMT = * ) ( NR(I), I = 1, 2 )
      IF( LSAME( DEF, 'N' ) ) THEN
        READ( NIN, FMT = * ) LBPAR
        IF( LBPAR.GT.0 )  READ( NIN, FMT = * ) ( BPAR(I), I = 1, LBPAR )
        READ( NIN, FMT = * ) LDPAR
        IF( LDPAR.GT.0 )  READ( NIN, FMT = * ) ( DPAR(I), I = 1, LDPAR )
        READ( NIN, FMT = * ) LIPAR
        IF( LIPAR.GT.0 )  READ( NIN, FMT = * ) ( IPAR(I), I = 1, LIPAR )
      END IF
*     Generate benchmark example
      CALL BB01AD( DEF, NR, DPAR, IPAR, BPAR, CHPAR, VEC, N, M, P, A,
     $             LDA, B, LDB, C, LDC, G, LDG, Q, LDQ, X, LDX, DWORK,
     $             LDWORK, INFO )
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
          WRITE( NOUT, FMT = 99979 ) ( A(I,J), J = 1, N )
   10   CONTINUE
        IF( VEC(5) ) THEN
          WRITE( NOUT, FMT = 99993 )
          DO 20  I = 1, N
            WRITE( NOUT, FMT = 99979 ) ( B(I,J), J = 1, M )
   20     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99992 )
        END IF
        IF( VEC(6) ) THEN
          WRITE( NOUT,FMT = 99991 )
          DO 30  I = 1, P
            WRITE( NOUT, FMT = 99979 ) ( C(I,J), J = 1, N )
   30     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99990 )
        END IF
        IF( .NOT.VEC(5) ) THEN
          WRITE( NOUT, FMT = 99989 )
          IF( .NOT.BPAR(2) ) THEN
            ISYMM = ( N * ( N + 1 ) ) / 2
            CALL DCOPY( ISYMM, G, 1, DWORK, 1 )
            IF( BPAR(3) ) THEN
              CALL MA02DD( 'Unpack', 'Upper', N, G, LDG, DWORK )
            ELSE
              CALL MA02DD( 'Unpack', 'Lower', N, G, LDG, DWORK )
            END IF
          END IF
          DO 40  I = 1, N
            WRITE( NOUT, FMT = 99979 ) ( G(I,J), J = 1, N )
   40     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99988 )
        END IF
        IF( .NOT.VEC(6) ) THEN
          IF( .NOT. BPAR(5) ) THEN
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
            WRITE( NOUT, FMT = 99979 ) ( Q(I,J), J = 1, N )
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
          DO 60  I = 1, N
            WRITE( NOUT, FMT = 99979 ) ( Q(I,J), J = 1, N )
   60     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99984 )
        END IF
        IF( VEC(5) ) THEN
          IF( .NOT.BPAR(2) ) THEN
            ISYMM = ( M * ( M + 1 ) ) / 2
            CALL DCOPY( ISYMM, G, 1, DWORK, 1 )
            IF( BPAR(3) ) THEN
              CALL MA02DD( 'Unpack', 'Upper', M, G, LDG, DWORK )
            ELSE
              CALL MA02DD( 'Unpack', 'Lower', M, G, LDG, DWORK )
            END IF
          END IF
          WRITE( NOUT, FMT = 99983 )
          DO 70  I = 1, N
            WRITE( NOUT, FMT = 99979 ) ( G(I,J), J = 1, N )
   70     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99982 )
        END IF
        IF( VEC(9) ) THEN
          WRITE( NOUT, FMT = 99981 )
          DO 80  I = 1, N
            WRITE( NOUT, FMT = 99979 ) ( X(I,J), J = 1, N )
   80     CONTINUE
        ELSE
          WRITE( NOUT, FMT = 99980 )
        END IF
      END IF
      STOP
*
99999 FORMAT (' BB01AD EXAMPLE PROGRAM RESULTS', /1X)
99998 FORMAT (' INFO on exit from BB03AD = ', I3)
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
99985 FORMAT (' W  = ')
99984 FORMAT (' W is not provided.')
99983 FORMAT (' R  = ')
99982 FORMAT (' R is not provided.')
99981 FORMAT (' X  = ')
99980 FORMAT (' X is not provided.')
99979 FORMAT (20(1X,F8.4))
*
      END
