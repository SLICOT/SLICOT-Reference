*     SB06ND EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX
      PARAMETER        ( NMAX = 20, MMAX = 20 )
      INTEGER          LDA, LDB, LDU, LDV, LDF
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDU = NMAX,
     $                   LDV = MMAX, LDF = MMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = MMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX + MAX(NMAX,3*MMAX) )
*     PARAMETER        ( LDWORK = 4*NMAX)
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, J, KMAX, M, N, NCONT
      CHARACTER*1      JOBU, JOBV
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), DWORK(LDWORK),
     $                 F(LDF,NMAX), U(LDU,NMAX), V(LDV,MMAX)
      INTEGER          IWORK(LIWORK), KSTAIR(NMAX)
C     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         AB01OD, DLASET, SB06ND
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, TOL, JOBU, JOBV
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), I = 1,N ), J = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99993 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
*           First put (A,B) into staircase form with triangular pivots
*           and determine the stairsizes.
            CALL AB01OD( 'A', JOBU, JOBV, N, M, A, LDA, B, LDB, U,
     $                   LDU, V, LDV, NCONT, KMAX, KSTAIR, TOL, IWORK,
     $                   DWORK, LDWORK, INFO )
*
            IF ( INFO.EQ.0 ) THEN
               IF( LSAME( JOBU, 'N' ) ) THEN
*                 Initialize U as the identity matrix.
                  CALL DLASET( 'Full', N, N, ZERO, ONE, U, LDU )
               END IF
*              Perform "deadbeat control" to give F.
               CALL SB06ND( N, M, KMAX, A, LDA, B, LDB, KSTAIR, U, LDU,
     $                      F, LDF, DWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99997 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99996 )
                  DO 60 I = 1, M
                     WRITE ( NOUT, FMT = 99995 ) ( F(I,J), J = 1,N )
   60             CONTINUE
               END IF
            ELSE
               WRITE ( NOUT, FMT = 99998 ) INFO
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB06ND EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB01OD = ',I2)
99997 FORMAT (' INFO on exit from SB06ND = ',I2)
99996 FORMAT (' The deadbeat feedback matrix F is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' M is out of range.',/' M = ',I5)
      END
