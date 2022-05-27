*     AB09BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDD
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = 2*NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX*( 2*NMAX + 5 +
     $                            MAX( NMAX, MMAX, PMAX ) ) +
     $                          ( NMAX*( NMAX + 1 ) )/2 )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL1, TOL2
      INTEGER          I, INFO, IWARN, J, M, N, NR, P
      CHARACTER*1      DICO, EQUIL, JOB, ORDSEL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK), HSV(NMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         AB09BD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, NR, TOL1, TOL2, DICO, JOB, EQUIL,
     $                      ORDSEL
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1, N )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99988 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*              Find a reduced ssr for (A,B,C).
               CALL AB09BD( DICO, JOB, EQUIL, ORDSEL, N, M, P, NR,
     $                      A, LDA, B, LDB, C, LDC, D, LDD, HSV, TOL1,
     $                      TOL2, IWORK, DWORK, LDWORK, IWARN, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 ) NR
                  WRITE ( NOUT, FMT = 99987 )
                  WRITE ( NOUT, FMT = 99995 ) ( HSV(J), J = 1,N )
                  IF( NR.GT.0 ) WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,NR )
   20             CONTINUE
                  IF( NR.GT.0 ) WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   40             CONTINUE
                  IF( NR.GT.0 ) WRITE ( NOUT, FMT = 99992 )
                  DO 60 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,NR )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99991 )
                  DO 70 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( D(I,J), J = 1,M )
   70             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' AB09BD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from AB09BD = ',I2)
99997 FORMAT (' The order of reduced model = ',I2)
99996 FORMAT (/' The reduced state dynamics matrix Ar is ')
99995 FORMAT (20(1X,F8.4))
99993 FORMAT (/' The reduced input/state matrix Br is ')
99992 FORMAT (/' The reduced state/output matrix Cr is ')
99991 FORMAT (/' The reduced input/output matrix Dr is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
99987 FORMAT (/' The Hankel singular values are')
      END
