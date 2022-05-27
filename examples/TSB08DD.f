*     SB08DD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDCR, LDD, LDDR
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDCR = MMAX, LDD = PMAX, LDDR = MMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = MAX( NMAX*( NMAX + 5 ),
     $                                 MMAX*( MMAX + 2 ),
     $                                 4*NMAX, 4*PMAX ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, INFO, IWARN, J, M, N, NQ, NR, P
      CHARACTER*1      DICO
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 CR(LDCR,NMAX), D(LDD,MMAX), DR(LDDR,MMAX),
     $                 DWORK(LDWORK)
*     .. External Subroutines ..
      EXTERNAL         SB08DD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL, DICO
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99990 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1, N ), I = 1, N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99989 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1, M ), I = 1, N )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99988 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1, N ), I = 1, P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1, M ), I = 1, P )
*              Find a RCFID for (A,B,C,D).
               CALL SB08DD( DICO, N, M, P, A, LDA, B, LDB, C, LDC,
     $                      D, LDD, NQ, NR, CR, LDCR, DR, LDDR, TOL,
     $                      DWORK, LDWORK, IWARN, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF( NQ.GT.0 ) WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, NQ
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1, NQ )
   20             CONTINUE
                  IF( NQ.GT.0 ) WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, NQ
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1, M )
   40             CONTINUE
                  IF( NQ.GT.0 ) WRITE ( NOUT, FMT = 99992 )
                  DO 60 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1, NQ )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99991 )
                  DO 70 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( D(I,J), J = 1, M )
   70             CONTINUE
                  IF( NR.GT.0 ) WRITE ( NOUT, FMT = 99986 )
                  DO 80 I = NQ-NR+1, NQ
                     WRITE ( NOUT, FMT = 99995 )
     $                     ( A(I,J), J = NQ-NR+1, NQ )
   80             CONTINUE
                  IF( NR.GT.0 ) WRITE ( NOUT, FMT = 99985 )
                  DO 90 I = NQ-NR+1, NQ
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1, M )
   90             CONTINUE
                  IF( NR.GT.0 ) WRITE ( NOUT, FMT = 99984 )
                  DO 100 I = 1, M
                     WRITE ( NOUT, FMT = 99995 )
     $                     ( CR(I,J), J = NQ-NR+1, NQ )
  100             CONTINUE
                  WRITE ( NOUT, FMT = 99983 )
                  DO 110 I = 1, M
                     WRITE ( NOUT, FMT = 99995 ) ( DR(I,J), J = 1, M )
  110             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB08DD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB08DD = ',I2)
99996 FORMAT (/' The numerator state dynamics matrix AQ is ')
99995 FORMAT (20(1X,F8.4))
99993 FORMAT (/' The numerator input/state matrix BQ is ')
99992 FORMAT (/' The numerator state/output matrix CQ is ')
99991 FORMAT (/' The numerator input/output matrix DQ is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
99986 FORMAT (/' The denominator state dynamics matrix AR is ')
99985 FORMAT (/' The denominator input/state matrix BR is ')
99984 FORMAT (/' The denominator state/output matrix CR is ')
99983 FORMAT (/' The denominator input/output matrix DR is ')
      END
