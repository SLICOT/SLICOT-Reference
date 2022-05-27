*     TG01JY EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDE
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDE = NMAX )
      INTEGER          LDWORK, LIWORK
      PARAMETER        ( LDWORK = 2*NMAX*NMAX + 
     $                            MAX( 2*( NMAX*( NMAX + MMAX + PMAX ) +
     $                                 MAX( MMAX, PMAX ) + NMAX - 1 ),
     $                                 10*NMAX + MAX( NMAX, 23 ) ),
     $                   LIWORK = 2*NMAX + MAX( MMAX, PMAX ) )
*     .. Local Scalars ..
      CHARACTER        CKSING, EQUIL, JOB, RESTOR, SYSTYP
      INTEGER          I, INFO, J, M, N, NR, P
*     .. Local Arrays ..
      INTEGER          INFRED(7), IWORK(LIWORK)
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 DWORK(LDWORK), E(LDE,NMAX), TOL(3)

*     .. External Subroutines ..
      EXTERNAL         TG01JY
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL(1), TOL(2), TOL(3), JOB,
     $                      SYSTYP, EQUIL, CKSING, RESTOR
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99988 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         READ ( NIN, FMT = * ) ( ( E(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99987 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99986 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
*              Find the irreducible descriptor system (Ar-lambda Er,Br,Cr).
               CALL TG01JY( JOB, SYSTYP, EQUIL, CKSING, RESTOR, N, M, P,
     $                      A, LDA, E, LDE, B, LDB, C, LDC, NR, INFRED,
     $                      TOL, IWORK, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99994 ) NR
                  WRITE ( NOUT, FMT = 99991 )
                  DO 10 I = 1, 4
                     IF( INFRED(I).GE.0 )
     $                  WRITE ( NOUT, FMT = 99990 ) I, INFRED(I)
   10             CONTINUE
                  WRITE ( NOUT, FMT = 99997 )
                  DO 20 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,NR )
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99996 )
                  DO 30 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( E(I,J), J = 1,NR )
   30             CONTINUE
                  WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, NR
                     WRITE ( NOUT, FMT = 99995 ) ( B(I,J), J = 1,M )
   40             CONTINUE
                  WRITE ( NOUT, FMT = 99992 )
                  DO 50 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( C(I,J), J = 1,NR )
   50             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TG01JY EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TG01JY = ',I2)
99997 FORMAT (/' The reduced state dynamics matrix Ar is ')
99996 FORMAT (/' The reduced descriptor matrix Er is ')
99995 FORMAT (20(1X,F8.4))
99994 FORMAT (' Order of reduced system =', I5 )
99993 FORMAT (/' The reduced input/state matrix Br is ')
99992 FORMAT (/' The reduced state/output matrix Cr is ')
99991 FORMAT (/' Achieved order reductions in different phases')
99990 FORMAT (' Phase',I2,':', I3, ' elliminated eigenvalue(s)' )
99988 FORMAT (/' N is out of range.',/' N = ',I5)
99987 FORMAT (/' M is out of range.',/' M = ',I5)
99986 FORMAT (/' P is out of range.',/' P = ',I5)
      END
