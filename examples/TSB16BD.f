*     SB16BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDD, LDDC, LDF, LDG
      PARAMETER        ( LDA = NMAX, LDB  = NMAX, LDC = PMAX,
     $                   LDD = PMAX, LDDC = MMAX, LDF = MMAX, LDG = NMAX
     $                 )
      INTEGER          LDWORK, LIWORK, MAXMP, MPMAX
      PARAMETER        ( LIWORK = 2*NMAX, MAXMP = MAX( MMAX, PMAX ),
     $                   MPMAX  = MMAX + PMAX )
      PARAMETER        ( LDWORK = ( NMAX + MAXMP )*MPMAX +
     $                            MAX ( NMAX*( 2*NMAX +
     $                                         MAX( NMAX, MPMAX ) + 5 )
     $                                      + ( NMAX*( NMAX + 1 ) )/2,
     $                                  4*MAXMP ) )
      CHARACTER        DICO, EQUIL, JOBCF, JOBD, JOBMR, ORDSEL
      INTEGER          I, INFO, IWARN, J, M, N, NCR, P
      DOUBLE PRECISION TOL1, TOL2
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DC(LDDC,PMAX), DWORK(LDWORK),
     $                 F(LDF,NMAX), G(LDG,PMAX), HSV(NMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         SB16BD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, NCR, TOL1, TOL2,
     $                      DICO, JOBD, JOBMR, JOBCF, EQUIL, ORDSEL
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
               READ ( NIN, FMT = * ) ( ( F(I,J), J = 1,N ), I = 1,M )
               READ ( NIN, FMT = * ) ( ( G(I,J), J = 1,P ), I = 1,N )
*              Find a reduced ssr for (A,B,C,D).
               CALL SB16BD( DICO, JOBD, JOBMR, JOBCF, EQUIL, ORDSEL, N,
     $                      M, P, NCR, A, LDA, B, LDB, C, LDC, D, LDD,
     $                      F, LDF, G, LDG, DC, LDDC, HSV, TOL1, TOL2,
     $                      IWORK, DWORK, LDWORK, IWARN, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 ) NCR
                  WRITE ( NOUT, FMT = 99987 )
                  WRITE ( NOUT, FMT = 99995 ) ( HSV(J), J = 1,N )
                  IF( NCR.GT.0 ) WRITE ( NOUT, FMT = 99996 )
                  DO 20 I = 1, NCR
                     WRITE ( NOUT, FMT = 99995 ) ( A(I,J), J = 1,NCR )
   20             CONTINUE
                  IF( NCR.GT.0 ) WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, NCR
                     WRITE ( NOUT, FMT = 99995 ) ( G(I,J), J = 1,P )
   40             CONTINUE
                  IF( NCR.GT.0 ) WRITE ( NOUT, FMT = 99992 )
                  DO 60 I = 1, M
                     WRITE ( NOUT, FMT = 99995 ) ( F(I,J), J = 1,NCR )
   60             CONTINUE
                  WRITE ( NOUT, FMT = 99991 )
                  DO 80 I = 1, P
                     WRITE ( NOUT, FMT = 99995 ) ( DC(I,J), J = 1,M )
   80             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' SB16BD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from SB16BD = ',I2)
99997 FORMAT (' The order of reduced controller = ',I2)
99996 FORMAT (/' The reduced controller state dynamics matrix Ac is ')
99995 FORMAT (20(1X,F8.4))
99993 FORMAT (/' The reduced controller input/state matrix Bc is ')
99992 FORMAT (/' The reduced controller state/output matrix Cc is ')
99991 FORMAT (/' The reduced controller input/output matrix Dc is ')
99990 FORMAT (/' N is out of range.',/' N = ',I5)
99989 FORMAT (/' M is out of range.',/' M = ',I5)
99988 FORMAT (/' P is out of range.',/' P = ',I5)
99987 FORMAT (/' The Hankel singular values of extended system are:')
      END
