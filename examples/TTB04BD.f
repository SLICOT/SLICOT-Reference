*     TB04BD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX, MDMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20,
     $                   MDMAX = NMAX + 1 )
      INTEGER          PMNMAX
      PARAMETER        ( PMNMAX = PMAX*MMAX*MDMAX )
      INTEGER          LDA, LDB, LDC, LDD, LDIGD, LDIGN
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX, LDIGD = PMAX, LDIGN = PMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX*( NMAX + PMAX ) +
     $                            MAX( NMAX + MAX( NMAX, PMAX ),
     $                                 NMAX*( 2*NMAX + 5 ) ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, IJ, INFO, J, K, M, MD, N, P
      CHARACTER*1      JOBD, ORDER, EQUIL
      CHARACTER*132    ULINE
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK), GD(PMNMAX),
     $                 GN(PMNMAX)
      INTEGER          IGD(LDIGD,MMAX), IGN(LDIGN,MMAX), IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TB04BD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL, JOBD, ORDER, EQUIL
      MD = N + 1
      ULINE(1:20) = ' '
      DO 20 I = 21, 132
         ULINE(I:I) = '-'
   20 CONTINUE
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99990 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99989 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*              Find the transfer matrix T(s) of (A,B,C,D).
               CALL TB04BD( JOBD, ORDER, EQUIL, N, M, P, MD, A, LDA, B,
     $                      LDB, C, LDC, D, LDD, IGN, LDIGN, IGD, LDIGD,
     $                      GN, GD, TOL, IWORK, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF ( LSAME( ORDER, 'I' ) ) THEN
                     WRITE ( NOUT, FMT = 99997 )
                  ELSE
                     WRITE ( NOUT, FMT = 99996 )
                  END IF
                  WRITE ( NOUT, FMT = 99995 )
                  DO 60 J = 1, M
                     DO 40 I = 1, P
                        IJ = ( (J-1)*P + I-1 )*MD + 1
                        WRITE ( NOUT, FMT = 99994 ) I, J,
     $                    ( GN(K), K = IJ,IJ+IGN(I,J) )
                        WRITE ( NOUT, FMT = 99993 )
     $                          ULINE(1:7*(IGD(I,J)+1)+21)
                        WRITE ( NOUT, FMT = 99992 )
     $                        ( GD(K), K = IJ,IJ+IGD(I,J) )
   40                CONTINUE
   60             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB04BD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB04BD = ',I2)
99997 FORMAT (/' The polynomial coefficients appear in increasing',
     $         ' order'/' of the powers of the indeterminate')
99996 FORMAT (/' The polynomial coefficients appear in decreasing',
     $         ' order'/' of the powers of the indeterminate')
99995 FORMAT (/' The coefficients of polynomials in the transfer matri',
     $       'x T(s) are ')
99994 FORMAT (/' element (',I2,',',I2,') is ',20(1X,F6.2))
99993 FORMAT (1X,A)
99992 FORMAT (20X,20(1X,F6.2))
99991 FORMAT (/' N is out of range.',/' N = ',I5)
99990 FORMAT (/' M is out of range.',/' M = ',I5)
99989 FORMAT (/' P is out of range.',/' P = ',I5)
      END
