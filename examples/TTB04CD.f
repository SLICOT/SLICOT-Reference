*     TB04CD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX, NPZMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20,
     $                   NPZMAX = NMAX )
      INTEGER          PMNMAX
      PARAMETER        ( PMNMAX = PMAX*MMAX*NPZMAX )
      INTEGER          LDA, LDB, LDC, LDD, LDGAIN, LDNP, LDNZ
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX,
     $                   LDD = PMAX, LDGAIN = PMAX, LDNP = PMAX,
     $                   LDNZ = PMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = NMAX*( NMAX + PMAX ) +
     $                            MAX( NMAX + MAX( NMAX, PMAX ),
     $                                 NMAX*( 2*NMAX + 3 ) ) )
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, IJ, INFO, J, K, M, N, NPZ, P
      CHARACTER*1      JOBD, EQUIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 D(LDD,MMAX), DWORK(LDWORK), GAINS(LDGAIN,MMAX),
     $                 POLESI(PMNMAX), POLESR(PMNMAX), ZEROSI(PMNMAX),
     $                 ZEROSR(PMNMAX)
      INTEGER          IWORK(LIWORK), NP(LDNP,MMAX), NZ(LDNZ,MMAX)
*     .. External Subroutines ..
      EXTERNAL         TB04CD
*     .. Intrinsic Functions ..
      INTRINSIC        MAX
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, TOL, JOBD, EQUIL
      NPZ = N
      IF ( N.LT.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LT.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,N ), J = 1,M )
            IF ( P.LT.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99990 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
               READ ( NIN, FMT = * ) ( ( D(I,J), J = 1,M ), I = 1,P )
*              Find the transfer matrix T(s) of (A,B,C,D) in the
*              pole-zero-gain form.
               CALL TB04CD( JOBD, EQUIL, N, M, P, NPZ, A, LDA, B, LDB,
     $                      C, LDC, D, LDD, NZ, LDNZ, NP, LDNP, ZEROSR,
     $                      ZEROSI, POLESR, POLESI, GAINS, LDGAIN, TOL,
     $                      IWORK, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 )
                  DO 60 J = 1, M
                     DO 40 I = 1, P
                        IJ = ( (J-1)*P + I-1 )*NPZ + 1
                        IF ( NZ(I,J).EQ.0 ) THEN
                           WRITE ( NOUT, FMT = 99996 ) I, J
                        ELSE
                           WRITE ( NOUT, FMT = 99995 ) I, J,
     $                        ( ZEROSR(K), ZEROSI(K),
     $                                 K = IJ,IJ+NZ(I,J)-1 )
                        END IF
                        WRITE ( NOUT, FMT = 99994 ) I, J,
     $                     ( POLESR(K), POLESI(K), K = IJ,IJ+NP(I,J)-1 )
                        WRITE ( NOUT, FMT = 99993 ) I, J, ( GAINS(I,J) )
   40                CONTINUE
   60             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB04CD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB04CD = ',I2)
99997 FORMAT (/' The poles, zeros and gains of the transfer matrix',
     $         ' elements: ')
99996 FORMAT (/' no zeros for element (',I2,',',I2,')')
99995 FORMAT (/' zeros of element (',I2,',',I2,') are ',//
     $         '   real part     imag part '// (2X,F9.4,5X,F9.4))
99994 FORMAT (/' poles of element (',I2,',',I2,') are ',//
     $         '   real part     imag part '// (2X,F9.4,5X,F9.4))
99993 FORMAT (/' gain of element (',I2,',',I2,') is ', F9.4)
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (/' M is out of range.',/' M = ',I5)
99990 FORMAT (/' P is out of range.',/' P = ',I5)
      END
