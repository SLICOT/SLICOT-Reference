*     MC03MD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          CP1MAX, CP2MAX, DP1MAX, DP2MAX, DP3MAX, RP1MAX
      PARAMETER        ( CP1MAX = 10, CP2MAX = 10, DP1MAX = 10,
     $                   DP2MAX = 10, DP3MAX = 20, RP1MAX = 10 )
      INTEGER          LDP11, LDP12, LDP21, LDP22, LDP31, LDP32
      PARAMETER        ( LDP11 = RP1MAX, LDP12 = CP1MAX,
     $                   LDP21 = CP1MAX, LDP22 = CP2MAX,
     $                   LDP31 = RP1MAX, LDP32 = CP2MAX )
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA
      INTEGER          CP1, CP2, DP1, DP2, DP3, I, INFO, J, K, RP1
*     .. Local Arrays ..
      DOUBLE PRECISION DWORK(CP1MAX),
     $                 P1(LDP11,LDP12,DP1MAX+1),
     $                 P2(LDP21,LDP22,DP2MAX+1),
     $                 P3(LDP31,LDP32,DP3MAX+1)
*     .. External Subroutines ..
      EXTERNAL         MC03MD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) RP1, CP1, CP2
      IF ( RP1.LT.0 .OR. RP1.GT.RP1MAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) RP1
      ELSE IF ( CP1.LT.0 .OR. CP1.GT.CP1MAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) CP1
      ELSE IF ( CP2.LT.0 .OR. CP2.GT.CP2MAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) CP2
      ELSE
         READ ( NIN, FMT = * ) DP1
         IF ( DP1.LE.-2 .OR. DP1.GT.DP1MAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) DP1
         ELSE
            DO 40 K = 1, DP1 + 1
               DO 20 J = 1, CP1
                  READ ( NIN, FMT = * ) ( P1(I,J,K), I = 1,RP1 )
   20          CONTINUE
   40       CONTINUE
            READ ( NIN, FMT = * ) DP2
            IF ( DP2.LE.-2 .OR. DP2.GT.DP2MAX ) THEN
               WRITE ( NOUT, FMT = 99991 ) DP2
            ELSE
               DO 80 K = 1, DP2 + 1
                  DO 60 J = 1, CP2
                     READ ( NIN, FMT = * ) ( P2(I,J,K), I = 1,CP1 )
   60             CONTINUE
   80          CONTINUE
               READ ( NIN, FMT = * ) DP3
               IF ( DP3.LE.-2 .OR. DP3.GT.DP3MAX ) THEN
                  WRITE ( NOUT, FMT = 99990 ) DP3
               ELSE
                  DO 120 K = 1, DP3 + 1
                     DO 100 J = 1, CP2
                        READ ( NIN, FMT = * ) ( P3(I,J,K), I = 1,RP1 )
  100                CONTINUE
  120             CONTINUE
                  READ ( NIN, FMT = * ) ALPHA
*                 Compute the coefficients of the polynomial matrix P(x)
                  CALL MC03MD( RP1, CP1, CP2, DP1, DP2, DP3, ALPHA, P1,
     $                         LDP11, LDP12, P2, LDP21, LDP22, P3,
     $                         LDP31, LDP32, DWORK, INFO )
*
                  IF ( INFO.NE.0 ) THEN
                     WRITE ( NOUT, FMT = 99998 ) INFO
                  ELSE
                     WRITE ( NOUT, FMT = 99997 ) DP3,
     $                     ( I-1, I = 1,DP3+1 )
                     DO 160 I = 1, RP1
                        DO 140 J = 1, CP2
                           WRITE ( NOUT, FMT = 99996 ) I, J,
     $                       ( P3(I,J,K), K = 1,DP3+1 )
  140                   CONTINUE
  160                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MC03MD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC03MD = ',I2)
99997 FORMAT (' The polynomial matrix P(x) (of degree ',I2,') is ',
     $       //' power of x         ',20I8)
99996 FORMAT (/' element (',I2,',',I2,') is ',20(1X,F7.2))
99995 FORMAT (/' RP1 is out of range.',/' RP1 = ',I5)
99994 FORMAT (/' CP1 is out of range.',/' CP1 = ',I5)
99993 FORMAT (/' CP2 is out of range.',/' CP2 = ',I5)
99992 FORMAT (/' DP1 is out of range.',/' DP1 = ',I5)
99991 FORMAT (/' DP2 is out of range.',/' DP2 = ',I5)
99990 FORMAT (/' DP3 is out of range.',/' DP3 = ',I5)
      END
