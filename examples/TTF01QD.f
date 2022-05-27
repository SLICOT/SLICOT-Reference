*     TF01QD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, NAMAX, NBMAX, NCMAX
      PARAMETER        ( NMAX = 20, NAMAX = 20, NBMAX = 20, NCMAX = 20 )
      INTEGER          LDH
      PARAMETER        ( LDH = NCMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, K, L, N, NA, NASUM, NB, NC, NL, NORD
      LOGICAL          ERROR
*     .. Local Arrays ..
      DOUBLE PRECISION AR(NAMAX), H(LDH,NMAX*NBMAX), MA(NAMAX)
      INTEGER          IORD(NCMAX*NBMAX)
*     .. External Subroutines ..
      EXTERNAL         TF01QD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, NA, NB, NC
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) N
      ELSE IF ( NA.LE.0 .OR. NA.GT.NAMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) NA
      ELSE IF ( NB.LE.0 .OR. NB.GT.NBMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) NB
      ELSE IF ( NC.LE.0 .OR. NC.GT.NCMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) NC
      ELSE
         ERROR = .FALSE.
         NL = 0
         K = 1
         NASUM = 0
         DO 40 I = 1, NC
            DO 20 J = 1, NB
               READ ( NIN, FMT = * ) NORD
               NASUM = NASUM + NORD
               IF ( NA.GE.NASUM ) THEN
                  READ ( NIN, FMT = * ) ( MA(NL+L), L = 1,NORD )
                  READ ( NIN, FMT = * ) ( AR(NL+L), L = 1,NORD )
                  IORD(K) = NORD
                  K = K + 1
                  NL = NL + NORD
               ELSE
                  WRITE ( NOUT, FMT = 99993 ) NA
                  ERROR = .TRUE.
               END IF
   20       CONTINUE
   40    CONTINUE
         IF ( .NOT. ERROR ) THEN
*           Compute M(1),...,M(N) from the given transfer function
*           matrix G(z).
            CALL TF01QD( NC, NB, N, IORD, AR, MA, H, LDH, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               WRITE ( NOUT, FMT = 99997 ) N
               DO 80 K = 1, N
                  WRITE ( NOUT, FMT = 99996 ) K,
     $                  ( H(1,(K-1)*NB+J), J = 1,NB )
                  DO 60 I = 2, NC
                     WRITE ( NOUT, FMT = 99995 )
     $                     ( H(I,(K-1)*NB+J), J = 1,NB )
   60             CONTINUE
   80          CONTINUE
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TF01QD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TF01QD = ',I2)
99997 FORMAT (' The Markov Parameters M(1),...,M(',I1,') are ')
99996 FORMAT (/' M(',I1,') : ',20(1X,F8.4))
99995 FORMAT (8X,20(1X,F8.4))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' NA is out of range.',/' NA = ',I5)
99992 FORMAT (/' NB is out of range.',/' NB = ',I5)
99991 FORMAT (/' NC is out of range.',/' NC = ',I5)
      END
