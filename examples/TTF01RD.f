*     TF01RD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, NAMAX, NBMAX, NCMAX
      PARAMETER        ( NMAX = 20, NAMAX = 20, NBMAX = 20, NCMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDH
      PARAMETER        ( LDA = NAMAX, LDB = NAMAX, LDC = NCMAX,
     $                   LDH = NCMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 2*NAMAX*NCMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, K, N, NA, NB, NC
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NAMAX), B(LDB,NBMAX), C(LDC,NAMAX),
     $                 H(LDH,NMAX*NBMAX), DWORK(LDWORK)
*     .. External Subroutines ..
      EXTERNAL         TF01RD
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
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), I = 1,NA ), J = 1,NA )
         IF ( NB.LE.0 .OR. NB.GT.NBMAX ) THEN
            WRITE ( NOUT, FMT = 99992 ) NB
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), I = 1,NA ), J = 1,NB )
            IF ( NC.LE.0 .OR. NC.GT.NCMAX ) THEN
               WRITE ( NOUT, FMT = 99991 ) NC
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), I = 1,NC ), J = 1,NA )
*              Compute M(1),...,M(N) from the system (A,B,C).
               CALL TF01RD( NA, NB, NC, N, A, LDA, B, LDB, C, LDC, H,
     $                      LDH, DWORK, LDWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  WRITE ( NOUT, FMT = 99997 ) N
                  DO 40 K = 1, N
                     WRITE ( NOUT, FMT = 99996 ) K,
     $                     ( H(1,(K-1)*NB+J), J = 1,NB )
                     DO 20 I = 2, NC
                        WRITE ( NOUT, FMT = 99995 )
     $                        ( H(I,(K-1)*NB+J), J = 1,NB )
   20                CONTINUE
   40             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TF01RD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TF01RD = ',I2)
99997 FORMAT (' The Markov Parameters M(1),...,M(',I1,') are ')
99996 FORMAT (/' M(',I1,') : ',20(1X,F8.4))
99995 FORMAT (8X,20(1X,F8.4))
99994 FORMAT (/' N is out of range.',/' N = ',I5)
99993 FORMAT (/' NA is out of range.',/' NA = ',I5)
99992 FORMAT (/' NB is out of range.',/' NB = ',I5)
99991 FORMAT (/' NC is out of range.',/' NC = ',I5)
      END
