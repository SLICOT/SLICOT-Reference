*     TF01PD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NH1MAX, NH2MAX, NRMAX, NCMAX
      PARAMETER        ( NH1MAX = 20, NH2MAX = 20, NRMAX = 20,
     $                   NCMAX = 20 )
      INTEGER          LDH, LDT
      PARAMETER        ( LDH = NH1MAX, LDT = NH1MAX*NRMAX )
*     .. Local Scalars ..
      INTEGER          I, INFO, J, NC, NCT, NH1, NH2, NR, NRT
*     .. Local Arrays ..
      DOUBLE PRECISION H(LDH,(NRMAX+NCMAX-1)*NH2MAX),
     $                 T(LDT,NH2MAX*NCMAX)
*     .. External Subroutines ..
      EXTERNAL         TF01PD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) NH1, NH2, NR, NC
      IF ( NH1.LE.0 .OR. NH1.GE.NH1MAX ) THEN
         WRITE ( NOUT, FMT = 99995 ) NH1
      ELSE IF ( NH2.LE.0 .OR. NH2.GT.NH2MAX ) THEN
         WRITE ( NOUT, FMT = 99994 ) NH2
      ELSE IF ( NR.LE.0 .OR. NR.GT.NRMAX ) THEN
         WRITE ( NOUT, FMT = 99993 ) NR
      ELSE IF ( NC.LE.0 .OR. NC.GT.NCMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) NC
      ELSE
         READ ( NIN, FMT = * )
     $      ( ( H(I,J), I = 1,NH1 ), J = 1,( NR+NC-1 )*NH2 )
*        Construct the NRT by NCT block Toeplitz expansion of M(k).
         CALL TF01PD( NH1, NH2, NR, NC, H, LDH, T, LDT, INFO )
*
         IF ( INFO.NE.0 ) THEN
            WRITE ( NOUT, FMT = 99998 ) INFO
         ELSE
            NRT = NH1*NR
            NCT = NH2*NC
            WRITE ( NOUT, FMT = 99997 ) NRT, NCT
            DO 20 I = 1, NRT
               WRITE ( NOUT, FMT = 99996 ) ( T(I,J), J = 1,NCT )
   20       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (' TF01PD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TF01PD = ',I2)
99997 FORMAT (' The ',I2,' by ',I2,' matrix T is ')
99996 FORMAT (20(1X,F8.4))
99995 FORMAT (/' NH1 is out of range.',/' NH1 = ',I5)
99994 FORMAT (/' NH2 is out of range.',/' NH2 = ',I5)
99993 FORMAT (/' NR is out of range.',/' NR = ',I5)
99992 FORMAT (/' NC is out of range.',/' NC = ',I5)
      END
