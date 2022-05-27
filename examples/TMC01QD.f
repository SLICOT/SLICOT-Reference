*     MC01QD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          DAMAX, DBMAX
      PARAMETER        ( DAMAX = 10, DBMAX = 10 )
*     .. Local Scalars ..
      INTEGER          DA, DB, DBB, DQ, DR, I, IMAX, INFO, IWARN
*     .. Local Arrays ..
      DOUBLE PRECISION A(DAMAX+1), B(DBMAX+1), RQ(DAMAX+1)
*     .. External Subroutines ..
      EXTERNAL         MC01QD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) DA
      IF ( DA.LE.-2 .OR. DA.GT.DAMAX ) THEN
         WRITE ( NOUT, FMT = 99991 ) DA
      ELSE
         READ ( NIN, FMT = * ) ( A(I), I = 1,DA+1 )
         READ ( NIN, FMT = * ) DB
         DBB = DB
         IF ( DB.LE.-1 .OR. DB.GT.DBMAX ) THEN
            WRITE ( NOUT, FMT = 99990 ) DB
         ELSE
            READ ( NIN, FMT = * ) ( B(I), I = 1,DB+1 )
*           Compute Q(x) and R(x) from the given A(x) and B(x).
            CALL MC01QD( DA, DB, A, B, RQ, IWARN, INFO )
*
            IF ( INFO.NE.0 ) THEN
               WRITE ( NOUT, FMT = 99998 ) INFO
            ELSE
               IF ( IWARN.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99997 ) IWARN
                  WRITE ( NOUT, FMT = 99996 ) DBB, DB
               END IF
               WRITE ( NOUT, FMT = 99995 )
               DQ = DA - DB
               DR = DB - 1
               IMAX = DQ
               IF ( DR.GT.IMAX ) IMAX = DR
               DO 20 I = 0, IMAX
                  IF ( I.LE.DQ .AND. I.LE.DR ) THEN
                     WRITE ( NOUT, FMT = 99994 ) I, RQ(DB+I+1), RQ(I+1)
                  ELSE IF ( I.LE.DQ ) THEN
                     WRITE ( NOUT, FMT = 99993 ) I, RQ(DB+I+1)
                  ELSE
                     WRITE ( NOUT, FMT = 99992 ) I, RQ(I+1)
                  END IF
   20          CONTINUE
            END IF
         END IF
      END IF
*
      STOP
*
99999 FORMAT (' MC01QD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from MC01QD = ',I2)
99997 FORMAT (' IWARN on exit from MC01QD = ',I2,/)
99996 FORMAT (' The degree of the denominator polynomial B(x) has been',
     $       ' reduced from ',I2,' to ',I2,/)
99995 FORMAT (' The coefficients of the polynomials Q(x) and R(x) are ',
     $       //'                    Q(x)            R(x) ',/' power of',
     $       ' x     coefficient     coefficient ')
99994 FORMAT (2X,I5,9X,F9.4,7X,F9.4)
99993 FORMAT (2X,I5,9X,F9.4)
99992 FORMAT (2X,I5,25X,F9.4)
99991 FORMAT (/' DA is out of range.',/' DA = ',I5)
99990 FORMAT (/' DB is out of range.',/' DB = ',I5)
      END
