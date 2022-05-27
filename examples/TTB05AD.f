*     TB05AD EXAMPLE PROGRAM TEXT
*
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        ( NIN = 5, NOUT = 6 )
      INTEGER          NMAX, MMAX, PMAX
      PARAMETER        ( NMAX = 20, MMAX = 20, PMAX = 20 )
      INTEGER          LDA, LDB, LDC, LDG, LDHINV
      PARAMETER        ( LDA = NMAX, LDB = NMAX, LDC = PMAX, LDG = PMAX,
     $                   LDHINV = NMAX )
      INTEGER          LIWORK
      PARAMETER        ( LIWORK = NMAX )
      INTEGER          LDWORK
      PARAMETER        ( LDWORK = 2*NMAX )
      INTEGER          LZWORK
      PARAMETER        ( LZWORK = NMAX*( NMAX+2 ) )
*     .. Local Scalars ..
      COMPLEX*16       FREQ
      DOUBLE PRECISION RCOND
      INTEGER          I, INFO, J, M, N, P
      CHARACTER*1      BALEIG, INITA
      LOGICAL          LBALBA, LBALEA, LBALEB, LBALEC, LINITA
*     .. Local Arrays ..
      COMPLEX*16       G(LDG,MMAX), HINVB(LDHINV,MMAX), ZWORK(LZWORK)
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,MMAX), C(LDC,NMAX),
     $                 DWORK(LDWORK), EVIM(NMAX), EVRE(NMAX)
      INTEGER          IWORK(LIWORK)
*     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
*     .. External Subroutines ..
      EXTERNAL         TB05AD
*     .. Executable Statements ..
*
      WRITE ( NOUT, FMT = 99999 )
*     Skip the heading in the data file and read the data.
      READ ( NIN, FMT = '()' )
      READ ( NIN, FMT = * ) N, M, P, FREQ, INITA, BALEIG
      LBALEC = LSAME( BALEIG, 'C' )
      LBALEB = LSAME( BALEIG, 'B' ) .OR. LSAME( BALEIG, 'E' )
      LBALEA = LSAME( BALEIG, 'A' )
      LBALBA = LBALEB.OR.LBALEA
      LINITA = LSAME( INITA,  'G' )
      IF ( N.LE.0 .OR. N.GT.NMAX ) THEN
         WRITE ( NOUT, FMT = 99992 ) N
      ELSE
         READ ( NIN, FMT = * ) ( ( A(I,J), J = 1,N ), I = 1,N )
         IF ( M.LE.0 .OR. M.GT.MMAX ) THEN
            WRITE ( NOUT, FMT = 99991 ) M
         ELSE
            READ ( NIN, FMT = * ) ( ( B(I,J), J = 1,M ), I = 1,N )
            IF ( P.LE.0 .OR. P.GT.PMAX ) THEN
               WRITE ( NOUT, FMT = 99990 ) P
            ELSE
               READ ( NIN, FMT = * ) ( ( C(I,J), J = 1,N ), I = 1,P )
*              Find the frequency response matrix of the ssr (A,B,C).
               CALL TB05AD( BALEIG, INITA, N, M, P, FREQ, A, LDA, B,
     $                      LDB, C, LDC, RCOND, G, LDG, EVRE, EVIM,
     $                      HINVB, LDHINV, IWORK, DWORK, LDWORK, ZWORK,
     $                      LZWORK, INFO )
*
               IF ( INFO.NE.0 ) THEN
                  WRITE ( NOUT, FMT = 99998 ) INFO
               ELSE
                  IF ( ( LBALEC ) .OR. ( LBALEA ) ) WRITE ( NOUT,
     $                FMT = 99997 ) RCOND
                  IF ( ( LINITA ) .AND. ( LBALBA ) )
     $               WRITE ( NOUT, FMT = 99996 )
     $                       ( EVRE(I), EVIM(I), I = 1,N )
                  WRITE ( NOUT, FMT = 99995 )
                  DO 20 I = 1, P
                     WRITE ( NOUT, FMT = 99994 ) ( G(I,J), J = 1,M )
   20             CONTINUE
                  WRITE ( NOUT, FMT = 99993 )
                  DO 40 I = 1, N
                     WRITE ( NOUT, FMT = 99994 ) ( HINVB(I,J), J = 1,M )
   40             CONTINUE
               END IF
            END IF
         END IF
      END IF
      STOP
*
99999 FORMAT (' TB05AD EXAMPLE PROGRAM RESULTS',/1X)
99998 FORMAT (' INFO on exit from TB05AD = ',I2)
99997 FORMAT (' RCOND = ',F4.2)
99996 FORMAT (/' Eigenvalues of the state transmission matrix A are ',
     $       /(1X,2F7.2,'*j'))
99995 FORMAT (/' The frequency response matrix G(freq) is ')
99994 FORMAT (20(' (',F5.2,',',F5.2,') ',:))
99993 FORMAT (/' H(inverse)*B is ')
99992 FORMAT (/' N is out of range.',/' N = ',I5)
99991 FORMAT (/' M is out of range.',/' M = ',I5)
99990 FORMAT (/' P is out of range.',/' P = ',I5)
      END
