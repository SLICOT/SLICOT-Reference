####################################################################
#  SLICOT routines makefile                                        #
#  Makefile for creating/updating the SLICOT Library object file   # 
#  on Windows platforms.                                           #
#  SLICOT, Release 5.8                      .\slicot\src\makefile  #
#  Vasile Sima                                                     #
#  October 31, 1996.                                               #
#  Revised December 23, 1999, ..., Oct. 4, 2001, Aug. 25, 2003,    #
#          Jan. 9, 2009, Nov. 17, 2010, Sep. 25, 2012,             #
#          May 08, 2015, Jan. 2017, Dec. 2022                      #
####################################################################
#
#  This is the makefile to create/update the library for SLICOT.
#  Most SLICOT Library routines are written for double precision.
#  Some routines have COMPLEX*16 variants.
#
#  The command
#       nmake
#  without any arguments creates or updates a library called
#       slicot.lib
#  in the next higher directory level.
#
#  To remove the object files after the library is created, enter
#       nmake clean
#
#######################################################################
 
!include <..\make.inc>

.SUFFIXES: .f .obj

SLSRC = \
    AB01MD.obj AB01ND.obj AB01OD.obj AB04MD.obj AB05MD.obj AB05ND.obj AB05OD.obj \
    AB05PD.obj AB05QD.obj AB05RD.obj AB05SD.obj AB07MD.obj AB07ND.obj AB08MD.obj \
    AB08MZ.obj AB08ND.obj AB08NW.obj AB08NX.obj AB08NY.obj AB08NZ.obj AB09AD.obj \
    AB09AX.obj AB09BD.obj AB09BX.obj AB09CD.obj AB09CX.obj AB09DD.obj AB09ED.obj \
    AB09FD.obj AB09GD.obj AB09HD.obj AB09HX.obj AB09HY.obj AB09ID.obj AB09IX.obj \
    AB09IY.obj AB09JD.obj AB09JV.obj AB09JW.obj AB09JX.obj AB09KD.obj AB09KX.obj \
    AB09MD.obj AB09ND.obj AB13AD.obj AB13AX.obj AB13BD.obj AB13CD.obj AB13DD.obj \
    AB13DX.obj AB13ED.obj AB13FD.obj AB13HD.obj AB13ID.obj AB13MD.obj AB8NXZ.obj \
    AG07BD.obj AG08BD.obj AG08BY.obj AG08BZ.obj AG8BYZ.obj \
    BB01AD.obj BB02AD.obj BB03AD.obj BB04AD.obj BD01AD.obj BD02AD.obj \
    DE01OD.obj DE01PD.obj delctg.obj DF01MD.obj DG01MD.obj DG01ND.obj DG01NY.obj \
    DG01OD.obj DK01MD.obj \
    FB01QD.obj FB01RD.obj FB01SD.obj FB01TD.obj FB01VD.obj FD01AD.obj \
    IB01AD.obj IB01BD.obj IB01CD.obj IB01MD.obj IB01MY.obj IB01ND.obj IB01OD.obj \
    IB01OY.obj IB01PD.obj IB01PX.obj IB01PY.obj IB01QD.obj IB01RD.obj IB03AD.obj \
    IB03BD.obj \
    MA01AD.obj MA01BD.obj MA01BZ.obj MA01CD.obj MA01DD.obj MA01DZ.obj MA02AD.obj \
    MA02AZ.obj MA02BD.obj MA02BZ.obj MA02CD.obj MA02CZ.obj MA02DD.obj MA02ED.obj \
    MA02ES.obj MA02EZ.obj MA02FD.obj MA02GD.obj MA02GZ.obj MA02HD.obj MA02HZ.obj \
    MA02ID.obj MA02IZ.obj MA02JD.obj MA02JZ.obj MA02MD.obj MA02MZ.obj MA02NZ.obj \
    MA02OD.obj MA02OZ.obj MA02PD.obj MA02PZ.obj MA02RD.obj MA02SD.obj \
    MB01KD.obj MB01LD.obj MB01MD.obj MB01ND.obj MB01OC.obj \
    MB01OD.obj MB01OE.obj MB01OH.obj MB01OO.obj MB01OS.obj MB01OT.obj MB01PD.obj \
    MB01QD.obj MB01RB.obj MB01RD.obj MB01RH.obj MB01RT.obj MB01RU.obj MB01RW.obj \
    MB01RX.obj MB01RY.obj MB01SD.obj MB01SS.obj MB01TD.obj MB01UD.obj MB01UW.obj \
    MB01UX.obj MB01UY.obj MB01UZ.obj MB01VD.obj MB01WD.obj MB01XD.obj MB01XY.obj \
    MB01YD.obj MB01ZD.obj MB02CD.obj MB02CU.obj MB02CV.obj MB02CX.obj MB02CY.obj \
    MB02DD.obj MB02ED.obj MB02FD.obj MB02GD.obj MB02HD.obj MB02ID.obj MB02JD.obj \
    MB02JX.obj MB02KD.obj MB02MD.obj MB02ND.obj MB02NY.obj MB02OD.obj MB02PD.obj \
    MB02QD.obj MB02QY.obj MB02RD.obj MB02RZ.obj MB02SD.obj MB02SZ.obj MB02TD.obj \
    MB02TZ.obj MB02UD.obj MB02UU.obj MB02UV.obj MB02UW.obj MB02VD.obj MB02WD.obj \
    MB02XD.obj MB02YD.obj MB03AB.obj MB03AD.obj MB03AE.obj MB03AF.obj MB03AG.obj \
    MB03AH.obj MB03AI.obj MB03BA.obj MB03BB.obj MB03BC.obj MB03BD.obj MB03BE.obj \
    MB03BF.obj MB03BG.obj MB03BZ.obj MB03CD.obj MB03CZ.obj MB03DD.obj MB03DZ.obj \
    MB03ED.obj MB03FD.obj MB03FZ.obj MB03GD.obj MB03GZ.obj MB03HD.obj MB03HZ.obj \
    MB03ID.obj MB03IZ.obj MB03JD.obj MB03JP.obj MB03JZ.obj MB03KA.obj MB03KB.obj \
    MB03KC.obj MB03KD.obj MB03KE.obj MB03LD.obj MB03LF.obj MB03LP.obj MB03LZ.obj \
    MB03MD.obj MB03MY.obj MB03ND.obj MB03NY.obj MB03OD.obj MB03OY.obj MB03PD.obj \
    MB03PY.obj MB03QD.obj MB03QG.obj MB03QV.obj MB03QW.obj MB03QX.obj MB03QY.obj \
    MB03RD.obj MB03RW.obj MB03RX.obj MB03RY.obj MB03RZ.obj MB03SD.obj MB03TD.obj \
    MB03TS.obj MB03UD.obj MB03VD.obj MB03VW.obj MB03VY.obj MB03WA.obj MB03WD.obj \
    MB03WX.obj MB03XD.obj MB03XP.obj MB03XS.obj MB03XU.obj MB03XZ.obj MB03YA.obj \
    MB03YD.obj MB03YT.obj MB03ZA.obj MB03ZD.obj MB04AD.obj MB04AZ.obj MB04BD.obj \
    MB04BP.obj MB04BZ.obj MB04CD.obj MB04DB.obj MB04DD.obj MB04DI.obj MB04DL.obj \
    MB04DP.obj MB04DS.obj MB04DY.obj MB04DZ.obj MB04ED.obj MB04FD.obj MB04FP.obj \
    MB04GD.obj MB04HD.obj MB04ID.obj MB04IY.obj MB04IZ.obj MB04JD.obj MB04KD.obj \
    MB04LD.obj MB04MD.obj MB04ND.obj MB04NY.obj MB04OD.obj MB04OW.obj MB04OX.obj \
    MB04OY.obj MB04PA.obj MB04PB.obj MB04PU.obj MB04PY.obj MB04QB.obj MB04QC.obj \
    MB04QF.obj MB04QS.obj MB04QU.obj MB04RB.obj MB04RD.obj MB04RS.obj MB04RT.obj \
    MB04RU.obj MB04RV.obj MB04RW.obj MB04RZ.obj MB04SU.obj MB04TB.obj MB04TS.obj \
    MB04TT.obj MB04TU.obj MB04TV.obj MB04TW.obj MB04TX.obj MB04TY.obj MB04UD.obj \
    MB04VD.obj MB04VX.obj MB04WD.obj MB04WP.obj MB04WR.obj MB04WU.obj MB04XD.obj \
    MB04XY.obj MB04YD.obj MB04YW.obj MB04ZD.obj MB05MD.obj MB05MY.obj MB05ND.obj \
    MB05OD.obj MB05OY.obj MB3JZP.obj MB3LZP.obj MB3OYZ.obj MB3PYZ.obj MB4DBZ.obj \
    MB4DLZ.obj MB4DPZ.obj MC01MD.obj MC01ND.obj MC01OD.obj MC01PD.obj MC01PY.obj \
    MC01QD.obj MC01RD.obj MC01SD.obj MC01SW.obj MC01SX.obj MC01SY.obj MC01TD.obj \
    MC01VD.obj MC01WD.obj MC01XD.obj MC03MD.obj MC03ND.obj MC03NX.obj MC03NY.obj \
    MD03AD.obj MD03BA.obj MD03BB.obj MD03BD.obj MD03BF.obj MD03BX.obj MD03BY.obj \
    NF01AD.obj NF01AY.obj NF01BA.obj NF01BB.obj NF01BD.obj NF01BE.obj NF01BF.obj \
    NF01BP.obj NF01BQ.obj NF01BR.obj NF01BS.obj NF01BU.obj NF01BV.obj NF01BW.obj \
    NF01BX.obj NF01BY.obj \
    SB01BD.obj SB01BX.obj SB01BY.obj SB01DD.obj SB01FY.obj SB01MD.obj SB02CX.obj \
    SB02MD.obj SB02MR.obj SB02MS.obj SB02MT.obj SB02MU.obj SB02MV.obj SB02MW.obj \
    SB02MX.obj SB02ND.obj SB02OD.obj SB02OU.obj SB02OV.obj SB02OW.obj SB02OX.obj \
    SB02OY.obj SB02PD.obj SB02QD.obj SB02RD.obj SB02RU.obj SB02SD.obj SB03MD.obj \
    SB03MU.obj SB03MV.obj SB03MW.obj SB03MX.obj SB03MY.obj SB03OD.obj SB03OR.obj \
    SB03OS.obj SB03OT.obj SB03OU.obj SB03OV.obj SB03OY.obj SB03OZ.obj SB03PD.obj \
    SB03QD.obj SB03QX.obj SB03QY.obj SB03RD.obj SB03SD.obj SB03SX.obj SB03SY.obj \
    SB03TD.obj SB03UD.obj SB04MD.obj SB04MR.obj SB04MU.obj SB04MW.obj SB04MY.obj \
    SB04ND.obj SB04NV.obj SB04NW.obj SB04NX.obj SB04NY.obj SB04OD.obj SB04OW.obj \
    SB04PD.obj SB04PX.obj SB04PY.obj SB04QD.obj SB04QR.obj SB04QU.obj SB04QY.obj \
    SB04RD.obj SB04RV.obj SB04RW.obj SB04RX.obj SB04RY.obj SB06ND.obj SB08CD.obj \
    SB08DD.obj SB08ED.obj SB08FD.obj SB08GD.obj SB08HD.obj SB08MD.obj SB08MY.obj \
    SB08ND.obj SB08NY.obj SB09MD.obj SB10AD.obj SB10DD.obj SB10ED.obj SB10FD.obj \
    SB10HD.obj SB10ID.obj SB10JD.obj SB10KD.obj SB10LD.obj SB10MD.obj SB10PD.obj \
    SB10QD.obj SB10RD.obj SB10SD.obj SB10TD.obj SB10UD.obj SB10VD.obj SB10WD.obj \
    SB10YD.obj SB10ZD.obj SB10ZP.obj SB16AD.obj SB16AY.obj SB16BD.obj SB16CD.obj \
    SB16CY.obj select.obj SG02AD.obj SG02CV.obj SG02CW.obj SG02CX.obj SG02ND.obj \
    SG03AD.obj SG03AX.obj SG03AY.obj SG03BD.obj SG03BR.obj SG03BS.obj SG03BT.obj \
    SG03BU.obj SG03BV.obj SG03BW.obj SG03BX.obj SG03BY.obj SG03BZ.obj \
    TB01ID.obj TB01IZ.obj TB01KD.obj TB01KX.obj TB01LD.obj TB01MD.obj TB01ND.obj \
    TB01PD.obj TB01PX.obj TB01TD.obj TB01TY.obj TB01UD.obj TB01UX.obj TB01UY.obj \
    TB01VD.obj TB01VY.obj TB01WD.obj TB01WX.obj TB01XD.obj TB01XZ.obj TB01YD.obj \
    TB01ZD.obj TB03AD.obj TB03AY.obj TB04AD.obj TB04AY.obj TB04BD.obj TB04BV.obj \
    TB04BW.obj TB04BX.obj TB04CD.obj TB05AD.obj TC01OD.obj TC04AD.obj TC05AD.obj \
    TD03AD.obj TD03AY.obj TD04AD.obj TD05AD.obj TF01MD.obj TF01MX.obj TF01MY.obj \
    TF01ND.obj TF01OD.obj TF01PD.obj TF01QD.obj TF01RD.obj TG01AD.obj TG01AZ.obj \
    TG01BD.obj TG01CD.obj TG01DD.obj TG01ED.obj TG01FD.obj TG01FZ.obj TG01GD.obj \
    TG01HD.obj TG01HU.obj TG01HX.obj TG01HY.obj TG01ID.obj TG01JD.obj TG01JY.obj \
    TG01KD.obj TG01KZ.obj TG01LD.obj TG01LY.obj TG01MD.obj TG01ND.obj TG01NX.obj \
    TG01OA.obj TG01OB.obj TG01OD.obj TG01OZ.obj TG01PD.obj TG01QD.obj TG01WD.obj \
    UD01BD.obj UD01CD.obj UD01DD.obj UD01MD.obj UD01MZ.obj UD01ND.obj UE01MD.obj \
    zelctg.obj

all: 	$(SLSRC)
	$(ARCH) $(ARCHFLAGS):$(SLICOTLIB) $(SLSRC)

clean:
	del *.obj

.f.obj:
	$(FORTRAN) $(OPTS) -c $<
