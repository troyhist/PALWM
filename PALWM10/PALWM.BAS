REM --------------------------------------------------------------------------
REM PALlette Work Manager ver. 1.0
REM Troy computing, Inc.
REM Kolevatow Sergey (c) 1995
REM --------------------------------------------------------------------------
DECLARE SUB BORDER (COL)
DECLARE SUB CONBORD (X1, Y1, X2, Y2, COL)
DECLARE FUNCTION CRSTRPAL$ (PAL())
DECLARE SUB DIR (FILESPEC$)
DECLARE FUNCTION PALLoad$ (FLNM$)
DECLARE SUB PALSave (FLNM$, NAME$, Caption$, usCol, fstCol, palType, Datas$)
DECLARE SUB PRKEY ()
REM --------------------------------------------------------------------------
Datas:
REM --------------------------------------------------------------------------
initPAL$ = "PAL95I": usCol = 255: fstCol = 0: palType = 1
DIM PAL$(768), PAL(256, 3): RED = 63: GRN = 63: BLU = 63
CURATR = 63: FLAGATR = 0
NAME$ = SPACE$(11): Caption$ = SPACE$(256)
REM --------------------------------------------------------------------------
PAL(15, 1) = 63: PAL(15, 2) = 63: PAL(15, 3) = 63
REM --------------------------------------------------------------------------
WORKING:
REM --------------------------------------------------------------------------
SCREEN 13: CLS : BORDER 15
LOCATE 1, 9: PRINT " PALlette Work Manager "
LOCATE 5, 10: PRINT "1.Create pallette"
LOCATE 7, 10: PRINT "2.Edit saved pallette"
LOCATE 9, 10: PRINT "3.View saved pallette "
LOCATE 11, 10: PRINT "4.Exit PALWM"
LOCATE 13, 10: PRINT "Choose menu item -"
REM --------------------------------------------------------------------------
CHITEM0:
REM --------------------------------------------------------------------------
FLAGK$ = INKEY$: IF FLAGK$ = "" THEN FLAGK$ = "  "
IF MID$(FLAGK$, 2, 1) <> "" AND MID$(FLAGK$, 2, 1) = CHR$(59) THEN GOTO HELP
IF ASC(FLAGK$) >= 49 AND ASC(FLAGK$) <= 52 THEN GOTO CHITEM1
IF FLAG0 = 0 THEN LOCATE 13, 29: PRINT "_": FLAG0 = 1: GOTO CHITEM0:
IF FLAG0 = 1 THEN LOCATE 13, 29: PRINT " ": FLAG0 = 0: GOTO CHITEM0:
REM --------------------------------------------------------------------------
CHITEM1:
REM --------------------------------------------------------------------------
IF FLAGK$ = "1" THEN GOTO CRPAL
IF FLAGK$ = "2" THEN GOTO LDPAL
IF FLAGK$ = "3" THEN VFLAG = 1: GOTO LDPAL
IF FLAGK$ = "4" THEN GOTO EXPWM
END
REM --------------------------------------------------------------------------
EXPWM:
REM --------------------------------------------------------------------------
CLS : SCREEN 0, 0, 0: WIDTH 80: END
REM --------------------------------------------------------------------------
CRPAL:
REM --------------------------------------------------------------------------
CLS : BORDER 15: LOCATE 1, 12: PRINT " Create pallette "
LOCATE 5, 3: INPUT "Work NAME    - ", FLNM$: IF FLNM$ = "" THEN GOTO CRPAL
LOCATE 7, 3: INPUT "Work CAPTION - ", Caption$
REM --------------------------------------------------------------------------
FOR A = 16 TO 255
PAL(A, 1) = 63: PAL(A, 2) = 63: PAL(A, 3) = 63
PALETTE A, 4144959
NEXT A
REM --------------------------------------------------------------------------
FLNM$ = LTRIM$(UCASE$(FLNM$))
NAME$ = LTRIM$(UCASE$(FLNM$))
IF LEN(FLNM$) > 8 THEN FLNM$ = LEFT$(FLNM$, 8)
IF LEN(NAME$) > 8 THEN NAME$ = LEFT$(NAME$, 8)
IF LEN(NAME$) < 8 THEN NAME$ = NAME$ + SPACE$(8 - LEN(NAME$))
IF LEN(Caption$) > 256 THEN Caption$ = LEFT$(Caption$, 256)
IF LEN(Caption$) < 256 THEN Caption$ = Caption$ + SPACE$(256 - LEN(Caption$))
FLNM$ = FLNM$ + ".PAL"
NAME$ = NAME$ + "PAL"
REM --------------------------------------------------------------------------
CRPALEDT:
REM --------------------------------------------------------------------------
CLS : BORDER 15: LOCATE 1, 5: PRINT " Create pallette - " + FLNM$ + " "
'CLS : BORDER 15: LOCATE 1, 5: PRINT " Create pallette - " + NAME$ + " "
LINE (5, 180)-(314, 195), 15, B
REM --------------------------------------------------------------------------
LINE (176, 95)-(184, 102), 4, BF: LINE (176, 95)-(184, 102), 15, B
LINE (176, 111)-(184, 118), 2, BF: LINE (176, 111)-(184, 118), 15, B
LINE (176, 127)-(184, 134), 1, BF: LINE (176, 127)-(184, 134), 15, B
REM --------------------------------------------------------------------------
'PALETTE 4, PAL(COL, 1): PALETTE 2, 256 * PAL(COL, 2): PALETTE 1, 65536 * PAL(COL, 3)
REM --------------------------------------------------------------------------
LOCATE 5, 3: INPUT "Color NUM (16-255) - ", COL$: COL = VAL(COL$)
IF COL < 16 OR COL > 255 THEN COL = 16: LOCATE 5, 23: PRINT 16; "       "
REM --------------------------------------------------------------------------
RED = PAL(COL, 1): GRN = PAL(COL, 2): BLU = PAL(COL, 3)
FLAGATR = 0: LOCATE 13, 20: PRINT CHR$(27)
PALETTE 4, RED: PALETTE 2, 256 * GRN: PALETTE 1, 65536 * BLU
PALETTE COL, 65536 * BLU + 256 * GRN + RED: PAINT (6, 181), COL, 15
REM --------------------------------------------------------------------------
LOCATE 7, 3: PRINT "Press <TAB> to change ATR"
LOCATE 9, 3: PRINT "Press <BSP> to change COL"
LOCATE 11, 3: PRINT "Press <ESC> to END"
LOCATE 13, 3: PRINT "Attrib RED -"; PAL(COL, 1)
LOCATE 15, 3: PRINT "Attrib GRN -"; PAL(COL, 2)
LOCATE 17, 3: PRINT "Attrib BLU -"; PAL(COL, 3)
LOCATE 19, 3: PRINT "Full color -"; 65536 * PAL(COL, 3) + 256 * PAL(COL, 2) + PAL(COL, 1); "    "
REM --------------------------------------------------------------------------
CHNGATR0:
REM --------------------------------------------------------------------------
KEYA$ = INKEY$
IF KEYA$ = "" THEN GOTO CHNGATR0
IF KEYA$ = "+" THEN CURATR = CURATR + 1
IF KEYA$ = "-" THEN CURATR = CURATR - 1
IF MID$(KEYA$, 2, 1) = CHR$(73) THEN CURATR = CURATR + 16
IF MID$(KEYA$, 2, 1) = CHR$(81) THEN CURATR = CURATR - 16
IF MID$(KEYA$, 2, 1) = CHR$(71) THEN CURATR = CURATR + 8
IF MID$(KEYA$, 2, 1) = CHR$(79) THEN CURATR = CURATR - 8
IF MID$(KEYA$, 2, 1) = CHR$(82) THEN CURATR = CURATR + 4
IF MID$(KEYA$, 2, 1) = CHR$(83) THEN CURATR = CURATR - 4
REM --------------------------------------------------------------------------
IF KEYA$ = CHR$(8) THEN
	LOCATE 5, 3: PRINT SPACE$(36)
	LOCATE 5, 3: INPUT "Color NUM (16-255) - ", COL$: COL = VAL(COL$)
	IF COL < 16 OR COL > 255 THEN COL = 16: LOCATE 5, 23: PRINT 16; "       "
	RED = PAL(COL, 1): GRN = PAL(COL, 2): BLU = PAL(COL, 3)
	PALETTE 4, RED: PALETTE 2, 256 * GRN: PALETTE 1, 65536 * BLU
	IF FLAGATR = 0 THEN CURATR = PAL(COL, 1)
	IF FLAGATR = 1 THEN CURATR = PAL(COL, 2)
	IF FLAGATR = 2 THEN CURATR = PAL(COL, 3)
	PALETTE COL, 65536 * BLU + 256 * GRN + RED: PAINT (6, 181), COL, 15
END IF
REM --------------------------------------------------------------------------
IF KEYA$ = CHR$(27) THEN GOTO CPMNU
REM --------------------------------------------------------------------------
IF KEYA$ = CHR$(9) THEN
	FLAGATR = FLAGATR + 1
	IF FLAGATR > 2 THEN FLAGATR = 0
	IF FLAGATR = 0 THEN CURATR = PAL(COL, 1)
	IF FLAGATR = 1 THEN CURATR = PAL(COL, 2)
	IF FLAGATR = 2 THEN CURATR = PAL(COL, 3)
END IF
REM --------------------------------------------------------------------------
IF CURATR > 63 THEN CURATR = 63
IF CURATR < 0 THEN CURATR = 0
REM --------------------------------------------------------------------------
IF FLAGATR = 0 THEN RED = CURATR: LOCATE 13, 20: PRINT CHR$(27): LOCATE 17, 20: PRINT CHR$(32)
IF FLAGATR = 1 THEN GRN = CURATR: LOCATE 15, 20: PRINT CHR$(27): LOCATE 13, 20: PRINT CHR$(32)
IF FLAGATR = 2 THEN BLU = CURATR: LOCATE 17, 20: PRINT CHR$(27): LOCATE 15, 20: PRINT CHR$(32)
REM --------------------------------------------------------------------------
LOCATE 13, 15: PRINT RED
LOCATE 15, 15: PRINT GRN
LOCATE 17, 15: PRINT BLU
LOCATE 19, 15: PRINT 65536 * BLU + 256 * GRN + RED; "    "
REM --------------------------------------------------------------------------
PALETTE COL, 65536 * BLU + 256 * GRN + RED
PALETTE 4, RED: PALETTE 2, 256 * GRN: PALETTE 1, 65536 * BLU
PAL(COL, 1) = RED: PAL(COL, 2) = GRN: PAL(COL, 3) = BLU
GOTO CHNGATR0
REM --------------------------------------------------------------------------
CPMNU:
REM --------------------------------------------------------------------------
CLS : BORDER 15: LOCATE 1, 12: PRINT " Create pallette "
LOCATE 5, 10: PRINT "1.Save pallette"
LOCATE 7, 10: PRINT "2.Pallette info"
LOCATE 9, 10: PRINT "3.Continue edit"
LOCATE 11, 10: PRINT "4.Exit Creat Pallette"
LOCATE 13, 10: PRINT "Choose menu item -"
REM --------------------------------------------------------------------------
CPITEM0:
REM --------------------------------------------------------------------------
FLAGP$ = INKEY$: IF FLAGP$ = "" THEN FLAGP$ = " "
IF ASC(FLAGP$) >= 49 AND ASC(FLAGP$) <= 52 THEN GOTO CPITEM1
IF FLAG0 = 0 THEN LOCATE 13, 29: PRINT "_": FLAG0 = 1: GOTO CPITEM0:
IF FLAG0 = 1 THEN LOCATE 13, 29: PRINT " ": FLAG0 = 0: GOTO CPITEM0:
REM --------------------------------------------------------------------------
CPITEM1:
REM --------------------------------------------------------------------------
IF FLAGP$ = "1" THEN GOTO SAVEPal
IF FLAGP$ = "2" THEN GOTO PALInfo
IF FLAGP$ = "3" THEN GOTO CRPALEDT
IF FLAGP$ = "4" THEN GOTO WORKING
REM --------------------------------------------------------------------------
PALInfo:
REM --------------------------------------------------------------------------
CLS : BORDER 15: LOCATE 1, 12: PRINT " Pallette Infom "
LOCATE 3, 3: PRINT "initPAL  : " + initPAL$
LOCATE 5, 3: PRINT "NAME     - "; FLNM$
LOCATE 7, 3: PRINT "Caption  - "; LEFT$(Caption$, 25)
LOCATE 9, 3: PRINT "usCol    -"; usCol
LOCATE 11, 3: PRINT "fstCol   -"; fstCol
LOCATE 13, 3: PRINT "palTYPE  -"; palType
LOCATE 23, 3: PRINT "Press any key then READY ..."
REM --------------------------------------------------------------------------
TMPCOL0 = 0: TMPCOL1 = 36: TMPCOL2 = 72: TMPCOL3 = 108: TMPCOL4 = 144: TMPCOL5 = 180: TMPCOL6 = 216
REM --------------------------------------------------------------------------
LINE (15, 111)-(304, 168), 15, B
FOR TMPX = 3 TO 38
LOCATE 15, TMPX: COLOR TMPCOL0: PRINT CHR$(219): TMPCOL0 = TMPCOL0 + 1
LOCATE 16, TMPX: COLOR TMPCOL1: PRINT CHR$(219): TMPCOL1 = TMPCOL1 + 1
LOCATE 17, TMPX: COLOR TMPCOL2: PRINT CHR$(219): TMPCOL2 = TMPCOL2 + 1
LOCATE 18, TMPX: COLOR TMPCOL3: PRINT CHR$(219): TMPCOL3 = TMPCOL3 + 1
LOCATE 19, TMPX: COLOR TMPCOL4: PRINT CHR$(219): TMPCOL4 = TMPCOL4 + 1
LOCATE 20, TMPX: COLOR TMPCOL5: PRINT CHR$(219): TMPCOL5 = TMPCOL5 + 1
LOCATE 21, TMPX: COLOR TMPCOL6: PRINT CHR$(219): TMPCOL6 = TMPCOL6 + 1
NEXT TMPX
REM --------------------------------------------------------------------------
COLOR 15
PRKEY
IF VFLAG = 1 THEN VFLAG = 0: GOTO WORKING
GOTO CPMNU
REM --------------------------------------------------------------------------
SAVEPal:
REM --------------------------------------------------------------------------
CLS : CONBORD 0, 0, 319, 32, 15: CONBORD 0, 104, 319, 199, 15: LOCATE 1, 13: PRINT " Save Pallette "
VIEW PRINT 6 TO 13: DIR "*.PAL"
VIEW PRINT 1 TO 25
LOCATE 15, 3: PRINT "Filename - " + FLNM$ + "        "
LOCATE 17, 3: PRINT "Exist    - ";
REM --------------------------------------------------------------------------
TEMPFILE$ = DIR$(FLNM$)
IF FLNM$ = TEMPFILE$ THEN PRINT "True ": GOTO EXSTFL ELSE PRINT "False"
REM --------------------------------------------------------------------------
CONFIRMNM:
REM --------------------------------------------------------------------------
LOCATE 19, 3: INPUT "Confirm  - ", TEMPYN0$: TEMPYN0$ = UCASE$(LTRIM$(TEMPYN0$))
IF TEMPYN0$ <> "Y" THEN GOTO CPMNU
LOCATE 21, 3: INPUT "Old name - ", TEMPYN1$: TEMPYN1$ = UCASE$(LTRIM$(TEMPYN1$))
IF TEMPYN1$ = "N" THEN GOTO NEWNAME
LOCATE 23, 3: PRINT "PALSave activating ... ";
TMPPAL$ = CRSTRPAL(PAL())
PALSave FLNM$, NAME$, Caption$, usCol, fstCol, palType, TMPPAL$
PRINT "Done"
GOTO CPMNU
REM --------------------------------------------------------------------------
EXSTFL:
REM --------------------------------------------------------------------------
LOCATE 19, 3: INPUT "Confirm  - ", TEMPYN0$: TEMPYN0$ = UCASE$(LTRIM$(TEMPYN0$))
IF TEMPYN0$ <> "Y" THEN GOTO CPMNU
LOCATE 21, 3: INPUT "Write by - ", TEMPYN1$: TEMPYN1$ = UCASE$(LTRIM$(TEMPYN1$))
IF TEMPYN1$ = "N" THEN GOTO NEWNAME
LOCATE 23, 3: PRINT "PALSave activating ... ";
TMPPAL$ = CRSTRPAL(PAL())
PALSave FLNM$, NAME$, Caption$, usCol, fstCol, palType, TMPPAL$
PRINT "Done"
GOTO CPMNU
REM --------------------------------------------------------------------------
NEWNAME:
REM --------------------------------------------------------------------------
LOCATE 23, 3: INPUT "New name - ", NNAME$: IF NNAME$ = "" THEN LOCATE 23, 3: PRINT SPACE$(36): GOTO NEWNAME
FLNM$ = LTRIM$(UCASE$(NNAME$))
NAME$ = LTRIM$(UCASE$(NNAME$))
IF LEN(FLNM$) > 8 THEN FLNM$ = LEFT$(FLNM$, 8)
IF LEN(NAME$) > 8 THEN NAME$ = LEFT$(NAME$, 8)
IF LEN(NAME$) < 8 THEN NAME$ = NAME$ + SPACE$(8 - LEN(NAME$))
FLNM$ = FLNM$ + ".PAL"
NAME$ = NAME$ + "PAL"
GOTO SAVEPal
REM --------------------------------------------------------------------------
LDPAL:
REM --------------------------------------------------------------------------
CLS : CONBORD 0, 0, 319, 32, 15: CONBORD 0, 104, 319, 199, 15: LOCATE 1, 13: PRINT " Load Pallette "
VIEW PRINT 6 TO 13: DIR "*.PAL"
VIEW PRINT 1 TO 25
REM --------------------------------------------------------------------------
LOCATE 15, 3: INPUT "Confirm  - ", TEMPYN0$: TEMPYN0$ = UCASE$(LTRIM$(TEMPYN0$))
IF TEMPYN0$ <> "Y" THEN GOTO WORKING
REM --------------------------------------------------------------------------
LDNAME:
REM --------------------------------------------------------------------------
LOCATE 17, 3: INPUT "NAME     - ", NNAME$: IF NNAME$ = "" THEN LOCATE 17, 3: PRINT SPACE$(36): GOTO LDNAME
FLNM$ = LTRIM$(UCASE$(NNAME$))
NAME$ = LTRIM$(UCASE$(NNAME$))
IF LEN(FLNM$) > 8 THEN FLNM$ = LEFT$(FLNM$, 8)
IF LEN(NAME$) > 8 THEN NAME$ = LEFT$(NAME$, 8)
IF LEN(NAME$) < 8 THEN NAME$ = NAME$ + SPACE$(8 - LEN(NAME$))
FLNM$ = FLNM$ + ".PAL"
NAME$ = NAME$ + "PAL"
REM --------------------------------------------------------------------------
LOCATE 19, 3: PRINT "Exist    - ";
REM --------------------------------------------------------------------------
TEMPFILE$ = DIR$(FLNM$)
IF FLNM$ = TEMPFILE$ THEN PRINT "True " ELSE PRINT "False": LOCATE 19, 3: PRINT SPACE$(36): GOTO LDNAME
REM --------------------------------------------------------------------------
LOCATE 21, 3: PRINT "PALLoad activating ... ";
TMPPAL$ = "": TMPPAL$ = PALLoad$(FLNM$)
REM --------------------------------------------------------------------------
initPAL$ = LEFT$(TMPPAL$, 6):   TMPPAL$ = MID$(TMPPAL$, 7)
Caption$ = LEFT$(TMPPAL$, 256): TMPPAL$ = MID$(TMPPAL$, 257)
usCol$ = LEFT$(TMPPAL$, 2):     TMPPAL$ = MID$(TMPPAL$, 3)
fstCol$ = LEFT$(TMPPAL$, 2):    TMPPAL$ = MID$(TMPPAL$, 3)
palType$ = LEFT$(TMPPAL$, 2):   TMPPAL$ = MID$(TMPPAL$, 3)
Datas$ = LEFT$(TMPPAL$, 768)
REM --------------------------------------------------------------------------
TMPCOL = 0
FOR TMPX = 1 TO 768 STEP 3
PAL(TMPCOL, 1) = ASC(MID$(Datas$, TMPX + 0, 1))
PAL(TMPCOL, 2) = ASC(MID$(Datas$, TMPX + 1, 1))
PAL(TMPCOL, 3) = ASC(MID$(Datas$, TMPX + 2, 1))
PALETTE TMPCOL, 65536 * PAL(TMPCOL, 3) + 256 * PAL(TMPCOL, 2) + PAL(TMPCOL, 1)
TMPCOL = TMPCOL + 1
NEXT TMPX
PRINT "Done": GOTO PALInfo
REM --------------------------------------------------------------------------
HELP:
REM --------------------------------------------------------------------------
CLS : BORDER 15: LOCATE 1, 14: PRINT " Help screen "
LOCATE 3, 10: PRINT "PALlette Work Manager"
LOCATE 5, 15: PRINT "Version 1.0"
LOCATE 7, 11: PRINT "Troy computing, Inc."
LOCATE 11, 4: PRINT "Programming:      Kolevatow Sergey"
LOCATE 13, 4: PRINT "Made date  :      05/XI.1995"
LOCATE 15, 4: PRINT "Last edit  :      05/XI.1995"
LOCATE 17, 4: PRINT "Real date  :      " + DATE$
LOCATE 24, 12: PRINT "Copyright (c) 1995";
PRKEY
GOTO WORKING
REM --------------------------------------------------------------------------

SUB BORDER (COL)
LINE (0, 0)-(319, 199), COL, B
LINE (2, 2)-(317, 197), COL, B
END SUB

SUB CONBORD (X1, Y1, X2, Y2, COL)
LINE (X1, Y1)-(X2, Y2), COL, B
LINE (X1 + 2, Y1 + 2)-(X2 - 2, Y2 - 2), COL, B
END SUB

FUNCTION CRSTRPAL$ (PAL())
TMPSTR$ = ""
WHILE TMPX <> 255
TMPSTR$ = TMPSTR$ + CHR$(PAL(TMPX, 1)) + CHR$(PAL(TMPX, 2)) + CHR$(PAL(TMPX, 3))
TMPX = TMPX + 1
WEND
CRSTRPAL$ = TMPSTR$
END FUNCTION

SUB DIR (FILESPEC$)
ON LOCAL ERROR GOTO DIRERR
FILES "*.PAL"
EXIT SUB
DIRERR:
IF ERR = 53 THEN PRINT "No files " + FILESPEC$ + " found"
SOUND 100, 1
RESUME NEXT
END SUB

FUNCTION PALLoad$ (FLNM$)
REM --------------------------------------------------------------------------
TMPNUM = FREEFILE
OPEN "B", #TMPNUM, FLNM$
REM --------------------------------------------------------------------------
initPAL$ = SPACE$(6)
Caption$ = SPACE$(256)
usCol$ = SPACE$(2)
fstCol$ = SPACE$(2)
palType$ = SPACE$(2)
Datas$ = SPACE$(768)
REM --------------------------------------------------------------------------
SEEK #1, 1: GET #TMPNUM, , initPAL$
SEEK #1, 18: GET #TMPNUM, , Caption$
SEEK #1, 274: GET #TMPNUM, , usCol$
SEEK #1, 276: GET #TMPNUM, , fstCol$
SEEK #1, 278: GET #TMPNUM, , palType$
SEEK #1, 280: GET #TMPNUM, , Datas$
REM --------------------------------------------------------------------------
CLOSE #TMPNUM
REM --------------------------------------------------------------------------
'PRINT "PALLoad$ DATA$="; LEN(Datas$): SLEEP
PALLoad$ = initPAL$ + Caption$ + usCol$ + fstCol$ + palType$ + Datas$
REM --------------------------------------------------------------------------
END FUNCTION

SUB PALSave (FLNM$, NAME$, Caption$, usCol, fstCol, palType, Datas$)
REM --------------------------------------------------------------------------
TMPNUM = FREEFILE
OPEN "B", #TMPNUM, FLNM$
REM --------------------------------------------------------------------------
initPAL$ = "PAL95I"
usCol$ = MKI$(usCol)
ftsCol$ = MKI$(fstCol)
palType$ = MKI$(palType)
REM --------------------------------------------------------------------------
SEEK #1, 1: PUT #TMPNUM, , initPAL$
SEEK #1, 7: PUT #TMPNUM, , NAME$
SEEK #1, 18: PUT #TMPNUM, , Caption$
SEEK #1, 274: PUT #TMPNUM, , usCol$
SEEK #1, 276: PUT #TMPNUM, , ftsCol$
SEEK #1, 278: PUT #TMPNUM, , palType$
SEEK #1, 280: PUT #TMPNUM, , Datas$
REM --------------------------------------------------------------------------
CLOSE #TMPNUM
REM --------------------------------------------------------------------------
END SUB

SUB PRKEY
WHILE INKEY$ = ""
WEND
END SUB

