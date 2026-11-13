DECLARE SUB wingame ()
DECLARE SUB plantkilled (x%)
DECLARE SUB sick (x%)
DECLARE SUB weedkill (x%)
DECLARE SUB attackplayer (x%)
DECLARE SUB duel (x%)
DECLARE SUB robplayer (x%)
DECLARE SUB petalpushers ()
DECLARE SUB news (o$)
DECLARE SUB savestats ()
DECLARE SUB PageSysop ()
DECLARE SUB pfileout (o$)
DECLARE SUB Readnews ()
DECLARE SUB Attack ()
DECLARE SUB listplayers ()
DECLARE SUB MapSkidRow ()
DECLARE SUB Yourstats ()
DECLARE SUB quitgame ()
DECLARE SUB CurrentTime ()
DECLARE SUB Regenerate ()
DECLARE SUB plantdies ()
DECLARE SUB macro ()
DECLARE SUB Chgsleeptime (sleeptime%)
DECLARE SUB NewBlood ()
DECLARE SUB endgame ()
DECLARE SUB in (o$, length%)
DECLARE SUB yesno ()
DECLARE SUB colour (b%)
DECLARE SUB createstats ()
DECLARE SUB fileout (o$)
DECLARE SUB clr ()
DECLARE SUB zip (o$)
DIM SHARED flag AS INTEGER, ansi AS INTEGER, m$, turns%, players%, name$
DIM SHARED a AS INTEGER, localmode%, map%, maxturns%, usernum%, sysopin%
DIM SHARED abort AS INTEGER, remt%, music AS INTEGER, weedkiller%
DIM i AS INTEGER
RANDOMIZE (TIMER)
m$ = CHR$(13)

'---------------------------------------------------------------------------
name$ = "HayesHo"       'Player's alias as it appears in the userlog
usernum% = 6            'Player's user number (if applicable)
remt% = -1              'Does the player have sysop access?
ansi = -1               'Color flag
localmode% = -1         'Sets local/remote mode for backspace compat.
sysopin% = -1           'Sysop is in/out
music = 0
'---------------------------------------------------------------------------

ON ERROR GOTO handler
ON KEY(7) GOSUB SysopChat
ON KEY(8) GOSUB exitgame
ON KEY(9) GOSUB hangup
ON KEY(6) GOSUB hold
ON KEY(5) GOSUB SysopToggle
ON TIMER(210) GOSUB exitgame
KEY(6) ON: KEY(5) ON: KEY(8) ON: KEY(9) ON
IF NOT localmode% THEN KEY(7) ON: OPEN "com2:2400,N,8,1,lf" FOR RANDOM AS #2 LEN = 1

clr
pfileout "lstitle"
IF remt% <> 0 THEN zip "êSoï, êSYSOPS play mods tooï??!! áHeheï." + m$

o$ = "lsstats.dat": OPEN o$ FOR INPUT AS #1: o$ = ""
INPUT #1, turns%, players%, totalturns%: i = players%: maxturns% = turns%

DIM SHARED name$(i), un%(i + 1), bldg%(i), apt%(i), limbs%(i), plant$(i)
DIM SHARED stage%(i), sleeptime%(i), lastdate$(i), blood%(i), score(i)
DIM SHARED money%(i), traps%(i), attacked%(i)

a = 0
FOR i = 1 TO players%
INPUT #1, name$(i), un%(i), bldg%(i), apt%(i), limbs%(i), plant$(i), stage%(i), sleeptime%(i), lastdate$(i)
INPUT #1, blood%(i), score(i), money%(i), traps%(i)
IF un%(i) = usernum% THEN a = i
IF name$(i) = "empty" AND plant$(i) = "LunaTech" THEN flag = -1
NEXT
CLOSE #1

IF NOT a AND flag = 0 THEN zip "êSorryï, êthe game is full nowï.  êTry another timeï..." + m$ + m$: endgame
IF NOT a AND flag THEN NewBlood

'Begin turn:
pfileout "lsmenu"
macro
Regenerate
zip "êYouï'êre standing on Skid Rowï..." + m$ + m$ + "éYou haveë"
zip STR$(turns%) + " éturns leftï."

DO
zip m$ + "â/ê/ë/": colour 7: zip "Skid Rowï[>ë"
in o$, 1
SELECT CASE o$
CASE "r": Readnews
CASE "a": Attack
CASE "l": listplayers
CASE "m": MapSkidRow
CASE "y": Yourstats
CASE "q"
zip m$ + "éAre you SUREï? á[éYá/éNá]ï:": yesno: IF flag THEN quitgame
CASE "p": PageSysop
CASE "c": Chgsleeptime sleeptime%(a)
CASE "t": zip m$ + "êIt is now ë" + TIME$ + m$
CASE "g": IF NOT ansi THEN ansi = -1 ELSE ansi = 0
CASE "n": clr: fileout "lsinfo.drx"
CASE "i": clr: fileout "lsdrax.drx"
CASE "?": clr: pfileout "lsmenu"
END SELECT
LOOP UNTIL turns% < 1

SysopChat:
zip m$ + m$ + "ï[ êSysop chat begin ï]" + m$ + m$
DO
s$ = INKEY$
IF s$ = CHR$(27) THEN EXIT DO
IF s$ <> "" THEN PRINT s$; : PRINT #2, s$;
IF NOT EOF(2) THEN
s$ = INPUT$(1, #2)
PRINT s$;
PRINT #2, s$;
END IF
LOOP
zip m$ + m$ + "ï[ êSysop chat end ï]" + m$ + m$
RETURN

exitgame:
savestats

hangup:
endgame

SysopToggle:
sysopin% = NOT sysopin%

hold:
zip m$ + "ïPlease hold..."
SHELL
zip m$ + "ïReturning..."
RETURN

handler:
IF ERR = 53 AND o$ = "lsstats.dat" THEN createstats: RESUME
ON ERROR GOTO 0

SUB Attack
DEFINT X-Z
x = 0
20 zip m$ + "éBuilding numberï[> ë": in o$, 2
building% = VAL(o$): IF building% > 10 OR building% < 1 THEN GOTO 20
21 zip m$ + "éAnd which floorï]> ë": in o$, 2
floor% = VAL(o$): IF floor% > 4 OR floor% < 1 THEN GOTO 21

IF floor% = apt%(a) AND building% = bldg%(a) THEN zip m$ + "áYou Ding Dong!" + m$: EXIT SUB

zip m$ + m$ + m$ + "ëSearchingï..." + m$
FOR i = 1 TO players%
IF bldg%(i) = building% AND floor% = apt%(i) THEN x = i: EXIT FOR
NEXT

IF attacked%(x) THEN zip "áDeja Vué..." + m$: EXIT SUB

IF x = 0 THEN
IF RND > .5 THEN zip "ê" + plant$(a) + " ëshuddersï..." + m$
zip "ëThe place is emptyï..." + m$: petalpushers
END IF

attacked%(x) = -1
IF sleeptime%(x) <> 0 THEN y = sleeptime%(x) / 100
sleep1$ = RIGHT$(STR$(y), LEN(STR$(y)) - 1) + ":00:00"
sleep2$ = RIGHT$(STR$(y + 8), LEN(STR$(y + 8)) - 1) + ":00:00"

IF TIME$ > sleep1$ AND TIME$ < sleep2$ THEN
 zip "êYou found ë" + name$(x) + "ï, êASLEEPï!" + m$ + m$
 news "ï" + name$(a) + " ëfound ï" + name$(x) + " ësleepingï!"

 IF traps%(x) > 1 THEN
  zip "ï" + name$(x) + " êhad a trap setï!" + m$
  IF weedkiller% THEN
    zip "êYou killed it with your weedkillerï." + m$
    weedkiller% = 0: score(a) = score(a) + 50: traps%(x) = traps%(x) - 1
  ELSE
    zip "êYou got caught in the âVENUS FLYTRAP êand the pod exploded in your faceï!" + m$
    IF RND > .5 THEN z% = INT(RND * 25) + 1: zip "You lost" + STR$(z%) + " blood pintsï." + m$: blood%(a) = blood%(a) - z%
    zip "ë" + name$(x): IF RND > .5 THEN zip " slept through itï!" + m$:  ELSE zip " woke up ïÄ ëjust in time to see you running awayï!" + m$
    news "ï" + plant$(x) + " êran ï'êem offï!": petalpushers
  END IF
 END IF

 fileout "lsattack.drx"
22 zip m$ + "á/é/á/éAttack Menué[> ë": in o$, 1
 SELECT CASE o$
 CASE "r": robplayer x
 CASE "a": attackplayer x
 CASE "d": IF stage%(a) > 0 THEN duel x
 CASE "s": IF stage%(a) > 0 THEN sick x
 CASE "w": IF weedkiller% THEN weedkill x
 CASE ELSE: o$ = "x"
 END SELECT
 IF o$ = "x" THEN GOTO 22
ELSE
zip "êYou found ï" + plant$(x) + ", êAWAKEï!" + m$
IF RND < .92 THEN
zip plant$(x) + " êslapped you sillyï!" + m$ + m$ + plant$(a) + " êfed a littleï." + m$
blood%(a) = blood%(a)
ELSE
zip "éYou slashed a limb offï!" + m$ + plant$(a) + " êfeasts noisilyï..." + m$
limbs%(x) = limbs%(x) - 1: blood%(a) = blood%(a) + INT(RND * 100)
IF limbs%(x) < 1 THEN plantkilled x%
END IF
END IF
petalpushers
END SUB

DEFSNG X-Z
SUB attackplayer (x%)
zip m$ + "éYou advance onto the sleeping unsuspecting victimï..." + m$ + m$
SLEEP 2
zip "áBlood spurts everywhere as you ghoulishly slash off a limbï!" + m$ + "ï"
zip plant$(a) + " éêrubs his tummyï..." + m$
SLEEP 3
zip plant$(a) + "égrubs down on the áFRESH MEATï!"
score(a) = score(a) + 150: blood%(a) = blood%(a) + INT(51 * RND + 50)
news "ï" + plant$(a) + " égored ï" + plant$(x%) + " éBIGTIMEï!"
limbs%(x%) = limbs%(x%) - 1
IF limbs%(x%) < 1 THEN plantkilled x%
END SUB

SUB Chgsleeptime (sleeptime%)
IF stage%(a) < 1 AND flag <> 2 THEN zip m$ + m$ + "êYour plant is already asleep ïÄ êPERMANENTLYï!" + m$: EXIT SUB
zip m$ + m$ + "êUse military format ï- ë0000ï-ë2300" + m$ + m$
6 zip m$ + "áOld timeï:ë "
o$ = RIGHT$(STR$(sleeptime%), LEN(STR$(sleeptime%)) - 1)
IF LEN(o$) < 4 THEN o$ = STRING$(4 - LEN(o$), 48) + o$
zip o$ + "   éNew timeï: ": in o$, 4
IF VAL(o$) > 2300 OR VAL(o$) < 0 THEN zip m$ + "ëOopsï!" + m$: GOTO 6
IF LEN(o$) <> 4 THEN zip m$ + "éEnter four digitsï!" + m$: GOTO 6
IF RIGHT$(o$, 2) <> "00" THEN zip m$ + "éMust be an even hourï!" + m$: GOTO 6
IF sleeptime% = VAL(o$) THEN zip m$ + "ëWhat a goofballï..." + m$
IF sleeptime% <> VAL(o$) THEN zip m$ + "êOkï.." + m$
sleeptime% = VAL(o$)
END SUB

SUB clr
IF NOT localmode% AND ansi THEN zip CHR$(27) + "[2J"
CLS
END SUB

SUB colour (b%) STATIC
IF NOT ansi THEN EXIT SUB
x$ = CHR$(27) + "[": z% = 0
IF NOT intense% AND b% < 38 THEN e$ = e$ + "1;": intense% = -1
IF intense% AND b% > 37 THEN e$ = e$ + "0;": intense% = 0
IF b% > 37 THEN b% = b% - 8: z% = 8
e$ = e$ + RIGHT$(e$, 2) + "m": IF NOT localmode% THEN PRINT #2, e$;
SELECT CASE b%
CASE 30: COLOR 16 - z%
CASE 31: COLOR 4 + z%
CASE 32: COLOR 2 + z%
CASE 33: COLOR 6 + z%
CASE 34: COLOR 1 + z%
CASE 35: COLOR 5 + z%
CASE 36: COLOR 3 + z%
CASE 37: COLOR 7 + z%
END SELECT
END SUB

SUB createstats
COLOR 10: zip m$ + m$ + m$ + "êThe stats for éLittle Shop of Horrorsê do not existï!" + m$
IF NOT remt% THEN colour 25: zip "Please contact the sysop so he can set up the game": colour 31: zip "!" + m$: endgame
zip "êSet up áLittle Shop of Horrors ênow?" + m$
3 zip m$ + "â[ëYêesï, ëNêoï, ëLêaterâ]ï: ë"
in o$, 1
IF o$ = "n" OR o$ = "N" THEN
zip m$ + m$ + "ëOKï, ëstats not createdï..." + m$
endgame
END IF
IF o$ = "l" OR o$ = "L" THEN
zip m$ + m$ + "ëProcrastinatorï!" + m$
endgame
END IF
IF o$ <> "y" AND o$ <> "Y" THEN GOTO 3
zip m$ + m$ + "ëGlad to hear itï!" + m$
4 zip m$ + "éNumber of players allowedï: "
in o$, 2: players% = VAL(o$): IF players% > 49 OR players% < 5 THEN GOTO 4
5 zip m$ + "éNumber of turns per dayï: "
in o$, 2: turns% = VAL(o$): IF turns% > 30 OR turns% < 1 THEN GOTO 5
zip m$ + m$ + "êOkï.." + m$ + "ëCreating mod statsï..."
OPEN "lsstats.dat" FOR OUTPUT AS #1
WRITE #1, turns%, players%, 0
FOR i = 1 TO players%
WRITE #1, "empty", 0, 0, 0, 5, "LunaTech", 1, 2000, "00-00-0000"
WRITE #1, 300, 0, 0, 0
NEXT
CLOSE #1
zip m$ + "éStats createdï." + m$ + m$
END SUB

SUB duel (x%)
zip m$ + "ï" + plant$(a) + " êscurries off to eradicate ï" + plant$(x%) + "!" + m$ + m$
news "ï" + plant$(a) + " êchallenged ï" + plant$(x%) + " êto a duelï!"
y% = limbs%(a) + stage%(a) + blood%(a) / 100
z% = limbs%(x%) + stage%(x%) + blood%(x%) / 100

IF y% > z% THEN
zip plant$(a) + " êsubdued ï" + plant$(x%) + "!ê" + m$
news "ï" + plant$(x%) + " êlostï!"
IF z% < y% * .2 THEN zip "Your plant éKILLED ê" + plant$(x%) + "ï!" + m$ + CHR$(34) + "êGood plantï," + CHR$(34) + " êyou say as you pat " + plant$(a) + " affectionatelyï." + m$: plantkilled x%
w% = INT((money%(x%) - (money%(x%) / 2) + 1) * RND + money%(x%))
money%(a) = money%(a) + w%: money%(x%) = money%(x%) - w%
zip "you took money.. change this"

ELSEIF y% < z% THEN
zip plant$(x%) + " êsubdued ï" + plant$(a) + "!ê" + m$
zip m$ + "Your plant was too weakï!" + m$
news "ï" + plant$(x%) + " êwas too strongï!"
IF y% < z% * .2 THEN zip "êYour plant got killedï!": news "ï" + plant$(x%) + " êkilled ï" + plant$(a) + "!": plantdies
END IF
END SUB

SUB endgame
CLOSE
zip "Game end": END
'Reload BBS
END SUB

SUB fileout (o$) STATIC
abort = 0: q$ = ""
OPEN o$ FOR INPUT AS #1
DO
IF abort THEN abort = 0: zip m$: CLOSE #1: EXIT SUB
LINE INPUT #1, o$
zip o$ + m$
LOOP UNTIL (EOF(1))
CLOSE #1
END SUB

SUB in (o$, length%)
o$ = ""
10 DO
q$ = INKEY$: TIMER ON
IF NOT localmode% THEN IF NOT EOF(2) THEN q$ = INPUT$(1, #2)
IF NOT localmode% THEN IF NOT INP(766) AND 128 THEN savestats: endgame
LOOP UNTIL q$ <> ""
TIMER OFF
IF ASC(q$) > 223 AND ASC(q$) < 240 THEN GOTO 10
IF q$ = CHR$(8) AND LEN(o$) < 1 THEN GOTO 10
IF q$ = CHR$(8) AND LEN(o$) > 0 THEN o$ = LEFT$(o$, LEN(o$) - 1): zip q$
IF q$ = m$ AND LEN(o$) > 0 AND LEN(o$) <= length% THEN zip m$: EXIT SUB
IF ASC(q$) < 32 THEN GOTO 10
IF LEN(o$) >= length% THEN GOTO 10
o$ = o$ + q$: zip q$: IF length% = 1 THEN o$ = LCASE$(o$): zip m$: EXIT SUB
GOTO 10
END SUB

SUB listplayers
colour 12
zip "   ÚÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜÄÜ¿" + m$
zip "   Þ       ëNo.        Score           Nameé                               Ý" + m$
zip "   ÀßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÄßÙ" + m$
FOR i = 1 TO players%
IF name$(i) <> "empty" AND plant$(i) <> "LunaTech" THEN
zip "          ï" + STR$(i) + STRING$(11 - LEN(STR$(i)), 32)
zip STR$(score(i)) + STRING$(17 - LEN(STR$(score(i))), 32) + name$(i) + m$
END IF
NEXT
zip " á  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _" + m$
zip "    Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä Ä" + m$

END SUB

SUB macro STATIC
colour 7: zip STRING$(78, 196) + m$
x% = INT(RND * 21): q$ = "ï" + plant$(a) + CHR$(196) + m$
SELECT CASE x%
CASE 0
zip q$ + "êManï'ês best friend ï-ê his plantï!"
CASE 1
IF NOT map% THEN zip "éWe sure could use a mapï..."
CASE 2
zip q$ + "éGo out and get me some áFRESH MEATï!"
CASE 3
zip q$ + "éFeed meï, é" + name$(a) + "ï, éFEED MEï!"
CASE 4
zip "êBite my tendrilsï..."
CASE 5
zip q$ + "éLikeï, émaybe we should MOVEï?"
CASE 6
zip "éYou found an unattended ambulanceï..." + m$ + m$ + "(áGain ï100 ápints of bloodï)"
blood%(a) = blood%(a) + 100: score(a) = score(a) + 100
CASE 7
zip "êYour plant has budded a áVenus Flytrapï!"
traps%(a) = traps%(a) + 1: score(a) = score(a) + 50
CASE 8
zip "éThis round is on MEï!" + m$ + "(áGain ë2 áturnsï)"
turns% = turns% + 2
CASE 9
zip q$ + "êIt ainï'êt easy being ágreenï!"
CASE 10
zip q$ + "éGo to the áBLOOD BANK éand make me a áWITHDRAWLï!"
CASE 11
zip "ï" + plant$(a) + " áATE éa little old lady that was crossing" + m$ + "the streetï!" + m$ + m$ + "Naughty naughtyï!  éThat's ï100 épointsï!"
score(a) = score(a) + 100
CASE 12
zip "ï" + plant$(a) + " égrows a stageï...": stage%(a) = stage%(a) + 1
score(a) = score(a) + 100
CASE 13
IF NOT hh% THEN
zip "éYou get to draw this weekï'és winner in the áHybrid Horror écontestï." + m$ + "éYou reach in and pull out an envelopeï." + m$ + m$ + "éAnd the winner isï" + CHR$(196)
z% = INT(101 * RND + 75)
DO: y% = INT(RND * players%) + 1
LOOP UNTIL name$(y%) <> "empty" AND plant$(y%) <> "LunaTech"
IF y% = a THEN zip "ëYOUï!  ëYOU WONï" + STR$(z%) + " ëDOLLARSï!!!" ELSE zip name$(y%)
money%(y%) = money%(y%) + z%: score(y%) = score(y%) + 200
news "éHH Winner ïÄ " + name$(y%)
END IF
hh% = -1
CASE 14
z% = INT(RND * 100) + 1
zip "êYou foundï" + STR$(z%) + " êdollarsï!"
money%(a) = money%(a) + z%
CASE 15
zip "êYou found a bottle of weedkillerï!": weedkiller% = -1
score(a) = score(a) + 50
CASE 16
zip "êYour plant has mitesï... ë" + plant$(a) + " êis really PO'ed at youï."
score(a) = score(a) - 10
CASE 17
zip "ëThe êFreaky Flower Company Ltdï. ëmade a mistake and sent you ANOTHER"
zip m$ + "éman-eating, ábloodthirsty êplantï!" + m$ + "ëIt looks "
z% = INT(3 * RND - 1)
IF z% = 1 THEN zip "a bit bigger than ï"
IF z% = 0 THEN zip "about the same size as ï"
IF z% = -1 THEN zip "a bit smaller than ï"
zip plant$(a) + "." + m$ + m$ + "âKeep itê? "
yesno
IF NOT flag THEN zip m$ + "êOKâ, êyou feed it to " + plant$(a) + " insteadâ.ë": blood%(a) = blood%(a) + 50
IF flag THEN
zip m$ + "êOKâ, êwhat do you want to name this oneâ?" + m$ + "[ê[ëNameê]â]ï:"
in q$, 24
zip "ê" + plant$(a) + " does not like the new plantâ!" + m$ + "êThey start to fightâ...ê" + m$
y% = INT(RND * 2)
IF y% = 1 THEN
zip plant$(a) + " winsâ!  êHe slurps down the other plantâ..."
ELSEIF y% = 0 THEN
zip q$ + " winsâ!  êYou watch in horror as he devours " + plant$(a) + "â!"
plant$(a) = q$: blood%(a) = blood%(a) + z% * INT(RND * 75)
stage% = stage% + z%: IF blood% < 100 THEN blood% = 100
IF stage% < 1 THEN stage% = 1: zip m$ + "éYour new plant is ë" + plant$(a) + "ï."
END IF
END IF
CASE 18
zip "éYou found a mapï!": map% = -1
END SELECT
colour 7: zip m$ + STRING$(78, 196) + m$
END SUB

SUB MapSkidRow
flag = 0
IF NOT map% THEN zip m$ + "éYou have no mapï!" + m$: EXIT SUB
zip m$ + STRING$(31, 32) + "ïSkid Row Map!" + m$
zip "é-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-ß-" + m$
FOR i = 1 TO players%
IF name$(i) <> "empty" AND plant$(i) <> "LunaTech" AND name$(i) <> name$(a) THEN
IF sleeptime%(i) <> 0 THEN x = sleeptime%(i) / 100
sleep1$ = RIGHT$(STR$(x), LEN(STR$(x)) - 1) + ":00:00"
sleep2$ = RIGHT$(STR$(x + 8), LEN(STR$(x + 8)) - 1) + ":00:00"
IF TIME$ > sleep1$ AND TIME$ < sleep2$ THEN
flag = -1
zip "      ëSomeone is sleeping atï" + STR$(apt%(i)) + " -" + m$
zip STR$(bldg%(i)) + " ëSkid Rowï!"
END IF
END IF
NEXT
IF NOT flag THEN zip m$ + STRING$(27, 32) + "ëEveryone is awakeï!!!" + m$
END SUB

SUB NewBlood
flag = 2
sleeptime% = 2000

zip m$ + "éAHAï!  éNew áBLOODï!!" + m$ + m$ + "éYou need to enter a sleep timeï."
Chgsleeptime sleeptime%: flag = 0
zip m$ + "êEnter a name for your plantï:" + m$ + "â[ê[ëNameê]â]ï:"
in plant$, 24
zip m$ + m$ + "êHeading down to Skid Rowï..." + m$

7 bldg% = INT(RND * 10) + 1
apt% = INT(RND * 4) + 1
FOR i = 1 TO players%
IF bldg%(i) = bldg% AND apt%(i) = apt% THEN flag = -1: EXIT FOR
NEXT
IF flag THEN GOTO 7

i = 1
WHILE name$(i) <> "empty" AND plant$(i) <> "LunaTech"
i = i + 1: WEND

name$(i) = name$
un%(i) = usernum%
bldg%(i) = bldg%
apt%(i) = apt%
limbs%(i) = 5
plant$(i) = plant$
stage%(i) = 1
sleeptime%(i) = sleeptime%
score(i) = 0
traps%(i) = 0
blood%(i) = INT(101 * RND + 250)
money%(i) = 300 + INT(RND * 50)
lastdate$(i) = "00-00-0000"
a = i
news "éNew Blood ïÄ " + plant$(a)
END SUB

SUB news (o$)
END SUB

SUB PageSysop
IF sysopin% THEN
colour 9: zip m$ + "Paging sysopï..." + m$
PLAY "mft150o3l8f.l16fl8f.l16n0l16ffl8a+a+gg+.l16g+l8g+l16g+g+g+g+l8>c+l16c+c+c+c+<l8f.l16fl8f.l16fffl8a+a+gg+.l16g+l8g+l16g+g+g+g+l8>c+l16c+c+c+c+"
END IF
IF NOT sysopin% THEN pfileout "lschat"
END SUB

SUB petalpushers
colour 4: zip m$ + STRING$(78, 196) + m$
END SUB

SUB pfileout (o$) STATIC
IF ansi AND NOT localmode% THEN o$ = o$ + ".ans" ELSE o$ = o$ + ".asc"
OPEN o$ FOR INPUT AS #1
DO
LINE INPUT #1, o$
PRINT o$
FOR i = 1 TO LEN(o$)
IF NOT localmode% THEN
IF NOT EOF(2) THEN
x$ = INPUT$(1, #2)
IF x$ = " " OR x$ = CHR$(19) THEN DO: LOOP UNTIL NOT EOF(2): x$ = ""
IF x$ = "a" THEN CLOSE #1: PRINT : PRINT #2, m$: EXIT SUB
END IF
END IF
IF NOT localmode% THEN PRINT #2, MID$(o$, i, 1);
NEXT i
IF NOT localmode% THEN PRINT #2,
LOOP UNTIL (EOF(1))
CLOSE #1
END SUB

SUB plantdies
zip m$ + m$ + "êEvicting you from your houseï..." + m$
plant$(a) = "LunaTech": name$(a) = "empty": un%(a) = 0: stage%(a) = 0
SLEEP 5
clr: zip "éBetter luck NEXT timeï!" + m$ + m$: quitgame
END SUB

SUB plantkilled (x%)
zip m$ + "ïYou KILLED " + plant$(x%) + "!" + m$ + m$
score(a) = score(a) + 500
news "ï" + plant$(x%) + " got killed by " + plant$(a) + "!!!"
name$(x%) = "empty": plant$(x%) = "LunaTech": x% = -1
FOR i = 1 TO players%
IF name$(i) <> "empty" AND plant$(i) <> "LunaTech" THEN x% = x% + 1
NEXT
IF NOT x% THEN wingame
zip "éGood jobï!  éOnlyë" + STR$(x%) + " éleft to killï!" + m$
END SUB

SUB quitgame
fileout "lsquit.drx"
13 zip m$ + "â/ê/ë/": colour 7: zip "Downtownï[>ë"
in o$, 1
SELECT CASE o$
CASE "q": savestats: endgame
CASE "y": Yourstats
CASE "l": listplayers
CASE "w": fileout "lswin.drx"
CASE "c": Chgsleeptime sleeptime%(a)
CASE "r": Readnews
CASE "p": PageSysop
CASE "g": IF NOT ansi THEN ansi = -1 ELSE ansi = 0
CASE "?": clr: fileout "lsquit.drx"
END SELECT
GOTO 13
END SUB

SUB Readnews
9 zip m$ + "âÜê-âßëNêew or ëOêldï? ": in o$, 1
IF o$ = "n" THEN fileout "lsnews.dat"
IF o$ = "o" THEN fileout "lsolds.dat"
END SUB

SUB Regenerate

zip "éRegenerating áblood" + m$ + "éPintsï:ë"

blood%(a) = blood%(a) - 25

FOR k = 1 TO INT(RND * 10) + 11
IF blood%(a) > 0 THEN zip STR$(blood%(a))

IF localmode% THEN
 j = 0: x = TIMER
 WHILE j < .1
  j = TIMER - x
 WEND
END IF

IF blood%(a) > 0 THEN zip STRING$(LEN(STR$(blood%(a))), 8)
blood%(a) = blood%(a) + 1
NEXT

IF blood%(a) > 0 THEN zip STR$(blood%(a)) + m$
IF blood%(a) < 1 THEN
IF RND > .5 THEN zip "ëYour plant starved to deathï!" ELSE zip "ëYou died from MAJOR loss of áBLOODï!"
news "ï" + plant$(a) + " êdied from MAJOR áBLOOD LOSSï!"
plantdies
END IF
IF blood%(a) < 50 THEN
zip "êYour plant is starvingï! ê" + m$ + plant$(a) + " shrivels up and gets smallerï!" + m$: stage%(a) = stage%(a) - 1
IF stage%(a) < 1 THEN stage%(a) = 1
END IF

colour 8: zip STRING$(78, 196) + m$ + m$
END SUB

SUB robplayer (x%)
zip m$ + "êYou sneak up to the sleeping soul and reach for his walletï..." + m$
SLEEP 3
zip "éAnd run outta there QUICKï!" + m$ + m$ + "ëIn the wallet you find ïÄ" + m$ + CHR$(9)
z% = INT((money%(x%) - (money%(x%) / 2) + 1) * RND + money%(x%))
zip STR$(z%) + " âBUCKSï!" + m$ + m$ + "êYou rush off to spend the moneyï..."
money%(a) = money%(a) + z%: money%(x%) = money%(x%) - z%
news "ï" + name$(a) + " ërobbed ï" + name$(x%) + " ëblindï!"
END SUB

SUB savestats
CLOSE #1
zip m$ + "éSaving statsï"
lastdate$(a) = DATE$
OPEN "lsstats.dat" FOR OUTPUT AS #1
WRITE #1, maxturns%, players%, totalturns% + (maxturns% - turns%)
FOR i = 1 TO players%
WRITE #1, name$(i), un%(i), bldg%(i), apt%(i), limbs%(i), plant$(i), stage%(i), sleeptime%(i), lastdate$(i)
WRITE #1, blood%(i), score(i), money%(i), traps%(i)
zip "."
NEXT
CLOSE #1
zip m$
END SUB

SUB sick (x%)
END SUB

SUB weedkill (x%)
END SUB

SUB wingame
zip m$ + "ïYOU DID IT!! YOU KILLED THEM ALL!!!" + m$ + m$ + "ëCONGRATULATIONS!" + m$
END SUB

SUB yesno STATIC
1 DO
o$ = INKEY$
IF NOT localmode% THEN IF NOT INP(766) AND 128 THEN savestats: endgame
IF NOT localmode% THEN IF NOT EOF(2) THEN o$ = INPUT$(1, #2)
LOOP UNTIL o$ <> ""
IF o$ = "y" OR o$ = "Y" THEN
flag = -1: o$ = "x": zip "ëYesï!" + m$
ELSEIF o$ = "n" OR o$ = "N" THEN flag = 0: o$ = "x": zip "ëNoï!" + m$
END IF
IF o$ <> "x" THEN GOTO 1
END SUB

SUB Yourstats
clr
zip "             éPlayer Statsï:" + m$ + "             ëÄÄÄÄÄÄÄÄÄÄÄÄÄ" + m$
zip " êÜëÜÜÄÄÄÄÄÄÄÄÄNameï: " + name$(a) + m$
zip "âÞêÞëÞ ________Limbsï:" + STR$(limbs%(a))
IF limbs%(a) < 5 THEN zip "   ëMISSINGï" + STR$(5 - limbs%(a)) + " ëlimbsï!"
zip m$
zip "âÞêÞëÞ ________Turnsï:" + STR$(turns%) + m$
zip "âÞêÞëÞ ________Trapsï:" + STR$(traps%(a)) + m$
zip "âÞêÞëÞ ________Scoreï:" + STR$(score(a)) + m$
zip "âÞêÞëÞ ________Moneyï:" + STR$(money%(a)) + m$
zip "âÞêÞëÞ ______Addressï:" + STR$(bldg%(a)) + " -" + STR$(apt%(a)) + m$
zip "âÞêÞëÞ __Blood Pintsï:" + STR$(blood%(a)) + m$
zip " êßëßßÄÄÄSleep Timeï:" + STR$(sleeptime%(a)) + m$ + m$
zip "              âPlant Statsï:" + m$ + "              êÄÄÄÄÄÄÄÄÄÄÄÄ" + m$
zip "áÚ": colour 5: zip "ÂéÂÄÄÄÄÄÄÄÄÄÄNameê: " + plant$(a) + m$
zip "áÀ": colour 5: zip "ÁéÁÄÄÄÄÄÄÄÄGrowthê: "
SELECT CASE stage%(a)
CASE IS < 1: colour 18: zip "DECEASEDï!"
CASE 1: zip "êYoung áblood êsappling"
CASE 2 TO 3: zip "éFlesh-eating êtriffid"
CASE 4 TO 5: zip "éMan-eating plant"
CASE IS > 5: zip "êMean âGreen êMutha"
END SELECT
zip m$ + m$
IF map% THEN zip "éYou have a map of Skid Rowï." + m$
IF traps%(a) > 0 THEN zip "êOne of your traps is setï." + m$
IF weedkiller% THEN zip "êYou have some weedkillerï." + m$
END SUB

SUB zip (o$)
abort = 0
FOR i = 1 TO LEN(o$)
q$ = INKEY$: IF NOT localmode% THEN IF NOT EOF(2) THEN q$ = INPUT$(1, #2)
IF q$ = "a" THEN abort = -1: EXIT SUB
IF q$ = " " THEN IF NOT localmode% THEN DO: LOOP UNTIL INKEY$ <> "" OR NOT EOF(2) OR NOT INP(766) AND 128: q$ = INKEY$: q$ = "":  ELSE DO: LOOP UNTIL INKEY$ <> "": q$ = INKEY$: q$ = ""
SELECT CASE ASC(MID$(o$, i, 1))
CASE 224 TO 239: colour ASC(MID$(o$, i, 1)) - 194
CASE 8: LOCATE CSRLIN, POS(0) - 1: PRINT " "; : LOCATE CSRLIN, POS(0) - 1: IF NOT localmode% THEN PRINT #2, CHR$(8) + CHR$(32) + CHR$(8);
CASE ELSE: PRINT MID$(o$, i, 1); : IF NOT localmode% THEN PRINT #2, MID$(o$, i, 1);
END SELECT
NEXT
END SUB

