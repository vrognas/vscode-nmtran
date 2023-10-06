$SIZES SD=20

$PROBLEM MAXIMAL PPRED MODEL

$DATA pheno.dta IGNORE=@

$INPUT ID TIME AMT WGT APGR DV
  EVID HT=DROP MDV

$INVALID

$VERYINVALID

$ANN

$ANNEAL

$DES

$DESIGN

$EST

$ESTIMATE METHOD=FO

$SUBROUTINE ADVAN
//#region
$ABBREVIATED PROTECT

$MIX
  P1 = THETA(2)
  P(2) = 1 - P1
  NSPOP = 2

TVF1 = THETA() ; Constrained between 0 and 1
TMP = TVF1 / (1 - TVF1) ; Constrained between 0 and +Inf
LGT = LOG(TMP) ; Constrained between -Inf and +Inf
TRX = LGT + ETA()  ; Constrained between -Inf and +Inf
X = EXP(TRX) / (1 + EXP(TRX))  ; Constrained between 0 and 1

$PK
; comment
//#endregion
A = 10E4 
B = 10E-4
C = 10**4
D = 10**-4
D = 10E(4)
D = 10**(4)
D = 10E(-4)
D = 10**(-4)

IF (A .GT. B) THEN
  C = A
ELSEIF (A .GE. B) THEN
  C = B
ELSEIF (A .LT. B) THEN
  C = B
ELSEIF (A .EQ. B) THEN
  C = B
ELSEIF (A .NEQ. B) THEN
  C = B
ELSEIF (A .EQN. B) THEN
  C = B
ELSEIF (A .NEN. B) THEN
  C = B
ELSEIF (A .NEQN. B) THEN
  C = B
ELSEIF (A > B) THEN
  C = B
ELSEIF (A >= B) THEN
  C = B
ELSEIF (A < B) THEN
  C = B
ELSEIF (A <= B) THEN
  C = B
ELSEIF (A == B) THEN
  C = B
ELSEIF (A /= B) THEN
  C = B
elseif (A =/ B) then
  C = B
ELSE
  C = 1
else
  C == 1
ENDIF

CL=THETA(1)*EXP(ETA(1)) ; Comment
V = THETA(2) * exp(ETA(2))  ;Comment in a sentence

S1=V

$ERROR
Y=F+F*EPS(1)

IPRED= LOG(F) ; input DV must be log-transformed
W = SQRT(THETA(proportional)**2 + (THETA(additive) / IPRED)**2) ; for additive+proportional RUV, on a log-scale
IWRES =(DV - IPRED) /W
Y = IPRED + W * EPS(1)

$SIGMA 1 FIX

$THETA (0,0.00469307, Inf) ; TVCL
$THETA (-Inf, 1.00916) ; TVV
$THETA (-Inf, 0, Inf) FIX ; TV Description


$OMEGA 0.0309626  ; IVCL
$OMEGA 0.031128  ; IVV
$OMEGA (-Inf, 0, Inf) FIX ; IIV Description


$SIGMA 0.013241
$SIGMA (-Inf, 1, Inf) FIX ; RUV Description

$COVARIANCE UNCONDITIONAL

$TABLE ID TIME AMT WGT APGR DV IPRED IWRES NOPRINT ONEHEADER FILE=maximal.csv

$TABLE ID TIME DV EVID MDV PRED IPRED RES IRES WRES IWRES CWRES
NOPRINT ONEHEADER FILE=sdtab ; Standard table file

$TABLE ID CL V ETAS(1:LAST)
NOPRINT ONEHEADER FILE=patab ; Model parameters - THETAs, ETAs and EPSes

$TABLE ID WT AGE
NOPRINT ONEHEADER FILE=cotab ; Continuous covariates

$TABLE ID SEX
NOPRINT ONEHEADER FILE=catab ; Categorical covariates