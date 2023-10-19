  ; Comment before $PROBLEM
$PROBLEM base model ; problem_text
$DATA data1.csv
      IGNORE=# ;comment in $DATA
      IGN=(DOSE.GT.0)
      ; Comment on a separate line

$INPUT ID TIME AMT WGT APGR DV
  EVID HT=DROP MDV

$PRED

IF (EVID.EQ.1) IPRED = THETA(1)*EXP(ETA(1))

IF (EVID.EQ.1) THEN
  IPRED = THETA(1)*EXP(ETA(1))
  
  ; AA = THETA(1)*EXP(ETA(1))
ELSEIF (EVID.EQ.2) THEN
  Y = IPRED + EPS(1)
ELSE
  Y = IPRED
ENDIF

IF (EVID.EQ.1) THEN
  IPRED = THETA(1)*EXP(ETA(1))
  Y = IPRED + EPS(1)
ELSE
  IPRED = 0
  Y = IPRED
ENDIF

W = -.1E-01
V = -1E1
U = -2E+1
T = -3E-1
S = -4E(1)
R = -5E(+1)
Q = -6E(-1)
P = .25**-.1E-01
O = (THETA(1)*EXP(ETA(1)))-EPS(1)**(-5)
N = THETA(1)*EXP(ETA(1))
M = ERR(1)
L = EPS(1)
K = ETA(1)
J = THETA(1)
I = EXP(((0+1*2)/(3-4))**5-1)
H = ((0+1*2)/(3-4))**5-1
G = (0+1*2)/(3-4)
F = (1*2)/(3-4)
E = (1*2)/3
D = 1*2/3
C = 1+2+3
B = 1+1
A = 1

; comment in $PRED
Y = IPRED + EPS(1)

$THTA
$OMEGA
$SIGMA
$ESTM
$COVR
$TAB
; comment last
