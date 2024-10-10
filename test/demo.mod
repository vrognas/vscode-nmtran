$PROBLEM For demo purposes only
$INPUT ID DV APGR TIME
$DATA d.csv IGNORE=@

$SUBROUTINES ADVAN1 TRANS1

$PK

A = THETA(1) ; Typical vale

IF (A == 0) THEN
  B = A + 1E-4 * 3**2
ELSEIF (A =/ 0) THEN ; invalid operator
  B = 0
ENDIF

$ERROR

Y = A / (EXP(ETA(1)) + EPS(1))

$THETA (-Inf, 0.1, Inf)
$OMEGA 0.01
$SIGMA 1
$ESTIMATION METHOD=1 INTERACTION MAXEVALS=9990 PRINT=2