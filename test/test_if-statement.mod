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
