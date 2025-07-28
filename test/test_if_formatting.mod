$PROBLEM Test IF formatting with basic indentation only

$PK
; Test basic multi-line IF/ELSEIF/ELSE/ENDIF blocks with THEN keyword

if amt>0 then
F1=1
elseif amt>10 then
F1=0.5
else
F1=0
endif

IF time > 24 THEN
CL = THETA(1) * 0.8
IF age > 65 THEN
CL = CL * 0.5
ENDIF
ELSE
CL = THETA(1)
ENDIF

; Test single-line IF statements (should NOT increase indentation)
IF amt>0 F1=1
IF dose>100 CL=THETA(1)*0.8
IF time>24 V=10

; Test mixed single-line and multi-line
IF dose>100 F1=0.9
IF time>12 THEN
CL=THETA(1)*0.8
ENDIF

; Test irregular indentation to be fixed
    if(dose==100)then
    F1=0.9
    elseif(dose==200)then
    F1=0.8
    ELSE
    F1=1.0
    ENDIF

; Test NMTRAN operators with multi-line IF
IF age.GE.65 THEN
CL = CL * 0.5
ELSEIF age.LT.18 THEN
CL = CL * 1.5
ENDIF

; Test nested IF blocks
IF amt>0 THEN
F1=1
IF dose>100 THEN
CL=CL*0.8
ENDIF
ELSEIF amt<0 THEN
F1=0
ENDIF
