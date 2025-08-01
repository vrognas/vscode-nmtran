$PROBLEM    MOXONIDINE PK ANALYSIS
;;
;; run 1, FOCEI, Lag model, all data
;;
$ABBREVIATED DERIV2=NO COMRES=6
$INPUT      ID VISI XAT2=DROP DGRP=DROP DOSE=DROP FLAG=DROP ONO=DROP
            XIME=DROP DVO=DROP NEUY=DROP SCR=DROP AGE SEX NYHA=DROP WT
            COMP ACE DIG DIU NUMB=DROP TAD TIME VIDD=DROP CLCR AMT SS
            II VID CMT=DROP CONO=DROP DV EVID=DROP OVID=DROP
$DATA       mx19.csv IGNORE=@
$SUBROUTINE ADVAN2 TRANS1
$PK

;-----------OCCASIONS----------
   VIS3               = 0
   IF(VISI.EQ.3) VIS3 = 1
   VIS8               = 0
   IF(VISI.EQ.8) VIS8 = 1

;----------IOV--------------------
   
   KPCL  = VIS3*ETA(4)+VIS8*ETA(5)
   KPKA  = VIS3*ETA(6)+VIS8*ETA(7)

;---------- PK model ------------------

   TVCL  = THETA(1)*(1+THETA(6)*(CLCR-65))
   TVV   = THETA(2)*WT

   CL    = TVCL*EXP(ETA(1)+KPCL)
   V     = TVV*EXP(ETA(2))  
   KA    = THETA(3)*EXP(ETA(3)+KPKA)
   ALAG1 = THETA(4)
   K     = CL/V
   S2    = V

$ERROR

     IPRED = LOG(.025)
     W     = THETA(5)
     IF(F.GT.0) IPRED = LOG(F)
     IRES  = IPRED-DV
     IWRES = IRES/W
     Y     = IPRED+ERR(1)*W

$THETA  (0,27.5)              ;TVCL
$THETA  (0,1.565)             ;TVV
$THETA  (0,2.1)               ;TVKA
$THETA  (0,.254)              ;LAG
$THETA  (0,.23)               ;RES ERR
$THETA  (0,.008,.02941)       ;CRCL on CL

$OMEGA  BLOCK(2) .3 .1 .3     ; IIV (CL-V)
$OMEGA  BLOCK(1) .3           ; IIV KA

$OMEGA  BLOCK(1) .3           ; IOV CL 
$OMEGA  BLOCK(1) SAME         ; IOV CL 

$OMEGA  BLOCK(1) .3           ; IOV KA
$OMEGA  BLOCK(1) SAME         ; IOV KA

$SIGMA  1  FIX
$ESTIMATION METHOD=1 INTER MAXEVALS=9990 PRINT=2 POSTHOC MSFO=msfb1
$COVARIANCE PRINT=E

$TAB ID TAD IPRED IWRES VISI                ONEHEADER NOPRINT FILE = sdtab1
$TAB ID CL V KA ALAG1 ETA(1) ETA(2) ETA(3)  ONEHEADER NOPRINT FILE = patab1
$TAB ID AGE WT CLCR                         ONEHEADER NOPRINT FILE = cotab1
$TAB ID SEX ACE DIG DIU COMP                ONEHEADER NOPRINT FILE = catab1

