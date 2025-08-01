;; Author: viktor.rognas@gmail.com
$SIZES      LVR=200 LVR2=100 LNP4=-100000 PD=-62
$PROBLEM    PK + CRF. No extra doses. Mean of conflicting SCr. CRCL_LINEAR.
$INPUT
  ID ;rand_number_numeric
  TIME ;time
  TAR ;time_after_randomization
  TAD ;time_after_dose
  TAFD ;time_after_first_dose
  TAFR ;time_after_first_dose_after_randomization
  CMT ;compartment
  EVID ;evid
  OCC ;occasion
  ODV ;dv
  DV ;lndv
  STYPE ;sample_type
  BLOQ ;below_limit_of_quantification
  dose_cms ;dose_cms
  AMT ;amount
  DUR ;infusion_duration
  DRATE ;infusion_rate
  RATE ;infusion_rate_flag
  d4te = DROP ;date
  CTIME = DROP ;clock_time
  DPK ;dose_from_pk
  DFP ;dose_from_protocol
  d4te_key_lab = DROP ;date_key_lab
  SCR ;creatinine_mg_per_dl mg/dL
  SCR_LOCF ;scr_locf mg/dL
  SCR_NOCB ;scr_nocb mg/dL
  SCR_LINEAR ;scr_linear mg/dL
  CRCL_LOCF ;crcl_cg_locf_abs mL/min
  CRCL_NOCB ;crcl_cg_nocb_abs mL/min
  CRCL ;crcl_cg_linear_abs mL/min
  BSA_NORM ;BSA_normalized
  CRCL_CG_LOCF ;crcl_cg_locf mL/min/m^3
  CRCL_CG_NOCB ;crcl_cg_nocb mL/min/m^3
  CRCL_CG_LINEAR ;crcl_cg_nocb mL/min/m^3
  new_row ;new_row
  DIALYSIS ;dialysis_affecting_pk
  AGE
  SEX = DROP
  WT

$DATA      pk_data_protocol.csv
  IGNORE=@
  IGNORE=(DFP==1)
  ;IGNORE(STYPE.EQ.3)

; IDs 5331 and 7430 corrected sampling/dose times.

; IGNORE(TAD.GT.12)
$ABBREVIATED PROTECT
$SUBROUTINE ADVAN=13 TOL=9
$MODEL      COMP=CMS1  ;1
            COMP=CMS2  ;2
            COMP=COL1  ;3
            COMP=INTM  ;4
            COMP=INTM2 ;5
            COMP=CAUC  ;6

; Sim_start
$PRIOR      NWPRI ; required for use of NWPRI

; Sim_end
$PK
"FIRST
" USE PRDATA, ONLY: MXSTP01
" MXSTP01=2147483647

  OCC1  = 0
  OCC2  = 0
  OCC3  = 0
  OCC4  = 0
  OCC5  = 0
  OCC6  = 0
  OCC7  = 0
  OCC8  = 0
  OCC9  = 0
  OCC10 = 0
  OCC11 = 0
  OCC12 = 0
  OCC13 = 0

  IF(OCC.EQ.1)  OCC1  = 1
  IF(OCC.EQ.2)  OCC2  = 1
  IF(OCC.EQ.3)  OCC3  = 1
  IF(OCC.EQ.4)  OCC4  = 1
  IF(OCC.EQ.5)  OCC5  = 1
  IF(OCC.EQ.6)  OCC6  = 1
  IF(OCC.EQ.7)  OCC7  = 1
  IF(OCC.EQ.8)  OCC8  = 1
  IF(OCC.EQ.9)  OCC9  = 1
  IF(OCC.EQ.10) OCC10 = 1
  IF(OCC.EQ.11) OCC11 = 1
  IF(OCC.EQ.12) OCC12 = 1
  IF(OCC.GE.13) OCC13 = 1

  IOVCL = OCC1*ETA(3)  + OCC2*ETA(4)   + OCC3*ETA(5)   + OCC4*ETA(6)   +&
          OCC5*ETA(7)  + OCC6*ETA(8)   + OCC7*ETA(9)   + OCC8*ETA(10)  +&
          OCC9*ETA(11) + OCC10*ETA(12) + OCC11*ETA(13) + OCC12*ETA(14) + OCC13*ETA(15)
  IOVFM = OCC1*ETA(16) + OCC2*ETA(17)  + OCC3*ETA(18)  + OCC4*ETA(19)  +&
          OCC5*ETA(20) + OCC6*ETA(21)  + OCC7*ETA(22)  + OCC8*ETA(23)  +&
          OCC9*ETA(24) + OCC10*ETA(25) + OCC11*ETA(26) + OCC12*ETA(27) + OCC13*ETA(28)
  IOVV2 = OCC1*ETA(29) + OCC2*ETA(30)  + OCC3*ETA(31)  + OCC4*ETA(32)  +&
          OCC5*ETA(33) + OCC6*ETA(34)  + OCC7*ETA(35)  + OCC8*ETA(36)  +&
          OCC9*ETA(37) + OCC10*ETA(38) + OCC11*ETA(39) + OCC12*ETA(40) + OCC13*ETA(41)

  CLCR = CRCL                 ; Cockgroft-gault CRCL
  IF(CRCL.EQ.-99) CLCR = 80   ; Some IDs have missing CRCL (4 of 324 IDs)
  IF(CRCL.GT.130) CLCR = 130 + (CRCL - 130) * THETA(19) ; Augmented renal clearance
  ; ---- For renal replacemnet therapy patients ---
  RRT = 0
  IF(DIALYSIS.EQ.1) RRT = 1 ; Renal replacement therapy

  TVFIL = 0
  IF(DIALYSIS.EQ.1) TVFIL = THETA(18)   ; Extra CMS renal clearance for pts on RRT (1.17 FIX)

  TVFILC = 0
  IF(DIALYSIS.EQ.1) TVFILC = THETA(9)  ; Extra colistin clearance for pts on RRT

                    CLR = (THETA(8) * CLCR * 60/1000) * EXP(ETA(44) + IOVCL) ; Renal CMS + added TVFIL here now, i.e. with IIV
  IF(DIALYSIS.EQ.1) CLR =                   TVFIL*EXP(ETA(44)+IOVCL)
  CLH  = THETA(1) * EXP(ETA(44)+IOVCL)  ; Hydrolysis/non-renal CMS
  V1   = THETA(4) * EXP(ETA(42))                   ; V1 CMS
  Q2   = THETA(5)                              ; Q2 CMS
  Q    = THETA(6) * EXP(ETA(43))                 ; Q CMS
  V2   = THETA(7) * EXP(ETA(1) + IOVV2)            ; V2 CMS
  CLCO = (THETA(3) * (1 + (CLCR - 80) * 60/1000 * THETA(17))+TVFILC) * EXP(ETA(46)*THETA(11)+IOVFM) ; CL colistin + added TVFILcoistin here now, i.e. with IIV
  VCO  = THETA(2) * (1 + RRT*THETA(10)) * EXP(IOVFM) ; V colistin
  CL2  = CLH ; Hydrolysis peripheral

  SCALE = THETA(11)

  K12 = Q/V1
  K21 = Q/V2
  K43 = CLH/V1
  K10 = CLR/V1
  K40 = CLR/V1
  K31 = 0
  K30 = CLCO/VCO
  K14 = K43
  K45 = Q2/V1
  K54 = Q2/V2
  K25 = CL2/V1

  F1 = THETA(14) ; 1 FIX

  D1 = DUR ; Infusion duration, (this parameters is called INF in Karaiskos et al 2016, https://doi.org/10.1016/j.ijantimicag.2016.06.008).
  f_u     = 0.34 ; To calculate the unbound colistin concentration
  CONVcol = 1.163000 ; Conversion factor umol/L to mg/L for colistin(1163 g/mol), Karaiskos et al. 2016, https://doi.org/10.1016/j.ijantimicag.2016.06.008

$DES
  DADT(1) =  K21*A(2) - K12*A(1) - K43*A(1) - K10* A(1)             ;CMS1c
  DADT(2) = -K21*A(2) + K12*A(1) - K43*A(2)                         ;CMS1p
  DADT(3) =                                 - K30*A(3) + K43*A(4)   ;Colistin
  DADT(4) =  K54*A(5) - K45*A(4) + K43*A(1) - K40*A(4) - K43*A(4)   ;CMS2c
  DADT(5) = -K54*A(5) + K45*A(4) + K43*A(2)                         ;CMS2p
  DADT(6) =  (A(3)*CONVcol) / VCO                                   ;AUCcol

$ERROR
  CPcms      = (A(1) / V1) + (A(4) / V1);
  CPcolu     = (A(3) / VCO) * f_u;
  CPcoltmg   = (A(3) / VCO) * CONVcol;
  CPcolumg   = CPcolu * CONVcol;
  CAUC       = A(6) ; Cumulative AUC, colistin_total_conc

  IF(TAFD <= 0) THEN
    CAVG = 0
  ELSE
    CAVG = CAUC / TAFD
  ENDIF

  IPR = 0.00001
  IF(CMT.EQ.3) IPR = A(3) / VCO
  IF(CMT.EQ.1) IPR = A(1) / V1 + A(4) / V1

               W = SQRT(THETA(15)**2 + THETA(12)**2 / (IPR**2)) * EXP(ETA(2))
  IF(CMT.EQ.1) W = SQRT(THETA(16)**2 + THETA(13)**2 / (IPR**2)) * EXP(ETA(45))

               LLOQ = LOG(0.026) ; In Karaiskos this was: LOG(0.030)
  IF(CMT.EQ.1) LLOQ = LOG(0.120 * 1000/1628)
  IPRED = LOG(0.0000000001)
  IF(IPR>0.0000000001) IPRED = LOG(IPR)

; M3-method for handling BLQ values
  DUM  = (LLOQ - IPRED) / W   ; Increase the lower the IPRED
  ;-- PHI(X) is the integral from -INF to X of N(0,1).
  DUM2 = PHI(DUM)	            ; Probability < LLOQ

    F_FLAG=0
    IRES  = DV-IPRED
    IWRES = IRES/W

  Y = IPRED + W * EPS(1)

  ;Sim_start
  BCMS = LOG(0.120*1000/1628)
  BMS = 0
  IF(CMT.EQ.1.AND.DV.LT.BCMS) BMS =1
  IF(BLOQ.EQ.1.OR.BMS.EQ.1) THEN
    F_FLAG=1
    IWRES = 0
    Y=DUM2
  ENDIF
  ;Sim_end

               STRA = 10+OCC
  IF(CMT.EQ.3) STRA = 30+OCC

$THETA  (0,5.49185) ; 1 CLH(ydrolysis)
 (0,81.1823) ; 2 V1 colistin
 (0,3.03556) ; 3 CL colistin
 (0,1.52234) ; 4 V1
 (0,7.27948) ; 5 Q2
 (0,603.98) ; 6 Q
 (0,12.9877) ; 7 V2
 (0,0.338272) ; 8 CRCL-CL cov relationship
 (0,6.63) FIX ; Pat omitted; 9 CLfilt colistin (RRT)
 (0,0.624) FIX ; Pat omitted_; 10 prop increase Vcol (RRT)
 1 FIX ; 11 Scale corr between CLCMS and CLCol
 (0,0.213913) ; 12 Add err Col
 (0,0.537686) ; 13 Add err CMS
 1 FIX ; 14 Total F (and F1) Study 3
 (0,0.339902) ; 15 Prop Err Col
 (0,0.566158) ; 16 Prop error CMS
 (-3,0.181772) ; 17 power CRCL C-G on CL colistin
 1.17 FIX ; 18 Additional CLCMS renal (RRT)
 (0,0.542839,1) ; 19 CRCL lin increase less than measured
; Prior value for theta CLH, based on from Karaiskos et al. 2016

;Sim_start
$THETAP  4.91 FIX ; 1                     prior CLH(ydrolysis)
$THETAP  69.5 FIX ; 2                        prior V1 colistin
$THETAP  4.10 FIX ; 3                        prior CL colistin
$THETAP  1.4 FIX ; 4                             prior V1 CMS
$THETAP  7.95 FIX ; 5                             prior Q2 CMS
$THETAP  463 FIX ; 6                              prior Q CMS
$THETAP  12.3 FIX ; 7                             prior V2 CMS
$THETAP  0.597 FIX ; 8           prior CRCL-CL cov relationship
$THETAP  6.63 FIX ; 9                    CLfilt colistin (RRT)
$THETAP  0.624 FIX ; 10                prop increase Vcol (RRT)
$THETAPV  1.320160E-01  FIX  ; 1            prior CLH(ydrolysis) variance
$THETAPV  4.638972E+01  FIX  ; 2                prior V colistin variance
$THETAPV  2.840890E-01  FIX  ; 3               prior CL colistin variance
$THETAPV  2.822400E-02  FIX  ; 4                    prior V1 CMS variance
$THETAPV  7.647503E-01  FIX  ; 5                    prior Q2 CMS variance
$THETAPV  3.781469E+04  FIX  ; 6                     prior Q CMS variance
$THETAPV  6.004700E-01  FIX  ; 7                    prior V2 CMS variance
$THETAPV  6.023312E-03  FIX  ; 8  prior CRCL-CL cov relationship variance
$THETAPV  6.347376E+00  FIX  ; 9     prior CLfilt colistin (RRT) variance
$THETAPV  6.230016E-02  FIX  ; 10 prior prop increase Vcol (RRT) variance
;Sim_end

$OMEGA  0  FIX  ; 1          V2
$OMEGA  0.260981  ; 2 Res err col
$OMEGA  BLOCK(1)
 0.0676983  ; 3       IOVCL
$OMEGA  BLOCK(1) SAME  ; 4       IOV_CL_OCC2
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1)
 0.0541416  ; 16      IOVFM
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1)
 0.0566257  ; 29      IOVV2
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME
$OMEGA  0  FIX  ; 42        V1_
$OMEGA  0  FIX  ; 43          Q
$OMEGA  0.0166767  ; 44    CLrenal
$OMEGA  0  FIX  ;         45
$OMEGA  0.0591144  ; 46       CLCO
$OMEGA  0  FIX  ; 47     CLHydr

;Sim_start
$OMEGAP  0  FIX  ;       1 V2
$OMEGAP  0.273  FIX  ; 2 Res err col
$OMEGAP  BLOCK(13) FIX
 0.17  ;   3  IOVCL
 0 0.17
 0 0 0.17
 0 0 0 0.17
 0 0 0 0 0.17
 0 0 0 0 0 0.17
 0 0 0 0 0 0 0.17
 0 0 0 0 0 0 0 0.17
 0 0 0 0 0 0 0 0 0.17
 0 0 0 0 0 0 0 0 0 0.17
 0 0 0 0 0 0 0 0 0 0 0.17
 0 0 0 0 0 0 0 0 0 0 0 0.17
 0 0 0 0 0 0 0 0 0 0 0 0 0.17
$OMEGAP  BLOCK(13) FIX
 0.2  ;   16 IOVFM
 0 0.2
 0 0 0.2
 0 0 0 0.2
 0 0 0 0 0.2
 0 0 0 0 0 0.2
 0 0 0 0 0 0 0.2
 0 0 0 0 0 0 0 0.2
 0 0 0 0 0 0 0 0 0.2
 0 0 0 0 0 0 0 0 0 0.2
 0 0 0 0 0 0 0 0 0 0 0.2
 0 0 0 0 0 0 0 0 0 0 0 0.2
 0 0 0 0 0 0 0 0 0 0 0 0 0.2
$OMEGAP  BLOCK(13) FIX
 0.0862  ;   29 IOVV2
 0 0.0862
 0 0 0.0862
 0 0 0 0.0862
 0 0 0 0 0.0862
 0 0 0 0 0 0.0862
 0 0 0 0 0 0 0.0862
 0 0 0 0 0 0 0 0.0862
 0 0 0 0 0 0 0 0 0.0862
 0 0 0 0 0 0 0 0 0 0.0862
 0 0 0 0 0 0 0 0 0 0 0.0862
 0 0 0 0 0 0 0 0 0 0 0 0.0862
 0 0 0 0 0 0 0 0 0 0 0 0 0.0862
$OMEGAPD  3 FIX ; 1 CL IIV and CL colistin (degrees of freedom)
$OMEGAPD  3 FIX ; 2 Res err col (degrees of freedom)
$OMEGAPD  3 FIX ; 3  IOVCL (degrees of freedom)
$OMEGAPD  3 FIX ; 16 IOVFM (degrees of freedom)
$OMEGAPD  3 FIX ; 29 IOVV2 (degrees of freedom)
;Sim_end

$SIGMA  1  FIX
$SIGMA  1  FIX

;Sim_start
;$SIMULATION (1234567)
$ESTIMATION METHOD=1 LAPLACIAN INTERACTION NSIG=3 SIGL=9
            PRINT=1 NOABORT MCETA=1 MAXEVAL=0
;Sim_end

; $COVARIANCE UNCONDITIONAL MATRIX=S
