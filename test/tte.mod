Tue Dec 17 17:33:08 CET 2019
;; 1. Based on: 704
;; 2. Description: diuretics deletion
;; 3. Label: Basic model
;; 4. Structural model: Generalized Log-logistic hazard
;; 5. Covariate model: diuretics deletion
;; 6. Interindividual variability:
;; 7. Interoccasion variability:
;; 8. Residual variability:
;; 9. Estimation: MAXEVAL=99999 METHOD=0 LIKELIHOOD SIGL=9 NSIG=3 NOABORT SADDLE_RESET=1 PRINT=1
$SIZES      PD=-100
$PROBLEM    diuretics deletion
;Sim_start
$INPUT      ID TIME DV EVID d4te=DROP d4te_key=DROP arm arm_key=DROP
            AGE SEX sex_key=DROP WT HT infection_type
            infection_type_key=DROP PATHOGEN PATHOGEN_key=DROP
            MIC_colistin MIC_meropenem siru_colistin_1=DROP
            siru_colistin_1_numeric siru_meropenem_1=DROP
            siru_meropenem_1_numeric albumin_g_per_dl albumin_locf
            albumin_baseline albumin_key=DROP
            serum_creatinine_mg_per_dl serum_creatinine_locf
            serum_creatinine_baseline serum_creatinine_key=DROP
            urea_mg_per_dl urea_locf urea_baseline urea_key=DROP
            blood_urea_nitrogen blood_urea_nitrogen_locf
            blood_urea_nitrogen_baseline crcl_cg crcl_cg_locf
            crcl_cg_baseline crcl_cg_baseline_bsa crcl_mdrd_4
            crcl_mdrd_4_locf crcl_mdrd_4_baseline
            crcl_mdrd_4_baseline_bsa crcl_mdrd_6 crcl_mdrd_6_locf
            crcl_mdrd_6_baseline crcl_mdrd_6_baseline_bsa cauc_tafr_24
            cauc_tafr_120 cauc_tafd_120_old cavg_tafr_24 cavg_tafr_120
            cavg_tafd_120_old cavg_micc_24 cavg_micc_120 sofa_data_set
            sofa_total sofa_respiration sofa_coagulation sofa_liver
            sofa_cardiovascular sofa_cns sofa_renal
            sofa_total_baseline sofa_renal_baseline dialysis
            ttdf_data_set ttdf_on_day_0 ttdf_on_the_same_day_as_ttd
            censored_after_or_at_ttdf ttdf_after_ttd
            censored_before_ttdf ttd_data_set ttd_after_censoring
            colistin_before_rand rand_number_alphanumeric=DROP
            clinical_failure center center_key=DROP country
            country_key=DROP pk_source pk_06 pk_03 pk_01 pk_db
            pk_protocol onset approp_emp_trt_key=DROP approp_emp_trt
            diuretics rand_rifle_key=DROP rand_rifle rand_AKI
            charlson_total MIC_colistin_central
            acinetobacter_pseudomonas
$DATA      aida_20191217.csv IGNORE=@ IGNORE=(ttd_data_set==-99)
            IGNORE=(ttd_after_censoring==1) IGNORE=(onset==1)

$SUBROUTINE ADVAN=13 TOL=9
$MODEL      COMP=(HAZARD)
$PK
  SHAPE = EXP(THETA(1) + ETA(1)) ;the ETA is a placeholder here
  BASE  = EXP(THETA(2)) ; shape
  GAMMA = EXP(THETA(3))

  COV1 = EXP(THETA(4) * (sofa_total_baseline - 6))

  COV2 = EXP(THETA(5) * (AGE - 65))

  ; COV3 = 1                               ;No
  ; IF(rand_rifle==4) COV3 = EXP(THETA(6)) ; Loss

  ; COV4 = EXP(THETA(6) * (charlson_total - 2))

  ; COV5 = 1
  ; IF(diuretics==1) COV5 = EXP(THETA(6))

  COV6 = 1 ; acinetobacter_baumannii
  IF(PATHOGEN==2) COV6 = EXP(THETA(6)) ; klebsiella_pneumoniae
  IF(PATHOGEN==3) COV6 = 1             ; pseudomonas_aeruginosa
  IF(PATHOGEN==4) COV6 = EXP(THETA(6)) ; other

  ; COV7 = EXP(THETA(7) * LOG(mic_colistin))

  ; COV8 = 1                               ;No
  ; IF(rand_rifle==3) COV8 = EXP(THETA(7)) ; Failure

  COV9 = EXP(THETA(7) * (cavg_micc_120 - 5))


  COV = COV1 * COV2 * COV6 * COV9

  LAMBDA  = BASE     ; lambda=scale

$DES
  DEL=1E-6 ; to keep from taking 0**power
  DADT(1) = &
    ((LAMBDA * SHAPE * ((LAMBDA * (T + DEL)) ** (SHAPE - 1))) / &
    (1 + (GAMMA * (T + DEL)) ** SHAPE)) * COV

$ERROR

  CUMHAZ = A(1)              ;Cummulative Hazard

  SUR = EXP(-CUMHAZ)            ; S(t) Survival Probability

  DELX = 1E-6                   ; To keep from taking 0**power

  HAZ = &
    ((LAMBDA * SHAPE * ((LAMBDA * (TIME + DELX)) ** (SHAPE - 1))) / &
    (1 + (GAMMA * (TIME + DELX)) ** SHAPE)) * COV

  PDF = SUR * HAZ            ; PDF(t) = S(t) * h(t) Probability Density Function of Event

  IF(DV == 0) Y = SUR         ; Censored event (prob of survival)

  IF(DV == 1) Y = PDF         ; Event. Likelihood = PDF(t) = S(t) * h(t)

;------------For Simulation----------------
  IF(ICALL == 4) THEN
      IF(NEWIND /= 2) THEN
        CALL RANDOM (2,R)
        ;TMP=R ;store the random number
      ENDIF
      DV  = 0
      TTE = 0
    IF(TIME == 28) TTE = 1
    IF(R > SUR) THEN
      DV  = 1
      TTE = 1
    ENDIF
  ENDIF

$THETA  0.854 ; SHAPE
$THETA  -2.145 ; SCALE
$THETA  -1.315 ; SCALE_gamma
$THETA  0.218 ; SOFA
$THETA  0.021 ; AGE
; $THETA  (-2.905) ; Loss
; $THETA (0.115) ; charlson_total
; $THETA (-0.511)    ;diuretics
$THETA  -0.72 ; klebsiella_other vs acinetobacter_psudomonas
; $THETA  (0.01) ; log(mic_colistin)
; $THETA  (0.662) ; Failure
$THETA  0.066 ; cavg_micc_120
$OMEGA  0  FIX  ;   ETA_BASE

$ESTIMATION MAXEVAL=99999 METHOD=0 LIKELIHOOD SIGL=9 NSIG=3 NOABORT
            SADDLE_RESET=1 PRINT=1

$COVARIANCE UNCOND MATRIX=R PFCOND=1 PRECOND=3
