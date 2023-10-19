$DES
  
  INH_BSG = 1 ;BSG not in model yet
  
  ;------ PKPD_BTP ------
  ; Inhibition of HB synthesis rate in newly produced RBC
  ; (unobserved, before RBC release to the blood)
  TRT_BTP = TREAT_BTP
  IF(T > TEND_BTP) TRT_BTP = 0
  INH_BTP = (1 - (TRT_BTP * (IMAX_BTP * DRUG**GAM_BTP) / (IC50_BTP**GAM_BTP + DRUG**GAM_BTP)))
  
  ; Observed blood RBC, MCH, HB, and PRE
  PR      = A(1)

  RT1     = A(2)
  RT2     = A(3)

  RT_IMM  = A(4)
  RT_MAT  = A(5)
  RT  = RT_IMM + RT_MAT

  RC1 = A(6)