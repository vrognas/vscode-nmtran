// This function returns descriptive info for a given control record for hover display.
//
// Why:
// Instead of simply naming the function 'getHoverInfoForControlRecord', we now call it
// 'explainControlRecordHover' to emphasize what the function does: it explains a control record.

export function explainControlRecordHover(controlRecord: string, fullControlRecord: string): string {
  // If an abbreviation is used, adopt the full control record name for explanation
  if (controlRecord !== fullControlRecord) {
    controlRecord = fullControlRecord;
  }

  // Return a human-readable explanation based on the recognized control record
  switch (controlRecord) {
    case '$ABBREVIATED':
      return 'Provides instructions about abbreviated code';
    case '$AES':
      return 'Marks the beginning of abbreviated code for the AES routine';
    case '$AESINITIAL':
      return 'Marks the beginning of abbreviated code for the AES routine';
    case '$ANNEAL':
      return 'Sets starting diagonal Omega values to facilitate EM search methods';
    case '$BIND':
      return 'Define data values used by `$PK`, `$DES`, and `$AES`';
    case '$CHAIN':
      return 'Supplies initial estimates for an entire problem run.';
    case '$CONTR':
      return 'Defines values for certain user-supplied routines';
    case '$COVARIANCE':
      return 'This step outputs: standard errors, covariance matrix, inverse covariance matrix, and the correlation form of the covariance matrix.';
    case '$COVR':
      return 'Synonym for $COVARIANCE.';
    case '$DATA':
      return 'Describes the NMTRAN data set.';
    case '$DEFAULT':
      return 'Specifies certain defaults for NONMEM.';
    case '$DES':
      return 'Used to compute differential equations.';
    case '$DESIGN':
      return 'Instructions for Clinical Trial Design Evaluation and Optimization';
    case '$ERROR':
      return 'Used to calculate the model result and intra-individual error in observed values.';
    case '$ESTIMATION':
      return 'Obtains parameter estimate.';
    case '$ESTM':
      return 'Obtains parameter estimate. More commonly coded as `$ESTIMATION`.';
    case '$ESTIMATE':
      return 'Obtains parameter estimate. More commonly coded as `$ESTIMATION`';
    case '$ETAS':
      return 'Specifies initial values for ETAs.';
    case '$PHIS':
      return 'Specifies initial values for PHIs.';
    case '$FORMAT':
      return 'Specifies significant digits for the NONMEM report file';
    case '$INDEX':
      return 'Defines values for the PRED/PREDPP INDXS array';
    case '$INDXS':
      return 'Defines values for the PRED/PREDPP INDXS array';
    case '$INFN':
      return 'Used to describe initialization processing for a NONMEM run, or NONMEM problem, or finalization processing for a NONMEM problem. It is used with PREDPP.';
    case '$INPUT':
      return 'Required. The items define the data item types that appear in the NMTRAN data records, and define the order of their appearance.';
    case '$LEVEL':
      return 'Specifies nested random levels above subject ID.';
    case '$MIX':
      return 'Used to describe the mixture probabilities of a mixture model. It is evaluated with individual records.';
    case '$MODEL':
      return 'Specifies the MODEL subroutine of PREDPP. Required with a general ADVAN (5,6,7,8,9,13,14,15,16,17,18).';
    case '$MSFI':
      return 'Gives the name of an input Model Specification File.';
    case '$NONPARAMETRIC':
      return 'Optional. When present, the `$ESTIMATION` record must also be present and must specify `METHOD=1` or `POSTHOC`.';
    case '$OLKJDF':
      return 'Specifies LKJ decorrelation degrees of freedom for each OMEGA block. OLKJDF is an option of the `$ESTIMATION` record. `$OLKJDF` is a separate record that allows the user to specify LKJ decorrelation degrees of freedom for each OMEGA block.';
    case '$OMEGA':
      return 'Supplies initial estimates for the NONMEM OMEGA Matrix';
    case '$OMEGAP':
      return 'Gives prior information for elements of the OMEGA matrix';
    case '$OMEGAPD':
      return 'Gives degrees of freedom (also called the dispersion factor) for OMEGA priors';
    case '$OMIT':
      return 'Optional. If a label of a data item type listed in the `$INPUT` record, or a synonym for such a data item type, appears in the `$OMIT` record, then data items of this type are excluded from template matching.';
    case '$OVARF':
      return 'Specifies the weighting to the standard deviations of OMEGA. The `$OVARF` is a separate record that allows the user to specify the weighting (inverse variance) to the standard deviations LKJ decorrelation degrees of freedom for each OMEGA block. Used with NUTS method.';
    case '$PK':
      return 'Used to model the values of basic and additional pharmacokinetic parameters. It is used with PREDPP. Basic PK parameters are typically the rate constants ("micro-constants") for use in kinetic formulas. $PK can compute instead parameters such as clearance and volume, and a translator ("TRANS") subroutine can be used to convert these to rate constants.';
    case '$PRED':
      return 'Used to model values for the DV data items. It is NOT used with PREDPP.';
    case '$PRIOR':
      return 'Optional. Specifies the use of the PRIOR feature of NONMEM. Note that `$PRIOR` is a control record, not a block of abbreviated code. Therefore, only those options that are listed here may be used. E.g., verbatim code may not be used. Options and arguments may be in any order, and may be on more than one line.';
    case '$PROBLEM':
      return 'Required. Identifies the start of a NONMEM problem specification. The text becomes a heading for the NONMEM printout.';
    case '$RCOV':
      return 'Used to load the variance-covariance matrix of estimates results from a previous problem, and use it for subsequent use in assessing total standard errors of table items without having to re-calculate the variance with a `$COVARIANCE` step.';
    case '$RCOVI':
      return 'Used to load the the variance-covariance information from the inverse-covariance file from a previous problem, and use it for subsequent use in assessing total standard errors of table items without having to re-calculate the variance with a `$COVARIANCE` step.';
    case '$SCATTERPLOT':
      return 'Requests that NONMEM generate one or more scatterplots';
    case '$SIGMA':
      return 'Supplies initial estimates for the NONMEM SIGMA Matrix';
    case '$SIGMAP':
      return 'Gives prior information for elements of the SIGMA matrix';
    case '$SIGMAPD':
      return 'Gives degrees of freedom (also called the dispersion factor) for SIGMA priors';
    case '$SIMULATION':
      return 'Optional. Requests that the NONMEM Simulation Step be implemented.';
    case '$SIMULATE':
      return 'Optional. Requests that the NONMEM Simulation Step be implemented. More commonly coded as `$SIMULATION`.';
    case '$SIML':
      return 'Optional. Requests that the NONMEM Simulation Step be implemented. More commonly coded as `$SIMULATION`.';
    case '$SIZES':
      return 'Optional. Defines Array sizes for NONMEM and PREDPP. If present, it must precede the first `$PROBLEM` or `$SUPER` record.';
    case '$SLKJDF':
      return 'Specifies LKJ decorrelation degrees of freedom for each SIGMA block. SLKJDF is an option of the `$ESTIMATION` record. `$SLKJDF` is a separate record that allows the user to specify LKJ decorrelation degrees of freedom for each SIGMA block.';
    case '$SUBROUTINES':
      return 'Optional. Describes the choice of subroutines for the NONMEM executable (also called the NONMEM load module).';
    case '$SUBS':
      return 'Optional. Describes the choice of subroutines for the NONMEM executable (also called the NONMEM load module). More commonly coded as `$SUBROUTINES`.';
    case '$SUPER':
      return 'Optional. Identifies the start of a NONMEM Superproblem.';
    case '$SVARF':
      return 'Specifies the weighting to the standard deviations of SIGMA.';
    case '$TABLE':
      return 'Requests that a NONMEM table be produced. Up to 10 `$TABLE` records may be included in a given problem.';
    case '$THETA':
      return 'Gives initial estimates and bounds for elements of the THETA matrix. Thetas are numbered in the order in which they are defined.';
    case '$THETAI':
      return 'Gives Instructions for Transforming Initial Thetas.';
    case '$THI':
      return 'Gives Instructions for Transforming Initial Thetas. More commonly coded as `$THETAI`.';
    case '$THETAP':
      return 'Gives prior information for elements of the THETA matrix';
    case '$THETAPV':
      return 'Gives variance information for THETA priors.';
    case '$THETAR':
      return 'Gives Instructions for Transforming Final Thetas';
    case '$THR':
      return 'Gives Instructions for Transforming Final Thetas. More commonly coded as `$THETAR`.';
    case '$TOL':
      return 'Used to specify compartment-specific NRD values. It is used with PREDPP’s general non-linear models (ADVAN6, ADVAN8, ADVAN9, ADVAN13, ADVAN14, ADVAN15, ADVAN16, ADVAN17, ADVAN18, and SS6 and SS9). NRD stands for "Number of Required Digits," although the precise meaning depends on the particular ADVAN or SS routine that uses it.';
    case '$TTDF':
      return 'Specifies t-distribution degrees of freedom for theta';
    case '$WARNINGS':
      return 'Control Display of NMTRAN Warning, Data Warning and Data Error messages';
    default:
      return `${controlRecord} not recognized.`;
  }
}
