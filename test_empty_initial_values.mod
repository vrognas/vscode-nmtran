$PROBLEM NMTRAN Parameter Syntax Examples

; THETA parameter syntax examples
$THETA 1.5                ; Simple initial value
$THETA (0,1.5)            ; Lower bound and initial value  
$THETA (0,1.5,10)         ; Lower bound, initial value, upper bound
$THETA (0,,0.346)         ; Empty initial value (valid for THETA only)

; OMEGA parameter syntax examples (variance parameters)
$OMEGA 0.1                ; Simple initial value (variance)
$OMEGA BLOCK(1) 0.1       ; Single diagonal block
$OMEGA BLOCK(2) 0.1 0.05 0.2  ; 2x2 block matrix

; SIGMA parameter syntax examples (residual error)
$SIGMA 0.01               ; Simple initial value (residual variance)
$SIGMA 1 FIX              ; Fixed parameter