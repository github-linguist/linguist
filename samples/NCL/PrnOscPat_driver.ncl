undef("PrnOscPat_driver")
function PrnOscPat_driver(eof[*][*][*]:numeric, eof_ts[*][*]:numeric, kPOP[1]:integer)
; =================================================================
; compute Principal Oscillation Patterns (POPs)
; =================================================================
local dim_ts, dim_eof, neof, ntim, nlat, mlon, dnam_ts, dnam_eof, neof, j \
    , cov0, cov1, cov0_inverse, A, z, Z, pr, pi, zr, zi, mean, stdev      \
    , evlr, eigi, eigr   
begin

  dim_ts  = dimsizes(eof_ts)        ; (neof,ntim)
  dim_eof = dimsizes(eof)           ; (neof,nlat,mlon)

  ntim    = dim_ts(1)
  neof    = dim_eof(0)
  nlat    = dim_eof(1)
  mlon    = dim_eof(2)

  dnam_ts = getvardims(eof_ts)      ; dimension names
  dnam_eof= getvardims(eof)         ; used at end for meta data

; =================================================================
; lag-0 and lag-1  matrices 
; =================================================================

  if (get_ncl_version().eq."6.1.2") then                ; bug in 6.1.2
      cov0    = covcorm(eof_ts,(/1,0/))			; lag-0 covariance matrix
  else
      cov0    = covcorm(eof_ts,(/0,1/))			; lag-0 covariance matrix (n x n)
  end if
                                                        ; either
  cov1    = covcorm_xy(eof_ts, eof_ts, (/0,1,0/))       ; lag-1 
 ;cov1    = covcorm_xy(eof_ts(:,0:ntim-2) \             ; alternative, brute force
 ;                    ,eof_ts(:,1:ntim-1), (/0,0,0/)) 
 ;printVarSummary(cov1)

; =================================================================
; matrix A contains information for evolution of the POP system. 
; POPs are eigenvectors of A.  
; =================================================================

  cov0_inverse = inverse_matrix(cov0)
  A = cov1#inverse_matrix(cov0)                         ; [*][*] => neof x neof

; =================================================================
; NCL 6.1.1 of dgeevx:  evlr(2,2,N,N) ; (left(0)/right(1), real(0)/imag(1),:,:)
; Eigenvalues are returned as attributes: eigi  = evlr@eigi  ; eigr  = evlr@eigr
; =================================================================

  evlr  = dgeevx_lapack(A, "B", "V", "V", "B", False)  

; =================================================================
; POP time series from eigenvalues and right eigenvectors 
; ================================================================= 
 ;PR   = (/ evlr(1,0,:,:) /)         ; right ev (1), real part (0)
 ;PI   = (/ evlr(1,1,:,:) /)         ; right ev (1), imag part (1)
                                     ; kPOP is what we want; use righteigenvector
  pr   = (/ evlr(1,0,kPOP-1,:) /)    ; right ev (1), real part (0), row 'kPOP-1' 
  pi   = (/ evlr(1,1,kPOP-1,:) /)    ; right ev (1), imag part (1), row 'kPOP-1'
   
  z    = inverse_matrix( (/ (/sum(pr*pr), sum(pr*pi)/) \
                          , (/sum(pr*pi), sum(pi*pi)/) /))#(/pr,pi/)#eof_ts
   
                                                        ; complex conjugate
  z    = (/z(0,:), -z(1,:)/)       		    	; real & imag series
  z    = dim_rmvmean_n(z,1)
  mean = dim_avg_n(z,1)                                 ; calculate mean
  stdev= dim_stddev_n(z,1)                              ; calculate stdev
  z    = dim_standardize_n(z,1,1)			; standardize time series

  z!0     = "nPOP"				        ; add meta data
  z!1     = dnam_ts(1)		
  z&nPOP  = (/0,1/)
  z&$dnam_ts(1)$ = eof_ts&$dnam_ts(1)$            
  z@stdev = stdev				
  z@mean  = mean			
  z@long_name = "POP timeseries"			
 ;printVarSummary(z)

; =================================================================
; POP spatial patterns
; =================================================================

  zr = pr(0)*eof(0,:,:)			; construct POP spatial domain
  zi = pi(0)*eof(0,:,:)
  do j=1,neof-1
     zr = zr + pr(j)*eof(j,:,:)
     zi = zi + pi(j)*eof(j,:,:)
  end do

  Z   = (/zr*stdev(0), -zi*stdev(1)/)	; scale patterns by time series stdev

  Z!0 = "nPOP"				; add meta data
  Z!1 = dnam_eof(1)
  Z!2 = dnam_eof(2)

  Z&nPOP          = (/0,1/)                             
  Z&$dnam_eof(1)$ = eof&$dnam_eof(1)$    
  Z&$dnam_eof(2)$ = eof&$dnam_eof(2)$    
  Z@long_name     = "POP pattern"
 ;printVarSummary(Z)

; =================================================================
; return POP time series and POP spatial patterns as a 
; variable of type 'list' which contains 2 variables
; =================================================================

  return( [/z, Z/] )    ; this is type "list"      
end
