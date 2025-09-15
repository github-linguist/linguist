; docformat = 'rst'

;+
;
; Class to manage a spectrum.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Create and destroy an
;   AmesPAHdbIDLSuite_Spectrum-instance::
;
;     IDL> spectrum = OBJ_NEW('AmesPAHdbIDLSuite_Spectrum')
;     IDL> spectrum->Set,data
;     IDL> spectrum->Plot
;     IDL> OBJ_DESTROY,spectrum
;
; :Author:
;   Dr. Christiaan Boersma
;
; :Copyright:
;   BSD licensed
;
; :History:
;   Changes::
;
;     07-08-2022
;     Pass EXTERNAL_NNLS to FIT in MCFIT. Christiaan Boersma.
;     05-18-2022
;     Use HISTOGRAM speed-up in PLOT. Christiaan Boersma.
;     04-28-2022
;     Clean up OBSERVATION in MCFIT when internally generated.
;     Christiaan Boersma.
;     04-27-2022
;     Added MCFIT and corrected small typos in description of FIT.
;     Christiaan Boersma.
;     08-17-2021
;     Don't try and access pointer when it is not set in DESCRIPTION.
;     Christiaan Boersma.
;     05-03-2021
;     Avoid potential issues when self.uids and self.data.uid don't have 
;     the same ordering. Christiaan Boersma.
;     05-02-2021
;     Added NOTICE-keyword to FIT. Christiaan Boersma.
;     04-30-2021
;     Refactored FIT to make use of matrix operation.
;     Christiaan Boersma.
;     09-19-2017
;     Fixed !NULL value in return for FWHM in FIT.
;     06-27-2016
;     Added EXTERNAL_NNLS-keyword to FIT. Christiaan Boersma.
;     03-23-2016
;     Removed unnecessary declaration of 'weights' in FIT. Christiaan
;     Boersma.
;     03-18-2016
;     Replaced [ ] with !NULL in NNLS to avoid compiler error for IDL
;     version older than 8. Christiaan Boersma.
;     11-05-2015
;     Added REGRID. Christiaan Boersma.
;     07-12-2015
;     Added method to FIT. Christiaan Boersma.
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;-

;+
;  Output spectrum description.
;
;  :Params:
;     Str: out, optional, type="string array"
;       Ouput to Str
;
; :Categories:
;   INFORMATIVE
;-
PRO AmesPAHdbIDLSuite_Spectrum::Description,Str

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Data::Description,Str

  Str = [Str, STRING(FORMAT='(A-12,":",X,g-8.4,X,A-0)', "shift", self.shift, "cm!U-1!N")]

  Str = [Str, STRING(FORMAT='(A-12,":",X,A-0)', "profile", self.profile)]

  IF PTR_VALID(self.fwhm) THEN BEGIN

    IF SIZE(*self.fwhm, /TYPE) EQ 8 THEN BEGIN

      IF N_ELEMENTS((*self.fwhm)) GT 4 THEN fwhm = (*self.fwhm)[0:3].fwhm $
      ELSE fwhm = (*self.fwhm).fwhm

      fwhm = STRJOIN(STRTRIM(STRING(FORMAT='(g-7.3)', fwhm), 2), ",")

      fwhm += ",..."

      Str = [Str, STRING(FORMAT='(A-12,":",X,A-0,X,A-0)', "FWHM", fwhm, "cm!U-1!N")]

      Str = [Str, STRING(FORMAT='(A-12,":",X,A-0)', "|_sectioned", "yes")]

    ENDIF ELSE Str = [Str, STRING(FORMAT='(A-12,":",X,g-8.4,X,A-0)', "FWHM", *self.fwhm, "cm!U-1!N")]

  ENDIF

  Str = STRJOIN(Str, "!C")

  IF N_PARAMS() GT 0 THEN RETURN

  PRINT,STRJOIN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX), STRING(10B))
END

;+
;  Plot the spectrum.
;
;  :Keywords:
;    Wavelength: in, optional, type=int
;      Whether to set the abscissa units to wavelength
;    Stick: in, optional, type=int
;      Whether to plot the spectrum as sticks
;    Fill: in, optional, type=int
;       Whether to solid-fill the spectrum
;    Oplot: in, optional, type=int
;      Whether to draw over a previous plot
;    Legend: in, optional, type=int
;      Whether to show a legend
;    Color: in, optional, type=int
;      Color to plot the spectrum with
;    _EXTRA: in, optional, type=struct
;      Required for IDL's keyword-inheritance mechanism
;
; :Categories:
;   PLOTTING
;-
PRO AmesPAHdbIDLSuite_Spectrum::Plot,Wavelength=Wavelength,Stick=Stick,Fill=Fill,Oplot=Oplot,Legend=Legend,Color=Color,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Setup,Oplot=Oplot,XSIZE=600,YSIZE=400

  x = *self.grid

  nx = N_ELEMENTS(x)

  xunits = self.units.abscissa.str

  xrange = [MAX(x, MIN=xmin), xmin]

  IF KEYWORD_SET(Wavelength) THEN BEGIN

     x = 1D4 / x

     xrange = [MIN(x, MAX=xmax), xmax]

     xunits = 'wavelength [!Mm!Xm]'
  ENDIF

  IF NOT KEYWORD_SET(Oplot) THEN self->AmesPAHdbIDLSuite_Plot::Plot,REFORM(REBIN(x,nx,self.nuids),self.nuids*nx),(*self.data).intensity,Color=Color,XRANGE=xrange,XTITLE=xunits,YTITLE=self.units.ordinate.str,/NoData,Stick=Stick,_EXTRA=EXTRA

  IF NOT KEYWORD_SET(Color) THEN Color = 2

  h = HISTOGRAM((*self.data).uid, MIN=0, REVERSE_INDICES=ri)

  FOR i = 0, self.nuids - 1 DO BEGIN

     select = ri[ri[(*self.uids)[i]]:ri[(*self.uids)[i]+1]-1]

     self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.data)[select].intensity,Stick=Stick,Fill=Fill,COLOR=Color+i
  ENDFOR

  IF SIZE(Legend, /TYPE) EQ 0 THEN Legend = 1

  IF Legend THEN BEGIN

     self->Description,outs

     self->AmesPAHdbIDLSuite_Plot::Legend,outs
  ENDIF

  self->AmesPAHdbIDLSuite_Plot::Restore
END

;+
; Write the spectrum to file as an IPAC-table.
;
; :Params:
;   Filename: in, optional, type=string
;     Output filename
;
; :Categories:
;   OUTPUT
;-
PRO AmesPAHdbIDLSuite_Spectrum::Write,Filename

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() LT 1 THEN Filename = OBJ_CLASS(self) + '.tbl'

  timestamp = SYSTIME()
  hdr = []
  FXADDPAR,hdr,"DATE",timestamp," Date this file was generated"
  FXADDPAR,hdr,"ORIGIN","NASA Ames Research Center"," Organization generating this file"
  FXADDPAR,hdr,"CREATOR",STRING(FORMAT='("IDL",X,A0,X,"on",X,A0)', !VERSION.RELEASE, !VERSION.OS_NAME)," Software used to create this file"
  FXADDPAR,hdr,"SOFTWARE","AmesPAHdbIDLSuite"," Program used to create this file"
  FXADDPAR,hdr,"AUTHOR","Dr. C. Boersma"," Author of the program"
  FXADDPAR,hdr,"TYPE",OBJ_CLASS(self)," AmesPAHdbIDLSuite data type"

  self->Description,description

  comments = STRSPLIT(description, "!C", /EXTRACT, /REGEX, COUNT=ncomments)

  FOR i = 0L, ncomments - 1 DO FXADDPAR,hdr,"COMMENT",comments[i]

  IF self.units.abscissa.str THEN $
     abscissa = STREGEX(self.units.abscissa.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT) $
  ELSE $
     abscissa = ['', 'abscissa', '']

  IF self.units.ordinate.str THEN $
     ordinate = STREGEX(self.units.ordinate.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT) $
  ELSE $
     ordinate = ['', 'ordinate', '']

  half_abscissa_len = STRLEN(abscissa[1]) / 2
  half_ordinate_len = STRLEN(ordinate[1]) / 2

  fmt1 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)',  6 + 3) + ',' + STRING(FORMAT='(I0)',  6 - 3) + 'X,' + $
          '"|")'

  fmt2 = '("|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)',  6 + 3) + ',' + STRING(FORMAT='(I0)',  6 - 3) + 'X,' + $
          '"|")'

  half_abscissa_len = STRLEN(abscissa[2]) / 2
  half_ordinate_len = STRLEN(ordinate[2]) / 2

  fmt3 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)',  6 + 3) + ',' + STRING(FORMAT='(I0)',  6 - 3) + 'X,' + $
          '"|")'

  cols = [STRING(FORMAT=fmt1,STRUPCASE(abscissa[1]),STRUPCASE(ordinate[1]),'UID'), $
          STRING(FORMAT=fmt2,"double","double","int"), $
          STRING(FORMAT=fmt3,abscissa[2],ordinate[2],"")]

  n = N_ELEMENTS(*self.grid)
  intensities = REFORM((*self.data).intensity, n, self.nuids)
  srt = SORT(*self.uids)
  intensities = intensities[*, srt]

  OPENW,funit,Filename,/GET_LUN
  PRINTF,funit,FORMAT='("\",A0)',hdr[0:WHERE(STRPOS(hdr, 'END') EQ 0)]
  PRINTF,funit,STRJOIN(cols, STRING( 10B ))
  n = N_ELEMENTS(*self.grid)

  FOR i = 0L, self.nuids - 1L DO BEGIN
     FOR j = 0L, n - 1L DO PRINTF,funit,FORMAT='(X,F25.6,X,F25.6,X,I)',(*self.grid)[j],intensities[j,i],(*self.uids)[i]
  ENDFOR
  CLOSE,funit
  FREE_LUN,funit

  PRINT
  PRINT,"========================================================="
  PRINT,"    WRITTEN IPAC TABLE: ", Filename
  PRINT,"========================================================="
  PRINT
END

;+
;  Implementation of the non-negative least-squares algorithm.
;
;  :Params:
;    A: in, required, type="double array (2D)"
;      Matrix
;    b: in, required, type="double array (1D)"
;      Vector
;    tol: in, optional, type=double
;      Tolerance
;    max_iter: in, optional, type=long
;      Maximum number of iterations
;
; :Categories:
;   FITTING
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Spectrum::NNLS,A,b,tol,max_iter

  COMPILE_OPT IDL2

  ON_ERROR, 2

  IF N_PARAMS() LT 4 THEN BEGIN
     max_iter = 100
     IF N_PARAMS() LT 3 THEN tol = 1D-16
  ENDIF

  A        = DOUBLE(A)
  b        = DOUBLE(b)
  tol      = DOUBLE(tol)
  max_iter = LONG(max_iter)

  s_struct = SIZE(A, /STRUCTURE)
  m        = s_struct.dimensions[0]
  n        = s_struct.dimensions[1]
  w        = MAKE_ARRAY(m, VALUE=-1D)
  x        = DBLARR(m)
  P        = !NULL
  Z        = INDGEN(m)

  k        = 0L

  WHILE (Z NE !NULL) AND (k LT max_iter) DO BEGIN

     IF WHERE(-w[Z] GT tol, /NULL) EQ !NULL THEN BREAK

     k += 1L

     tapbp = A#b

     IF P EQ !NULL THEN BEGIN
        w     = -tapbp
     ENDIF ELSE BEGIN
        tapap = A#TRANSPOSE(A[P, *])
        w     = -tapbp + tapap#x[P]
     ENDELSE

     IF WHERE(-w[Z] GT tol, /NULL) EQ !NULL THEN BREAK

     wi      = MAX(-w[Z], i)
     P       = [P, Z[i]]
     nz      = N_ELEMENTS(Z)
     IF nz EQ 1 THEN BEGIN
        Z = !NULL
     ENDIF ELSE BEGIN
        Z = Z[WHERE(HISTOGRAM([i], MIN=0, MAX=nz-1) EQ 0)]
     ENDELSE

     WHILE 1 DO BEGIN

        zz    = DBLARR(m)
        zz[P] = LA_LEAST_SQUARES(A[P,*],b)

        IF WHERE(zz[P] LE 0, /NULL) EQ !NULL THEN BEGIN
           x = zz
           BREAK
        ENDIF

        P_ZN    = P[WHERE(zz[P] LE tol, /NULL)]
        alpha   = MIN(x[P_ZN]/(x[P_ZN] - zz[P_ZN]))
        x[P]    = x[P] + alpha*(zz[P]-x[P])
        temp    = WHERE(ABS(x[P]) LE tol, /NULL)
        Z       = [Z, P[temp]]
        np      = N_ELEMENTS(P)
        IF np EQ N_ELEMENTS(temp) THEN BEGIN
           P = !NULL
        ENDIF ELSE IF temp NE !NULL THEN BEGIN
           P = P[WHERE(HISTOGRAM(temp, MIN=0, MAX=np-1) EQ 0)]
        ENDIF
     ENDWHILE
  ENDWHILE
  b=x
  max_iter=k
END

;+
;  Perform a spectroscopic fit.
;
;  :Returns:
;    AmesPAHdbIDLSuite_Fitted_Spectrum
;
;  :Params:
;    observation: in, required, type="double array (1D) or AmesPAHdbIDLSuite_Observation"
;      Observed spectrum
;    error: in, optional, type="double array (1D)"
;      Uncertainties associated with observation
;
;  :Keywords:
;    EXTERNAL_NNLS: in, optional, type=int
;     Whether to use an externally defined NNLS-routine
;    NOTICE: in, optional, type=int, default=1
;     Whether to show notices
;
; :Categories:
;   FITTING
;-
FUNCTION AmesPAHdbIDLSuite_Spectrum::Fit,observation,error,EXTERNAL_NNLS=external_nnls,Notice=Notice

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF SIZE(Notice, /TYPE) EQ 0 THEN Notice = 1

  type = SIZE(observation, /STRUCTURE)

  has_error = 0

  IF type.type_name EQ 'OBJREF' THEN BEGIN

     IF OBJ_CLASS(observation) EQ 'AMESPAHDBIDLSUITE_OBSERVATION' THEN BEGIN

        observation->AbscissaUnitsTo,1,Notice=Notice

        observation_s = observation->get()

        IF NOT ARRAY_EQUAL(*self.grid, observation_s.data.x) THEN BEGIN
           PRINT
           PRINT,"========================================================="
           PRINT,"      DATA AND OBSERVATION GRIDS ARE NOT THE SAME        "
           PRINT,"========================================================="
           PRINT
           RETURN,OBJ_NEW()
        ENDIF

        IF TOTAL(observation_s.data.ystdev) GT 0 THEN has_error = 1
     ENDIF ELSE BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT," OBJECT SHOULD BE AN AMESPAHDBIDLSUITE_OBSERVATION: "+type.type_name
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN,OBJ_NEW()
     ENDELSE
  ENDIF ELSE BEGIN

     IF N_PARAMS() GT 1 THEN BEGIN

        tmp = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                      X=*self.grid, $
                      Y=observation, $
                      ErrY=error)

        has_error = 1
     ENDIF ELSE tmp = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                              X=*self.grid, $
                              Y=observation)

     observation_s = tmp->get()

     OBJ_DESTROY,tmp
  ENDELSE

  ny = N_ELEMENTS(observation_s.data.y)

  matrix = TRANSPOSE(REFORM((*self.data).intensity, ny, self.nuids))

  m = matrix

  b = observation_s.data.y - observation_s.data.continuum

  IF has_error THEN BEGIN

     IF Notice THEN BEGIN
       PRINT
       PRINT,"========================================================="
       PRINT,"                     DOING NNLC                          "
       PRINT,"========================================================="
       PRINT
     ENDIF

     b /= observation_s.data.ystdev

     m /= TRANSPOSE(REBIN(observation_s.data.ystdev, ny, self.nuids))

     method = 'NNLC'
  ENDIF ELSE BEGIN
     IF Notice THEN BEGIN
       PRINT
       PRINT,"========================================================="
       PRINT,"                     DOING NNLS                          "
       PRINT,"========================================================="
       PRINT
     ENDIF

     method = 'NNLS'
  ENDELSE

  READS,!VERSION.RELEASE,idl_version

  IF idl_version GE 8.0 AND NOT KEYWORD_SET(EXTERNAL_NNLS) THEN BEGIN

     self->NNLS,m,b

     weights = b
  ENDIF ELSE IF NOT KEYWORD_SET(EXTERNAL_NNLS) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"         FIT REQUIRES IDL VERSION 8.0 OR HIGHER: "+!VERSION.RELEASE
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN,OBJ_NEW()
  ENDIF ELSE BEGIN
     IF Notice THEN BEGIN
       PRINT
       PRINT,"========================================================="
       PRINT,"                   USING EXTERNAL NNLS                   "
       PRINT,"========================================================="
       PRINT
     ENDIF
     weights = DBLARR(self.nuids, /NOZERO)
     enorm = 0D
     w = DBLARR(self.nuids)
     indx = LONARR(self.nuids)
     mode = 0
     NNLS,m,ny,self.nuids,b,weights,enorm,w,indx,mode
  ENDELSE

  valid = WHERE(weights GT 0, nvalid)

  IF nvalid EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                UNABLE TO FIND SOLUTION                  "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN,OBJ_NEW()
  ENDIF

  IF Notice THEN BEGIN
    PRINT
    PRINT,"========================================================="
    PRINT," NOTICE: PLEASE TAKE CONSIDERABLE CARE WHEN INTERPRETING "
    PRINT," THESE RESULTS AND PUTTING THEM IN AN ASTRONOMICAL       "
    PRINT," CONTEXT. THERE ARE MANY SUBTLETIES THAT NEED TO BE TAKEN"
    PRINT," INTO ACCOUNT, RANGING FROM PAH SIZE, INCLUSION OF       "
    PRINT," HETEROATOMS, ETC. TO DETAILS OF THE APPLIED EMISSION    "
    PRINT," MODEL, BEFORE ANY THOROUGH ASSESSMENT CAN BE MADE.      "
    PRINT,"========================================================="
    PRINT
  ENDIF

  uids = (*self.data)[UNIQ((*self.data).uid)].uid

  _weights = REPLICATE({AmesPAHdbIDLSuite_Weights_S, $
                        uid:0L, $
                        weight:0D}, nvalid)

  _weights.uid = uids[valid]

  _weights.weight = weights[valid]

  data = REPLICATE({AmesPAHdbIDLSuite_Fitted_S, $
                    intensity:0D, $
                    uid:0L}, ny * nvalid)

  FOR i = 0, nvalid - 1 DO BEGIN

     data[i*ny:(i+1)*ny-1].uid = uids[valid[i]]

     data[i*ny:(i+1)*ny-1].intensity = weights[valid[i]] * REFORM(matrix[valid[i], *], /OVERWRITE)
  ENDFOR

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Fitted_Spectrum', $
                 Type=self.type, $
                 Version=self.version, $
                 Data=data, $
                 PAHdb=self.database, $
                 Uids=uids[valid], $
                 Model=*self.model, $
                 Units=self.units, $
                 Shift=self.shift, $
                 Grid=*self.grid, $
                 Profile=self.profile, $
                 FWHM=self.fwhm NE !NULL ? *self.fwhm : !NULL, $
                 Observation=observation_s, $
                 Weights=_weights, $
                 Method=method)
END

;+
;  Perform spectroscopic fits using a Monte Carlo approach.
;
;  :Returns:
;    AmesPAHdbIDLSuite_MCFitted_Spectrum
;
;  :Params:
;    observation: in, required, type="double array (1D) or AmesPAHdbIDLSuite_Observation"
;      Observed spectrum
;    error: in, required, type="double array (1D)"
;      Uncertainties associated with observation
;    samples: in, required, type=int
;      Number of Monte Carlo samples
;
;  :Keywords:
;    UNIFORM: in, optional, type=int
;     Whether to use a uniform rather than a normal distribution to permutate the errors.
;    EXTERNAL_NNLS: in, optional, type=int
;     Whether to use an externally defined NNLS-routine.
;
; :Categories:
;   FITTING
;-
FUNCTION AmesPAHdbIDLSuite_Spectrum::MCFit,observation,error,samples,EXTERNAL_NNLS=external_nnls,Uniform=Uniform

  COMPILE_OPT IDL2

  ON_ERROR,2

  type = SIZE(observation, /STRUCTURE)

  IF type.type_name EQ 'OBJREF' THEN BEGIN

    samples = error

    obs = observation

    obs_s = obs->Get()

    y = obs_s.data.y
    ystdev = obs_s.data.ystdev
  ENDIF ELSE BEGIN

     IF N_PARAMS() GT 1 THEN $
       obs = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                      X=*self.grid, $
                      Y=observation, $
                      ErrY=error) $
     ELSE $
       obs = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                     X=*self.grid, $
                     Y=observation)

     y = observation
     ystdev = error
  ENDELSE

  ny = N_ELEMENTS(y)

  obj = OBJARR(samples)

  PRINT
  PRINT,"========================================================="
  PRINT,"                 DOING MONTE CARLO                       "
  PRINT,"========================================================="
  PRINT

  PRINT
  PRINT,"========================================================="
  IF KEYWORD_SET(Uniform) THEN $ 
     PRINT,"           DRAWING FROM UNIFORM DISTRIBUTION             " $
   ELSE $
     PRINT,"            DRAWING FROM NORMAL DISTRIBUTION          "
  PRINT,"========================================================="
  PRINT

  PRINT
  PRINT,"========================================================="
  FOR i = 0L, samples - 1L DO BEGIN
  
    PRINT,FORMAT='("' + STRING(13B) + 'mc:",X,I5,"/",I5,$)',i+1L,samples

    obs->Set,Y=y + ystdev * (NOT KEYWORD_SET(Uniform) ? RANDOMU(seed, ny, /DOUBLE, /NORMAL) $
                                                      : (2D * RANDOMU(seed, ny, /DOUBLE, /UNIFORM) - 1D))

    obj[i] = self->Fit(obs, EXTERNAL_NNLS=external_nnls, Notice=0)
  ENDFOR
  PRINT
  PRINT,"========================================================="

  IF type.type_name NE 'OBJREF' THEN OBJ_DESTROY,obs

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_MCFitted_Spectrum', $
                  Type=self.type, $
                  Obj=obj, $
                  Distribution=KEYWORD_SET(Uniform) ? 'uniform' : 'normal')
END

;+
;  Resample the spectrum onto a provided grid.
;
;  :Params:
;    grid: in, required, type="double array (1D)"
;      The grid
;
; :Categories:
;   MANIPULATE
;-
PRO AmesPAHdbIDLSuite_Spectrum::Resample,grid

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                         NO DATA                         "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  ngrid1 = N_ELEMENTS(grid)

  Data = REPLICATE({AmesPAHdbSpectrum_S, $
                    intensity:0D, $
                    uid:0L}, ngrid1 * self.nuids)

  ngrid2 = N_ELEMENTS(*self.grid)

  uids = (*self.data)[UNIQ((*self.data).uid)].uid

  i = 0

  FOR j = 0, ngrid1 - 1 DO BEGIN

     idx = i

     WHILE i + 1 LT ngrid2 - 1 DO BEGIN

        IF (*self.grid)[i+1] GT grid[j] THEN BREAK

        idx = [idx, ++i]
     ENDWHILE

     FOR k = 0, self.nuids - 1 DO BEGIN

        Data[k*ngrid1+j].uid = uids[k]

        IF N_ELEMENTS(idx) EQ 1 THEN BEGIN

           IF (*self.grid)[i] GE grid[0] AND (*self.grid)[i] LE grid[ngrid1-1] THEN Data[k*ngrid1+j].intensity = (*self.data)[k*ngrid2+i].intensity
        ENDIF ELSE Data[k*ngrid1+j].intensity = MEAN((*self.data)[k*ngrid2+idx].intensity)
     ENDFOR
  ENDFOR

  PTR_FREE,self.grid

  self.grid = PTR_NEW(grid)

  PTR_FREE,self.data

  self.data = PTR_NEW(Data)
END

;+
;  Co-adds the spectra
;
;  :Returns:
;    AmesPAHdb_Coadded_Spectrum
;
;  :Keywords:
;    Weights: in, optional, type=struct
;      Use the provided weights when co-adding
;    Average: in, optional, type=int
;      Take the average
;
; :Categories:
;   CALCULATE
;-
FUNCTION AmesPAHdbIDLSuite_Spectrum::Coadd,Weights=weights,Average=Average

  COMPILE_OPT IDL2

  ON_ERROR,2

  select1 = WHERE((*self.data).uid EQ (*self.uids)[0], nselect1)

  data = REPLICATE({AmesPAHdbIDLSuite_Coadd_S, intensity:0D}, nselect1)

  data.intensity = (*self.data)[select1].intensity

  IF KEYWORD_SET(Weights) THEN BEGIN

     select2 = WHERE(Weights.uid EQ (*self.uids)[0], nselect2)

     IF nselect2 EQ 0 THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"          NO WEIGHT DEFINED FOR UID: "+STRING(FORMAT='(I-0)',(*self.uids)[0])
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN,0
     ENDIF

     data.intensity *= Weights[select2].weight
  ENDIF

  FOR i = 1, self.nuids - 1 DO BEGIN

     select1 = WHERE((*self.data).uid EQ (*self.uids)[i])

     IF KEYWORD_SET(Weights) THEN BEGIN

        select2 = WHERE(Weights.uid EQ (*self.uids)[i], nselect2)

        IF nselect2 EQ 0 THEN BEGIN
           PRINT
           PRINT,"========================================================="
           PRINT,"          NO WEIGHT DEFINED FOR UID: "+STRING(FORMAT='(I-0)',(*self.uids)[i])
           PRINT,"========================================================="
           PRINT
           self.state = 0
           RETURN,0
        ENDIF

       data.intensity += Weights[select2].weight * (*self.data)[select1].intensity
    ENDIF ELSE data.intensity += (*self.data)[select1].intensity
  ENDFOR

  IF KEYWORD_SET(Average) THEN data.intensity /= self.nuids

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Coadded_Spectrum', $
                 Type=self.type, $
                 Version=self.version, $
                 Data=data, $
                 PAHdb=self.database, $
                 Uids=*self.uids, $
                 Model=*self.model, $
                 Units=self.units, $
                 Shift=self.shift, $
                 Grid=*self.grid, $
                 Profile=self.profile, $
                 FWHM=*self.fwhm, $
                 Weights=Weights, $
                 Averaged=KEYWORD_SET(Average))
END

;+
; Retrieves the AmesPAHdbIDLSuite_Spectrum representation in a
; structure.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Spectrum::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0

  struct = self->AmesPAHdbIDLSuite_Data::Get()

  struct.type = OBJ_CLASS(self)+'_S'

  RETURN,CREATE_STRUCT(struct, 'shift', self.shift, 'grid', *self.grid, 'profile', self.profile, 'fwhm', *self.fwhm)
END

;+
; Populates the AmesPAHdbIDLSuite_Spectrum-instance.
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Type: in, optional, type=string
;     Type of Data
;   Version: in, optional, type=string
;    Versioning information
;   Data: in, optional, type=struct
;     Data structure
;   PAHdb: in, optional, type=pointer
;     Pointer to parsed database file
;   Uids: in, optional, type="long array (1D)"
;     UIDs in Data
;   Model: in, optional, type=string
;     References
;   Units: in, optional, type="AmesPAHdb_Units_S struct"
;     Units
;   Shift: in, optional, type=float
;     Shift
;   Grid: in, optional, type="float array"
;     Grid
;   Profile: in, optional, type=string
;     Profile
;   FWHM: in, optional, type=float
;     FWHM
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN BEGIN

     tag = WHERE(TAG_NAMES(Struct) EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN

        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           IF NOT KEYWORD_SET(Shift) THEN self.shift = Struct.shift

           IF NOT KEYWORD_SET(Grid) THEN BEGIN

              IF PTR_VALID(self.grid) THEN PTR_FREE,self.grid

              self.grid = PTR_NEW(Struct.grid)
           ENDIF

           IF NOT KEYWORD_SET(Profile) THEN self.profile = Struct.profile

           IF NOT KEYWORD_SET(FWHM) THEN BEGIN

              IF PTR_VALID(self.fwhm) THEN PTR_FREE,self.fwhm

              self.fwhm = PTR_NEW(Struct.fwhm)
           ENDIF

           s = Struct

           s.type = 'AMESPAHDBIDLSUITE_Data_S'

           self->AmesPAHdbIDLSuite_Data::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units
        ENDIF
     ENDIF
  ENDIF ELSE self->AmesPAHdbIDLSuite_Data::Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  IF KEYWORD_SET(Shift) THEN self.shift = Shift

  IF KEYWORD_SET(Grid) THEN BEGIN

     IF PTR_VALID(self.grid) THEN PTR_FREE,self.grid

     self.grid = PTR_NEW(Grid)
  ENDIF

  IF KEYWORD_SET(Profile) THEN self.profile = Profile

  IF KEYWORD_SET(FWHM) THEN BEGIN

     IF PTR_VALID(self.fwhm) THEN PTR_FREE,self.fwhm

     self.fwhm = PTR_NEW(FWHM)
  ENDIF
END

;+
; Retrieves the abscissa valuesa.
;
; :Returns:
;   double array (1D)
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Spectrum::GetGrid

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.grid) THEN RETURN, *self.grid

  RETURN,0
END

;+
; Clean-up an AmesPAHdbIDLSuite_Spectrum-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Cleanup

  self->AmesPAHdbIDLSuite_Data::Cleanup

  IF PTR_VALID(self.grid) THEN PTR_FREE,self.grid

  IF PTR_VALID(self.fwhm) THEN PTR_FREE,self.fwhm
END

;+
; Create an AmesPAHdbIDLSuite_Spectrum-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Spectrum-instance
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Type: in, optional, type=string
;     Type of Data
;   Version: in, optional, type=string
;    Versioning information
;   Data: in, optional, type=struct
;     Data structure
;   PAHdb: in, optional, type=pointer
;     Pointer to parsed database file
;   Uids: in, optional, type="long array (1D)"
;     UIDs in Data
;   Model: in, optional, type=string
;     References
;   Units: in, optional, type="AmesPAHdb_Units_S struct"
;     Units
;   Shift: in, optional, type=float
;     Shift
;   Grid: in, optional, type="float array"
;     Grid
;   Profile: in, optional, type=string
;     Profile
;   FWHM: in, optional, type=float
;     FWHM
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_Spectrum::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.state = self->AmesPAHdbIDLSuite_Plot::Init()

  IF self.state EQ 1 THEN BEGIN

     IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM $
     ELSE self->Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM
  ENDIF

  RETURN,self.state
END

;+
; Defines the AmesPAHdbIDLSuite_Spectrum Class
;
; :Fields:
;   shift: type=double
;     Applied band shift
;   grid: type=pointer
;     Pointer to abscissa values
;   profile: type=string
;     Applied band profile
;   fwhm: type=pointer
;     Pointer to the FWHM of the applied band profile
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Spectrum__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Spectrum, $
          INHERITS AmesPAHdbIDLSuite_Plot, $
          INHERITS AmesPAHdbIDLSuite_Data, $
          shift:0D, $
          grid:PTR_NEW(), $
          profile:'', $
          fwhm:PTR_NEW()}
END

; END OF amespahdbidlsuite_spectrum__define.pro
