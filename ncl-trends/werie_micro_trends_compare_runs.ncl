; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin
 c     = addfile ("/Volumes/data1/werie-hr/annual.micro.werie.nc", "r")             ; add file
 n     = addfile ("/Volumes/data1/werie-hr/annual.micro.werie.th19.nc", "r")             ; add file
 p     = addfile ("/Volumes/data1/werie-hr/annual.micro.werie.th22.nc", "r")             ; add file
 g     = addfile ("/Volumes/data1/werie-hr/annual.micro.werie.growsea.nc", "r")             ; add file
 lat   = c->lat
 lon   = c->lon
 year  = c->year

; =========================
 ; Load Duration Variables
 ; =========================
 micro_cmgr    = c->$"mic.med.mean_annual_growth"$
 micro_nmgr    = n->$"mic.med.mean_annual_growth"$
 micro_pmgr    = p->$"mic.med.mean_annual_growth"$
 micro_gmgr    = g->$"mic.med.mean_annual_growth"$

 sst    = c->mean_annual_sst
 ;printVarSummary(sst)

 micro_cfst    = c->$"mic.med.first_day"$
 micro_nfst    = n->$"mic.med.first_day"$
 micro_pfst    = p->$"mic.med.first_day"$
 micro_gfst    = g->$"mic.med.first_day"$

 micro_clst    = c->$"mic.med.last_day"$
 micro_nlst    = n->$"mic.med.last_day"$
 micro_plst    = p->$"mic.med.last_day"$
 micro_glst    = g->$"mic.med.last_day"$

 micro_cwnd    = c->$"mic.med.season_window"$
 micro_nwnd    = n->$"mic.med.season_window"$
 micro_pwnd    = p->$"mic.med.season_window"$
 micro_gwnd    = g->$"mic.med.season_window"$

 micro_cgd  = c->$"mic.med.growing_days"$
 micro_ngd  = n->$"mic.med.growing_days"$
 micro_pgd  = p->$"mic.med.growing_days"$
 micro_ggd  = g->$"mic.med.growing_days"$
 print("data loaded from netcdf")

  ; =========================
 ; Mean Statistics
 ; =========================
 mu_micro_cmgr = dim_avg_n_Wrap(micro_cmgr,0)
 mu_micro_nmgr = dim_avg_n_Wrap(micro_nmgr,0)
 mu_micro_pmgr = dim_avg_n_Wrap(micro_pmgr,0)
 mu_micro_gmgr = dim_avg_n_Wrap(micro_gmgr,0)
 ;printVarSummary(mu_micro_cmgr)

 mu_sst = dim_avg_n_Wrap(sst,0)
 ;printVarSummary(mu_sst)

 mu_micro_cfst = dim_avg_n_Wrap(micro_cfst,0)
 mu_micro_nfst = dim_avg_n_Wrap(micro_nfst,0)
 mu_micro_pfst = dim_avg_n_Wrap(micro_pfst,0)
 mu_micro_gfst = dim_avg_n_Wrap(micro_gfst,0)
 ;printVarSummary(mu_micro_cfst)

 mu_micro_clst = dim_avg_n_Wrap(micro_clst,0)
 mu_micro_nlst = dim_avg_n_Wrap(micro_nlst,0)
 mu_micro_plst = dim_avg_n_Wrap(micro_plst,0)
 mu_micro_glst = dim_avg_n_Wrap(micro_glst,0)
 ;printVarSummary(mu_micro_clst)

 mu_micro_cwnd = dim_avg_n_Wrap(micro_cwnd,0)
 mu_micro_nwnd = dim_avg_n_Wrap(micro_nwnd,0)
 mu_micro_pwnd = dim_avg_n_Wrap(micro_pwnd,0)
 mu_micro_gwnd = dim_avg_n_Wrap(micro_gwnd,0)
 ;printVarSummary(mu_micro_cwnd)

 mu_micro_cgd = dim_avg_n_Wrap(micro_cgd,0)
 mu_micro_ngd = dim_avg_n_Wrap(micro_ngd,0)
 mu_micro_pgd = dim_avg_n_Wrap(micro_pgd,0)
 mu_micro_ggd = dim_avg_n_Wrap(micro_ggd,0)
 ;printVarSummary(mu_micro_cgd)

print("means calculated")

  ; =========================
 ; Copy Attributes 
 ; =========================

  copy_VarAtts(sst(0,:,:),mu_sst(:,:))

  copy_VarAtts(micro_cmgr(0,:,:),mu_micro_cmgr(:,:))
  copy_VarAtts(micro_nmgr(0,:,:),mu_micro_nmgr(:,:))
  copy_VarAtts(micro_pmgr(0,:,:),mu_micro_pmgr(:,:))
  copy_VarAtts(micro_gmgr(0,:,:),mu_micro_gmgr(:,:))

  copy_VarAtts(micro_cfst(0,:,:),mu_micro_cfst(:,:))
  copy_VarAtts(micro_nfst(0,:,:),mu_micro_nfst(:,:))
  copy_VarAtts(micro_pfst(0,:,:),mu_micro_pfst(:,:))
  copy_VarAtts(micro_gfst(0,:,:),mu_micro_gfst(:,:))

  copy_VarAtts(micro_clst(0,:,:),mu_micro_clst(:,:))
  copy_VarAtts(micro_nlst(0,:,:),mu_micro_nlst(:,:))
  copy_VarAtts(micro_plst(0,:,:),mu_micro_plst(:,:))
  copy_VarAtts(micro_glst(0,:,:),mu_micro_glst(:,:))

  copy_VarAtts(micro_cwnd(0,:,:),mu_micro_cwnd(:,:))
  copy_VarAtts(micro_nwnd(0,:,:),mu_micro_nwnd(:,:))
  copy_VarAtts(micro_pwnd(0,:,:),mu_micro_pwnd(:,:))
  copy_VarAtts(micro_gwnd(0,:,:),mu_micro_gwnd(:,:))

  copy_VarAtts(micro_cgd(0,:,:),mu_micro_cgd(:,:))
  copy_VarAtts(micro_ngd(0,:,:),mu_micro_ngd(:,:))
  copy_VarAtts(micro_pgd(0,:,:),mu_micro_pgd(:,:))
  copy_VarAtts(micro_ggd(0,:,:),mu_micro_ggd(:,:))

print("attributes copied")
 

;  ; ;  ; ===========================
; ; ; ; Trends
; ; ; ; ===========================
; ;  ;trends
; ;  ;mann-kendal trend
  mk_micro_cmgr = trend_manken(micro_cmgr,True,0)
  mk_micro_nmgr = trend_manken(micro_nmgr,True,0)
  mk_micro_pmgr = trend_manken(micro_pmgr,True,0)
  mk_micro_gmgr = trend_manken(micro_gmgr,True,0)
  
  mk_sst = trend_manken(sst,True,0)

  mk_micro_cfst = trend_manken(micro_cfst,True,0)
  mk_micro_nfst = trend_manken(micro_nfst,True,0)
  mk_micro_pfst = trend_manken(micro_pmgr,True,0)
  mk_micro_gfst = trend_manken(micro_gmgr,True,0)

  mk_micro_clst = trend_manken(micro_clst,True,0)
  mk_micro_nlst = trend_manken(micro_nlst,True,0)
  mk_micro_plst = trend_manken(micro_plst,True,0)
  mk_micro_glst = trend_manken(micro_glst,True,0)

  mk_micro_cwnd = trend_manken(micro_cwnd,True,0)
  mk_micro_nwnd = trend_manken(micro_nwnd,True,0)
  mk_micro_pwnd = trend_manken(micro_pwnd,True,0)
  mk_micro_gwnd = trend_manken(micro_gwnd,True,0)

  mk_micro_cgd = trend_manken(micro_cgd,True,0)
  mk_micro_ngd = trend_manken(micro_ngd,True,0)
  mk_micro_pgd = trend_manken(micro_pgd,True,0)
  mk_micro_ggd = trend_manken(micro_ggd,True,0)
  print("trends calculated")
  
; ; copy variable coordinates
  
  copy_VarCoords(micro_cmgr(0,:,:),mk_micro_cmgr(0,:,:))
  copy_VarCoords(micro_nmgr(0,:,:),mk_micro_nmgr(0,:,:))
  copy_VarCoords(micro_pmgr(0,:,:),mk_micro_pmgr(0,:,:))
  copy_VarCoords(micro_gmgr(0,:,:),mk_micro_gmgr(0,:,:))

  copy_VarCoords(sst(0,:,:),mk_sst(0,:,:))

  copy_VarCoords(micro_cfst(0,:,:),mk_micro_cfst(0,:,:))
  copy_VarCoords(micro_nfst(0,:,:),mk_micro_nfst(0,:,:))
  copy_VarCoords(micro_pfst(0,:,:),mk_micro_pfst(0,:,:))
  copy_VarCoords(micro_gfst(0,:,:),mk_micro_gfst(0,:,:))

  copy_VarCoords(micro_clst(0,:,:),mk_micro_clst(0,:,:))
  copy_VarCoords(micro_nlst(0,:,:),mk_micro_nlst(0,:,:))
  copy_VarCoords(micro_plst(0,:,:),mk_micro_plst(0,:,:))
  copy_VarCoords(micro_glst(0,:,:),mk_micro_glst(0,:,:))

  copy_VarCoords(micro_cwnd(0,:,:),mk_micro_cwnd(0,:,:))
  copy_VarCoords(micro_nwnd(0,:,:),mk_micro_nwnd(0,:,:))
  copy_VarCoords(micro_pwnd(0,:,:),mk_micro_pwnd(0,:,:))
  copy_VarCoords(micro_gwnd(0,:,:),mk_micro_gwnd(0,:,:))

  copy_VarCoords(micro_cgd(0,:,:),mk_micro_cgd(0,:,:))
  copy_VarCoords(micro_ngd(0,:,:),mk_micro_ngd(0,:,:))
  copy_VarCoords(micro_pgd(0,:,:),mk_micro_pgd(0,:,:))
  copy_VarCoords(micro_ggd(0,:,:),mk_micro_ggd(0,:,:))
print("coordinates set for variables")

; ; copy variable attributes

  copy_VarAtts(micro_cmgr(0,:,:),mk_micro_cmgr(0,:,:))
  copy_VarAtts(micro_nmgr(0,:,:),mk_micro_nmgr(0,:,:))
  copy_VarAtts(micro_pmgr(0,:,:),mk_micro_pmgr(0,:,:))
  copy_VarAtts(micro_gmgr(0,:,:),mk_micro_gmgr(0,:,:))

  copy_VarAtts(sst(0,:,:),mk_sst(0,:,:))

  copy_VarAtts(micro_cfst(0,:,:),mk_micro_cfst(0,:,:))
  copy_VarAtts(micro_nfst(0,:,:),mk_micro_nfst(0,:,:))
  copy_VarAtts(micro_pfst(0,:,:),mk_micro_pfst(0,:,:))
  copy_VarAtts(micro_gfst(0,:,:),mk_micro_gfst(0,:,:))

  copy_VarAtts(micro_clst(0,:,:),mk_micro_clst(0,:,:))
  copy_VarAtts(micro_nlst(0,:,:),mk_micro_nlst(0,:,:))
  copy_VarAtts(micro_plst(0,:,:),mk_micro_plst(0,:,:))
  copy_VarAtts(micro_glst(0,:,:),mk_micro_glst(0,:,:))

  copy_VarAtts(micro_cwnd(0,:,:),mk_micro_cwnd(0,:,:))
  copy_VarAtts(micro_nwnd(0,:,:),mk_micro_nwnd(0,:,:))
  copy_VarAtts(micro_pwnd(0,:,:),mk_micro_pwnd(0,:,:))
  copy_VarAtts(micro_gwnd(0,:,:),mk_micro_gwnd(0,:,:))

  copy_VarAtts(micro_cgd(0,:,:),mk_micro_cgd(0,:,:))
  copy_VarAtts(micro_ngd(0,:,:),mk_micro_ngd(0,:,:))
  copy_VarAtts(micro_pgd(0,:,:),mk_micro_pgd(0,:,:))
  copy_VarAtts(micro_ggd(0,:,:),mk_micro_ggd(0,:,:))
print("attributes set for variables")

;  Correct Units and Long Names for Plotting

  mk_micro_cfst@units = "Julian days per year"
  mk_micro_cfst@long_name = "Trend in bloom onset date"
  mk_micro_nfst@units = "Julian days per year"
  mk_micro_nfst@long_name = "Trend in bloom onset date"
  mk_micro_pfst@units = "Julian days per year"
  mk_micro_pfst@long_name = "Trend in bloom onset date"
  mk_micro_gfst@units = "Julian days per year"
  mk_micro_gfst@long_name = "Trend in bloom onset date"

  mk_micro_clst@units = "Julian days per year"
  mk_micro_clst@long_name = "Trend in bloom termination date"
  mk_micro_nlst@units = "Julian days per year"
  mk_micro_nlst@long_name = "Trend in bloom termination date"
  mk_micro_plst@units = "Julian days per year"
  mk_micro_plst@long_name = "Trend in bloom termination date"
  mk_micro_glst@units = "Julian days per year"
  mk_micro_glst@long_name = "Trend in bloom termination date"

  mk_micro_cwnd@units = "Julian days per year"
  mk_micro_cwnd@long_name = "Trend in bloom season length"
  mk_micro_nwnd@units = "Julian days per year"
  mk_micro_nwnd@long_name = "Trend in bloom season length"
  mk_micro_pwnd@units = "Julian days per year"
  mk_micro_pwnd@long_name = "Trend in bloom season length"
  mk_micro_gwnd@units = "Julian days per year"
  mk_micro_gwnd@long_name = "Trend in bloom season length"

  mk_micro_cgd@units = "Julian days per year"
  mk_micro_cgd@long_name = "Trend in annual growing days"
  mk_micro_ngd@units = "Julian days per year"
  mk_micro_ngd@long_name = "Trend in annual growing days"
  mk_micro_pgd@units = "Julian days per year"
  mk_micro_pgd@long_name = "Trend in annual growing days"
  mk_micro_ggd@units = "Julian days per year"
  mk_micro_ggd@long_name = "Trend in annual growing days"

  print("units and long names assigned")
; ; ; ===========================
; ; ; Figure List
; ; ; 1: Start Day Trend & Mean
; ; ; 2: End Day Trend & Mean
; ; ; 3: firstumulated Above Trend & Mean
; ; ; 4: lastsonal Days Trend & Mean
; ; ; 6: Number of Events Trend & Mean

; ===========================
; Figure 1a: Mean Growth Rate: Compare Thresholds
; ===========================

 wks_type = "pdf"
 wks_type@wkPaperSize = "B"
 wks_type@wkOrientation = "landscape"
 wks   = gsn_open_wks (wks_type, "../pdf/mean_gr_micro_werie_compare_thres.pdf") 

 plot = new(6,graphic)
 pres = True
 pres@gsnMaximize    = True                ; maximize plots

 ;SUB FIGURE A
 resA                 = True                    ; plot mods desired
 resA@gsnDraw         = False                   ; don't draw yet
 resA@gsnFrame        = False                   ; don't advance frame yet
 resA@tiMainOn        = False

;  ;NE US map domain and details
 resA@mpMinLonF   = -83.75
 resA@mpMaxLonF   = -82.45
 resA@mpMinLatF   = 41.25
 resA@mpMaxLatF   = 42.25
 resA@tmXBLabelFontHeightF = 0.02                 ; resA tick labels
 resA@tmYLLabelFontHeightF = 0.02    
 resA@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
 resA@gsnMajorLonSpacing = 1              ; change maj lon tm spacing
 resA@mpDataBaseVersion = "HighRes"

 ; contouring
 resA@cnLinesOn       = False                   ; turn off contour lines
 resA@cnLineLabelsOn  = False                   ; tuen off line labels
 resA@cnFillOn        = True                    ; turn on color
 ;resA@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
  resA@cnMinLevelValF       =  0.15               ; set min contour level
  resA@cnMaxLevelValF       =  0.35             ; set max contour level
  resA@cnLevelSpacingF      =  0.01               ; set contour spacing
 resA@cnMissingValFillColor = "black"
 
  ; cmap = read_colormap_file("amwg256")
  ; cmap = cmap(::-1,:) ; reverse the color map
  ; resA@cnFillPalette   = cmap
 resA@cnFillPalette = "amwg256"

  resA@cnFillMode      = "RasterFill"
  resA@cnFillDrawOrder      = "PreDraw"     ;add coastline after contour fill to 
 
 ;label bar 
 resA@lbLabelBarOn    = True                   ; 
 resA@lbOrientation        = "Vertical"         ; vertical label bar
 resA@lbLabelFontHeightF = 0.02

 ;figure text
 resA@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resA@gsnCenterString    = "Control Annual Mean"   ; add titles
 resA@gsnLeftString    = " "
 resA@gsnRightString    = " "

 ;address warnings
 resA@trGridType           = "TriangularMesh"
 resA@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(mgr)
 ;mu_mgr = mask(mu_mgr,lsd.eq.1,True)    ;this is probably no longer needed
 resA@mpFillOn                     = True   ;don't fill in things like rivers
 resA@mpFillDrawOrder      = "PostDraw"     ;add coastline after contour fill to 
 resA@mpLandFillColor        = "grey81"  
 plot(1) = gsn_csm_contour_map_ce(wks,mu_micro_cmgr(:,:), resA)
 ;print("plot 1a is done")

;  SUB Figure B
resB                 = True                    ; plot mods desired
 resB@gsnDraw         = False                   ; don't draw yet
 resB@gsnFrame        = False                   ; don't advance frame yet
 resB@tiMainOn        = False

;  ;NE US map domain and details
 resB@mpMinLonF   = -83.75
 resB@mpMaxLonF   = -82.45
 resB@mpMinLatF   = 41.25
 resB@mpMaxLatF   = 42.25
 resB@tmXBLabelFontHeightF = 0.02                 ; resB tick labels
 resB@tmYLLabelFontHeightF = 0.02   
 resB@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
 resB@gsnMajorLonSpacing = 1              ; change maj lon tm spacing 
 resB@mpDataBaseVersion = "HighRes"

 ; contouring
 resB@cnLinesOn       = False                   ; turn off contour lines
 resB@cnLineLabelsOn  = False                   ; tuen off line labels
 resB@cnFillOn        = True   
 resB@cnFillMode      = "RasterFill"
 resB@cnFillDrawOrder      = "PreDraw"     ;add coastline after contour fill to 
 ;delete(cmap) 
 ; cmap = read_colormap_file("WhiteYellowOrangeRed")
 ; cmap = cmap(::-1,:) ; reverse the color map
 ; resB@cnFillPalette   = cmap
 ;resB@cnFillPalette   = "BlueDarkOrange18"
 resB@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resB@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
  resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
  resB@cnMinLevelValF       =  0.0000              ; set min contour level
  resB@cnMaxLevelValF       =  0.0030               ; set max contour level
  resB@cnLevelSpacingF      =  0.0002               ; set contour spacing

 ;map
 resB@mpFillOn                     = True   ;don't fill in things like rivers
 resB@mpFillDrawOrder      = "PostDraw"     ;add coastline after contour fill to 
 resB@mpLandFillColor        = "grey81"  

 ;label bar 
 resB@lbLabelBarOn    = True                   ; 
 resB@lbOrientation        = "Vertical"         ; vertical label bar
 resB@lbLabelFontHeightF = 0.02

 ;figure text
 resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resB@gsnCenterString    = "Trend Control (20.5)"   ; add titles
 resB@gsnLeftString    = " "
 resB@gsnRightString    = " "

 ;address warnings
 resB@trGridType           = "TriangularMesh"
 resB@gsnAddCyclic         = False

; ; ;stippling overlay -- invisble contours on map
 res2 = True                            ; res2 probability plots
 res2@gsnDraw             = False       ; Do not draw plot
 res2@gsnFrame            = False       ; Do not advance frome
 res2@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
 res2@cnMinLevelValF      = 0.89        ; set min contour level
 res2@cnMaxLevelValF      = 1.01        ; set max contour level
 res2@cnLevelSpacingF     = 0.01        ; set contour spacing
 res2@cnInfoLabelOn       = False       ; turn off info label
 res2@cnLinesOn           = False       ; do not draw contour lines
 res2@cnLineLabelsOn      = False       ; do not draw contour labels
 res2@cnFillScaleF        = 0.6         ; add extra density
 res2@trGridType           = "TriangularMesh"
 res2@gsnAddCyclic         = False
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt     = True
  opt@gsnShadeFillType = "pattern"
  opt@gsnShadeMid = 17 
  opt@trGridType           = "TriangularMesh"
  opt@gsnAddCyclic         = False
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "


  resB@tiMainString    = "Annual Trend Microsystis"   ; add titles
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_cmgr(1,:,:), resB)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_cmgr(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(4)=plotA
  delete(plotA) 
  delete(plotC)

   ;SUB FIGURE C - micro_hitoms
 resC                 = True                    ; plot mods desired
 resC@gsnDraw         = False                   ; don't draw yet
 resC@gsnFrame        = False                   ; don't advance frame yet
 resC@tiMainOn        = False

;  ;NE US map domain and details
 resC@mpMinLonF   = -83.75
 resC@mpMaxLonF   = -82.45
 resC@mpMinLatF   = 41.25
 resC@mpMaxLatF   = 42.25
 resC@tmXBLabelFontHeightF = 0.02                 ; resC tick labels
 resC@tmYLLabelFontHeightF = 0.02    
 resC@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
 resC@gsnMajorLonSpacing = 1              ; change maj lon tm spacing
 resC@mpDataBaseVersion = "HighRes"

 ; contouring
 resC@cnLinesOn       = False                   ; turn off contour lines
 resC@cnLineLabelsOn  = False                   ; tuen off line labels
 resC@cnFillOn        = True                    ; turn on color
 ;resC@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resC@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
  resC@cnMinLevelValF       =  0.15              ; set min contour level
  resC@cnMaxLevelValF       =  0.35               ; set max contour level
  resC@cnLevelSpacingF      =  0.01               ; set contour spacing
 resC@cnMissingValFillColor = "black"
 
  ; cmap = read_colormap_file("amwg256")
  ; cmap = cmap(::-1,:) ; reverse the color map
  ; resC@cnFillPalette   = cmap
 resC@cnFillPalette = "amwg256"

  resC@cnFillMode      = "RasterFill"
  resC@cnFillDrawOrder      = "PreDraw"     ;add coastline after contour fill to 
 
 ;label bar 
 resC@lbLabelBarOn    = True                   ; 
 resC@lbOrientation        = "Vertical"         ; vertical label bar
 resC@lbLabelFontHeightF = 0.02

 ;figure text
 resC@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resC@gsnCenterString    = "22 C Threshold: Annual Mean"   ; add titles
 resC@gsnLeftString    = " "
 resC@gsnRightString    = " "

 ;address warnings
 resC@trGridType           = "TriangularMesh"
 resC@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(mgr)
 ;mu_mgr = mask(mu_mgr,lsd.eq.1,True)    ;this is probably no longer needed
 resC@mpFillOn                     = True   ;don't fill in things like rivers
 resC@mpFillDrawOrder      = "PostDraw"     ;add coastline after contour fill to 
 resC@mpLandFillColor        = "grey81"  
 plot(2) = gsn_csm_contour_map_ce(wks,mu_micro_pmgr(:,:), resC)
 ;print("plot 1a is done")

;  SUB Figure D - micro_hitoms
resD                 = True                    ; plot mods desired
 resD@gsnDraw         = False                   ; don't draw yet
 resD@gsnFrame        = False                   ; don't advance frame yet
 resD@tiMainOn        = False

;  ;NE US map domain and details
 resD@mpMinLonF   = -83.75
 resD@mpMaxLonF   = -82.45
 resD@mpMinLatF   = 41.25
 resD@mpMaxLatF   = 42.25
 resD@tmXBLabelFontHeightF = 0.02                 ; resD tick labels
 resD@tmYLLabelFontHeightF = 0.02   
 resD@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
 resD@gsnMajorLonSpacing = 1              ; change maj lon tm spacing 
 resD@mpDataBaseVersion = "HighRes"

 ; contouring
 resD@cnLinesOn       = False                   ; turn off contour lines
 resD@cnLineLabelsOn  = False                   ; tuen off line labels
 resD@cnFillOn        = True   
 resD@cnFillMode      = "RasterFill"
 resD@cnFillDrawOrder      = "PreDraw"     ;add coastline after contour fill to 
 ;delete(cmap) 
 ; cmap = read_colormap_file("WhiteYellowOrangeRed")
 ; cmap = cmap(::-1,:) ; reverse the color map
 ; resD@cnFillPalette   = cmap
 resD@cnFillPalette   = "WhiteYellowOrangeRed"
 ;esD@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
  resD@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
  resD@cnMinLevelValF       =  0.0000              ; set min contour level
  resD@cnMaxLevelValF       =  0.0030               ; set max contour level
  resD@cnLevelSpacingF      =  0.0002               ; set contour spacing

 ;map
 resD@mpFillOn                     = True   ;don't fill in things like rivers
 resD@mpFillDrawOrder      = "PostDraw"     ;add coastline after contour fill to 
 resD@mpLandFillColor        = "grey81"  

 ;label bar 
 resD@lbLabelBarOn    = True                   ; 
 resD@lbOrientation        = "Vertical"         ; vertical label bar
 resD@lbLabelFontHeightF = 0.02

 ;figure text
 resD@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resD@gsnCenterString    = "Threshold 22 C Trend"   ; add titles
 resD@gsnLeftString    = " "
 resD@gsnRightString    = " "

 ;address warnings
 resD@trGridType           = "TriangularMesh"
 resD@gsnAddCyclic         = False

; ; ;stippling overlay -- invisble contours on map
 res2 = True                            ; res2 probability plots
 res2@gsnDraw             = False       ; Do not draw plot
 res2@gsnFrame            = False       ; Do not advance frome
 res2@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
 res2@cnMinLevelValF      = 0.89        ; set min contour level
 res2@cnMaxLevelValF      = 1.01        ; set max contour level
 res2@cnLevelSpacingF     = 0.01        ; set contour spacing
 res2@cnInfoLabelOn       = False       ; turn off info label
 res2@cnLinesOn           = False       ; do not draw contour lines
 res2@cnLineLabelsOn      = False       ; do not draw contour labels
 res2@cnFillScaleF        = 0.6         ; add extra density
 res2@trGridType           = "TriangularMesh"
 res2@gsnAddCyclic         = False
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt     = True
  opt@gsnShadeFillType = "pattern"
  opt@gsnShadeMid = 17 
  opt@trGridType           = "TriangularMesh"
  opt@gsnAddCyclic         = False
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "


  resD@tiMainString    = "Annual Trend Microsystis: 99th"   ; add titles
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_pmgr(1,:,:), resD)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_pmgr(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(5)=plotA
  delete(plotA) 
  delete(plotC)

   ;SUB FIGURE E - micro_lotoms
 resE                 = True                    ; plot mods desired
 resE@gsnDraw         = False                   ; don't draw yet
 resE@gsnFrame        = False                   ; don't advance frame yet
 resE@tiMainOn        = False

;  ;NE US map domain and details
 resE@mpMinLonF   = -83.75
 resE@mpMaxLonF   = -82.45
 resE@mpMinLatF   = 41.25
 resE@mpMaxLatF   = 42.25
 resE@tmXBLabelFontHeightF = 0.02                 ; resE tick labels
 resE@tmYLLabelFontHeightF = 0.02    
 resE@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
 resE@gsnMajorLonSpacing = 1              ; change maj lon tm spacing
 resE@mpDataBaseVersion = "HighRes"

 ; contouring
 resE@cnLinesOn       = False                   ; turn off contour lines
 resE@cnLineLabelsOn  = False                   ; tuen off line labels
 resE@cnFillOn        = True                    ; turn on color
 ;resE@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resE@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
  resE@cnMinLevelValF       =  0.15               ; set min contour level
  resE@cnMaxLevelValF       =  0.35               ; set max contour level
  resE@cnLevelSpacingF      =  0.01               ; set contour spacing
 resE@cnMissingValFillColor = "black"
 
  ; cmap = read_colormap_file("amwg256")
  ; cmap = cmap(::-1,:) ; reverse the color map
  ; resE@cnFillPalette   = cmap
 resE@cnFillPalette = "amwg256"

  resE@cnFillMode      = "RasterFill"
  resE@cnFillDrawOrder      = "PreDraw"     ;add coastline after contour fill to 
 
 ;label bar 
 resE@lbLabelBarOn    = True                   ; 
 resE@lbOrientation        = "Vertical"         ; vertical label bar
 resE@lbLabelFontHeightF = 0.02

 ;figure text
 resE@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resE@gsnCenterString    = "Threshold 19 C Annual Mean"   ; add titles
 resE@gsnLeftString    = " "
 resE@gsnRightString    = " "

 ;address warnings
 resE@trGridType           = "TriangularMesh"
 resE@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(mgr)
 ;mu_mgr = mask(mu_mgr,lsd.eq.1,True)    ;this is probably no longer needed
 resE@mpFillOn                     = True   ;don't fill in things like rivers
 resE@mpFillDrawOrder      = "PostDraw"     ;add coastline after contour fill to 
 resE@mpLandFillColor        = "grey81"  
 plot(0) = gsn_csm_contour_map_ce(wks,mu_micro_nmgr(:,:), resE)
 ;print("plot 1a is done")

;  SUB Figure F - micro_lotoms
resF                 = True                    ; plot mods desired
 resF@gsnDraw         = False                   ; don't draw yet
 resF@gsnFrame        = False                   ; don't advance frame yet
 resF@tiMainOn        = False

;  ;NE US map domain and details
 resF@mpMinLonF   = -83.75
 resF@mpMaxLonF   = -82.45
 resF@mpMinLatF   = 41.25
 resF@mpMaxLatF   = 42.25
 resF@tmXBLabelFontHeightF = 0.02                 ; resF tick labels
 resF@tmYLLabelFontHeightF = 0.02   
 resF@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
 resF@gsnMajorLonSpacing = 1              ; change maj lon tm spacing 
 resF@mpDataBaseVersion = "HighRes"

 ; contouring
 resF@cnLinesOn       = False                   ; turn off contour lines
 resF@cnLineLabelsOn  = False                   ; tuen off line labels
 resF@cnFillOn        = True   
 resF@cnFillMode      = "RasterFill"
 resF@cnFillDrawOrder      = "PreDraw"     ;add coastline after contour fill to 
 ;delete(cmap) 
 ; cmap = read_colormap_file("WhiteYellowOrangeRed")
 ; cmap = cmap(::-1,:) ; reverse the color map
 ; resF@cnFillPalette   = cmap
 resF@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resF@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
  resF@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
  resF@cnMinLevelValF       =  0.0000              ; set min contour level
  resF@cnMaxLevelValF       =  0.0030               ; set max contour level
  resF@cnLevelSpacingF      =  0.0002               ; set contour spacing

 ;map
 resF@mpFillOn                     = True   ;don't fill in things like rivers
 resF@mpFillDrawOrder      = "PostDraw"     ;add coastline after contour fill to 
 resF@mpLandFillColor        = "grey81"  

 ;label bar 
 resF@lbLabelBarOn    = True                   ; 
 resF@lbOrientation        = "Vertical"         ; vertical label bar
 resF@lbLabelFontHeightF = 0.02

 ;figure text
 resF@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resF@gsnCenterString    = "Threshold 19 C Trend"   ; add titles
 resF@gsnLeftString    = " "
 resF@gsnRightString    = " "

 ;address warnings
 resF@trGridType           = "TriangularMesh"
 resF@gsnAddCyclic         = False

; ; ;stippling overlay -- invisble contours on map
 res2 = True                            ; res2 probability plots
 res2@gsnDraw             = False       ; Do not draw plot
 res2@gsnFrame            = False       ; Do not advance frome
 res2@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels
 res2@cnMinLevelValF      = 0.89        ; set min contour level
 res2@cnMaxLevelValF      = 1.01        ; set max contour level
 res2@cnLevelSpacingF     = 0.01        ; set contour spacing
 res2@cnInfoLabelOn       = False       ; turn off info label
 res2@cnLinesOn           = False       ; do not draw contour lines
 res2@cnLineLabelsOn      = False       ; do not draw contour labels
 res2@cnFillScaleF        = 0.6         ; add extra density
 res2@trGridType           = "TriangularMesh"
 res2@gsnAddCyclic         = False
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

  plotA = gsn_csm_contour_map_ce(wks,mk_micro_nmgr(1,:,:), resF)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_nmgr(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(3)=plotA
  delete(plotA) 
  delete(plotC)

  pres@txString = "Mean Annual Growth Rate"     ; new resource added in NCL V6.4.0
  gsn_panel(wks,plot,(/2,3/),pres)

  print("Figure 1a is saved")

; ;   ; ; ===========================
; ; ; ; Figure 2: SST
; ; ; ; ===========================

; ;  wks_type = "pdf"
; ;  wks_type@wkPaperSize = "B"
; ;  wks_type@wkOrientation = "landscape"
; ;  wks   = gsn_open_wks (wks_type, "../pdf/sst_werie_micro_compare_thresh.pdf") 

; ;  delete(plot)
; ;  plot = new(2,graphic)
; ;  pres = True
; ;  pres@gsnMaximize    = True                ; maximize plots

; ;  ; contouring
; ;  delete(resA@cnFillPalette)
; ;  cmap = read_colormap_file("amwg256")
; ;  cmap = cmap(::-1,:) ; reverse the color map
; ;  resA@cnFillPalette   = "amwg256"
; ;  resA@cnMissingValFillColor = "black"
; ;  ;resA@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
; ;  resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
; ;  delete(resA@cnMinLevelValF)
; ;  resA@cnMinLevelValF       =  11.0              ; set min contour level
; ;  delete(resA@cnMaxLevelValF)
; ;  resA@cnMaxLevelValF       =  12.0               ; set max contour level
; ;  delete(resA@cnLevelSpacingF)
; ;  resA@cnLevelSpacingF      =  0.05             ; set contour spacing

; ;  ;figure text
; ;  resA@gsnCenterString    = "Annual Mean"   ; add titles
; ;  resA@gsnLeftString    = " "
; ;  resA@gsnRightString    = " "

; ;  ;address warnings
; ;  resA@trGridType           = "TriangularMesh"
; ;  resA@gsnAddCyclic         = False
 
; ;  ;overlay the coastline and land fill after mapping
; ;  ;printVarSummary(lsd)
; ;  ;printVarSummary(mgr)
; ;  ;mu_mgr = mask(mu_mgr,lsd.eq.1,True)    ;this is probably no longer needed
; ;  plot(0) = gsn_csm_contour_map_ce(wks,mu_sst(:,:), resA)
; ;  ;print("plot 1a is done")

; ; ;  SUB Figure B
; ;  delete(resB@cnFillPalette)
; ;  delete(resB@cnMinLevelValF)
; ;  delete(resB@cnMaxLevelValF)
; ;  delete(resB@cnLevelSpacingF)
; ;  resB@cnFillPalette   = "WhiteYellowOrangeRed"
; ;  ;resB@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
; ;  resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
; ;  resB@cnMinLevelValF       =  0.00              ; set min contour level
; ;  resB@cnMaxLevelValF       =  0.12               ; set max contour level
; ;  resB@cnLevelSpacingF      =  0.01               ; set contour spacing

; ;  ;figure text
; ;  resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
; ;  resB@gsnCenterString    = "Trend"   ; add titles
; ;  resB@gsnLeftString    = " "
; ;  resB@gsnRightString    = " "

; ; ; ; ;stippling overlay -- invisble contours on map
; ;  ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
; ;  res2@gsnLeftString    = " "
; ;  res2@gsnRightString    = " "

; ; ;  ;stippling overlay - visible only where significant
; ;   opt@gsnLeftString    = " "
; ;   opt@gsnRightString    = " "
  
; ;   plotA = gsn_csm_contour_map_ce(wks,mk_sst(1,:,:), resB)
; ;   plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_sst(0,:,:)), res2)
; ;   plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
; ;   overlay(plotA,plotC)                       ; result will be plotA
; ;   delete(plotB)
; ;   plot(1)=plotA
; ;   delete(plotA) 
; ;   delete(plotC)

; ;   pres@txString = "Annual SST"     ; new resource added in NCL V6.4.0
; ;   gsn_panel(wks,plot,(/1,2/),pres)

; ;   print("Figure 2 is saved")

; ; ===========================
; ; Figure 3: Mean Annual Growing Days
; ; ===========================

 wks_type = "pdf"
 wks_type@wkPaperSize = "B"
 wks_type@wkOrientation = "landscape"
 wks   = gsn_open_wks (wks_type, "../pdf/growing_days_werie_micro_compare_thres.pdf") 

 delete(plot)
 plot = new(6,graphic)
 pres = True
 pres@gsnMaximize    = True                ; maximize plots

 resA@mpMinLonF   = -83.75
 resA@mpMaxLonF   = -82.45
 resA@mpMinLatF   = 41.25
 resA@mpMaxLatF   = 42.25
 ; contouring
 delete(resA@cnFillPalette)
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 ;resA@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 resA@cnFillPalette   = "amwg256"
 delete(resA@cnMinLevelValF)
 resA@cnMinLevelValF       =  40               ; set min contour level
 delete(resA@cnMaxLevelValF)
 resA@cnMaxLevelValF       =  135               ; set max contour level
 delete(resA@cnLevelSpacingF)
 resA@cnLevelSpacingF      =  5               ; set contour spacing

 ;figure text
 resA@gsnCenterString    = "Control (20.5 C) Annual Mean"   ; add titles
 resA@gsnLeftString    = " "
 resA@gsnRightString    = " "

 ;address warnings
 resA@trGridType           = "TriangularMesh"
 resA@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(1) = gsn_csm_contour_map_ce(wks,mu_micro_cgd(:,:), resA)
 ;print("plot 1a is done")

;  SUB Figure B
 resB@mpMinLonF   = -83.75
 resB@mpMaxLonF   = -82.45
 resB@mpMinLatF   = 41.25
 resB@mpMaxLatF   = 42.25

 delete(resB@cnFillPalette)
 resB@cnFillPalette   = "BlueWhiteOrangeRed"
 ;esB@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resB@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
  delete(resB@cnMinLevelValF)
 resB@cnMinLevelValF       = -2              ; set min contour level
  delete(resB@cnMaxLevelValF)
  resB@cnMaxLevelValF       =  2               ; set max contour level
  delete(resB@cnLevelSpacingF)
 resB@cnLevelSpacingF      =  0.2               ; set contour spacing

 ;figure text
 resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resB@gsnCenterString    = "Control (20.5C) Trend"   ; add titles
 resB@gsnLeftString    = " "
 resB@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_cgd(1,:,:), resB)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_cgd(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(4)=plotA
  delete(plotA) 
  delete(plotC)

  ; Plot C: micro_hitoms
 ; contouring
 ;resC@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resC@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels

 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 delete(resC@cnFillPalette)
 resC@cnFillPalette   = "amwg256"
  delete(resC@cnMinLevelValF)
  resC@cnMinLevelValF       =  40               ; set min contour level
  delete(resC@cnMaxLevelValF)
  resC@cnMaxLevelValF       =  135              ; set max contour level
  delete(resC@cnLevelSpacingF)
  resC@cnLevelSpacingF      =  5               ; set contour spacing

 ;figure text
 resC@gsnCenterString    = "Threshold 22 C Annual Mean"   ; add titles
 resC@gsnLeftString    = " "
 resC@gsnRightString    = " "

 ;address warnings
 resC@trGridType           = "TriangularMesh"
 resC@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(2) = gsn_csm_contour_map_ce(wks,mu_micro_pgd(:,:), resC)
 ;print("plot 1a is done")

;  SUB Figure D - micro_hitoms
 resD@mpMinLonF   = -83.75
 resD@mpMaxLonF   = -82.45
 resD@mpMinLatF   = 41.25
 resD@mpMaxLatF   = 42.25

 delete(resD@cnFillPalette)
 resD@cnFillPalette   = "BlueWhiteOrangeRed"
 ;resD@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resD@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resD@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
  delete(resD@cnMinLevelValF)
 resD@cnMinLevelValF       =  -2             ; set min contour level
  delete(resD@cnMaxLevelValF)
  resD@cnMaxLevelValF       =  2               ; set max contour level
  delete(resD@cnLevelSpacingF)
 resD@cnLevelSpacingF      =  0.2               ; set contour spacing

 ;figure text
 resD@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resD@gsnCenterString    = "Threshold 22 C Trend"   ; add titles
 resD@gsnLeftString    = " "
 resD@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_pgd(1,:,:), resD)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_pgd(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(5)=plotA
  delete(plotA) 
  delete(plotC)

 ; Plot E: micro_lo
 ; contouring
 ;resE@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resE@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels

 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 
 delete(resE@cnFillPalette)
 resE@cnFillPalette   = "amwg256"
 delete(resE@cnMinLevelValF)
 resE@cnMinLevelValF       =  40              ; set min contour level
 delete(resE@cnMaxLevelValF)
 resE@cnMaxLevelValF       =  135               ; set max contour level
 delete(resE@cnLevelSpacingF)
 resE@cnLevelSpacingF      =  5               ; set contour spacing

 ;figure text
 resE@gsnCenterString    = "Threshold 19 C Mean"   ; add titles
 resE@gsnLeftString    = " "
 resE@gsnRightString    = " "

 ;address warnings
 resE@trGridType           = "TriangularMesh"
 resE@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(0) = gsn_csm_contour_map_ce(wks,mu_micro_ngd(:,:), resE)
 ;print("plot 1a is done")

;  SUB Figure F - micro_hitoms
 resF@mpMinLonF   = -83.75
 resF@mpMaxLonF   = -82.45
 resF@mpMinLatF   = 41.25
 resF@mpMaxLatF   = 42.25

 delete(resF@cnFillPalette)
 resF@cnFillPalette   = "BlueWhiteOrangeRed"
 ;resF@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resF@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resF@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(resF@cnMinLevelValF)
 resF@cnMinLevelValF       = -2             ; set min contour level
 delete(resF@cnMaxLevelValF)
resF@cnMaxLevelValF       =  2               ; set max contour level
 delete(resF@cnLevelSpacingF)
 resF@cnLevelSpacingF      =  0.2               ; set contour spacing

 ;figure text
 resF@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resF@gsnCenterString    = "Threshold 19 C Trend"   ; add titles
 resF@gsnLeftString    = " "
 resF@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_ngd(1,:,:), resF)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_ngd(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(3)=plotA
  delete(plotA) 
  delete(plotC)

  pres@txString = "Annual Growing Days"     ; new resource added in NCL V6.4.0
  gsn_panel(wks,plot,(/2,3/),pres)

  print("Figure 3 is saved")

; ; ===========================
; ; Figure 4: First Day of Growth
; ; ===========================

 wks_type = "pdf"
 wks_type@wkPaperSize = "B"
 wks_type@wkOrientation = "landscape"
 wks   = gsn_open_wks (wks_type, "../pdf/first_werie_micro_compare_thresh.pdf") 

 delete(plot)
 plot = new(6,graphic)
 pres = True
 pres@gsnMaximize    = True                ; maximize plots

 resA@mpMinLonF   = -83.75
 resA@mpMaxLonF   = -82.45
 resA@mpMinLatF   = 41.25
 resA@mpMaxLatF   = 42.25

 ; contouring
 delete(resA@cnFillPalette)
 ;resA@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 cmap = read_colormap_file("amwg256")
 cmap = cmap(::-1,:) ; reverse the color map
 resA@cnFillPalette   = cmap
 delete(resA@cnMinLevelValF)
 resA@cnMinLevelValF       =  150               ; set min contour level
 delete(resA@cnMaxLevelValF)
 resA@cnMaxLevelValF       =  200               ; set max contour level
 delete(resA@cnLevelSpacingF)
 resA@cnLevelSpacingF      =  2.5               ; set contour spacing

 ;figure text
 resA@gsnCenterString    = "Control (20.5 C) Annual Mean"   ; add titles
 resA@gsnLeftString    = " "
 resA@gsnRightString    = " "

 ;address warnings
 resA@trGridType           = "TriangularMesh"
 resA@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(1) = gsn_csm_contour_map_ce(wks,mu_micro_cfst(:,:), resA)
 ;print("plot 1a is done")

;  SUB Figure B
 resB@mpMinLonF   = -83.75
 resB@mpMaxLonF   = -82.45
 resB@mpMinLatF   = 41.25
 resB@mpMaxLatF   = 42.25

 delete(resB@cnFillPalette)
 ;resB@cnFillPalette   = "BlueWhiteOrangeRed"
 delete(cmap)
 cmap = read_colormap_file("BlueWhiteOrangeRed")
 cmap = cmap(::-1,:) ; reverse the color map
 resB@cnFillPalette   = cmap
 ;resB@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(resB@cnMinLevelValF)
 resB@cnMinLevelValF       = -1              ; set min contour level
 delete(resB@cnMaxLevelValF)
 resB@cnMaxLevelValF       =  1               ; set max contour level
 delete(resB@cnLevelSpacingF)
 resB@cnLevelSpacingF      =  0.1              ; set contour spacing

 ;figure text
 resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resB@gsnCenterString    = "Control (20.5C) Trend"   ; add titles
 resB@gsnLeftString    = " "
 resB@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_cfst(1,:,:), resB)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_cfst(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(4)=plotA
  delete(plotA) 
  delete(plotC)

  ; Plot C: micro_hitoms
 ; contouring
 ;resC@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resC@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
  delete(cmap)
 cmap = read_colormap_file("amwg256")
 cmap = cmap(::-1,:) ; reverse the color map
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 delete(resC@cnFillPalette)
 resC@cnFillPalette   = cmap
 resC@cnMinLevelValF       =  150               ; set min contour level
 delete(resC@cnMaxLevelValF)
 resC@cnMaxLevelValF       =  200               ; set max contour level
 delete(resC@cnLevelSpacingF)
 resC@cnLevelSpacingF      =  2.5               ; set contour spacing

 ;figure text
 resC@gsnCenterString    = "Threshold 22 C Annual Mean"   ; add titles
 resC@gsnLeftString    = " "
 resC@gsnRightString    = " "

 ;address warnings
 resC@trGridType           = "TriangularMesh"
 resC@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(2) = gsn_csm_contour_map_ce(wks,mu_micro_pfst(:,:), resC)
 ;print("plot 1a is done")

;  SUB Figure D - micro_hitoms
 resD@mpMinLonF   = -83.75
 resD@mpMaxLonF   = -82.45
 resD@mpMinLatF   = 41.25
 resD@mpMaxLatF   = 42.25

 delete(cmap)
 cmap = read_colormap_file("BlueWhiteOrangeRed")
 cmap = cmap(::-1,:) ; reverse the color map
 delete(resD@cnFillPalette)
 resD@cnFillPalette   = cmap
 ;resD@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resD@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(resD@cnMinLevelValF)
 resD@cnMinLevelValF       = -0.0025             ; set min contour level
 delete(resD@cnMaxLevelValF)
 resD@cnMaxLevelValF       =  0.0025               ; set max contour level
 delete(resD@cnLevelSpacingF)
 resD@cnLevelSpacingF      =  0.0005               ; set contour spacing

 ;figure text
 resD@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resD@gsnCenterString    = "Threshold 22 C Trend"   ; add titles
 resD@gsnLeftString    = " "
 resD@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_pfst(1,:,:), resD)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_pfst(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(5)=plotA
  delete(plotA) 
  delete(plotC)

 ; Plot E: micro_lo
 ; contouring
 ;resE@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resE@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(cmap)
 cmap = read_colormap_file("amwg256")
 cmap = cmap(::-1,:) ; reverse the color map
 delete(resE@cnFillPalette)
 resE@cnFillPalette   = cmap
 delete(resE@cnMinLevelValF)
 resE@cnMinLevelValF       =  150               ; set min contour level
 delete(resE@cnMaxLevelValF)
 resE@cnMaxLevelValF       =  200               ; set max contour level
 delete(resE@cnLevelSpacingF)
 resE@cnLevelSpacingF      =  2.5               ; set contour spacing

 ;figure text
 resE@gsnCenterString    = "Threshold 19 C Mean"   ; add titles
 resE@gsnLeftString    = " "
 resE@gsnRightString    = " "

 ;address warnings
 resE@trGridType           = "TriangularMesh"
 resE@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(0) = gsn_csm_contour_map_ce(wks,mu_micro_nfst(:,:), resE)
 ;print("plot 1a is done")

;  SUB Figure F - micro_hitoms
 resF@mpMinLonF   = -83.75
 resF@mpMaxLonF   = -82.45
 resF@mpMinLatF   = 41.25
 resF@mpMaxLatF   = 42.25

 delete(cmap)
 cmap = read_colormap_file("BlueWhiteOrangeRed")
 cmap = cmap(::-1,:) ; reverse the color map
 delete(resF@cnFillPalette)
 resF@cnFillPalette   = cmap
 ;resF@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resF@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(resF@cnMinLevelValF)
 resF@cnMinLevelValF       = -1             ; set min contour level
 delete(resF@cnMaxLevelValF)
resF@cnMaxLevelValF       =  1               ; set max contour level
 delete(resF@cnLevelSpacingF)
 resF@cnLevelSpacingF      =  0.1               ; set contour spacing

 ;figure text
 resF@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resF@gsnCenterString    = "Threshold 19 C Trend"   ; add titles
 resF@gsnLeftString    = " "
 resF@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_nfst(1,:,:), resF)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_nfst(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(3)=plotA
  delete(plotA) 
  delete(plotC)

  pres@txString = "First Day of Bloom"     ; new resource added in NCL V6.4.0
  gsn_panel(wks,plot,(/2,3/),pres)

  print("Figure 4: First Day is saved")

; ; ; ===========================
; ; ; Figure 5: Last Day of Growth
; ; ; ===========================

 wks_type = "pdf"
 wks_type@wkPaperSize = "B"
 wks_type@wkOrientation = "landscape"
 wks   = gsn_open_wks (wks_type, "../pdf/last_werie_micro_compare_thresh.pdf") 

 delete(plot)
 plot = new(6,graphic)
 pres = True
 pres@gsnMaximize    = True                ; maximize plots

 resA@mpMinLonF   = -83.75
 resA@mpMaxLonF   = -82.45
 resA@mpMinLatF   = 41.25
 resA@mpMaxLatF   = 42.25
 ; contouring
 delete(resA@cnFillPalette)
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 resA@cnFillPalette   = "amwg256"
 delete(resA@cnMinLevelValF)
 resA@cnMinLevelValF       =  245               ; set min contour level
 delete(resA@cnMaxLevelValF)
 resA@cnMaxLevelValF       =  285               ; set max contour level
 delete(resA@cnLevelSpacingF)
 resA@cnLevelSpacingF      =  2.5               ; set contour spacing

 ;figure text
 resA@gsnCenterString    = "Control (20.5 C) Annual Mean"   ; add titles
 resA@gsnLeftString    = " "
 resA@gsnRightString    = " "

 ;address warnings
 resA@trGridType           = "TriangularMesh"
 resA@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(1) = gsn_csm_contour_map_ce(wks,mu_micro_clst(:,:), resA)
 ;print("plot 1a is done")

;  SUB Figure B
 resB@mpMinLonF   = -83.75
 resB@mpMaxLonF   = -82.45
 resB@mpMinLatF   = 41.25
 resB@mpMaxLatF   = 42.25

 delete(resB@cnFillPalette)
 resB@cnFillPalette   = "BlueWhiteOrangeRed"
 ;resB@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resB@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(resB@cnMinLevelValF)
 resB@cnMinLevelValF       = -2             ; set min contour level
 delete(resB@cnMaxLevelValF)
 resB@cnMaxLevelValF       =  2               ; set max contour level
 delete(resB@cnLevelSpacingF)
 resB@cnLevelSpacingF      =  0.2               ; set contour spacing

 ;figure text
 resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resB@gsnCenterString    = "Control (20.5C) Trend"   ; add titles
 resB@gsnLeftString    = " "
 resB@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_clst(1,:,:), resB)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_clst(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(4)=plotA
  delete(plotA) 
  delete(plotC)

  ; Plot C: micro_hitoms
 ; contouring
 resC@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 delete(resC@cnFillPalette)
 resC@cnFillPalette   = "amwg256"
 resC@cnMinLevelValF       =  245               ; set min contour level
 delete(resC@cnMaxLevelValF)
 resC@cnMaxLevelValF       =  285               ; set max contour level
 delete(resC@cnLevelSpacingF)
 resC@cnLevelSpacingF      =  2.5               ; set contour spacing

 ;figure text
 resC@gsnCenterString    = "Threshold 22 C Annual Mean"   ; add titles
 resC@gsnLeftString    = " "
 resC@gsnRightString    = " "

 ;address warnings
 resC@trGridType           = "TriangularMesh"
 resC@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(2) = gsn_csm_contour_map_ce(wks,mu_micro_plst(:,:), resC)
 ;print("plot 1a is done")

;  SUB Figure D - micro_hitoms
 resD@mpMinLonF   = -83.75
 resD@mpMaxLonF   = -82.45
 resD@mpMinLatF   = 41.25
 resD@mpMaxLatF   = 42.25

 delete(resD@cnFillPalette)
 resD@cnFillPalette   = "BlueWhiteOrangeRed"
 ;resD@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resD@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resD@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(resD@cnMinLevelValF)
 resD@cnMinLevelValF       = -2             ; set min contour level
 delete(resD@cnMaxLevelValF)
 resD@cnMaxLevelValF       =  2               ; set max contour level
 delete(resD@cnLevelSpacingF)
 resD@cnLevelSpacingF      =  0.2               ; set contour spacing

 ;figure text
 resD@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resD@gsnCenterString    = "Threshold 22 C Trend"   ; add titles
 resD@gsnLeftString    = " "
 resD@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_plst(1,:,:), resD)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_plst(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(5)=plotA
  delete(plotA) 
  delete(plotC)

 ; Plot E: micro_lo
 ; contouring
 ;resE@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resE@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 delete(resE@cnFillPalette)
 resE@cnFillPalette   = "amwg256"
 resE@cnMinLevelValF       =  245               ; set min contour level
 delete(resE@cnMaxLevelValF)
 resE@cnMaxLevelValF       =  285               ; set max contour level
 delete(resE@cnLevelSpacingF)
 resE@cnLevelSpacingF      =  2.5               ; set contour spacing

 ;figure text
 resE@gsnCenterString    = "Threshold 19 C Mean"   ; add titles
 resE@gsnLeftString    = " "
 resE@gsnRightString    = " "

 ;address warnings
 resE@trGridType           = "TriangularMesh"
 resE@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(0) = gsn_csm_contour_map_ce(wks,mu_micro_nlst(:,:), resE)
 ;print("plot 1a is done")

;  SUB Figure F - micro_hitoms
 resF@mpMinLonF   = -83.75
 resF@mpMaxLonF   = -82.45
 resF@mpMinLatF   = 41.25
 resF@mpMaxLatF   = 42.25

 delete(resF@cnFillPalette)
 resF@cnFillPalette   = "BlueWhiteOrangeRed"
 ;resF@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resF@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resF@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(resF@cnMinLevelValF)
 resF@cnMinLevelValF       = -2             ; set min contour level
 delete(resF@cnMaxLevelValF)
 resF@cnMaxLevelValF       =  2               ; set max contour level
 delete(resF@cnLevelSpacingF)
 resF@cnLevelSpacingF      =  0.2               ; set contour spacing

 ;figure text
 resF@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resF@gsnCenterString    = "Threshold 19 C Trend"   ; add titles
 resF@gsnLeftString    = " "
 resF@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_nlst(1,:,:), resF)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_nlst(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(3)=plotA
  delete(plotA) 
  delete(plotC)

  pres@txString = "Last Day of Bloom"     ; new resource added in NCL V6.4.0
  gsn_panel(wks,plot,(/2,3/),pres)

  print("Figure 5: Last Day is saved")

; ; ===========================
; ; Figure 6: Season Window of Growth
; ; ===========================

 wks_type = "pdf"
 wks_type@wkPaperSize = "B"
 wks_type@wkOrientation = "landscape"
 wks   = gsn_open_wks (wks_type, "../pdf/season_window_werie_micro_compare_thresh.pdf") 

 delete(plot)
 plot = new(6,graphic)
 pres = True
 pres@gsnMaximize    = True                ; maximize plots

 resA@mpMinLonF   = -83.75
 resA@mpMaxLonF   = -82.45
 resA@mpMinLatF   = 41.25
 resA@mpMaxLatF   = 42.25
 ; contouring
 delete(resA@cnFillPalette)
 resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 resA@cnFillPalette   = "amwg256"
 delete(resA@cnMinLevelValF)
 resA@cnMinLevelValF       =  55               ; set min contour level
 delete(resA@cnMaxLevelValF)
 resA@cnMaxLevelValF       =  135               ; set max contour level
 delete(resA@cnLevelSpacingF)
 resA@cnLevelSpacingF      =  5               ; set contour spacing

 ;figure text
 resA@gsnCenterString    = "Control (20.5 C) Annual Mean"   ; add titles
 resA@gsnLeftString    = " "
 resA@gsnRightString    = " "

 ;address warnings
 resA@trGridType           = "TriangularMesh"
 resA@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(1) = gsn_csm_contour_map_ce(wks,mu_micro_cwnd(:,:), resA)
 ;print("plot 1a is done")

;  SUB Figure B
 resB@mpMinLonF   = -83.75
 resB@mpMaxLonF   = -82.45
 resB@mpMinLatF   = 41.25
 resB@mpMaxLatF   = 42.25

 delete(resB@cnFillPalette)
 resB@cnFillPalette   = "BlueWhiteOrangeRed"
 ;resB@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resB@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ; delete(resB@cnMinLevelValF)
 resB@cnMinLevelValF       = -3              ; set min contour level
 ; delete(resB@cnMaxLevelValF)
  resB@cnMaxLevelValF       =  3               ; set max contour level
 ; delete(resB@cnLevelSpacingF)
 resB@cnLevelSpacingF      =  0.25               ; set contour spacing

 ;figure text
 resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resB@gsnCenterString    = "Control (20.5C) Trend"   ; add titles
 resB@gsnLeftString    = " "
 resB@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_cwnd(1,:,:), resB)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_cwnd(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(4)=plotA
  delete(plotA) 
  delete(plotC)

  ; Plot C: micro_hitoms
 ; contouring
 resC@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ;resC@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 delete(resC@cnFillPalette)
 
 resC@cnFillPalette   = "amwg256"
 delete(resC@cnMinLevelValF)
 resC@cnMinLevelValF       =  55               ; set min contour level
 delete(resC@cnMaxLevelValF)
 resC@cnMaxLevelValF       =  135               ; set max contour level
 delete(resC@cnLevelSpacingF)
 resC@cnLevelSpacingF      =  5               ; set contour spacing

 ;figure text
 resC@gsnCenterString    = "Threshold 22 C Annual Mean"   ; add titles
 resC@gsnLeftString    = " "
 resC@gsnRightString    = " "

 ;address warnings
 resC@trGridType           = "TriangularMesh"
 resC@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(2) = gsn_csm_contour_map_ce(wks,mu_micro_pwnd(:,:), resC)
 ;print("plot 1a is done")

;  SUB Figure D - micro_hitoms
 resD@mpMinLonF   = -83.75
 resD@mpMaxLonF   = -82.45
 resD@mpMinLatF   = 41.25
 resD@mpMaxLatF   = 42.25

 delete(resD@cnFillPalette)
 resD@cnFillPalette   = "BlueWhiteOrangeRed"
 ;resD@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resD@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resD@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(resD@cnMinLevelValF)
 resD@cnMinLevelValF       = -3             ; set min contour level
 delete(resD@cnMaxLevelValF)
 resD@cnMaxLevelValF       =  3               ; set max contour level
 delete(resD@cnLevelSpacingF)
 resD@cnLevelSpacingF      =  0.25               ; set contour spacing

 ;figure text
 resD@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resD@gsnCenterString    = "Threshold 22 C Trend"   ; add titles
 resD@gsnLeftString    = " "
 resD@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_pwnd(1,:,:), resD)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_pwnd(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(5)=plotA
  delete(plotA) 
  delete(plotC)

 ; Plot E: micro_lo
 ; contouring
 ;resE@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resE@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 delete(resE@cnFillPalette)
 resE@cnFillPalette   = "amwg256"
 delete(resE@cnMinLevelValF)
 resE@cnMinLevelValF       =  55               ; set min contour level
 delete(resE@cnMaxLevelValF)
 resE@cnMaxLevelValF       =  135               ; set max contour level
 delete(resE@cnLevelSpacingF)
 resE@cnLevelSpacingF      =  5               ; set contour spacing

 ;figure text
 resE@gsnCenterString    = "Threshold 19 C Mean"   ; add titles
 resE@gsnLeftString    = " "
 resE@gsnRightString    = " "

 ;address warnings
 resE@trGridType           = "TriangularMesh"
 resE@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(0) = gsn_csm_contour_map_ce(wks,mu_micro_nwnd(:,:), resE)
 ;print("plot 1a is done")

;  SUB Figure F - micro_hitoms
 resF@mpMinLonF   = -83.75
 resF@mpMaxLonF   = -82.45
 resF@mpMinLatF   = 41.25
 resF@mpMaxLatF   = 42.25

 delete(resF@cnFillPalette)
 resF@cnFillPalette   = "BlueWhiteOrangeRed"
 ;resF@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resF@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resF@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 delete(resF@cnMinLevelValF)
 resF@cnMinLevelValF       = -3             ; set min contour level
 delete(resF@cnMaxLevelValF)
 resF@cnMaxLevelValF       =  3               ; set max contour level
 delete(resF@cnLevelSpacingF)
 resF@cnLevelSpacingF      =  0.25               ; set contour spacing

 ;figure text
 resF@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resF@gsnCenterString    = "Threshold 19 C Trend"   ; add titles
 resF@gsnLeftString    = " "
 resF@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_nwnd(1,:,:), resF)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_nwnd(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .899, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(3)=plotA
  delete(plotA) 
  delete(plotC)

  pres@txString = "Season Window Length"     ; new resource added in NCL V6.4.0
  gsn_panel(wks,plot,(/2,3/),pres)

  print("Figure 6: Season Window is saved")
end

