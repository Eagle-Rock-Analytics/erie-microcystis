; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin
 f     = addfile ("/Volumes/data1/werie-hr/annual.micro.werie.nc", "r")             ; add file
 lat   = f->lat
 lon   = f->lon
 year  = f->year

; =========================
 ; Load Duration Variables
 ; =========================
 micro_medmgr    = f->$"mic.med.mean_annual_growth"$
 micro_himgr    = f->$"mic.hi.mean_annual_growth"$
 micro_lomgr    = f->$"mic.hi.mean_annual_growth"$
 sst    = f->mean_annual_sst
 printVarSummary(sst)

 ; first    = f->first_day
 ; last    = f->last_day
 ; sw  = f->season.window
 micro_medgd  = f->$"mic.hi.growing_days"$
 micro_higd  = f->$"mic.lo.growing_days"$
 print("data loaded from netcdf")

  ; =========================
 ; Mean Statistics
 ; =========================
 mu_micro_medmgr = dim_avg_n_Wrap(micro_medmgr,0)
 printVarSummary(mu_micro_medmgr)
 mu_micro_himgr = dim_avg_n_Wrap(micro_himgr,0)
 printVarSummary(mu_micro_himgr)
 mu_sst = dim_avg_n_Wrap(sst,0)
 printVarSummary(mu_sst)
 ; mu_first = dim_avg_n_Wrap(first,0)
 ; printVarSummary(mu_first)
 ; mu_last = dim_avg_n_Wrap(last,0)
 ; printVarSummary(mu_last)
 mu_micro_medgd = dim_avg_n_Wrap(micro_medgd,0)
 printVarSummary(mu_micro_medgd)
 mu_micro_higd = dim_avg_n_Wrap(micro_higd,0)
 printVarSummary(mu_micro_higd)

  ; copy_VarAtts(first(0,:,:),mu_first(:,:))
  ; copy_VarAtts(last(0,:,:),mu_last(:,:))
  copy_VarAtts(micro_medmgr(0,:,:),mu_micro_medmgr(:,:))
  copy_VarAtts(micro_himgr(0,:,:),mu_micro_himgr(:,:))
  copy_VarAtts(sst(0,:,:),mu_sst(:,:))
  copy_VarAtts(micro_medgd(0,:,:),mu_micro_medgd(:,:))
  copy_VarAtts(micro_higd(0,:,:),mu_micro_higd(:,:))
 print("means calculated")

;  ; ;  ; ===========================
; ; ; ; Trends
; ; ; ; ===========================
; ;  ;trends
; ;  ;mann-kendal trend
  mk_micro_medmgr = trend_manken(micro_medmgr,True,0)
  mk_micro_himgr = trend_manken(micro_himgr,True,0)
  mk_sst = trend_manken(sst,True,0)
  ; mk_first = trend_manken(first,True,0)
  ; mk_last = trend_manken(last,True,0)
  mk_micro_medgd = trend_manken(micro_medgd,True,0)
  mk_micro_higd = trend_manken(micro_higd,True,0)

; ; copy variable attributes
  ; copy_VarCoords(first(0,:,:),mk_first(0,:,:))
  ; copy_VarCoords(last(0,:,:),mk_last(0,:,:))
  copy_VarCoords(micro_medmgr(0,:,:),mk_micro_medmgr(0,:,:))
  copy_VarCoords(micro_himgr(0,:,:),mk_micro_himgr(0,:,:))
  copy_VarCoords(sst(0,:,:),mk_sst(0,:,:))
  copy_VarCoords(micro_medgd(0,:,:),mk_micro_medgd(0,:,:))
  copy_VarCoords(micro_higd(0,:,:),mk_micro_higd(0,:,:))

  ; copy_VarAtts(first(0,:,:),mk_first(0,:,:))
  ; copy_VarAtts(last(0,:,:),mk_last(0,:,:))
  copy_VarAtts(micro_medmgr(0,:,:),mk_micro_medmgr(0,:,:))
  copy_VarAtts(micro_himgr(0,:,:),mk_micro_himgr(0,:,:))
  copy_VarAtts(sst(0,:,:),mk_sst(0,:,:))
  copy_VarAtts(micro_medgd(0,:,:),mk_micro_medgd(0,:,:))
  copy_VarAtts(micro_higd(0,:,:),mk_micro_higd(0,:,:))

;  printVarSummary(mk_gd)

;   ; mk_mgr@units = "Julian days per year"
;   ; mk_mgr@long_name = "Trend in bloom onset date"

;   ; mk_sst@units = "Julian days per year"
;   ; mk_sst@long_name = "Trend in last bloom date"

;   ; mk_countdays@units = "days per year"
;   ; mk_countdays@long_name = "Trend in bloom lastson length"

;   ; mk_annmean@units = "per day per year"
;   ; mk_annmean@long_name = "Trend in mean annual growth rate"

  print("trend statistics calculated")
; ; ; ===========================
; ; ; Figure List
; ; ; 1: Start Day Trend & Mean
; ; ; 2: End Day Trend & Mean
; ; ; 3: firstumulated Above Trend & Mean
; ; ; 4: lastsonal Days Trend & Mean
; ; ; 6: Number of Events Trend & Mean

; ; ===========================
; ; Figure 1: Mean Growth Rate
; ; ===========================

 wks_type = "pdf"
 wks_type@wkPaperSize = "B"
 wks_type@wkOrientation = "landscape"
 wks   = gsn_open_wks (wks_type, "../pdf/mean_gr_micro_werie.pdf") 

 plot = new(4,graphic)
 pres = True
 pres@gsnMaximize    = True                ; maximize plots

 ;SUB FIGURE A
 resA                 = True                    ; plot mods desired
 resA@gsnDraw         = False                   ; don't draw yet
 resA@gsnFrame        = False                   ; don't advance frame yet
 resA@tiMainOn        = False

;  ;NE US map domain and details
 resA@mpMinLonF   = -83.75
 resA@mpMaxLonF   = -80.0
 resA@mpMinLatF   = 41.25
 resA@mpMaxLatF   = 43
 resA@tmXBLabelFontHeightF = 0.02                 ; resA tick labels
 resA@tmYLLabelFontHeightF = 0.02    
 resA@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
 resA@gsnMajorLonSpacing = 1              ; change maj lon tm spacing
 resA@mpDataBaseVersion = "HighRes"

 ; contouring
 resA@cnLinesOn       = False                   ; turn off contour lines
 resA@cnLineLabelsOn  = False                   ; tuen off line labels
 resA@cnFillOn        = True                    ; turn on color
 resA@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 ;resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ; resA@cnMinLevelValF       =  160               ; set min contour level
 ; resA@cnMaxLevelValF       =  190               ; set max contour level
 ; resA@cnLevelSpacingF      =  2               ; set contour spacing
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
 resA@gsnCenterString    = "Microsystis:50th Annual Mean"   ; add titles
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
 plot(0) = gsn_csm_contour_map_ce(wks,mu_micro_medmgr(:,:), resA)
 ;print("plot 1a is done")

;  SUB Figure B
resB                 = True                    ; plot mods desired
 resB@gsnDraw         = False                   ; don't draw yet
 resB@gsnFrame        = False                   ; don't advance frame yet
 resB@tiMainOn        = False

;  ;NE US map domain and details
 resB@mpMinLonF   = -83.75
 resB@mpMaxLonF   = -80.0
 resB@mpMinLatF   = 41.25
 resB@mpMaxLatF   = 43
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
 resB@gsnCenterString    = "Annual Trend Microsystis:50"   ; add titles
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


  resB@tiMainString    = "Annual Trend Microsystis:Med"   ; add titles
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_medmgr(1,:,:), resB)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_medmgr(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .949, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(1)=plotA
  delete(plotA) 
  delete(plotC)

   ;SUB FIGURE C - micro_hitoms
 resC                 = True                    ; plot mods desired
 resC@gsnDraw         = False                   ; don't draw yet
 resC@gsnFrame        = False                   ; don't advance frame yet
 resC@tiMainOn        = False

;  ;NE US map domain and details
 resC@mpMinLonF   = -83.75
 resC@mpMaxLonF   = -80.0
 resC@mpMinLatF   = 41.25
 resC@mpMaxLatF   = 43
 resC@tmXBLabelFontHeightF = 0.02                 ; resC tick labels
 resC@tmYLLabelFontHeightF = 0.02    
 resC@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
 resC@gsnMajorLonSpacing = 1              ; change maj lon tm spacing
 resC@mpDataBaseVersion = "HighRes"

 ; contouring
 resC@cnLinesOn       = False                   ; turn off contour lines
 resC@cnLineLabelsOn  = False                   ; tuen off line labels
 resC@cnFillOn        = True                    ; turn on color
 resC@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 ;resC@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ; resC@cnMinLevelValF       =  160               ; set min contour level
 ; resC@cnMaxLevelValF       =  190               ; set max contour level
 ; resC@cnLevelSpacingF      =  2               ; set contour spacing
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
 resC@gsnCenterString    = "Microsystis:99th Annual Mean"   ; add titles
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
 plot(2) = gsn_csm_contour_map_ce(wks,mu_micro_himgr(:,:), resC)
 ;print("plot 1a is done")

;  SUB Figure D - micro_hitoms
resD                 = True                    ; plot mods desired
 resD@gsnDraw         = False                   ; don't draw yet
 resD@gsnFrame        = False                   ; don't advance frame yet
 resD@tiMainOn        = False

;  ;NE US map domain and details
 resD@mpMinLonF   = -83.75
 resD@mpMaxLonF   = -80.0
 resD@mpMinLatF   = 41.25
 resD@mpMaxLatF   = 43
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
 resD@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 ; resD@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ; resD@cnMinLevelValF       =  -3.5              ; set min contour level
 ; resD@cnMaxLevelValF       =  0.0               ; set max contour level
 ; resD@cnLevelSpacingF      =  0.25               ; set contour spacing

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
 resD@gsnCenterString    = "Microsystis:99th"   ; add titles
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
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_himgr(1,:,:), resD)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_himgr(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .949, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(3)=plotA
  delete(plotA) 
  delete(plotC)

  pres@txString = "Mean Annual Growth Rate"     ; new resource added in NCL V6.4.0
  gsn_panel(wks,plot,(/2,2/),pres)

  print("Figure 1 is saved")

  ; ; ===========================
; ; Figure 2: SST
; ; ===========================

 wks_type = "pdf"
 wks_type@wkPaperSize = "B"
 wks_type@wkOrientation = "landscape"
 wks   = gsn_open_wks (wks_type, "../pdf/sst_werie_micro.pdf") 

 delete(plot)
 plot = new(2,graphic)
 pres = True
 pres@gsnMaximize    = True                ; maximize plots

 ; contouring
 delete(resA@cnFillPalette)
 cmap = read_colormap_file("amwg256")
 cmap = cmap(::-1,:) ; reverse the color map
 resA@cnFillPalette   = "amwg256"
 resA@cnMissingValFillColor = "black"
 resA@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 ;  resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ; resA@cnMinLevelValF       =  246               ; set min contour level
 ; resA@cnMaxLevelValF       =  270               ; set max contour level
 ; resA@cnLevelSpacingF      =  2               ; set contour spacing
 ;figure text
 resA@gsnCenterString    = "Annual Mean"   ; add titles
 resA@gsnLeftString    = " "
 resA@gsnRightString    = " "

 ;address warnings
 resA@trGridType           = "TriangularMesh"
 resA@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(mgr)
 ;mu_mgr = mask(mu_mgr,lsd.eq.1,True)    ;this is probably no longer needed
 plot(0) = gsn_csm_contour_map_ce(wks,mu_sst(:,:), resA)
 ;print("plot 1a is done")

;  SUB Figure B
 delete(resB@cnFillPalette)
 resB@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resB@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 resB@cnMinLevelValF       =  0               ; set min contour level
 resB@cnMaxLevelValF       =  0.20               ; set max contour level
 resB@cnLevelSpacingF      =  0.02               ; set contour spacing

 ;figure text
 resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resB@gsnCenterString    = "Trend"   ; add titles
 resB@gsnLeftString    = " "
 resB@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_sst(1,:,:), resB)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_sst(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .949, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(1)=plotA
  delete(plotA) 
  delete(plotC)

  pres@txString = "Annual SST"     ; new resource added in NCL V6.4.0
  gsn_panel(wks,plot,(/1,2/),pres)

  print("Figure 2 is saved")

; ; ; ; ===========================
; ; ; ; Figure 3: First Growing Day
; ; ; ; ===========================

; ;  wks_type = "pdf"
; ;  wks_type@wkPaperSize = "B"
; ;  wks_type@wkOrientation = "landscape"
; ;  wks   = gsn_open_wks (wks_type, "../pdf/first_growday_ches_gs.pdf") 

; ;  plot = new(2,graphic)
; ;  pres = True
; ;  pres@gsnMaximize    = True                ; maximize plots

; ;  ; contouring
; ;  delete(resA@cnFillPalette)
; ;  cmap = read_colormap_file("amwg256")
; ;  cmap = cmap(::-1,:) ; reverse the color map
; ;  resA@cnFillPalette   = cmap
; ;  resA@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
; ;  ; resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
; ;  ; resA@cnMinLevelValF       =  50               ; set min contour level
; ;  ; resA@cnMaxLevelValF       =  250              ; set max contour level
; ;  ; delete(resA@cnLevelSpacingF)
; ;  ; resA@cnLevelSpacingF      =  10              ; set contour spacing

; ;  ;figure text
; ;  resA@gsnCenterString    = "Mean: MJJAS"   ; add titles
; ;  resA@gsnLeftString    = " "
; ;  resA@gsnRightString    = " "

; ;  ;address warnings
; ;  resA@trGridType           = "TriangularMesh"
; ;  resA@gsnAddCyclic         = False
 
; ;  ;overlay the coastline and land fill after mapping
; ;  ;printVarSummary(lsd)
; ;  ;printVarSummary(mgr)
; ;  ;mu_mgr = mask(mu_mgr,lsd.eq.1,True)    ;this is probably no longer needed
; ;  plot(0) = gsn_csm_contour_map_ce(wks,mu_first(:,:), resA)
; ;  ;print("plot 1a is done")

; ; ;  SUB Figure B
; ;  delete(resB@cnFillPalette)
; ;  delete(cmap)
; ;  cmap = read_colormap_file("BlueWhiteOrangeRed")
; ;  cmap = cmap(::-1,:) ; reverse the color map
; ;  ;resB@cnFillPalette   = "BlueWhiteOrangeRed"
; ;  resB@cnFillPalette   = cmap
; ;  ;resB@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
; ;   resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
; ;   resB@cnMinLevelValF       =  -3.0              ; set min contour level
; ;   resB@cnMaxLevelValF       =  3.0               ; set max contour level
; ;   resB@cnLevelSpacingF      =  0.2               ; set contour spacing

; ;  ;figure text
; ;  resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
; ;  resB@gsnCenterString    = "Trend: MJJAS"   ; add titles
; ;  resB@gsnLeftString    = " "
; ;  resB@gsnRightString    = " "

; ; ; ; ;stippling overlay -- invisble contours on map
; ;  ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
; ;  res2@gsnLeftString    = " "
; ;  res2@gsnRightString    = " "

; ; ;  ;stippling overlay - visible only where significant
; ;   opt@gsnLeftString    = " "
; ;   opt@gsnRightString    = " "
  
; ;   plotA = gsn_csm_contour_map_ce(wks,mk_first(1,:,:), resB)
; ;   plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_first(0,:,:)), res2)
; ;   plotC = gsn_contour_shade(plotB, .949, 1, opt)  ; shade all areas with p between .99 and 1
; ;   overlay(plotA,plotC)                       ; result will be plotA
; ;   delete(plotB)
; ;   plot(1)=plotA
; ;   delete(plotA) 
; ;   delete(plotC)

; ;   pres@txString = "First Growing Day (Julian Day)"     ; new resource added in NCL V6.4.0
; ;   gsn_panel(wks,plot,(/1,2/),pres)

; ;   print("Figure 3 is saved")

; ;   ; ; ===========================
; ; ; ; Figure 4: Last Growing Day
; ; ; ; ===========================

; ;  wks_type = "pdf"
; ;  wks_type@wkPaperSize = "B"
; ;  wks_type@wkOrientation = "landscape"
; ;  wks   = gsn_open_wks (wks_type, "../pdf/last_growday_ches_gs.pdf") 

; ;  plot = new(2,graphic)
; ;  pres = True
; ;  pres@gsnMaximize    = True                ; maximize plots

; ;  ; contouring
; ;  delete(resA@cnFillPalette)
; ;  ;cmap = read_colormap_file("amwg256")
; ;  ;cmap = cmap(::-1,:) ; reverse the color map
; ;  resA@cnFillPalette   = "amwg256"
; ;  ; resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
; ;  ; resA@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
; ;  ; resA@cnMinLevelValF       =  45               ; set min contour level
; ;  ; resA@cnMaxLevelValF       =  102               ; set max contour level
; ;  ; delete(resA@cnLevelSpacingF)
; ;  ; resA@cnLevelSpacingF      =  3              ; set contour spacing

; ;  ;figure text
; ;  resA@gsnCenterString    = "Mean: MJJAS"   ; add titles
; ;  resA@gsnLeftString    = " "
; ;  resA@gsnRightString    = " "

; ;  ;address warnings
; ;  resA@trGridType           = "TriangularMesh"
; ;  resA@gsnAddCyclic         = False
 
; ;  ;overlay the coastline and land fill after mapping
; ;  ;printVarSummary(lsd)
; ;  ;printVarSummary(mgr)
; ;  ;mu_mgr = mask(mu_mgr,lsd.eq.1,True)    ;this is probably no longer needed
; ;  plot(0) = gsn_csm_contour_map_ce(wks,mu_last(:,:), resA)
; ;  ;print("plot 1a is done")

; ; ;  SUB Figure B
; ;  delete(resB@cnFillPalette)
; ;  ;delete(cmap)
; ;  ;cmap = read_colormap_file("MPL_afmhot")
; ;  ;cmap = cmap(::-1,:) ; reverse the color map
; ;  resB@cnFillPalette   = "BlueWhiteOrangeRed"
; ;  resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
; ;  resB@cnMinLevelValF       =  -0.5               ; set min contour level
; ;  resB@cnMaxLevelValF       =  0.5               ; set max contour level
; ;  resB@cnLevelSpacingF      =  0.05               ; set contour spacing

; ;  ;figure text
; ;  resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
; ;  resB@gsnCenterString    = "Trend: MJJAS"   ; add titles
; ;  resB@gsnLeftString    = " "
; ;  resB@gsnRightString    = " "

; ; ; ; ;stippling overlay -- invisble contours on map
; ;  ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
; ;  res2@gsnLeftString    = " "
; ;  res2@gsnRightString    = " "

; ; ;  ;stippling overlay - visible only where significant
; ;   opt@gsnLeftString    = " "
; ;   opt@gsnRightString    = " "
  
; ;   plotA = gsn_csm_contour_map_ce(wks,mk_last(1,:,:), resB)
; ;   plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_last(0,:,:)), res2)
; ;   plotC = gsn_contour_shade(plotB, .949, 1, opt)  ; shade all areas with p between .99 and 1
; ;   overlay(plotA,plotC)                       ; result will be plotA
; ;   delete(plotB)
; ;   plot(1)=plotA
; ;   delete(plotA) 
; ;   delete(plotC)

; ;   pres@txString = "Last Growing Day (Julian Day)"     ; new resource added in NCL V6.4.0
; ;   gsn_panel(wks,plot,(/1,2/),pres)

; ;   print("Figure 4 is saved")

  ; ; ===========================
; ; Figure 6: Mean Annual Growing Days
; ; ===========================

 wks_type = "pdf"
 wks_type@wkPaperSize = "B"
 wks_type@wkOrientation = "landscape"
 wks   = gsn_open_wks (wks_type, "../pdf/growing_days_werie_micro.pdf") 

 delete(plot)
 plot = new(4,graphic)
 pres = True
 pres@gsnMaximize    = True                ; maximize plots

 resA@mpMinLonF   = -83.75
 resA@mpMaxLonF   = -80.0
 resA@mpMinLatF   = 41.25
 resA@mpMaxLatF   = 43
 ; contouring
 delete(resA@cnFillPalette)
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 resA@cnFillPalette   = "amwg256"
 ; resA@cnMinLevelValF       =  0               ; set min contour level
 ; delete(resA@cnMaxLevelValF)
 ; resA@cnMaxLevelValF       =  1.5               ; set max contour level
 ; delete(resA@cnLevelSpacingF)
 ; resA@cnLevelSpacingF      =  0.05               ; set contour spacing

 ;figure text
 resA@gsnCenterString    = "Microsystis:50th Mean"   ; add titles
 resA@gsnLeftString    = " "
 resA@gsnRightString    = " "

 ;address warnings
 resA@trGridType           = "TriangularMesh"
 resA@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(0) = gsn_csm_contour_map_ce(wks,mu_micro_medgd(:,:), resA)
 ;print("plot 1a is done")

;  SUB Figure B
 resB@mpMinLonF   = -83.75
 resB@mpMaxLonF   = -80.0
 resB@mpMinLatF   = 41.25
 resB@mpMaxLatF   = 43

 delete(resB@cnFillPalette)
 ;resB@cnFillPalette   = "BlueWhiteOrangeRed"
 resB@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resB@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resB@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ; delete(resB@cnMinLevelValF)
 resB@cnMinLevelValF       = 0              ; set min contour level
 ; delete(resB@cnMaxLevelValF)
  resB@cnMaxLevelValF       =  2.4               ; set max contour level
 ; delete(resB@cnLevelSpacingF)
 resB@cnLevelSpacingF      =  0.2               ; set contour spacing

 ;figure text
 resB@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resB@gsnCenterString    = "Trend Microsystis:50th"   ; add titles
 resB@gsnLeftString    = " "
 resB@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_medgd(1,:,:), resB)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_medgd(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .949, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(1)=plotA
  delete(plotA) 
  delete(plotC)

  ; Plot C: micro_hitoms
 ; contouring
 resC@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 ;cmap = read_colormap_file("amwg256")
 ;cmap = cmap(::-1,:) ; reverse the color map
 delete(resC@cnFillPalette)
 resC@cnFillPalette   = "amwg256"
 ; resC@cnMinLevelValF       =  0               ; set min contour level
 ; delete(resC@cnMaxLevelValF)
 ; resC@cnMaxLevelValF       =  1.5               ; set max contour level
 ; delete(resC@cnLevelSpacingF)
 ; resC@cnLevelSpacingF      =  0.05               ; set contour spacing

 ;figure text
 resC@gsnCenterString    = "Microsystis:99th Mean"   ; add titles
 resC@gsnLeftString    = " "
 resC@gsnRightString    = " "

 ;address warnings
 resC@trGridType           = "TriangularMesh"
 resC@gsnAddCyclic         = False
 
 ;overlay the coastline and land fill after mapping
 ;printVarSummary(lsd)
 ;printVarSummary(sdm)
 ;mu_sdm = mask(mu_sdm,lsd.eq.1,True)    ;this is probably no longer needed
 plot(2) = gsn_csm_contour_map_ce(wks,mu_micro_higd(:,:), resC)
 ;print("plot 1a is done")

;  SUB Figure D - micro_hitoms
 resB@mpMinLonF   = -83.75
 resB@mpMaxLonF   = -80.0
 resB@mpMinLatF   = 41.25
 resB@mpMaxLatF   = 43

 delete(resD@cnFillPalette)
 resD@cnFillPalette   = "BlueWhiteOrangeRed"
 ;resD@cnFillPalette   = "WhiteYellowOrangeRed"
 ;resD@cnLevelSelectionMode = "AutomaticLevels"     ; set manual contour levels
 resD@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
 ; delete(resD@cnMinLevelValF)
 resD@cnMinLevelValF       = -2.5             ; set min contour level
 ; delete(resD@cnMaxLevelValF)
  resD@cnMaxLevelValF       =  2.5               ; set max contour level
 ; delete(resD@cnLevelSpacingF)
 resD@cnLevelSpacingF      =  0.25               ; set contour spacing

 ;figure text
 resD@gsnStringFontHeightF = 0.025              ;change the size of the left and center string labels
 resD@gsnCenterString    = "Trend Microsystis:99th"   ; add titles
 resD@gsnLeftString    = " "
 resD@gsnRightString    = " "

; ; ;stippling overlay -- invisble contours on map
 ;res2@gsnCenterString    = "Trend: MJJAS"   ; add titles
 res2@gsnLeftString    = " "
 res2@gsnRightString    = " "

;  ;stippling overlay - visible only where significant
  opt@gsnLeftString    = " "
  opt@gsnRightString    = " "
  
  plotA = gsn_csm_contour_map_ce(wks,mk_micro_higd(1,:,:), resD)
  plotB = gsn_csm_contour(wks,gsn_add_cyclic_point(mk_micro_higd(0,:,:)), res2)
  plotC = gsn_contour_shade(plotB, .949, 1, opt)  ; shade all areas with p between .99 and 1
  overlay(plotA,plotC)                       ; result will be plotA
  delete(plotB)
  plot(3)=plotA
  delete(plotA) 
  delete(plotC)

  pres@txString = "Annual Growing Days"     ; new resource added in NCL V6.4.0
  gsn_panel(wks,plot,(/2,2/),pres)

  print("Figure 6 is saved")

end

