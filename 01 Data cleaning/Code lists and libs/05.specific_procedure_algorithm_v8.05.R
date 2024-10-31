###################################################################################################
## NCHDA specific procedures algorithm is written by John Stickley
## Last updated: 2022-12-16 by John Stickley
## Changes
## 01. added missing checks for the new codes
## 02. added a new check to prevent procedures falling through to lower risk SP
##   step:22 mitral_valve_replacement
##   step:23 ross_konno_a
##   step:24 ross_konno_b
##   step:25 ross
##   step:26 aortic_root_replacement
##   step:27 aortic_valve_replacement
##   step:28 tricuspid_valve_replacement
##   step:29 pulmonary_valve_replacement
##   step:30 mitral_valve_repair
##   step:31 aortic_valve_repair
##   step:32 tricuspid_valve_repair
##   Note: we should probably check other SPs to assess whether this method removes
##   overly complex cases being allocated to procedures below in the hierarchy
###################################################################################################

## when TRUE this changes the output from the old standard to a new more detailed output
## for example '99:unallocated' becomes '99:unallocated(SPnn)'
debug <- TRUE
##  debug <- FALSE

###################################################################################################
sp_algorithm <- function(sp) {
  ###############################################################################################
  ## allocate code list and exclude any invalid procedure codes (intersect - procedures_valid)
  ## remove any valid codes that are ignored (setdiff - procedures_exclude) or minor_excluded
  ## use default dplyr version of setdiff > loaded last
  code_list <- as.list(sp[c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10")])

  ## currently used for valve repairs to exclude AVSD
  diagnosis_list <- as.list(sp[c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10")])
  diagnosis_list <- as.list(diagnosis_list[!is.na(diagnosis_list)])
  ## print(code_list)

  ## drop any invalid procedures
  code_list <- intersect(code_list, procedures_valid_nchda)

  ## remove any procedure codes excluded from the NCHDA list
  code_list <- setdiff(code_list, procedures_exclude_non_nchda)

  ## remove any procedure codes excluded from the NCHDA minor_and_excluded list
  code_list <- setdiff(code_list, minor_and_excluded_algorithm)

  ## remove any diagnostic catheters
  code_list_diagnostic <- code_list
  code_list <- setdiff(code_list, diagnostic)

  ## remove any NA codes from list
  code_list <- code_list[!is.na(code_list)]
  code_list_diagnostic <- code_list_diagnostic[!is.na(code_list_diagnostic)]

  ## there should only be valid procedure codes having removed invalid/excluded and minor_and_excluded codes
  sp_algorithm_group <- ""


  ###############################################################################################
  ## Step:00 excluded (no qualifying codes present)
  ## use the code_list_diagnostic > this ensures we only exclude the diagnostic catheters until step 87.
  if (length(code_list_diagnostic) == 0) {
    sp_algorithm_group <- "00:no_qualifying_codes"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:01 norwood
  ## include 1
  ## 121000. Norwood type procedure

  ## include 2

  ## exclude
  ## 122020. Hypoplastic left heart syndrome hybrid approach (transcatheter and surgery) Stage 1
  ## 122021. Hypoplastic left heart syndrome hybrid approach (transcatheter & surgery)

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("121000")
  code_2 <- c()
  exclude <- c("122020", "122021")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "01:norwood"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:02 heart_transplant
  ## include 1
  ## 123701. Heart transplant
  ## 123702. Transplantation of heart - orthotopic allotransplant
  ## 123703. Transplantation of heart - heterotopic (piggy back) allotransplant
  ## 123704. Prosthetic heart (total artificial heart) implantation
  ## 123706. Transplantation of heart: ABO incompatible donor

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("123701", "123702", "123703", "123704", "123706")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "02:heart_transplant"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:03 lung_transplant
  ## include 1
  ## 123213. Transplantation of heart and lungs > because of the added risk of the lungs
  ## 123713. Single lung transplant
  ## 123720. Double lung transplant
  ## 123760. Lung(s) transplant

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(1, 2, 4)
  code_1 <- c("123213", "123713", "123720", "123760")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "03:lung_transplant"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:04 common_arterial_trunk_aorta_repair

  ## include 1
  ## 121100. Common arterial trunk (truncus) repair

  ## include 2
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 121803. Aortic coarct/hypoplasia repair: subclavian flap aortoplasty
  ## 121810. Aortic coarct/hypoplasia repair: extended resection & end/end anast
  ## 121815. Aortic coarct/hypoplasia repair: resection + insertion tube graft
  ## 121830. Aortic arch repair
  ## 122100. Interrupted aortic arch repair

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("121100")
  code_2 <- c("121800", "121801", "121802", "121803", "121810", "121815", "121830", "122100")
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(code_2 %in% code_list))) {
    sp_algorithm_group <- "04:common_arterial_trunk_aorta_repair"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:05 common_arterial_trunk_repair

  ## include 1
  ## 121100. Common arterial trunk (truncus) repair

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("121100")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "05:common_arterial_trunk_repair"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:06 cctga_repair_a

  ## include 1
  ## 122925. Arterial switch & atrial inversion procedures ('double switch')
  ## 122926. Atrial inversion and Rastelli procedures

  ## include

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122925", "122926")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "06:cctga_repair_a"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:07 cctga_repair_b

  ## include 1
  ## 122901. Senning procedure (atrial inversion)
  ## 122902. Mustard procedure (atrial inversion)

  ## include 2
  ## 122745. REV procedure: LV to Ao via VSD & direct RV-PA anast (Lecompte)
  ## 122911. Rastelli procedure: LV to Ao via VSD & RV to pulm trunk conduit
  ## 122921. Arterial switch procedure

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122901", "122902")
  code_2 <- c("122745", "122911", "122921")
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(code_2 %in% code_list))) {
    sp_algorithm_group <- "07:cctga_repair_b"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:08 atrial_switch

  ## include 1
  ## 122901. Senning procedure (atrial inversion)
  ## 122902. Mustard procedure (atrial inversion)

  ## include 2

  ## exclude
  ## 122911. Rastelli procedure: LV to Ao via VSD & RV to pulm trunk conduit
  ## 122921. Arterial switch procedure

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122901", "122902")
  code_2 <- c()
  exclude <- c("122911", "122921")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "08:atrial_switch"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:09 rastelli_rev

  ## include 1
  ## 122745. REV procedure: LV to Ao via VSD & direct RV-PA anast (Lecompte)
  ## 122911. Rastelli procedure: LV to Ao via VSD & RV to pulm trunk conduit

  ## include 2

  ## exclude
  ## 122901. Senning procedure (atrial inversion)
  ## 122902. Mustard procedure (atrial inversion)
  ## 122921. Arterial switch procedure

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122745", "122911")
  code_2 <- c()
  exclude <- c("122901", "122902", "122921")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "09:rastelli_rev"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:10 transposition_complex

  ## include 1
  ## 122702. Double outlet right ventricle repair with intraventricular tunnel
  ## 122778. Aortic root translocation to over left ventricle (including Nikaidoh)
  ## 122940. Complex transposition of great arteries repair

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122702", "122778", "122940")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "10:transposition_complex"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:11 transposition_arch

  ## include 1
  ## 122921. Arterial switch procedure

  ## include 2
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 121803. Aortic coarct/hypoplasia repair: subclavian flap aortoplasty
  ## 121810. Aortic coarct/hypoplasia repair: extended resection & end/end anast
  ## 121815. Aortic coarct/hypoplasia repair: resection + insertion tube graft
  ## 121830. Aortic arch repair
  ## 122100. Interrupted aortic arch repair

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122921")
  code_2 <- c("121800", "121801", "121802", "121803", "121810", "121815", "121830", "122100")
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(code_2 %in% code_list))) {
    sp_algorithm_group <- "11:transposition_arch"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:12 transposition_vsd
  ## include 1
  ## 122921. Arterial switch procedure

  ## include 2
  ## 120801. Ventricular septal defect (VSD) closure
  ## 120802. VSD closure by direct suture
  ## 120803. VSD closure using patch
  ## 120816. Closure of multiple VSDs

  ## exclude
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 121803. Aortic coarct/hypoplasia repair: subclavian flap aortoplasty
  ## 121810. Aortic coarct/hypoplasia repair: extended resection & end/end anast
  ## 121815. Aortic coarct/hypoplasia repair: resection + insertion tube graft
  ## 121830. Aortic arch repair
  ## 122100. Interrupted aortic arch repair
  ## 122901. Senning procedure (atrial inversion)
  ## 122902. Mustard procedure (atrial inversion)

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122921")
  code_2 <- c("120801", "120802", "120803", "120816")
  exclude <- c("121800", "121801", "121802", "121803", "121810", "121815", "121830", "122100", "122901", "122902")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(code_2 %in% code_list)) & (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "12:transposition_vsd"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:13 transposition

  ## include 1
  ## 122921. Arterial switch procedure

  ## include 2

  ## exclude
  ## 120801. VSD closure
  ## 120802. VSD closure by direct suture
  ## 120803. VSD closure using patch
  ## 120816. Closure of multiple VSDs
  ## 120828. VSD closure with prosthesis - intraoperatively
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 121803. Aortic coarct/hypoplasia repair: subclavian flap aortoplasty
  ## 121810. Aortic coarct/hypoplasia repair: extended resection & end/end anast
  ## 121815. Aortic coarct/hypoplasia repair: resection + insertion tube graft
  ## 121830. Aortic arch repair
  ## 122100. Interrupted aortic arch repair
  ## 122601. Tetralogy of Fallot repair
  ## 122901. Senning procedure (atrial inversion)
  ## 122902. Mustard procedure (atrial inversion)
  ## 122920. Double outlet RV repair

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122921")
  code_2 <- c("")
  exclude <- c(
    "121801", "121802", "121803", "120816", "120828", "121800", "121801", "121802", "121803",
    "121810", "121815", "121830", "122100", "122601", "122901", "122902", "122920"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "13:transposition"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:14 tapvc_shunt

  ## include 1
  ## 120000. TAPVC repair

  ## include 2
  ## 123103. Modified R Blalock interposition shunt
  ## 123104. Modified L Blalock interposition shunt
  ## 123105. Waterston (ascending Ao-RPA) anastomosis
  ## 123106. Central systemic-PA interposition shunt
  ## 123130. Systemic-to-pulmonary arterial shunt procedure
  ## 123146. Modified Blalock interposition shunt
  ## 123601. Right ventricle to pulmonary arterial tree conduit construction

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120000")
  code_2 <- c("123103", "123104", "123105", "123106", "123130", "123146", "123601")
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(code_2 %in% code_list))) {
    sp_algorithm_group <- "14:tapvc_shunt"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:15 tapvc

  ## include 1
  ## 120000. TAPVC repair

  ## include 2

  ## exclude
  ## 122601. Tetralogy of Fallot repair
  ## 122613. Tetralogy of Fallot repair with transannular patch
  ## 122620. Tetralogy of Fallot repair without transannular patch
  ## 122701. Double outlet RV (Fallot-type) repair
  ## 122702. Double outlet RV repair with intraventricular tunnel
  ## 123001. Fontan type procedure
  ## 123005. Total cavopulmonary connection (TCPC) using extracardiac inferior caval vein (IVC)-pulmonary artery conduit with fenestration
  ## 123006. Total cavopulmonary connection (TCPC) with fenestrated lateral atrial tunnel
  ## 123032. Fontan procedure with direct atrio-pulmonary anastomosis
  ## 123050. Total cavopulmonary connection (TCPC)
  ## 123051. Total cavopulmonary conn (TCPC) with lateral atrial tunnel
  ## 123054. Total cavopulmonary conn (TCPC) using extracardiac IVC-PA conduit
  ## 123092. Total cavopulmonary connection (TCPC) using intra-extracardiac conduit: fenestrated,
  ## 123093. Total cavopulmonary connection (TCPC) using intra-extracardiac conduit: nonfenestrated,
  ## 123103. Modified R Blalock interposition shunt
  ## 123104. Modified L Blalock interposition shunt
  ## 123105. Waterston (ascending Ao-RPA) anastomosis
  ## 123106. Central systemic-PA interposition shunt
  ## 123111. Bidirectional superior cavopulmonary (Glenn) anastomosis
  ## 123115. Hemi-Fontan procedure
  ## 123130. Systemic-to-pulmonary arterial shunt procedure
  ## 123144. Bilateral bidirectional superior cavopulmonary (Glenn) anastomoses
  ## 123146. Modified Blalock interposition shunt
  ## 123172. Superior caval vein to pulmonary artery anastomosis

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120000")
  code_2 <- c()
  exclude <- c(
    "122601", "122613", "122620", "122701", "122702", "123001", "123005", "123006", "123032",
    "123032", "123050", "123051", "123054", "123092", "123093", "123103", "123104", "123105",
    "123106", "123111", "123115", "123130", "123144", "123146", "123172"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "15:tapvc"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:16 fontan

  ## include 1
  ## 123001. Fontan type procedure
  ## 123005. Total cavopulmonary connection (TCPC) using extracardiac inferior caval vein (IVC)-pulmonary artery conduit with fenestration
  ## 123006. Total cavopulmonary connection (TCPC) with fenestrated lateral atrial tunnel
  ## 123013. Fontan procedure with atrio-ventricular connection
  ## 123028. Fontan-type connection without fenestration
  ## 123032. Fontan procedure with direct atrio-pulmonary anastomosis
  ## 123050. Total cavopulmonary connection (TCPC)
  ## 123051. Total cavopulmonary conn (TCPC) with lateral atrial tunnel
  ## 123054. Total cavopulmonary conn (TCPC) using extracardiac IVC-PA conduit
  ## 123092. Total cavopulmonary connection (TCPC) using intra-extracardiac conduit: fenestrated,
  ## 123093. Total cavopulmonary connection (TCPC) using intra-extracardiac conduit: nonfenestrated,
  ## 127044. Completion of total cavopulmonary connection (TCPC): hepatic veins to pulmonary artery: fenestrated,
  ## 127045. Completion of total cavopulmonary connection (TCPC): hepatic veins to pulmonary artery: nonfenestrated,

  ## include 2

  ## exclude
  ## 123034. Conversion of Fontan repair to total cavopulmonary connection
  ## 123037. Fontan type procedure revision or conversion

  ## allow only

  type_procedure <- c(1, 2, 7)
  code_1 <- c(
    "123001", "123005", "123006", "123013", "123028", "123032", "123050", "123051", "123054",
    "123092", "123093", "127044", "127045"
  )
  code_2 <- c()
  exclude <- c("123034", "123037")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "16:fontan"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:17 glenn

  ## include 1
  ## 123111. Bidirectional superior cavopulmonary (Glenn) anastomosis
  ## 123115. Hemi-Fontan procedure
  ## 123144. Bilateral bidirectional superior cavopulmonary (Glenn) anastomoses
  ## 123145. Unidirectional superior cavopulmonary (Glenn) anastomosis
  ## 123172. Superior caval vein to pulmonary artery anastomosis

  ## include 2

  ## exclude
  ## 120000. TAPVC repair
  ## 120619. 1.5 ventricle repair: superior cavopulmonary (Glenn) anastomosis + right ventricular outflow tract reconstruction
  ## 120903. Damus-Kaye-Stansel type procedure: pulmonary trunk to aorta end/side anastomosis
  ## 121630. Ross procedure (Ao root replacement with pulm autograft)
  ## 122022. Hypoplastic left heart syndrome hybrid approach (transcatheter & surgery) 'stage 2': aortopulmonary amalgamation + superior cavopulmonary anastomosis(es) + debanding of pulmonary arteries
  ## 122023. Hypoplastic left heart syndrome hybrid approach (transcatheter & surgery) 'stage 2': aortopulmonary amalgamation + superior cavopulmonary anastomosis(es) + debanding of pulmonary arteries + arch repair
  ## 122100. Interrupted aortic arch repair
  ## 122911. Rastelli procedure: LV to Ao via VSD & RV to pulm trunk conduit

  ## allow only

  type_procedure <- c(1, 2, 7)
  code_1 <- c("123111", "123115", "123144", "123145", "123172")
  code_2 <- c()
  exclude <- c("120000", "120619", "120903", "121630", "122022", "122023", "122100", "122911")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "17:glenn"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:18 avsd_fallot_a

  ## include 1
  ## 120511. AVSD & Tetralogy of Fallot repair

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(1, 7)
  code_1 <- c("120511")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "18:avsd_fallot_a"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:19 avsd_fallot_b

  ## include 1
  ## 120501. AVSD: complete (common valve orifice) repair
  ## 120510. AVSD: 'intermediate' repair
  ## 120571. Atrioventricular septal defect (AVSD) repair with direct ventricular component closure & patch to atrial component (Nunn/Wilson)

  ## include 2
  ## 122601. Tetralogy of Fallot repair
  ## 122613. Tetralogy of Fallot repair with transannular patch
  ## 122620. Tetralogy of Fallot repair without transannular patch

  ## exclude

  ## allow only

  type_procedure <- c(1, 7)
  code_1 <- c("120501", "120510", "120571")
  code_2 <- c("122601", "122613", "122620")
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(code_2 %in% code_list))) {
    sp_algorithm_group <- "19:avsd_fallot_b"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:20 avsd_complete

  ## include 1
  ## 120501. AVSD: complete (common valve orifice) repair
  ## 120510. AVSD: 'intermediate' repair
  ## 120571. Atrioventricular septal defect (AVSD) repair with direct ventricular component closure & patch to atrial component (Nunn/Wilson)

  ## include 2

  ## exclude
  ## 120401. AVSD: partial (primum ASD) repair
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 121803. Aortic coarct/hypoplasia repair: subclavian flap aortoplasty
  ## 121810. Aortic coarct/hypoplasia repair: extended resection & end/end anast
  ## 121815. Aortic coarct/hypoplasia repair: resection + insertion tube graft
  ## 121830. Aortic arch repair
  ## 122601. Tetralogy of Fallot repair
  ## 122613. Tetralogy of Fallot repair with transannular patch
  ## 122620. Tetralogy of Fallot repair without transannular patch
  ## 122701. Double outlet RV (Fallot-type) repair
  ## 122801. Pulmonary atresia & VSD (including Fallot-type) repair

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120501", "120510", "120571")
  code_2 <- c()
  exclude <- c(
    "120401", "121800", "121801", "121802", "121803", "121810", "121815", "121830",
    "122601", "122613", "122620", "122701", "122801"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "20:avsd_complete"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:21 avsd_partial

  ## include 1
  ## 120401. AVSD: partial (primum ASD) repair

  ## include 2

  ## exclude
  ## 120501. AVSD: complete (common valve orifice) repair
  ## 120510. AVSD: 'intermediate' repair
  ## 120571. Atrioventricular septal defect (AVSD) repair with direct ventricular component closure & patch to atrial component (Nunn/Wilson)
  ## 120641. RV outflow tract obstruction relief
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. Left ventricular outflow tract myectomy-myotomy
  ## 120712. Left ventricular outflow tract obstruction relief: complex (Konno etc)
  ## 120713. LV outflow tract obstruction relief
  ## 120822. Subaortic obstruction relief
  ## 120903. Damus-Kaye-Stansel type procedure: pulmonary trunk to aorta end/side anastomosis
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 121803. Aortic coarct/hypoplasia repair: subclavian flap aortoplasty
  ## 121810. Aortic coarct/hypoplasia repair: extended resection & end/end anast
  ## 121815. Aortic coarct/hypoplasia repair: resection + insertion tube graft
  ## 121830. Aortic arch repair
  ## 122601. Tetralogy of Fallot repair
  ## 122613. Tetralogy of Fallot repair with transannular patch
  ## 122620. Tetralogy of Fallot repair without transannular patch
  ## 122701. Double outlet RV (Fallot-type) repair
  ## 122801. Pulmonary atresia & VSD (including Fallot-type) repair
  ## 123601. RV to pulmonary artery conduit construction

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120401")
  code_2 <- c()
  exclude <- c(
    "120501", "120510", "120641", "120701", "120711", "120712", "120713", "120822", "120903",
    "121800", "121801", "121802", "121803", "121810", "121815", "121830", "122601", "122613",
    "122620", "122701", "122801", "123601","120571"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "21:avsd_partial"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:22 mitral_valve_replacement

  ## include 1
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 120489. Mitral valve replacement with stented bovine jugular vein conduit

  ## include 2

  ## exclude
  ## 120211. Tricuspid valvar replacement
  ## 120283. Tricuspid valve repair converted to tricuspid valvar replacement
  ## 120511. AVSD & Tetralogy of Fallot repair
  ## 121321. Pulmonary valvar replacement (not conduit)
  ## 121322. Pulmonary valvar replacement using homograft
  ## 121355. Pulmonary valve repair converted to pulmonary valvar replacement
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121630. Ross procedure (Ao root replacement with pulm autograft)
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement
  ## 123601. RV to pulmonary artery conduit construction

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120311", "120384", "120445", "120489")
  code_2 <- c()
  exclude <- c(
    "120211", "120283", "120511", "121321", "121322", "121355", "121621", "121622",
    "121628", "121629", "121630", "121633", "121650", "121663", "121664", "121697",
    "121777", "121790", "121791", "121799", "123601"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "22:mitral_valve_replacement"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP22)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:23 ross_konno_a

  ## include 1
  ## 121662. Ross-Konno procedure

  ## include 2

  ## exclude
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement
  ## 123111. Bidirectional superior cavopulmonary (Glenn) anastomosis
  ## 123115. Hemi-Fontan procedure
  ## 123144. Bilateral bidirectional superior cavopulmonary (Glenn) anastomoses
  ## 123172. Superior caval vein to pulmonary artery anastomosis
  ## 121642. Aortic aneurysm repair

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("121662")
  code_2 <- c()
  exclude <- c(
    "120311", "120384", "120445", "121633", "121650", "121663", "121664", "121777",
    "121790", "121791", "121799", "123111", "123115", "123144", "123172", "121642"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "23:ross_konno_a" #FEP: I corrected this value
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP23)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:24 ross_konno_b

  ## include 1
  ## 121630. Ross procedure: aortic valve or root replacement with pulmonary autograft & pulmonary valvar replacement

  ## include 2
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. Left ventricular outflow tract myectomy-myotomy
  ## 120712. Left ventricular outflow tract obstruction relief: complex (Konno etc)
  ## 120713. Left ventricular outflow tract obstruction relief
  ## 120822. Subaortic obstruction relief

  ## exclude
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121642. Aortic aneurysm repair
  ## 121650. Aortic root replacement (not Ross)
  ## 121662. Ross-Konno procedure
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement
  ## 123111. Bidirectional superior cavopulmonary (Glenn) anastomosis
  ## 123115. Hemi-Fontan procedure
  ## 123144. Bilateral bidirectional superior cavopulmonary (Glenn) anastomoses
  ## 123172. Superior caval vein to pulmonary artery anastomosis

  ## allow only


  type_procedure <- c(1)
  code_1 <- c("121630")
  code_2 <- c("120701", "120711", "120712", "120713", "120822")
  exclude <- c(
    "120311", "120384", "120445", "121633", "121642", "121650", "121662", "121663", "121664", "121777",
    "121790", "121791", "121799", "123111", "123115", "123144", "123172"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(code_2 %in% code_list)) & (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "24:ross_konno_b"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
      (any(code_2 %in% code_list)) & (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP24)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:25 ross
  ## include 1
  ## 121630. Ross procedure: aortic valve or root replacement with pulmonary autograft & pulmonary valvar replacement

  ## include 2

  ## exclude
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 120712. LV outflow tract obstruction relief - complex (Konno etc)
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121662. Ross-Konno procedure
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement
  ## 123111. Bidirectional superior cavopulmonary (Glenn) anastomosis
  ## 123115. Hemi-Fontan procedure
  ## 123144. Bilateral bidirectional superior cavopulmonary (Glenn) anastomoses
  ## 123172. Superior caval vein to pulmonary artery anastomosis

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("121630")
  code_2 <- c()
  exclude <- c(
    "120311", "120384", "120445", "120712", "121633", "121650", "121662", "121663",
    "121664", "121790", "121791", "121799", "123111", "123115", "123144", "123172"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "25:ross"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP25)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:26 aortic_root_replacement

  ## include 1
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique

  ## include 2

  ## exclude
  ## 120017. Scimitar syndrome (partially anomalous pulmonary venous connection) repair
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. Left ventricular outflow tract myectomy-myotomy
  ## 120712. Left ventricular outflow tract obstruction relief: complex (Konno etc)
  ## 120713. Left ventricular outflow tract obstruction relief
  ## 120822. Subaortic obstruction relief
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("121633", "121650", "121663", "121664", "121790", "121791")
  code_2 <- c()
  exclude <- c("120017", "120311", "120384", "120445", "120701", "120711", "120712", "120713", "120822", "121799")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "26:aortic_root_replacement"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP26)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:27 aortic_valve_replacement

  ## include 1
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),

  ## include 2

  ## exclude
  ## 120211. Tricuspid valvar replacement
  ## 120283. Tricuspid valve repair converted to tricuspid valvar replacement
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. Left ventricular outflow tract myectomy-myotomy
  ## 120712. Left ventricular outflow tract obstruction relief: complex (Konno etc)
  ## 120713. Left ventricular outflow tract obstruction relief
  ## 120822. Subaortic obstruction relief
  ## 121321. Pulmonary valvar replacement (not conduit)
  ## 121355. Pulmonary valve repair converted to pulmonary valvar replacement
  ## 121322. Pulmonary valvar replacement using homograft
  ## 121630. Ross procedure (Ao root replacement with pulm autograft)
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("121621", "121622", "121628", "121629", "121697", "121777")
  code_2 <- c()
  exclude <- c(
    "120211", "120283", "120311", "120384", "120445", "120701", "120711", "120712",
    "120713", "120822", "121321", "121355", "121322", "121630", "121633", "121650",
    "121663", "121664", "121790", "121791", "121799"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "27:aortic_valve_replacement"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP27)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:28 tricuspid_valve_replacement

  ## include 1
  ## 120211. Tricuspid valvar replacement
  ## 120283. Tricuspid valve repair converted to tricuspid valvar replacement

  ## include 2

  ## exclude
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 121321. Pulmonary valvar replacement (not conduit)
  ## 121355. Pulmonary valve repair converted to pulmonary valvar replacement
  ## 121322. Pulmonary valvar replacement using homograft
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121630. Ross procedure (Ao root replacement with pulm autograft)
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120211", "120283")
  code_2 <- c()
  exclude <- c(
    "120311", "120384", "120445", "121321", "121355", "121322", "121621", "121622",
    "121628", "121629", "121630", "121633", "121650", "121663", "121664", "121777",
    "121697", "121790", "121791", "121799"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "28:tricuspid_valve_replacement"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP28)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:29 pulmonary_valve_replacement

  ## include 1
  ## 121321. Pulmonary valvar replacement (not conduit)
  ## 121322. Pulmonary valvar replacement using homograft
  ## 121355. Pulmonary valve repair converted to pulmonary valvar replacement

  ## include 2

  ## exclude
  ## 120211. Tricuspid valvar replacement
  ## 120283. Tricuspid valve repair converted to tricuspid valvar replacement
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121630. Ross procedure (Ao root replacement with pulm autograft)
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("121321", "121322", "121355")
  code_2 <- c()
  exclude <- c(
    "120211", "120283", "120311", "120384", "120445", "121621", "121622", "121628",
    "121629", "121630", "121633", "121650", "121663", "121664", "121777", "121697",
    "121790", "121791", "121799"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "29:pulmonary_valve_replacement"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP29)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:30 mitral_valve_repair

  ## include 1
  ## 120300. Mitral valvar procedure
  ## 120301. Mitral valvotomy
  ## 120303. Mitral leaflet (valvoplasty) procedure
  ## 120304. Mitral valvar annuloplasty

  ## include 2

  ## exclude
  ## 120211. Tricuspid valvar replacement
  ## 120283. Tricuspid valve repair converted to tricuspid valvar replacement
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 120489. Mitral valve replacement with stented bovine jugular vein conduit
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. Left ventricular outflow tract myectomy-myotomy
  ## 120712. Left ventricular outflow tract obstruction relief: complex (Konno etc)
  ## 120713. Left ventricular outflow tract obstruction relief
  ## 120801. VSD closure
  ## 120802. VSD closure by direct suture
  ## 120803. VSD closure using patch
  ## 120822. Subaortic obstruction relief
  ## 121321. Pulmonary valvar replacement (not conduit)
  ## 121322. Pulmonary valvar replacement using homograft
  ## 121355. Pulmonary valve repair converted to pulmonary valvar replacement
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement
  ## 122300. Anomalous coronary artery
  ## 122380. Anomalous aortic origin of coronary artery repair

  ## added 30-01-2021 additional exclude codes
  ## 120200. Tricuspid valvar procedure
  ## 120204. Tricuspid valvar annuloplasty
  ## 121600. Aortic valvar procedure
  ## 123580. Open ablation procedure for atrial arrhythmia
  ## 122308. Coronary arterial bypass graft (CABG) procedure
  ## 123601. Right ventricle to pulmonary arterial tree conduit construction
  ## 120143. Atrial septectomy
  ## 121602. Aortic valvotomy: open
  ## 121611. Aortic cusp(s) repair (valvoplasty)
  ## 121830. Aortic arch repair
  ## 122309. Coronary arterial procedure
  ## 123553. Maze operation
  ## 129001. Atrioventricular valvar repair
  ## 120002. Partially APVC repair
  ## 120020. Pulmonary vein procedure
  ## 120816. Closure of multiple ventricular septal defect (VSD)s
  ## 121403. Pulmonary trunk band removal (de-band)
  ## 121614. 'Annuloplasty' of aortic valve
  ## 121680. Aortic sinus of Valsalva procedure
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 123020. Fenestration of atrial septum
  ## 123142. Takedown of Glenn
  ## 124802. Atrioventricular septal defect (AVSD): suturing together superior + inferior bridging leaflets to left ventricular side of septum ('cleft')
  ## 120078. Partially anomalous pulmonary venous connection repair: baffle redirection to left atrium & systemic vein translocated to right atrial appendage (Warden)
  ## 120144. Blade atrial septostomy
  ## 120157. Atrial baffle procedure
  ## 120209. Ebstein malformation of tricuspid valve repair
  ## 120400. Atrioventricular septal defect procedure
  ## 120409. AVSD: partial with isolated ventricular component (VSD) repair
  ## 120440. Atrioventricular septal defect (AVSD): left atrioventricular valvar procedure
  ## 120643. Right ventricle to pulmonary artery valveless conduit construction (including 'Sano')
  ## 120737. Left ventricular aneurysm repair
  ## 121302. Pulmonary valvotomy - open
  ## 121401. Pulmonary trunk arterioplasty
  ## 121402. Pulmonary trunk band (PA band)
  ## 121420. Pulmonary arterioplasty/ reconstruction
  ## 121605. Balloon aortic valvotomy
  ## 121640. Supravalvar aortic stenosis repair
  ## 121642. Aorta aneurysm repair
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 122920. Double outlet right ventricle repair
  ## 122952. Pulmonary venous pathway procedure (post Senning/Mustard)
  ## 122979. Atrial inversion procedure (Mustard or Senning) revision
  ## 123034. Conversion of Fontan repair to total cavopulmonary connection
  ## 123103. Modified R Blalock interposition shunt
  ## 123106. Central systemic-to-pulmonary arterial interposition shunt
  ## 124801. Common atrioventricular valvar leaflet (valvoplasty) procedure


  ## added 30-01-2021 additional exclude code to remove multiple valve repairs
  ## 121600. Aortic valvar procedure
  ## 121602. Aortic valvotomy - open
  ## 121604. Aortic valvotomy - closed
  ## 121611. Aortic valvoplasty
  ## 121614. Annuloplasty of aortic valve
  ## 120200. Tricuspid valvar procedure
  ## 120202. Tricuspid leaflet (valvoplasty) procedure
  ## 120204. Tricuspid valvar annuloplasty
  ## 120209. Ebsteins malformation of tricuspid valve repair
  ## 120286. Ebstein malformation or dysplasia of tricuspid valve repair: cone procedure,

  ## allow only

  # browser()

  type_procedure <- c(1)
  code_1 <- c("120300", "120301", "120303", "120304")
  code_2 <- c()
  exclude <- c(
    "120211", "120283", "120311", "120384", "120445", "120489", "120701", "120711", "120712",
    "120713", "120801", "120802", "120803", "120822", "121321", "121322", "121355", "121621",
    "121622", "121628", "121629", "121633", "121650", "121663", "121664", "121777", "121697",
    "121790", "121791", "121799", "122300", "122380", "120200", "120204", "121600", "123580",
    "122308", "123601", "120143", "121602", "121611", "121830", "122309", "123553", "129001",
    "120002", "120020", "120816", "121403", "121614", "121680", "121801", "121802", "123020",
    "123142", "124802", "120078", "120144", "120157", "120209", "120400", "120409", "120440",
    "120643", "120737", "121302", "121401", "121402", "121420", "121605", "121640", "121642",
    "121800", "122920", "122952", "122979", "123034", "123103", "123106", "124801", "121600",
    "121602", "121604", "121611", "121614", "120200", "120202", "120204", "120209", "120286"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list)) &
    (!any(avsd_diagnosis %in% diagnosis_list))
  ) {
    sp_algorithm_group <- "30:mitral_valve_repair"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP30)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:31 aortic_valve_repair

  ## include 1
  ## 121600. Aortic valvar procedure
  ## 121602. Aortic valvotomy - open
  ## 121604. Aortic valvotomy - closed
  ## 121611. Aortic valvoplasty
  ## 121614. Annuloplasty of aortic valve

  ## include 2

  ## exclude
  ## 120211. Tricuspid valvar replacement
  ## 120283. Tricuspid valve repair converted to tricuspid valvar replacement
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 120489. Mitral valve replacement with stented bovine jugular vein conduit
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. LV outflow tract myectomy / myotomy
  ## 120712. LV outflow tract obstruction relief - complex (Konno etc)
  ## 120713. LV outflow tract obstruction relief
  ## 120822. Subaortic obstruction relief
  ## 121321. Pulmonary valvar replacement (not conduit)
  ## 121322. Pulmonary valvar replacement using homograft
  ## 121355. Pulmonary valve repair converted to pulmonary valvar replacement
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121630. Ross procedure (Ao root replacement with pulm autograft)
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121640. Supravalvar aortic stenosis repair
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement

  ## added 30-01-2021 additional exclude codes
  ## 120002. Partially APVC repair
  ## 120143. Atrial septectomy
  ## 120202. Tricuspid leaflet (valvoplasty) procedure
  ## 120304. Mitral valvar annuloplasty
  ## 120319. Mitral subvalvar apparatus procedure
  ## 120600. RV outflow tract procedure
  ## 120641. RV outflow tract obstruction relief
  ## 120801. Ventricular septal defect (VSD) closure
  ## 120802. VSD closure by direct suture
  ## 120803. VSD closure using patch
  ## 120816. Closure of multiple ventricular septal defect (VSD)s
  ## 120901. Ventricular septation procedure
  ## 121004. Application of bilateral pulmonary arterial bands & transcatheter placement of stent in arterial duct
  ## 121005. Hypoplastic left heart biventricular repair
  ## 121302. Pulmonary valvotomy - open
  ## 121402. Pulmonary trunk band (PA band)
  ## 121420. Pulmonary arterioplasty/ reconstruction
  ## 121421. Pulmonary arterioplasty/ reconstruction - central (proximal to hilar bifurcation)
  ## 121635. Ascending aorta replacement & aortic valvar resuspension
  ## 121661. Aortic valve closure-oversewing
  ## 121665. Ascending aorta replacement
  ## 121690. Aorto-left ventricular tunnel closure
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 121810. Aortic coarct/hypoplasia repair: extended resection & end/end anast
  ## 121830. Aortic arch repair
  ## 122309. Coronary arterial procedure
  ## 122380. Anomalous aortic origin of coronary artery repair
  ## 122801. Pulmonary atresia & ventricular septal defect (VSD) (including Fallot-type) repair
  ## 123601. Right ventricle to pulmonary arterial tree conduit construction
  ## 123610. Replacement of cardiac conduit
  ## 129001. Atrioventricular valvar repair

  ## added 30-01-2021 additional exclude code to remove multiple valve repairs
  ## 120300. Mitral valvar procedure
  ## 120301. Mitral valvotomy
  ## 120303. Mitral leaflet (valvoplasty) procedure
  ## 120304. Mitral valvar annuloplasty
  ## 120200. Tricuspid valvar procedure
  ## 120202. Tricuspid leaflet (valvoplasty) procedure
  ## 120204. Tricuspid valvar annuloplasty
  ## 120209. Ebsteins malformation of tricuspid valve repair
  ## 120286. Ebstein malformation or dysplasia of tricuspid valve repair: cone procedure

  ## allow only

  type_procedure <- c(1, 2)
  code_1 <- c("121600", "121602", "121604", "121611", "121614")
  code_2 <- c()
  exclude <- c(
    "120211", "120283", "120311", "120384", "120445", "120489", "120701", "120711",
    "120712", "120713", "120822", "121321", "121322", "121355", "121621", "121622",
    "121628", "121629", "121630", "121633", "121640", "121650", "121663", "121664",
    "121790", "121791", "121697", "121777", "121790", "121791", "121799", "120002",
    "120143", "120202", "120304", "120319", "120600", "120641", "120801", "120802",
    "120803", "120816", "120901", "121004", "121005", "121302", "121402", "121420",
    "121421", "121635", "121661", "121665", "121690", "121800", "121801", "121802",
    "121810", "121830", "122309", "122380", "122801", "123601", "123610", "129001",
    "120300", "120301", "120303", "120304", "120200", "120202", "120204", "120209",
    "120286"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list)) &
    (!any(avsd_diagnosis %in% diagnosis_list))
  ) {
    sp_algorithm_group <- "31:aortic_valve_repair"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP31)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:32 tricuspid_valve_repair

  ## include 1
  ## 120200. Tricuspid valvar procedure
  ## 120202. Tricuspid leaflet (valvoplasty) procedure
  ## 120204. Tricuspid valvar annuloplasty
  ## 120209. Ebsteins malformation of tricuspid valve repair
  ## 120286. Ebstein malformation or dysplasia of tricuspid valve repair: cone procedure,

  ## include 2

  ## exclude

  ## 120211. Tricuspid valvar replacement
  ## 120283. Tricuspid valve repair converted to tricuspid valvar replacement
  ## 120300. Mitral valvar procedure
  ## 120303. Mitral leaflet (valvoplasty) procedure
  ## 120311. Mitral valvar replacement
  ## 120384. Mitral valve repair converted to mitral valvar replacement
  ## 120445. AVSD: L AV valvar replacement
  ## 120489. Mitral valve replacement with stented bovine jugular vein conduit
  ## 120801. VSD closure
  ## 120802. VSD closure by direct suture
  ## 120803. VSD closure using patch
  ## 121321. Pulmonary valvar replacement (not conduit)
  ## 121322. Pulmonary valvar replacement using homograft
  ## 121355. Pulmonary valve repair converted to pulmonary valvar replacement
  ## 121600. Aortic valvar procedure
  ## 121611. Aortic cusp(s) repair (valvoplasty)
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement
  ## 122601. Tetralogy of Fallot repair
  ## 122613. Tetralogy of Fallot repair with transannular patch
  ## 122620. Tetralogy of Fallot repair without transannular patch
  ## 122701. Double outlet RV (Fallot-type) repair
  ## 122801. Pulmonary atresia & VSD (including Fallot-type) repair

  ## added 30-01-2021 additional exclude codes
  ## 120002. Partially APVC repair
  ## 120003. Pulmonary vein stenosis repair
  ## 120020. Pulmonary vein procedure
  ## 120039. Superior caval vein (SVC) procedure
  ## 120078. Partially anomalous pulmonary venous connection repair: baffle redirection to left atrium & systemic vein translocated to right atrial appendage (Warden)
  ## 120110. Sinus venosus ASD closure
  ## 120143. Atrial septectomy
  ## 120157. Atrial baffle procedure
  ## 120319. Mitral subvalvar apparatus procedure
  ## 120409. Atrioventricular septal defect (AVSD): partial with isolated ventricular component (VSD) repair
  ## 120440. Atrioventricular septal defect (AVSD): left atrioventricular valvar procedure
  ## 120635. Double chambered right ventricle repair
  ## 120641. Right ventricular outflow tract obstruction relief
  ## 120643. Right ventricle to pulmonary artery valveless conduit construction (Japanese modification: 'Sano') as part of Norwood procedure
  ## 120713. Left ventricular outflow tract obstruction relief
  ## 120806. VSD enlargement
  ## 120816. Closure of multiple ventricular septal defect (VSD)s
  ## 120819. Open fenestration of ventricular septal defect (VSD) patch
  ## 120822. Subaortic obstruction relief
  ## 120835. Ventricular septal defect (VSD) enlargement/ creation
  ## 120903. Damus-Kaye-Stansel type procedure: pulmonary trunk to aorta end/side anastomosis
  ## 121402. Pulmonary trunk band (PA band)
  ## 121420. Pulmonary arterioplasty/ reconstruction
  ## 121505. Application of right pulmonary artery band
  ## 121506. Application of left pulmonary artery band
  ## 121640. Supravalvar aortic stenosis repair
  ## 121642. Aorta aneurysm repair
  ## 121659. Aortic dissection repair
  ## 121680. Aortic sinus of Valsalva procedure
  ## 121830. Aortic arch repair
  ## 122300. Anomalous coronary artery (eg ALCAPA) repair
  ## 122308. Coronary arterial bypass graft (CABG) procedure
  ## 122309. Coronary arterial procedure
  ## 122500. Systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) unifocalisation procedure
  ## 122621. Absent pulmonary valve syndrome (Fallot type) repair
  ## 122920. Double outlet right ventricle repair
  ## 123020. Fenestration of atrial septum
  ## 123103. Modified R Blalock interposition shunt
  ## 123144. Bilateral bidirectional superior cavopulmonary (Glenn) anastomoses
  ## 123310. Traumatic injury of heart repair
  ## 123601. Right ventricle to pulmonary arterial tree conduit construction
  ## 123610. Replacement of cardiac conduit
  ## 123825. Transluminal left atrial appendage occlusion with device
  ## 124600. AV valvar procedure in double inlet ventricle
  ## 129001. Atrioventricular valvar repair

  ## added 30-01-2021 additional exclude code to remove multiple valve repairs
  ## 120300. Mitral valvar procedure
  ## 120301. Mitral valvotomy
  ## 120303. Mitral leaflet (valvoplasty) procedure
  ## 120304. Mitral valvar annuloplasty
  ## 121600. Aortic valvar procedure
  ## 121602. Aortic valvotomy - open
  ## 121604. Aortic valvotomy - closed
  ## 121611. Aortic valvoplasty
  ## 121614. Annuloplasty of aortic valve

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120200", "120202", "120204", "120209", "120286")
  code_2 <- c()
  exclude <- c(
    "120211", "120283", "120300", "120303", "120311", "120384", "120445", "120489",
    "120801", "120802", "120803", "121321", "121322", "121355", "121600", "121611",
    "121621", "121622", "121628", "121629", "121633", "121650", "121663", "121664",
    "121697", "121777", "121790", "121791", "121799", "122601", "122613", "122620",
    "122701", "122801", "120002", "120003", "120020", "120039", "120078", "120110",
    "120143", "120157", "120319", "120409", "120440", "120635", "120641", "120643",
    "120713", "120806", "120816", "120819", "120822", "120835", "120903", "121402",
    "121420", "121505", "121506", "121640", "121642", "121659", "121680", "121830",
    "122300", "122308", "122309", "122500", "122621", "122920", "123020", "123103",
    "123144", "123310", "123601", "123610", "123825", "124600", "129001", "120300",
    "120301", "120303", "120304", "121600", "121602", "121604", "121611", "121614"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list)) &
    (!any(avsd_diagnosis %in% diagnosis_list))
  ) {
    sp_algorithm_group <- "32:tricuspid_valve_repair"
    return(sp_algorithm_group)
  }

  ## new check to stop any further checks to prevent falling through to inappropriate SP
  ## lower in the hierarchy
  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(exclude %in% code_list))) {
    ifelse(debug == TRUE,
      sp_algorithm_group <- "99:unallocated(SP32)",
      sp_algorithm_group <- "99:unallocated"
    )

    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:33 pulmonary_atresia_vsd

  ## include 1
  ## 122801. Pulmonary atresia & ventricular septal defect (VSD) (including Fallot-type) repair
  ## 122811. Pulmonary atresia, ventricular septal defect (VSD) & systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) repair
  ## 122821. Pulmonary atresia, ventricular septal defect (VSD) & systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) repair: status post complete unifocalisation with all usable collaterals incorporated,
  ## 122822. Pulmonary atresia, ventricular septal defect (VSD) & systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) repair: status post incomplete unifocalisation with not all usable collaterals incorporated,

  ## include 2

  ## exclude
  ## 120401. AVSD: partial (primum ASD) repair
  ## 120501. AVSD: complete (common valve orifice) repair
  ## 120510. AVSD: 'intermediate' repair
  ## 120571. Atrioventricular septal defect (AVSD) repair with direct ventricular component closure & patch to atrial component (Nunn/Wilson)

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122801", "122811", "122821", "122822")
  code_2 <- c()
  exclude <- c("120401", "120501", "120510", "120571")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "33:pulmonary_atresia_vsd"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:34 mapca_unifocalisation

  ## include 1
  ## 122500. Systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) unifocalisation procedure
  ## 122567. Unilateral Systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) unifocalisation procedure,
  ## 122587. Bilateral systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) unifocalisation procedure: complete with all usable collaterals incorporated,
  ## 122588. Bilateral systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) unifocalisation procedure: incomplete with not all usable collaterals incorporated,

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(1, 2)
  code_1 <- c("122500", "122567", "122587", "122588")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "34:mapca_unifocalisation"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:35 absent_pulmonary_valve_syndrome

  ## include 1
  ## 122621. Absent pulmonary valve syndrome (Fallot type) repair

  ## include 2

  ## exclude
  ## 120401. AVSD: partial (primum ASD) repair
  ## 120501. AVSD: complete (common valve orifice) repair
  ## 120510. AVSD: 'intermediate' repair
  ## 120571. Atrioventricular septal defect (AVSD) repair with direct ventricular component closure & patch to atrial component (Nunn/Wilson)

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122621")
  code_2 <- c()
  exclude <- c("120401", "120501", "120510", "120571")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "35:absent_pulmonary_valve_syndrome"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:36 fallot

  ## include 1
  ## 122601. Tetralogy of Fallot repair
  ## 122613. Tetralogy of Fallot repair with transannular patch
  ## 122620. Tetralogy of Fallot repair without transannular patch
  ## 122701. Double outlet right ventricle with subaortic or doubly committed ventricular septal defect (VSD) & pulmonary stenosis (Fallot-type) repair

  ## include 2

  ## exclude
  ## 120000. TAPVC repair
  ## 120401. AVSD: partial (primum ASD) repair
  ## 120501. AVSD: complete (common valve orifice) repair
  ## 120510. AVSD: 'intermediate' repair
  ## 120571. Atrioventricular septal defect (AVSD) repair with direct ventricular component closure & patch to atrial component (Nunn/Wilson)
  ## 120619. 1.5 ventricle repair: RV outflow tract reconstruction + Glenn anastomosis
  ## 120903. Damus-Kaye-Stansel type procedure: pulm trunk to aorta end/side anastomosis
  ## 122921. Arterial switch procedure
  ## 123005. Total cavopulmonary connection (TCPC) using extracardiac inferior caval vein (IVC)-pulmonary artery conduit with fenestration
  ## 123050. Total cavopulmonary connection (TCPC)
  ## 123054. Total cavopulmonary conn (TCPC) using extracardiac IVC-PA conduit
  ## 123092. Total cavopulmonary connection (TCPC) using intra-extracardiac conduit: fenestrated,
  ## 123093. Total cavopulmonary connection (TCPC) using intra-extracardiac conduit: nonfenestrated,

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122601", "122613", "122620", "122701")
  code_2 <- c()
  exclude <- c(
    "120000", "120401", "120501", "120510", "120571", "120619", "120903", "122921",
    "123005", "123050", "123054", "123092", "123093"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "36:fallot"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:37 rv_pa_conduit

  ## include 1
  ## 123601. Right ventricle to pulmonary artery conduit construction

  ## include 2

  ## exclude
  ## 120700. LV outflow tract procedure
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. LV outflow tract myectomy / myotomy
  ## 120713. LV outflow tract obstruction relief
  ## 120801. VSD closure
  ## 120802. VSD closure by direct suture
  ## 120803. VSD closure using patch
  ## 120822. Subaortic obstruction relief
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121630. Ross procedure (Ao root replacement with pulm autograft)
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121640. Supravalvar aortic stenosis repair
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement
  ## 123610. Replacement of cardiac conduit
  ## 123646. Replacement of right ventricle to pulmonary artery conduit

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("123601")
  code_2 <- c()
  exclude <- c(
    "120700", "120701", "120711", "120713", "120801", "120802", "120803", "120822", "121621",
    "121622", "121628", "121629", "121630", "121633", "121640", "121650", "121663", "121664",
    "121697", "121777", "121790", "121791", "121799", "123610", "123646"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "37:rv_pa_conduit"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:38 vsd_rvoto

  ## include 1
  ## 120801. VSD closure
  ## 120802. VSD closure by direct suture
  ## 120803. VSD closure using patch

  ## include 2
  ## 120641. Right ventricular outflow tract obstruction relief
  ## 120821. Subpulmonary obstruction relief

  ## exclude
  ## 120700. LV outflow tract procedure
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. LV outflow tract myectomy / myotomy
  ## 120713. LV outflow tract obstruction relief
  ## 120822. Subaortic obstruction relief
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121630. Ross procedure (Ao root replacement with pulm autograft)
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121640. Supravalvar aortic stenosis repair
  ## 121650. Aortic root replacement (not Ross)
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 121799. Aortic root replacement of implanted pulmonary autograft & pulmonary valve re-replacement

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120801", "120802", "120803")
  code_2 <- c("120641", "120821")
  exclude <- c(
    "120700", "120701", "120711", "120713", "120822", "121621", "121622", "121628", "121629",
    "121630", "121633", "121640", "121650", "121663", "121664", "121697", "121777", "121790",
    "121791", "121799"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (any(code_2 %in% code_list)) & (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "38:vsd_rvoto"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:39 supra_valvar_aortic_stenosis

  ## include 1
  ## 121640. Supravalvar aortic stenosis repair

  ## include 2

  ## exclude
  ## 120700. Left ventricular outflow tract procedure
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. Left ventricular outflow tract myectomy-myotomy
  ## 120712. LV outflow tract obstruction relief - complex (Konno etc)
  ## 120713. Left ventricular outflow tract obstruction relief
  ## 120822. Subaortic obstruction relief
  ## 121401. Pulmonary trunk arterioplasty
  ## 121420. Pulmonary arterioplasty/ reconstruction
  ## 121421. Pulmonary arterioplasty/ reconstruction - central (proximal to hilar bifurcation)
  ## 121422. Pulmonary arterioplasty/ reconstruction - peripheral (at/beyond hilar bifurcation)
  ## 121430. Pulmonary artery origin from ascending aorta (hemitruncus) repair
  ## 121431. Pulmonary artery ligation
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121662. Ross-Konno procedure
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("121640")
  code_2 <- c()
  exclude <- c(
    "120700", "120701", "120711", "120712", "120713", "120822", "121401", "121420", "121421",
    "121422", "121430", "121431", "121621", "121622", "121628", "121629", "121633", "121650",
    "121662", "121663", "121664", "121697", "121777", "121790", "121791"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "39:supra_valvar_aortic_stenosis"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:40 sub_valvar_aortic_stenosis

  ## include 1
  ## 120700. LV outflow tract procedure
  ## 120701. Subaortic fibromuscular shelf resection
  ## 120711. LV outflow tract myectomy / myotomy
  ## 120713. LV outflow tract obstruction relief
  ## 120822. Subaortic obstruction relief

  ## include 2

  ## exclude
  ## 120712. LV outflow tract obstruction relief - complex (Konno etc)
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121633. Aortic valvar & asc aorta replacement + coronary arterial inclusion (Bentall)
  ## 121650. Aortic root replacement (not Ross)
  ## 121662. Ross-Konno procedure
  ## 121663. Aortic root replacement using homograft
  ## 121664. Aortic root replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 121777. Aortic valvar replacement using autologous pericardium constructed valve leaflets (Ozaki),
  ## 121790. Aortic root replacement using bioprosthesis
  ## 121791. Aortic root replacement: valve sparing technique
  ## 123601. Right ventricle to pulmonary arterial tree conduit construction

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120700", "120701", "120711", "120713", "120822")
  code_2 <- c()
  exclude <- c(
    "120712", "121621", "121622", "121628", "121629", "121633", "121650", "121662",
    "121663", "121664", "121697", "121777", "121790", "121791", "123601"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "40:sub_valvar_aortic_stenosis"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:41 ap_window

  ## include 1
  ## 121201. Aortopulmonary window closure

  ## include 2

  ## exclude
  ## 121100. Common arterial trunk (truncus) repair
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 121803. Aortic coarct/hypoplasia repair: subclavian flap aortoplasty
  ## 121810. Aortic coarct/hypoplasia repair: extended resection & end/end anast
  ## 121815. Aortic coarct/hypoplasia repair: resection + insertion tube graft
  ## 121830. Aortic arch repair
  ## 122100. Interrupted aortic arch repair

  ## allow only

  type_procedure <- c(1, 2)
  code_1 <- c("121201")
  code_2 <- c()
  exclude <- c("121100", "121800", "121801", "121802", "121803", "121810", "121815", "121830", "122100")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "41:ap_window"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:42 anomalous_coronary

  ## include 1
  ## 122300. Anomalous coronary artery (eg ALCAPA) repair
  ## 122380. Anomalous aortic origin of coronary artery repair

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("122300", "122380")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "42:anomalous_coronary"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:43 cor_triatriatum

  ## include 1
  ## 120131. Cor triatriatum (divided LA) repair

  ## include 2

  ## exclude
  ## 120000. TAPVC repair
  ## 121000. Norwood type procedure

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120131")
  code_2 <- c()
  exclude <- c("120000", "121000")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "43:cor_triatriatum"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:44 pa_band

  ## include 1
  ## 121402. Pulmonary trunk band (PA band)

  ## include 2

  ## exclude

  ## allow only
  ## 120142. Atrial septectomy: closed (Blalock Hanlon)
  ## 120143. Atrial septectomy
  ## 120190. Interatrial communication creation-enlargement
  ## 122400. Arterial duct procedure
  ## 122410. Arterial duct (PDA) closure
  ## 122420. Patent arterial duct (PDA) closure - surgical
  ## 128725. Cardiac support using ECMO circuitry

  ## allow only

  type_procedure <- c(1, 2)
  code_1 <- c("121402")
  code_2 <- c()
  exclude <- c()
  allow_only <- c("122400", "122410", "122420", "128725", "120142", "120143", "120190")
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list)) & (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "44:pa_band"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:45 arterial_shunt

  ## include 1
  ## 123103. Modified R Blalock interposition shunt
  ## 123104. Modified L Blalock interposition shunt
  ## 123105. Waterston (ascending Ao-RPA) anastomosis
  ## 123106. Central systemic-PA interposition shunt
  ## 123130. Systemic-to-pulmonary arterial shunt procedure
  ## 123146. Modified Blalock interposition shunt

  ## include 2

  ## exclude

  ## allow only
  ## 120142. Atrial septectomy: closed (Blalock Hanlon)
  ## 120143. Atrial septectomy
  ## 120190. Interatrial communication creation-enlargement
  ## 122400. Arterial duct procedure
  ## 122410. Arterial duct (PDA) closure
  ## 122420. Patent arterial duct (PDA) closure - surgical
  ## 128725. Cardiac support using ECMO circuitry

  ## allow only

  type_procedure <- c(1, 2)
  code_1 <- c("123103", "123104", "123105", "123106", "123130", "123146")
  code_2 <- c()
  exclude <- c()
  allow_only <- c("122400", "122410", "122420", "128725", "120142", "120143", "120190")
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list) &
    (!length(code_list_remaining) > 0))) {
    sp_algorithm_group <- "45:arterial_shunt"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:46 iaa

  ## include 1
  ## 122100. Interrupted aortic arch repair

  ## include 2

  ## exclude
  ## 121100. Common arterial trunk (truncus) repair
  ## 122601. Tetralogy of Fallot repair
  ## 122613. Tetralogy of Fallot repair with transannular patch
  ## 122620. Tetralogy of Fallot repair without transannular patch
  ## 121621. Aortic valvar replacement
  ## 121622. Aortic valvar replacement using homograft
  ## 121628. Aortic valvar replacement using heterograft bioprosthesis
  ## 121629. Aortic valvar replacement using mechanical prosthesis
  ## 121697. Aortic valve repair converted to aortic valvar replacement
  ## 122701. Double outlet RV (Fallot-type) repair
  ## 122921. Arterial switch procedure
  ## 123111. Bidirectional superior cavopulmonary (Glenn) anastomosis
  ## 123115. Hemi-Fontan procedure
  ## 123144. Bilateral bidirectional superior cavopulmonary (Glenn) anastomoses
  ## 123172. Superior caval vein to pulmonary artery anastomosis

  ## allow only

  type_procedure <- c(1, 2)
  code_1 <- c("122100")
  code_2 <- c()
  exclude <- c(
    "121100", "122601", "122613", "122620", "121621", "121628", "121629", "121697",
    "122701", "122921", "123111", "123115", "123144", "123172"
  )
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "46:iaa"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:47 coarctation_hypoplasia

  ## include 1
  ## 121800. Coarctation / hypoplasia of aorta repair
  ## 121801. Aortic coarct/hypoplasia repair: resection & end/end anast
  ## 121802. Aortic coarct/hypoplasia repair: patch aortoplasty
  ## 121803. Aortic coarct/hypoplasia repair: subclavian flap aortoplasty
  ## 121810. Aortic coarct/hypoplasia repair: extended resection & end/end anast
  ## 121815. Aortic coarct/hypoplasia repair: resection + insertion tube graft
  ## 121830. Aortic arch repair

  ## include 2

  ## exclude

  ## allow only
  ## 120055. Coronary sinus ASD repair
  ## 120101. ASD closure
  ## 120102. ASD closure with direct suture
  ## 120103. ASD closure with patch
  ## 120110. Sinus venosus ASD closure
  ## 120153. Patent foramen ovale (PFO) direct closure
  ## 121711. Vascular ring procedure
  ## 121731. Aortopexy
  ## 121732. Pulmonary arterial sling repair
  ## 122400. Arterial duct procedure
  ## 122410. Arterial duct (PDA) closure
  ## 122420. Patent arterial duct (PDA) closure - surgical
  ## 128725. Cardiac support using ECMO circuitry

  type_procedure <- c(1, 2)
  code_1 <- c("121800", "121801", "121802", "121803", "121810", "121815", "121830")
  code_2 <- c()
  exclude <- c()
  allow_only <- c(
    "120055,", "120101", "120102", "120103", "120110", "120153", "121711", "121731", "121732",
    "122400", "122410", "122420", "128725"
  )
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list) &
    (length(code_list_remaining) == 0))) {
    sp_algorithm_group <- "47:coarctation_hypoplasia"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:48 pulmonary_vein_stenosis

  ## include 1
  ## 120003. Pulmonary vein stenosis repair
  ## 120020. Pulmonary vein procedure

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120003", "120020")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "48:pulmonary_vein_stenosis"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:49 conduit_replacement

  ## include 1
  ## 123610. Replacement of cardiac conduit
  ## 123646. Replacement of right ventricle to pulmonary artery conduit

  ## include 2

  ## exclude

  ## allow only


  type_procedure <- c(1)
  code_1 <- c("123610", "123646")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "49:conduit_replacement"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:50 multiple_vsd

  ## include 1
  ## 120816. Closure of multiple VSDs

  ## include 2

  ## exclude

  ## allow only
  ## 120055. Coronary sinus ASD repair
  ## 120101. ASD closure
  ## 120102. ASD closure with direct suture
  ## 120103. ASD closure with patch
  ## 120110. Sinus venosus ASD closure
  ## 120153. Patent foramen ovale (PFO) direct closure
  ## 120801. VSD closure
  ## 120802. VSD closure by direct suture
  ## 120803. VSD closure using patch
  ## 120838. Removal of ventricular septal defect (VSD) implanted device
  ## 121401. Pulmonary trunk arterioplasty
  ## 121403. Pulmonary trunk band removal (de-band)
  ## 121420. Pulmonary arterioplasty/ reconstruction
  ## 121421. Pulmonary arterioplasty/ reconstruction - central (proximal to hilar bifurcation)
  ## 121711. Vascular ring procedure
  ## 121731. Aortopexy
  ## 121732. Pulmonary arterial sling repair
  ## 122400. Arterial duct procedure
  ## 122410. Arterial duct (PDA) closure
  ## 122420. Patent arterial duct (PDA) closure - surgical
  ## 128725. Cardiac support using ECMO circuitry

  type_procedure <- c(1)
  code_1 <- c("120816")
  code_2 <- c()
  exclude <- c()
  allow_only <- c(
    "120055", "120101", "120102", "120103", "120110", "120153", "120801", "120802", "120803", "120838",
    "121401", "121403", "121420", "121421", "121711", "121731", "121732", "122400", "122410", "122420",
    "128725"
  )
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list) & (length(code_list_remaining) == 0))) {
    sp_algorithm_group <- "50:multiple_vsd"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:51 vsd

  ## include 1
  ## 120801. VSD closure
  ## 120802. VSD closure by direct suture
  ## 120803. VSD closure using patch

  ## include 2

  ## exclude

  ## allow only
  ## 120055. Coronary sinus ASD repair
  ## 120101. ASD closure
  ## 120102. ASD closure with direct suture
  ## 120103. ASD closure with patch
  ## 120110. Sinus venosus ASD closure
  ## 120153. Patent foramen ovale (PFO) direct closure
  ## 120838. Removal of ventricular septal defect (VSD) implanted device
  ## 121403. Pulmonary trunk band removal (de-band)
  ## 121711. Vascular ring procedure
  ## 121731. Aortopexy
  ## 121732. Pulmonary arterial sling repair
  ## 122400. Arterial duct procedure
  ## 122410. Arterial duct (PDA) closure
  ## 122420. Patent arterial duct (PDA) closure - surgical
  ## 128725. Cardiac support using ECMO circuitry

  type_procedure <- c(1)
  code_1 <- c("120801", "120802", "120803")
  code_2 <- c()
  exclude <- c()
  allow_only <- c(
    "120055", "120101", "120102", "120103", "120110", "120153", "120838",
    "121403", "121711", "121731", "121732", "122400", "122410", "122420",
    "128725"
  )
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "51:vsd"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:52 sinus_venosus_asd_papvc

  ## include 1
  ## 120002. Partially anomalous pulmonary venous connection repair
  ## 120078. Partially anomalous pulmonary venous connection repair: baffle redirection to left atrium & systemic vein translocated to right atrial appendage (Warden)
  ## 120110. Sinus venosus ASD closure

  ## include 2

  ## exclude
  ## 120017. Scimitar syndrome (partially anomalous pulmonary venous connection) repair

  ## allow only

  type_procedure <- c(1)
  code_1 <- c("120002", "120078", "120110")
  code_2 <- c()
  exclude <- c("120017")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "52:sinus_venosus_asd_papvc"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:53 vascular_ring

  ## include 1
  ## 121711. Vascular ring procedure
  ## 121731. Aortopexy
  ## 121732. Pulmonary arterial sling repair

  ## include 2

  ## exclude

  ## allow only
  ## 120153. Patent foramen ovale (PFO) direct closure
  ## 122400. Arterial duct procedure
  ## 122410. Arterial duct (PDA) closure
  ## 122420. Patent arterial duct (PDA) closure - surgical
  ## 121401. Pulmonary trunk arterioplasty
  ## 121402. Pulmonary trunk band (PA band)
  ## 121420. Pulmonary arterioplasty/ reconstruction
  ## 121421. Pulmonary arterioplasty/ reconstruction: central (proximal to hilar bifurcation)
  ## 126420. Tracheal procedure
  ## 126440. Tracheobronchial reconstruction procedure
  ## 128725. Cardiac support using ECMO circuitry

  type_procedure <- c(1, 2)
  code_1 <- c("121711", "121731", "121732")
  code_2 <- c()
  exclude <- c()
  allow_only <- c(
    "120153", "122400", "122410", "122420", "121401", "121402", "121420", "121421",
    "126420", "126440", "128725"
  )
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "53:vascular_ring"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:54 asd

  ## include 1
  ## 120055. Coronary sinus ASD repair
  ## 120101. ASD closure
  ## 120102. ASD closure with direct suture
  ## 120103. ASD closure with patch

  ## include 2

  ## exclude

  ## allow only
  ## 120153. Patent foramen ovale (PFO) direct closure
  ## 121711. Vascular ring procedure
  ## 121731. Aortopexy
  ## 121732. Pulmonary arterial sling repair
  ## 122200. Systemic arterial procedure
  ## 122400. Arterial duct procedure
  ## 122410. Arterial duct (PDA) closure
  ## 122420. Patent arterial duct (PDA) closure - surgical
  ## 122999. Removal of interatrial communication ('ASD') implanted device
  ## 128725. Cardiac support using ECMO circuitry

  type_procedure <- c(1)
  code_1 <- c("120055", "120101", "120102", "120103")
  code_2 <- c()
  exclude <- c()
  allow_only <- c("120153", "121711", "121731", "121732", "122400", "122410", "122420", "122999", "128725")
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list)) & (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "54:asd"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:55 pda_ligation

  ## include 1
  ## 122400. Arterial duct procedure
  ## 122410. Arterial duct (PDA) closure
  ## 122420. Patent arterial duct (PDA) closure - surgical

  ## include 2

  ## exclude

  ## allow only
  ## 128725. Cardiac support using ECMO circuitry

  type_procedure <- c(1, 2)
  code_1 <- c("122400", "122410", "122420")
  code_2 <- c()
  exclude <- c()
  allow_only <- c("128725")
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "55:pda_ligation"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:56 arrhythmia_surgical

  ## include 1
  ## 123540. Arrhythmia surgical procedure
  ## 123550. Surgical localisation and mapping procedure for arrhythmia
  ## 123580. Surgical ablation procedure for atrial arrhythmia
  ## 123581. Surgical ablation procedure for ventricular arrhythmia
  ## 123869. Percutaneous radiofrequency epicardial ablation procedure for arrhythmia
  ## 123553. Maze operation
  ## 123572. Cox-Maze IV procedure

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(1, 2, 11)
  code_1 <- c("123540", "123550", "123580", "123581", "123869", "123553", "123572")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "56:arrhythmia_surgical"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:57 pacemaker_epicardial

  ## include 1
  ## 123450. Pacemaker system placement: single chamber
  ## 123451. Pacemaker system placement: dual chamber
  ## 123463. Pacemaker system placement: permanent epicardial
  ## 123467. Pacemaker system placement: permanent
  ## 123485. Pulse generator box placement
  ## 123513. Pulse generator box replacement

  ## include 2

  ## exclude

  ## allow only
  ## 123470. Pacemaker wire procedure
  ## 123514. Removal of complete implanted cardiac pacemaker system
  ## 123468. Pacemaker procedure
  ## 123484. Pacemaker lead revision procedure
  ## 124475. Removal of implanted pacemaker lead
  ## 130501. Diagnostic catheterisation procedure

  type_procedure <- c(2, 11)
  code_1 <- c("123450", "123451", "123463", "123467", "123485", "123513")
  code_2 <- c()
  exclude <- c()
  allow_only <- c("123470", "123514", "123468", "123484", "124475", "130501")
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "57:pacemaker_epicardial"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:58 pda_stent

  ## include 1
  ## 121014. Stent placement in arterial duct (PDA)

  ## include 2

  ## exclude

  ## allow only
  ## 120141. Balloon atrial septostomy by pull back (Rashkind)
  ## 121004. Application of bilateral pulmonary arterial bands & transcatheter placement of stent in arterial duct

  type_procedure <- c(3)
  code_1 <- c("121014")
  code_2 <- c()
  exclude <- c()
  allow_only <- c("121004", "120141")
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "58:pda_stent"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:59 pulmonary_valve_replacement_transluminal

  ## include 1
  ## 121351. Transluminal pulmonary valvar insertion with stent mounted valve
  ## 121385. Transluminal pulmonary valvar insertion with stent mounted valve including prestenting
  ## 121387. Transluminal pulmonary valvar insertion with stent mounted valve with intentional rupture of previous conduit

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("121351", "121385", "121387")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "59:pulmonary_valve_replacement_transluminal"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:60 rvot_stent

  ## include 1
  ## 120618. Stent placement in RV outflow tract

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("120618")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "60:rvot_stent"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:61 pulmonary_valve_radiofrequency

  ## include 1
  ## 121309. Pulmonary valvar transluminal perforation & dilation

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("121309")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "61:pulmonary_valve_radiofrequency"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:62 blade_atrial_septostomy

  ## include 1
  ## 120144. Blade atrial septostomy

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("120144")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "62:blade_atrial_septostomy"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:63 balloon_atrial_septostomy

  ## include 1
  ## 120141. Balloon atrial septostomy by pull back (Rashkind)
  ## 120195. Balloon atrial septostomy

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("120141", "120195")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if (
    (sp["type_procedure"] %in% type_procedure == TRUE) &
      (any(code_1 %in% code_list))
  ) {
    sp_algorithm_group <- "63:balloon_atrial_septostomy"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:64 pulmonary_vein_intervention

  ## include 1
  ## 120021. Balloon dilation of pulmonary vein
  ## 120022. Stent placement in pulmonary vein
  ## 120023. Balloon dilation of pulmonary vein using cutting balloon
  ## 120024. Balloon dilation of pulmonary vein or pathway

  ## include 2

  ## exclude

  ## allow only


  type_procedure <- c(3)
  code_1 <- c("120021", "120022", "120023", "120024")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "64:pulmonary_vein_intervention"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:65 coarctation_stent

  ## include 1
  ## 121817. Stent placement at site of aortic coarctation
  ## 121822. Stent placement at site of aortic recoarctation
  ## 121848. Stent placement at site of native aortic coarctation-hypoplasia

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("121817", "121822", "121848")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "65:coarctation_stent"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:66 coarctation_balloon

  ## include 1
  ## 121804. Balloon dilation of native aortic coarctation-hypoplasia
  ## 121827. Aortic coarctation transluminal obstruction relief

  ## include 2

  ## exclude
  ## 121605. Balloon dilation of aortic valve
  ## 121808. Balloon dilation of aortic recoarctation
  ## 121817. Stent placement at site of aortic coarctation
  ## 121822. Stent placement at site of aortic recoarctation
  ## 121848. Stent placement at site of native aortic coarctation-hypoplasia

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("121804", "121827")
  code_2 <- c()
  exclude <- c("121605", "121808", "121817", "121822", "121848")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "66:coarctation_balloon"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:67 recoarctation_balloon

  ## include 1
  ## 121808. Balloon dilation of aortic re-coarctation

  ## include 2

  ## exclude
  ## 121817. Stent placement at site of aortic coarctation
  ## 121822. Stent placement at site of aortic recoarctation
  ## 121848. Stent placement at site of native aortic coarctation-hypoplasia

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("121808")
  code_2 <- c()
  exclude <- c("121817", "121822", "121848")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "67:recoarctation_balloon"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:68 balloon_aortic_valve

  ## include 1
  ## 121605. Balloon dilation of aortic valve
  ## 121611. Aortic valvoplasty

  ## include 2

  ## exclude
  ## 121625. Aortic valvar transluminal perforation & dilation

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("121605", "121611")
  code_2 <- c()
  exclude <- c("121625")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "68:balloon_aortic_valve"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:69 balloon_pulmonary_valve

  ## include 1
  ## 121305.  Balloon dilation of pulmonary valve

  ## include 2

  ## exclude
  ## 121309. Pulmonary valvar transluminal perforation & dilation

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("121305")
  code_2 <- c()
  exclude <- c("121309")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "69:balloon_pulmonary_valve"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:70 vsd_transluminal

  ## include 1
  ## 120801. VSD closure
  ## 120807. VSD closure with transluminal prosthesis

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("120801", "120807")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "70:vsd_transluminal"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:71 pfo_transluminal

  ## include 1
  ## 120107. PFO closure with transluminal prosthesis

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("120107")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "71:pfo_transluminal"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:72 asd_transluminal

  ## include 1
  ## 120101. ASD closure
  ## 120106. ASD closure with transluminal prosthesis

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("120101", "120106")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "72:asd_transluminal"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:73 pda_transluminal

  ## include 1
  ## 122404. Arterial duct (PDA) closure with transluminal prosthesis
  ## 122421. Arterial duct (PDA) closure with transluminal coil
  ## 122422. Arterial duct (PDA) closure with transluminal Amplatzer plug

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("122404", "122421", "122422")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "73:pda_transluminal"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:74 pa_stent

  ## include 1
  ## 121513. Stent placement in RPA
  ## 121514. Stent placement in LPA
  ## 121550. Stent placement in pulmonary tree

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("121513", "121514", "121550")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "74:pa_stent"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:75 pa_ballooning

  ## include 1
  ## 121503. Balloon dilation of right pulmonary artery
  ## 121504. Balloon dilation of left pulmonary artery
  ## 121405. Balloon dilation of pulmonary trunk
  ## 121553. Balloon dilation of pulmonary tree with cutting balloon

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("121503", "121504", "121405", "121553")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "75:pa_ballooning"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:76 mapca_transluminal

  ## include 1
  ## 122518. Systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) occlusion
  ## 122519. Transluminal procedure to systemic-to-pulmonary collateral artery (MAPCA(s))
  ## 122562. Stent placement in systemic-to-pulmonary collateral artery (MAPCA(s))
  ## 122565. Transluminal occlusion of systemic-to-pulmonary collateral artery(ies) (MAPCA(s)) with coil-device
  ## 122572. Balloon dilation of systemic-to-pulmonary collateral artery(ies) (MAPCA(s))

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("122518", "122519", "122562", "122565", "122572")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "76:mapca_transluminal"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:77 conduit_balloon_stent

  ## include 1
  ## 123614. Balloon dilation of cardiac conduit
  ## 123623. Stent placement in cardiac conduit

  ## include 2

  ## exclude
  ## 121386. Transluminal pulmonary valvar prestenting procedure in preparation for valve replacement

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("123614", "123623")
  code_2 <- c()
  exclude <- c("121386")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "77:conduit_balloon_stent"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:78 stent_dilatation

  ## include 1
  ## 124510. Stent redilation

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3)
  code_1 <- c("124510")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "78:stent_dilatation"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:79 ep_ablation

  ## include 1
  ## 123546. Transluminal cryoablation procedure for arrhythmia
  ## 123548. Transluminal radiofrequency ablation procedure for arrhythmia
  ## 123557. Transluminal procedure for arrhythmia
  ## 123582. Transluminal procedure for atrial arrhythmia
  ## 123583. Transluminal procedure for ventricular arrhythmia
  ## 123584. Transluminal ablation procedure with pulmonary vein exclusion
  ## 123840. Transluminal ablation procedure for arrhythmia

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3, 10)
  code_1 <- c("123546", "123548", "123557", "123582", "123583", "123584", "123840")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "79:ep_ablation"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:80 icd_catheter

  ## include 1
  ## 124231. Implantable cardioverter & defibrillator (ICD) implantation
  ## 124233. Implantable cardioverter & defibrillator (ICD) implantation: endocardial
  ## 124261. Implantable cardioverter & defibrillator (ICD) implantation: single chamber
  ## 124264. Implantable cardioverter & defibrillator (ICD) implantation: dual chamber
  ## 124279. Subcutaneous implantable cardioverter & defibrillator (ICD) implantation

  ## include 2

  ## exclude
  ## 123473. Cardiac resynchronisation therapy (biventricular pacing)
  ## 124265. Implantable cardioverter & defibrillator (ICD) implantation: biventricular

  ## allow only

  type_procedure <- c(3, 10)
  code_1 <- c("124231", "124233", "124261", "124264", "124279")
  code_2 <- c()
  exclude <- c("123473", "124265")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "80:icd_catheter"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:81 icd_surgical

  ## include 1
  ## 124231. Implantable cardioverter & defibrillator (ICD) implantation
  ## 124232. Implantable cardioverter & defibrillator (ICD) implantation: epicardial
  ## 124261. Implantable cardioverter & defibrillator (ICD) implantation: single chamber
  ## 124264. Implantable cardioverter & defibrillator (ICD) implantation: dual chamber
  ## 124279. Subcutaneous implantable cardioverter & defibrillator (ICD) implantation

  ## exclude
  ## 124265. Implantable cardioverter & defibrillator (ICD) implantation: biventricular
  ## 123473. Cardiac resynchronisation therapy (biventricular pacing)

  ## allow only


  type_procedure <- c(2, 11)
  code_1 <- c("124231", "124232", "124261", "124264", "124279")
  code_2 <- c()
  exclude <- c("123473", "124265")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "81:icd_surgical"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:82 pacemaker_crt_bv

  ## include 1
  ## 124265. Implantable cardioverter & defibrillator (ICD) implantation: biventricular
  ## 123452. Pacemaker system placement: biventricular
  ## 123473. Cardiac resynchronisation therapy (biventricular pacing)

  ## include 2

  ## exclude
  ## 124370. Pacemaker system placement: percutaneous leadless

  ## allow only

  type_procedure <- c(2, 3, 10, 11)
  code_1 <- c("124265", "123452", "123473")
  code_2 <- c()
  exclude <- c("124370")
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (!any(exclude %in% code_list))) {
    sp_algorithm_group <- "82:pacemaker_crt_bv"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:83 pacemaker_endocardial

  ## include 1
  ## 123450. Pacemaker system placement: single chamber
  ## 123451. Pacemaker system placement: dual chamber
  ## 123513. Pulse generator box replacement
  ## 123464. Pacemaker system placement: permanent endocardial
  ## 123467. Pacemaker system placement: permanent
  ## 123485. Pulse generator box placement
  ## 124370. Pacemaker system placement: percutaneous leadless

  ## include 2

  ## exclude

  ## allow only
  ## 123470. Pacemaker wire procedure
  ## 123484. Pacemaker wire revision procedure
  ## 123514. Removal of complete implanted cardiac pacemaker system
  ## 123468. Pacemaker procedure
  ## 124475. Removal of implanted pacemaker lead
  ## 130501. Diagnostic cardiovascular catheterisation procedure
  ## 123460. Pacemaker system placement: temporary
  ## 120625. Transluminal right ventricular biopsy
  ## 130506. Diagnostic cardiovascular catheterisation procedure: haemodynamic data obtained
  ## 130512. Electrophysiological study (EPS)

  type_procedure <- c(3, 10)
  code_1 <- c("123450", "123451", "123513", "123464", "123467", "123485", "124370")
  code_2 <- c()
  exclude <- c()
  allow_only <- c(
    "123470", "123484", "123514", "123468", "124475", "130501", "123460", "120625",
    "130506", "130512"
  )
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "83:pacemaker_endocardial"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:84 pacemaker_lead

  ## include 1
  ## 123470. Pacemaker wire procedure
  ## 123484. Pacemaker wire revision procedure
  ## 124475. Removal of implanted pacemaker lead

  ## include 2

  ## exclude

  ## allow only
  ## 120625. Transluminal right ventricular biopsy
  ## 124507. Transluminal diagnostic test occlusion
  ## 130501. Diagnostic catheterisation procedure
  ## 130505. Diagnostic cardiovascular catheterisation procedure: angiographic data obtained
  ## 130506. Diagnostic cardiovascular catheterisation procedure: haemodynamic data obtained
  ## 130507. Diagnostic cardiovascular catheterisation procedure with haemodynamic alteration (challenge)
  ## 130508. Diagnostic cardiovascular catheterisation procedure with electrophysiological alteration (challenge)
  ## 130513. Catheterisation study for pulmonary hypertension evaluation

  type_procedure <- c(2, 3, 10, 11)
  code_1 <- c("123470", "123484", "124475")
  code_2 <- c()
  exclude <- c()
  allow_only <- c("120625", "124507", "130501", "130505", "130506", "130507", "130508", "130513")
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list)) &
    (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "84:pacemaker_lead"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:85 ep_miscellaneous

  ## include 1
  ## 123460. Pacemaker system placement: temporary
  ## 123514. Removal of complete implanted cardiac pacemaker system
  ## 124234. Implantable cardioverter & defibrillator (ICD) transluminal removal
  ## 124239. Implantable cardioverter & defibrillator (ICD) procedure
  ## 128074. Endoscopic cervical sympathectomy: right sided
  ## 128075. Endoscopic cervical sympathectomy: left sided

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(2, 3, 10, 11)
  code_1 <- c("123460", "123514", "124234", "124239", "128074", "128075")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "85:ep_miscellaneous"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:86 ep_diagnostic

  ## include 1
  ## 130512. Electrophysiological study (EPS)
  ## 130517. Electrophysiological study (EPS) with three dimensional mapping

  ## include 2

  ## exclude

  ## allow only

  type_procedure <- c(3, 5, 10)
  code_1 <- c("130512", "130517")
  code_2 <- c()
  exclude <- c()
  allow_only <- c()
  code_list_remaining <- setdiff(code_list, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)

  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list))) {
    sp_algorithm_group <- "86:ep_diagnostic"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:87 catheter_diagnostic
  ## include 1
  ## 120625. Transluminal right ventricular biopsy
  ## 124507. Transluminal diagnostic test occlusion
  ## 130501. Diagnostic catheterisation procedure
  ## 130505. Diagnostic cardiovascular catheterisation procedure: angiographic data obtained
  ## 130506. Diagnostic cardiovascular catheterisation procedure: haemodynamic data obtained
  ## 130507. Diagnostic cardiovascular catheterisation procedure with haemodynamic alteration (challenge)
  ## 130508. Diagnostic cardiovascular catheterisation procedure with electrophysiological alteration (challenge)
  ## 130513. Catheterisation study for pulmonary hypertension evaluation

  ## include 2

  ## exclude

  ## allow only
  ## 122341. Transluminal intracoronary echocardiography (IVUS)
  ## 124559. Transluminal procedure using adjunctive therapy
  ## 130124. Transluminal intracardiac echocardiographic examination

  type_procedure <- c(3, 5)
  code_1 <- c("120625", "124507", "130501", "130505", "130506", "130507", "130508", "130513")
  code_2 <- c()
  exclude <- c()
  allow_only <- c("122341", "124559", "130124")
  code_list_remaining <- setdiff(code_list_diagnostic, code_1)
  code_list_remaining <- setdiff(code_list_remaining, code_2)
  code_list_remaining <- setdiff(code_list_remaining, allow_only)


  if ((sp["type_procedure"] %in% type_procedure == TRUE) & (any(code_1 %in% code_list_diagnostic)) &
    (length(code_list_remaining) == 0)) {
    sp_algorithm_group <- "87:catheter_diagnostic"
    return(sp_algorithm_group)
  }


  ################################################################################################
  ## step:99 unallocated
  if (sp_algorithm_group == "") {
    sp_algorithm_group <- "99:unallocated"
    return(sp_algorithm_group)
  }
  ################################################################################################

  return(sp_algorithm_group)
}