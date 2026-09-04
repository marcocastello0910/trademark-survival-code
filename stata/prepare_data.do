version 19
clear all
set more off
set linesize 120

global root : environment THESIS_ROOT
if "$root" == "" global root "."
global out  "$root/output"
capture mkdir "$out"

* The .dta files carry placeholder dates in 2030; real data end at ~March 2024:
global cutoff = date("2024-03-31","YMD")   // administrative censoring date
global coh_min 1995                         // first registration cohort
global coh_max 2015                         // last cohort (>=~6 years observable)

cap log close
log using "$out/prepare_data.log", replace text


*==============================================================================
* 1. CLASSES: distinct class count + goods/services scheme (from intl_class)
*==============================================================================
use serial_no intl_class_cd using "$root/intl_class.dta", clear
drop if missing(intl_class_cd)
duplicates drop serial_no intl_class_cd, force
destring intl_class_cd, gen(nice_num) force
gen byte is_good_cls = inrange(nice_num,1,34)
gen byte is_svc_cls  = inrange(nice_num,35,45)
gen byte one = 1                                   // constant: (count) needs a numeric variable
collapse (count) n_classes=one (max) any_svc=is_svc_cls (max) any_good=is_good_cls, ///
         by(serial_no)
label var n_classes "Distinct Nice classes"
* robustness scheme: pure goods / pure services / mixed
gen byte svc_scheme = .
replace  svc_scheme = 0 if any_good==1 & any_svc==0
replace  svc_scheme = 1 if any_svc==1  & any_good==0
replace  svc_scheme = 2 if any_svc==1  & any_good==1
label define schemelbl 0 "Pure goods" 1 "Pure services" 2 "Mixed"
label values svc_scheme schemelbl
label var svc_scheme "Class scheme (robustness)"
keep serial_no n_classes svc_scheme
save "$out/classinfo.dta", replace          // intermediate file (erased at the end of the script)

*==============================================================================
* 2. PRIMARY CLASS -> main goods/services label (from classification)
*==============================================================================
use serial_no class_primary_cd using "$root/classification.dta", clear
drop if missing(class_primary_cd)
bysort serial_no (class_primary_cd): keep if _n==1     // deterministic choice
destring class_primary_cd, gen(prim_num) force
gen byte is_service = .
replace  is_service = 0 if inrange(prim_num,1,34)      // goods
replace  is_service = 1 if inrange(prim_num,35,45)     // services
label define svclbl 0 "Goods" 1 "Services"
label values is_service svclbl
label var is_service "Service (primary Nice class 35-45)"
keep serial_no prim_num is_service
save "$out/primclass.dta", replace          // intermediate file (erased at the end of the script)

*==============================================================================
* 3. CORE: REGISTERED marks from the case file
*==============================================================================
use serial_no registration_dt reg_cancel_dt reg_cancel_cd abandon_dt             ///
    cfh_status_cd renewal_dt use_afdv_acc_in incontest_ack_in opposit_pend_in     ///
    serv_mark_in std_char_claim_in mark_draw_cd lb_use_file_in lb_itu_file_in     ///
    using "$root/case_file.dta", clear

keep if !missing(registration_dt)                       // population at risk
gen int reg_year = year(registration_dt)
label var reg_year "Registration year (cohort)"

*--- SURVIVAL OUTCOME ---------------------------------------------------------
* "death" = cancellation of the registration before the cutoff.
* (2030 placeholders -> treated as ALIVE). reg_cancel_cd=="2" = Section 8.
gen byte cancelled = !missing(reg_cancel_dt) & reg_cancel_dt <= $cutoff
gen double t_years  = cond(cancelled,                                            ///
                           (reg_cancel_dt - registration_dt)/365.25,             ///
                           ($cutoff       - registration_dt)/365.25)
drop if t_years <= 0                                    // a few inconsistent dates
label var cancelled "Registration cancelled (event)"
label var t_years   "Years since registration (analysis time)"

* milestones (intermediate outcomes, for the later R analyses)
gen byte sec8_acc = (use_afdv_acc_in==1)   // Section 8 declaration accepted (~year 6)
gen byte renewed  = !missing(renewal_dt)   // ten-year renewal (~year 10)
gen byte incontest= (incontest_ack_in==1)  // Section 15 incontestability
label var sec8_acc "Section 8 accepted (~year 6)"
label var renewed  "Renewed (~year 10)"

*--- PREDICTORS (fixed at or near registration) -------------------------------
gen byte std_char   = (std_char_claim_in==1)            // word mark
gen str1 draw1      = substr(mark_draw_cd,1,1)
gen byte word_mark  = inlist(draw1,"1","4")             // word mark (typeset / standard character)
gen byte design_mark= inlist(draw1,"2","3","5")         // design mark / with a graphic element
gen byte opp_pending= (opposit_pend_in==1)              // WEAK PROXY (event.dta needed for a real variable)
gen byte basis_itu  = (lb_itu_file_in==1)               // intent-to-use filing basis
label var std_char    "Standard characters (word mark)"
label var design_mark "Design mark / with a graphic element"
label var opp_pending "Opposition pending (weak proxy)"

*==============================================================================
* 4. MERGE + COHORT RESTRICTION -> tm_survival.dta
*==============================================================================
merge 1:1 serial_no using "$out/primclass.dta", keep(master match) nogen
merge 1:1 serial_no using "$out/classinfo.dta", keep(master match) nogen
replace n_classes = 1 if missing(n_classes) & !missing(prim_num)  // fallback

keep if inrange(reg_year, $coh_min, $coh_max)
compress
save "$out/tm_survival.dta", replace
count
di as txt "tm_survival.dta created: `r(N)' registered marks, cohorts $coh_min-$coh_max"

* clean up the intermediate files (only the raw data and tm_survival.dta remain)
erase "$out/classinfo.dta"
erase "$out/primclass.dta"

log close
