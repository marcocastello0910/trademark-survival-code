version 19
clear all
set more off
set linesize 120

global root "/Users/marco/Desktop/Tesi magistrale"
global out  "$root/output"
capture mkdir "$out"

* i .dta contengono date-placeholder al 2030; il dato reale arriva a ~marzo 2024:
global cutoff = date("2024-03-31","YMD")   // data di censura amministrativa
global coh_min 1995                         // prima coorte di registrazione
global coh_max 2015                         // ultima coorte (>=~6 anni osservabili)

cap log close
log using "$out/prepare_data.log", replace text


*==============================================================================
* 1. CLASSI: numero di classi distinte + schema beni/servizi (da intl_class)
*==============================================================================
use serial_no intl_class_cd using "$root/intl_class.dta", clear
drop if missing(intl_class_cd)
duplicates drop serial_no intl_class_cd, force
destring intl_class_cd, gen(nice_num) force
gen byte is_good_cls = inrange(nice_num,1,34)
gen byte is_svc_cls  = inrange(nice_num,35,45)
gen byte one = 1                                   // costante: (count) richiede numerica
collapse (count) n_classes=one (max) any_svc=is_svc_cls (max) any_good=is_good_cls, ///
         by(serial_no)
label var n_classes "N. classi Nice distinte"
* schema robustezza: puro beni / puro servizi / misto
gen byte svc_scheme = .
replace  svc_scheme = 0 if any_good==1 & any_svc==0
replace  svc_scheme = 1 if any_svc==1  & any_good==0
replace  svc_scheme = 2 if any_svc==1  & any_good==1
label define schemelbl 0 "Pure goods" 1 "Pure services" 2 "Mixed"
label values svc_scheme schemelbl
label var svc_scheme "Schema classi (robustezza)"
keep serial_no n_classes svc_scheme
save "$out/classinfo.dta", replace          // file intermedio (cancellato a fine script)

*==============================================================================
* 2. CLASSE PRIMARIA -> beni/servizi principale (da classification)
*==============================================================================
use serial_no class_primary_cd using "$root/classification.dta", clear
drop if missing(class_primary_cd)
bysort serial_no (class_primary_cd): keep if _n==1     // scelta deterministica
destring class_primary_cd, gen(prim_num) force
gen byte is_service = .
replace  is_service = 0 if inrange(prim_num,1,34)      // beni
replace  is_service = 1 if inrange(prim_num,35,45)     // servizi
label define svclbl 0 "Goods" 1 "Services"
label values is_service svclbl
label var is_service "Servizio (classe primaria Nice 35-45)"
keep serial_no prim_num is_service
save "$out/primclass.dta", replace          // file intermedio (cancellato a fine script)

*==============================================================================
* 3. NUCLEO: marchi REGISTRATI dal case_file
*==============================================================================
use serial_no registration_dt reg_cancel_dt reg_cancel_cd abandon_dt             ///
    cfh_status_cd renewal_dt use_afdv_acc_in incontest_ack_in opposit_pend_in     ///
    serv_mark_in std_char_claim_in mark_draw_cd lb_use_file_in lb_itu_file_in     ///
    using "$root/case_file.dta", clear

keep if !missing(registration_dt)                       // popolazione a rischio
gen int reg_year = year(registration_dt)
label var reg_year "Anno di registrazione (coorte)"

*--- ESITO di sopravvivenza ---------------------------------------------------
* "morte" = cancellazione della registrazione entro il cutoff.
* (placeholder futuri al 2030 -> trattati come VIVI). reg_cancel_cd=="2" = Sez.8.
gen byte cancelled = !missing(reg_cancel_dt) & reg_cancel_dt <= $cutoff
gen double t_years  = cond(cancelled,                                            ///
                           (reg_cancel_dt - registration_dt)/365.25,             ///
                           ($cutoff       - registration_dt)/365.25)
drop if t_years <= 0                                    // poche date incoerenti
label var cancelled "Registrazione cancellata (evento)"
label var t_years   "Anni dalla registrazione (analysis time)"

* milestone (esiti intermedi, utili per analisi R future)
gen byte sec8_acc = (use_afdv_acc_in==1)   // dichiarazione Sez.8 accettata (~anno 6)
gen byte renewed  = !missing(renewal_dt)   // rinnovo decennale (~anno 10)
gen byte incontest= (incontest_ack_in==1)  // incontestabilita' Sez.15
label var sec8_acc "Sez.8 accettata (~anno 6)"
label var renewed  "Rinnovato (~anno 10)"

*--- PREDITTORI (fissati al/vicino alla registrazione) ------------------------
gen byte std_char   = (std_char_claim_in==1)            // marchio denominativo
gen str1 draw1      = substr(mark_draw_cd,1,1)
gen byte word_mark  = inlist(draw1,"1","4")             // denominativo (typeset/std char)
gen byte design_mark= inlist(draw1,"2","3","5")         // figurativo / con elemento grafico
gen byte opp_pending= (opposit_pend_in==1)              // PROXY DEBOLE (serve event.dta per una vera var)
gen byte basis_itu  = (lb_itu_file_in==1)               // deposito intent-to-use
label var std_char    "Caratteri standard (denominativo)"
label var design_mark "Marchio figurativo / con grafica"
label var opp_pending "Opposizione pendente (proxy debole)"

*==============================================================================
* 4. UNIONE + RESTRIZIONE COORTI -> tm_survival.dta
*==============================================================================
merge 1:1 serial_no using "$out/primclass.dta", keep(master match) nogen
merge 1:1 serial_no using "$out/classinfo.dta", keep(master match) nogen
replace n_classes = 1 if missing(n_classes) & !missing(prim_num)  // fallback

keep if inrange(reg_year, $coh_min, $coh_max)
compress
save "$out/tm_survival.dta", replace
count
di as txt "tm_survival.dta creato: `r(N)' marchi registrati, coorti $coh_min-$coh_max"

* pulizia dei file intermedi (restano solo i dati grezzi e tm_survival.dta)
erase "$out/classinfo.dta"
erase "$out/primclass.dta"

log close
