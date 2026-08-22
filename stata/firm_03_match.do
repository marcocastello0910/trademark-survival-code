clear all
set more off
version 17

local ROOT "/Users/marco/Desktop/Tesi magistrale"
cd "`ROOT'"
do "firm_namekey.do"

local H    8               // survival horizon, as in Parts III-IV
local Y0   2005
local Y1   2015
local ASOF = mdy(3, 31, 2024)   // data cut of the case-files extract

log using "output/firm_match_diag.txt", replace text name(diag)

* --- marks -------------------------------------------------------------------
use serial_no reg_year registration_dt cancelled t_years prim_num is_service ///
    using "output/tm_survival.dta", clear

gen double age_obs = (`ASOF' - registration_dt) / 365.25
keep if age_obs >= `H'                                  // observable at h
gen byte surv = !(cancelled == 1 & t_years <= `H')
keep if inrange(reg_year, `Y0', `Y1')

quietly count
display as text "marks in `Y0'-`Y1' observable at h=`H': " as result r(N)
sort serial_no
tempfile marks
save `marks'

* --- owners ------------------------------------------------------------------
use "output/owners_sample.dta", clear
namekey own_name, generate(name_key)
keep if length(name_key) > 1
quietly count
display as text "owners with a usable normalised name: " as result r(N)

keep serial_no name_key own_entity_cd own_addr_country_cd
sort serial_no
merge 1:1 serial_no using `marks', keep(match) nogenerate
quietly count
display as text "marks with an owner name            : " as result r(N)

* --- Compustat ---------------------------------------------------------------
preserve
    use "output/compustat_firm.dta", clear
    keep if length(name_key) > 1
    bysort name_key: gen byte _nf = _N
    quietly count if _nf > 1
    display as text _n "dropping rows on ambiguous names    : " as result r(N)
    drop if _nf > 1
    drop _nf
    keep gvkey name_key conm naics naics_sector rd rd_years at revt emp
    sort name_key
    tempfile cs
    save `cs'
restore

sort name_key
merge m:1 name_key using `cs', keep(match) nogenerate
quietly count
display as text "marks matched to a Compustat firm   : " as result r(N)
egen byte _tag = tag(gvkey)
quietly count if _tag
display as text "distinct firms matched              : " as result r(N)
drop _tag

* --- firm-level indicator ----------------------------------------------------
* is_service is undefined for 0.5% of marks; those are excluded from the ratio
* rather than counted as services, so is_goods is left missing for them.
gen byte is_goods = (is_service == 0) if !missing(is_service)
gen byte _one = 1

* Country is used only to separate US from foreign parents.
gen byte foreign = (own_addr_country_cd != "US")

collapse (sum)    N_raw = _one                          ///
         (mean)   S = surv goods_share = is_goods       ///
                  foreign_share = foreign               ///
         (count)  n_classified = is_goods               ///
         (firstnm) conm naics naics_sector rd rd_years at revt emp, ///
         by(gvkey)

gen double adj = N_raw * S
gen str8 Sector = cond(goods_share > 0.5, "Goods", "Services")
gen str2 own_country = cond(foreign_share < 0.5, "US", "XX")
label variable own_country "US if most marks carry a US owner address"

quietly count
display as text _n "firm-level rows                     : " as result r(N)
quietly count if rd_years > 0
display as text "  with R&D reported                 : " as result r(N)
quietly count if rd > 0 & !missing(rd)
display as text "  with R&D > 0                      : " as result r(N)

save "output/firm_panel.dta", replace
export delimited using "output/firm_panel.csv", replace   // consumed by the R analyses

* --- how the sample thins with a minimum mark count --------------------------
display as text _n "=== usable sample by minimum trademark count (R&D > 0) ==="
display as text "  min N    firms    goods  services"
foreach k in 1 3 5 10 20 50 {
    quietly count if rd > 0 & N_raw >= `k'
    local a = r(N)
    quietly count if rd > 0 & N_raw >= `k' & Sector == "Goods"
    local g = r(N)
    quietly count if rd > 0 & N_raw >= `k' & Sector == "Services"
    local s = r(N)
    display as text "  " %5.0f `k' "    " as result %5.0f `a' "    " %5.0f `g' "     " %5.0f `s'
}

display as text _n "=== largest matched firms (sanity check) ==="
gsort -N_raw
list conm N_raw S adj goods_share rd Sector if rd > 0 in 1/15, ///
    noobs sep(0) abbreviate(12)

log close diag
display as text _n "Saved firm_panel.dta, firm_panel.csv and firm_match_diag.txt"
