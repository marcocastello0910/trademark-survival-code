clear all
set more off
version 17

local ROOT "/Users/marco/Desktop/Tesi magistrale"
cd "`ROOT'"
do "firm_namekey.do"

local Y0 2005
local Y1 2015          // overlap with the trademark cohorts at h = 8

* Columns 8, 10, 11 and 12 are gvkey, cusip, naics and sic: they are codes with
* leading zeros, so they must be read as strings.
import delimited "data_external/compustat_funda_2005_2018.csv", ///
    clear varnames(1) case(lower) stringcols(8 10 11 12)

quietly count
display as text "raw rows: " as result r(N)
egen byte _tag = tag(gvkey)
quietly count if _tag
display as text "raw firms: " as result r(N)
drop _tag

* --- de-duplicate and restrict ----------------------------------------------
keep if indfmt == "INDL"
quietly count
display as text "after INDL only: " as result r(N)

keep if curcd == "USD"
quietly count
display as text "after USD only : " as result r(N)

duplicates drop gvkey fyear, force

* --- sector from NAICS, as a cross-check on the Nice-based split -------------
gen str2 naics2 = substr(naics, 1, 2)
gen str8 naics_sector = ""
replace naics_sector = "Goods"    if inlist(naics2, "11", "21", "23", "31", "32", "33")
replace naics_sector = "Services" if naics_sector == "" & naics2 != ""

namekey conm, generate(name_key)
save "output/compustat_clean.dta", replace

* --- firm level over the trademark window ------------------------------------
keep if inrange(fyear, `Y0', `Y1')

gen byte _one = 1
collapse (mean) rd = xrd at revt emp        ///
         (count) rd_years = xrd             ///
         (sum)   n_years = _one             ///
         (lastnm) conm name_key naics naics_sector, by(gvkey)

quietly count
display as text _n "firm level `Y0'-`Y1': " as result r(N) as text " firms"
quietly count if rd_years > 0
display as text "  with R&D reported in >=1 year: " as result r(N)
quietly count if rd > 0 & !missing(rd)
display as text "  with R&D > 0                 : " as result r(N)

* A normalised name that collapses two different firms cannot be matched safely.
gen byte usable = length(name_key) > 1
quietly count if usable
display as text "  usable name key              : " as result r(N)

bysort name_key: gen byte _n_firms = _N if usable
quietly count if _n_firms > 1 & usable
display as text "  rows on ambiguous name keys  : " as result r(N) ///
    as text "  -> dropped at match time"
drop _n_firms usable

save "output/compustat_firm.dta", replace
display as text _n "Saved compustat_clean.dta and compustat_firm.dta"
