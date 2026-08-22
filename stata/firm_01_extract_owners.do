clear all
set more off
version 17

local ROOT "/Users/marco/Desktop/Tesi magistrale"
cd "`ROOT'"

* --- the marks we care about -------------------------------------------------
use serial_no using "output/tm_survival.dta", clear
duplicates drop serial_no, force
sort serial_no
quietly count
display as text "sample marks: " as result r(N)
save "output/_tm_keys.dta", replace

* --- block pass over the owner table ----------------------------------------
describe using "data_external/owner.dta"
local NOBS = r(N)
local CH   = 4000000
local NCH  = ceil(`NOBS' / `CH')
display as text "owner.dta rows: " as result `NOBS' as text "  blocks: " as result `NCH'

local blocks ""
forvalues i = 1/`NCH' {
    local lo = (`i' - 1) * `CH' + 1
    local hi = min(`i' * `CH', `NOBS')

    use serial_no own_name own_seq own_type_cd own_entity_cd own_addr_country_cd ///
        using "data_external/owner.dta" in `lo'/`hi', clear
    merge m:1 serial_no using "output/_tm_keys.dta", keep(match) nogenerate

    * Whole blocks can contain no sample mark at all; -by- cannot generate a
    * variable on an empty dataset, so those are skipped rather than saved.
    quietly count
    if r(N) == 0 {
        display as text "  block `i'/`NCH' (`lo'-`hi') kept " as result 0
        continue
    }

    gen byte prio = cond(own_type_cd == 20, 0,          ///
                    cond(own_type_cd == 30, 1,          ///
                    cond(own_type_cd == 10, 2,          ///
                    cond(own_type_cd  < 40, 3, 4))))
    bysort serial_no (prio own_seq): gen byte _first = (_n == 1)
    keep if _first
    drop _first
    compress
    save "output/_own_block`i'.dta", replace
    local blocks "`blocks' `i'"
    display as text "  block `i'/`NCH' (`lo'-`hi') kept " as result _N
}

* --- accumulate, then re-select across block boundaries ----------------------
* A mark's rows can straddle two blocks, so the priority rule is applied once
* more on the combined file.
local first : word 1 of `blocks'
use "output/_own_block`first'.dta", clear
foreach i of local blocks {
    if `i' != `first' append using "output/_own_block`i'.dta"
}
bysort serial_no (prio own_seq): gen byte _first = (_n == 1)
keep if _first
drop _first

* Country is recorded only for foreign addresses; blank means a US address.
replace own_addr_country_cd = "US" if own_addr_country_cd == ""

compress
save "output/owners_sample.dta", replace

quietly count
display as text _n "one owner per mark: " as result r(N)
tabulate own_type_cd if own_type_cd < 45, missing
quietly count if prio == 4
display as text "marks where only an assignee exists: " as result r(N)

* --- tidy up -----------------------------------------------------------------
forvalues i = 1/`NCH' {
    capture erase "output/_own_block`i'.dta"
}
erase "output/_tm_keys.dta"
display as text _n "Saved output/owners_sample.dta"
