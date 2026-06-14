version 17
clear
set more off

args tab5_dir
if `"`tab5_dir'"' == "" {
    local dofile `"`c(filename)'"'
    local dofile : subinstr local dofile "\" "/", all
    local slash = strrpos(`"`dofile'"', "/")
    if `slash' > 0 {
        local tab5_dir = substr(`"`dofile'"', 1, `slash' - 1)
    }
    else {
        local tab5_dir `"`c(pwd)'"'
    }
}
cd `"`tab5_dir'"'
capture mkdir "boots_Y_SUM_mar26"
capture mkdir "advregs_Y_SUM_mar26"

import delimited "data_heterogeneity_boots_y_sum.csv", clear 

save "data_heterogeneity_boots_y_sum.dta", replace





clear

********************************************** FROM HERE WE START THE BOOTSTRAP PART **********************************************

***** LET'S BOOTSTRP THE ERRORS COL. 1

*Load bootstrap data
use "data_heterogeneity_boots_y_sum.dta", clear

capture drop quantili
sort bootstrap

*****Defining "Mineral (05)" and "Cement (13)" as Metals.
ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = "." if `var' == "NA"
} 


gen industrymode_aggregated = industry_mode
replace industrymode_aggregated = "Metals" if industry_mode=="Mineral (05)"|industry_mode=="Cement (13)"


********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

*egen quantili=xtile(TE_indiv_boots), n(10) by(bootstrap)
*save "data_heterogeneity_boots.dta", replace
*sort bootstrap quantili
*keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

gen TE_indiv_boots = export_futurex - pred_sum
***CODE IF WE WANT 25% value
capture drop quantili
egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap)
**save "data_heterogeneity_boots.dta", replace
sort bootstrap quantili
keep if quantili ==1 |quantili ==4


********************************************************************************
********************************************************************************

gen Special=0
replace Special= 1 if strpos(industrymode_aggregated,"Arms (19)")
replace Special= 1 if strpos(industrymode_aggregated,"Art (21)")
replace Special= 1 if strpos(industrymode_aggregated,"Precis. inst. (18)")
replace Special= 1 if strpos(industrymode_aggregated,"Special (22)")



gen Agriculture=0
replace Agriculture = 1 if strpos(industrymode_aggregated, "Animal (01)")
replace Agriculture = 1 if strpos(industrymode_aggregated, "Vegetable (02)")
replace Agriculture = 1 if strpos(industrymode_aggregated, "Prep. food (04)")
replace Agriculture = 1 if strpos(industrymode_aggregated, "Fats/oils (03)")


gen Chemicals=0
replace Chemicals= 1 if strpos(industrymode_aggregated,"Chemical (06)")
replace Chemicals= 1 if strpos(industrymode_aggregated,"Plastics (07)")
replace Chemicals= 1 if strpos(industrymode_aggregated,"Mineral (05)")
replace Chemicals= 1 if strpos(industrymode_aggregated,"Cement (13)")

gen Manufacturing=0
replace Manufacturing= 1 if strpos(industrymode_aggregated,"Manuf. (20)")
replace Manufacturing= 1 if strpos(industrymode_aggregated,"Machinery (16)")

replace Manufacturing= 1 if strpos(industrymode_aggregated,"Vehicles (17)")


gen Metals=0
replace Metals= 1 if strpos(industrymode_aggregated,"Metals")
replace Metals= 1 if strpos(industrymode_aggregated,"Metals (15)")
replace Metals= 1 if strpos(industrymode_aggregated,"Jewel (14)")
replace Metals= 1 if strpos(industrymode_aggregated,"Mineral (05)")
replace Metals= 1 if strpos(industrymode_aggregated,"Cement (13)")

gen Textile=0
replace Textile= 1 if strpos(industrymode_aggregated, "Textile (11)")
replace Textile= 1 if strpos(industrymode_aggregated, "Leather (08)")
replace Textile= 1 if strpos(industrymode_aggregated, "Footwear (12)")

gen Wood=0
replace Wood= 1 if strpos(industrymode_aggregated,"Wood (09)")
replace Wood= 1 if strpos(industrymode_aggregated,"Paper (10)")



gen Land=0
replace Land = 1 if via_mode=="Land"
gen Air=0
replace Air = 1 if via_mode=="Air"
gen Sea=0
replace Sea = 1 if via_mode=="Sea"

gen Jan = 0
replace Jan = 1 if month==1
gen Feb = 0
replace Feb = 1 if month==2
gen Mar = 0
replace Mar = 1 if month==3
gen Apr = 0
replace Apr = 1 if month==4
gen May = 0
replace May = 1 if month==5
gen Jun = 0
replace Jun = 1 if month==6
gen Jul = 0
replace Jul = 1 if month==7
gen Aug = 0
replace Aug = 1 if month==8
gen Sep = 0
replace Sep = 1 if month==9
gen Oct = 0
replace Oct = 1 if month==10
gen Nov = 0
replace Nov = 1 if month==11
gen Dec = 0
replace Dec = 1 if month==12

gen dummy = 0
*bys bootstrap: replace dummy=1 if quantili ==10
bys bootstrap: replace dummy=1 if quantili ==4

****** SAVING A DATA FOR EACH BOOT. SAMPLE:


forval q = 1/100 {
	preserve
        keep if bootstrap==`q'
        save "boots_Y_SUM_mar26/col1_adv_boot`q'.dta", replace
restore
     
}


*SAVING COEFFICIENTS AND STANDARD ERRORS FOR EACH BOOTSTRAP

forvalues q = 1/100 {

    clear
    tempfile col1all_results_boot_`q'
    save `col1all_results_boot_`q'', emptyok

    capture confirm file "boots_Y_SUM_mar26/col1_adv_boot`q'.dta"
    if _rc {
        di as error "Bootstrap file q=`q' not found"
        continue
    }

    use "boots_Y_SUM_mar26/col1_adv_boot`q'.dta", clear

    count
    if r(N)==0 {
        di as error "Bootstrap file q=`q' has 0 observations"
        continue
    }

    local success = 0

    foreach v of varlist TE_indiv_boots lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land Agriculture Chemicals Manufacturing Metals Wood Special Textile Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec {

        capture confirm variable `v'
        if _rc {
            di as error "Variable `v' not found in q=`q'"
            continue
        }

        tempfile `v'
        capture noisily statsby, saving(``v'', replace): reg `v' dummy `variables'
        if _rc {
            di as error "statsby failed for variable `v' in q=`q'"
            continue
        }

        clear
        use ``v'', clear
        gen Variable = "`v'"
        append using `col1all_results_boot_`q''
        save `col1all_results_boot_`q'', replace
        local success = 1

        use "boots_Y_SUM_mar26/col1_adv_boot`q'.dta", clear
    }

    if `success' == 0 {
        di as error "No regressions succeeded for q=`q'"
        continue
    }

    use `col1all_results_boot_`q'', clear

    capture confirm variable _b_dummy
    if !_rc rename _b_dummy Coeff

    capture confirm variable _b_cons
    if !_rc drop _b_cons

    save "advregs_Y_SUM_mar26/col1_adv_regression_`q'.dta", replace
}



clear
cd "advregs_Y_SUM_mar26"
local fnames: dir "." files "col1_adv_regression_*.dta"
drop _all

foreach f of local fnames {
    append using `"`f'"'
}   
sort Variable
keep Coeff Variable
cd `"`tab5_dir'"'
save "adv_regressions_all_together_col1", replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id) 
drop id
sort Variable
gen id = _n
replace Variable = "TE_indiv_original_SAM_SUM" if Variable == "TE_indiv_boots"
save "adv_boot_coefficients_col1.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 Variable id using "original_coefficients_advcadiff_nocontrol_y_sum.dta"
gen t_stat = Coeff_orig/coeff_sd
gen significance_five = 0
**significnce 5%
replace significance_five = 1 if abs(t_stat)>1.96
**significance 10% (reproducing the paper)
gen significance_ten = 0
replace significance_ten = 1 if abs(t_stat)>1.645
******* COMPUTING p-values:
gen t_stat_abs = abs(t_stat)
gen p_values = 2*ttail(99, t_stat_abs[_n])

order Variable Coeff_orig p_values
drop Coeff coeff_sd id _merge t_stat* significance*
gen significance = 1 if p_values<=0.05

cd `"`tab5_dir'"'

dataout, save(annual_case_adv_cadiff_col1_boot) tex replace









clear

***** LET'S BOOTSTRP THE ERRORS COL. 2

*Load bootstrap data
use "data_heterogeneity_boots_y_sum.dta", clear

capture drop quantili
sort bootstrap

*****Defining "Mineral (05)" and "Cement (13)" as Metals.
ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = "." if `var' == "NA"
} 


gen industrymode_aggregated = industry_mode
replace industrymode_aggregated = "Metals" if industry_mode=="Mineral (05)"|industry_mode=="Cement (13)"

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

*egen quantili=xtile(TE_indiv_boots), n(10) by(bootstrap)
*save "data_heterogeneity_boots.dta", replace
*sort bootstrap quantili
*keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

gen TE_indiv_boots = export_futurex - pred_sum
**CODE IF WE WANT 25% value
capture drop quantili
egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap)
*save "data_heterogeneity_boots.dta", replace
sort bootstrap quantili
keep if quantili ==1 |quantili ==4

********************************************************************************
********************************************************************************

gen Special=0
replace Special= 1 if strpos(industrymode_aggregated,"Arms (19)")
replace Special= 1 if strpos(industrymode_aggregated,"Art (21)")
replace Special= 1 if strpos(industrymode_aggregated,"Precis. inst. (18)")
replace Special= 1 if strpos(industrymode_aggregated,"Special (22)")



gen Agriculture=0
replace Agriculture = 1 if strpos(industrymode_aggregated, "Animal (01)")
replace Agriculture = 1 if strpos(industrymode_aggregated, "Vegetable (02)")
replace Agriculture = 1 if strpos(industrymode_aggregated, "Prep. food (04)")
replace Agriculture = 1 if strpos(industrymode_aggregated, "Fats/oils (03)")


gen Chemicals=0
replace Chemicals= 1 if strpos(industrymode_aggregated,"Chemical (06)")
replace Chemicals= 1 if strpos(industrymode_aggregated,"Plastics (07)")
replace Chemicals= 1 if strpos(industrymode_aggregated,"Mineral (05)")
replace Chemicals= 1 if strpos(industrymode_aggregated,"Cement (13)")

gen Manufacturing=0
replace Manufacturing= 1 if strpos(industrymode_aggregated,"Manuf. (20)")
replace Manufacturing= 1 if strpos(industrymode_aggregated,"Machinery (16)")

replace Manufacturing= 1 if strpos(industrymode_aggregated,"Vehicles (17)")


gen Metals=0
replace Metals= 1 if strpos(industrymode_aggregated,"Metals")
replace Metals= 1 if strpos(industrymode_aggregated,"Metals (15)")
replace Metals= 1 if strpos(industrymode_aggregated,"Jewel (14)")
replace Metals= 1 if strpos(industrymode_aggregated,"Mineral (05)")
replace Metals= 1 if strpos(industrymode_aggregated,"Cement (13)")

gen Textile=0
replace Textile= 1 if strpos(industrymode_aggregated, "Textile (11)")
replace Textile= 1 if strpos(industrymode_aggregated, "Leather (08)")
replace Textile= 1 if strpos(industrymode_aggregated, "Footwear (12)")

gen Wood=0
replace Wood= 1 if strpos(industrymode_aggregated,"Wood (09)")
replace Wood= 1 if strpos(industrymode_aggregated,"Paper (10)")



gen Land=0
replace Land = 1 if via_mode=="Land"
gen Air=0
replace Air = 1 if via_mode=="Air"
gen Sea=0
replace Sea = 1 if via_mode=="Sea"

gen Jan = 0
replace Jan = 1 if month==1
gen Feb = 0
replace Feb = 1 if month==2
gen Mar = 0
replace Mar = 1 if month==3
gen Apr = 0
replace Apr = 1 if month==4
gen May = 0
replace May = 1 if month==5
gen Jun = 0
replace Jun = 1 if month==6
gen Jul = 0
replace Jul = 1 if month==7
gen Aug = 0
replace Aug = 1 if month==8
gen Sep = 0
replace Sep = 1 if month==9
gen Oct = 0
replace Oct = 1 if month==10
gen Nov = 0
replace Nov = 1 if month==11
gen Dec = 0
replace Dec = 1 if month==12

gen dummy = 0
*bys bootstrap: replace dummy=1 if quantili ==10
bys bootstrap: replace dummy=1 if quantili ==4

****** SAVING A DATA FOR EACH BOOT. SAMPLE:


****** SAVING A DATA FOR EACH BOOT. SAMPLE:


forval q = 1/100 {
	preserve
        keep if bootstrap==`q'
        save "boots_Y_SUM_mar26/col2_adv_boot`q'.dta", replace
restore
     
}


*SAVING COEFFICIENTS AND STANDARD ERRORS FOR EACH BOOTSTRAP


forvalues q = 1/100 {

    clear
    tempfile col2all_results_boot_`q'
    save `col2all_results_boot_`q'', emptyok

    * Check that the input file exists
    capture confirm file "boots_Y_SUM_mar26/col2_adv_boot`q'.dta"
    if _rc {
        di as error "Bootstrap file q=`q' not found"
        continue
    }

    use "boots_Y_SUM_mar26/col2_adv_boot`q'.dta", clear

    * Skip empty bootstrap samples
    count
    if r(N) == 0 {
        di as error "Bootstrap file q=`q' has 0 observations"
        continue
    }

    local success = 0

    foreach v of varlist TE_indiv_boots lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land Agriculture Chemicals Manufacturing Metals Wood Special Textile Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec {

        * Start from the sector controls each time
        local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile
        local not `v'
        local variables : list variables - not

        capture confirm variable `v'
        if _rc {
            di as error "Variable `v' not found in q=`q'"
            continue
        }

        tempfile `v'
        capture noisily statsby, saving(``v'', replace): reg `v' dummy `variables'
        if _rc {
            di as error "statsby failed for variable `v' in q=`q'"
            continue
        }
    }

    * Preserve your original behavior:
    * append only this subset of variables to the final results file
    foreach v of varlist TE_indiv_boots lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec {

        capture confirm file ``v''
        if _rc {
            di as error "Tempfile for `v' was not created in q=`q'"
            continue
        }

        clear
        use ``v'', clear
        gen Variable = "`v'"
        append using `col2all_results_boot_`q''
        save `col2all_results_boot_`q'', replace
        local success = 1
    }

    if `success' == 0 {
        di as error "No regressions were successfully appended for q=`q'"
        continue
    }

    clear
    use `col2all_results_boot_`q'', clear

    capture confirm variable _b_dummy
    if !_rc rename _b_dummy Coeff

    capture confirm variable _b_cons
    if !_rc drop _b_cons

    save "advregs_Y_SUM_mar26/col2_adv_regression_`q'.dta", replace
}



clear
cd "advregs_Y_SUM_mar26"
local fnames: dir "." files "col2_adv_regression_*.dta"
drop _all

foreach f of local fnames {
    append using `"`f'"'
}   
sort Variable
keep Coeff Variable
cd `"`tab5_dir'"'
save "adv_regressions_all_together_col2", replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id) 
drop id
sort Variable
gen id = _n
save "adv_boot_coefficients_col2.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 id using "original_coefficients_advcadiff_control_sec_y_sum.dta"
gen t_stat = Coeff_orig/coeff_sd
gen significance_five = 0
**significnce 5%
replace significance_five = 1 if abs(t_stat)>1.96
**significance 10% (reproducing the paper)
gen significance_ten = 0
replace significance_ten = 1 if abs(t_stat)>1.645
******* COMPUTING p-values:
gen t_stat_abs = abs(t_stat)
gen p_values = 2*ttail(99, t_stat_abs[_n])

order Variable Coeff_orig p_values
drop Coeff coeff_sd id _merge t_stat* significance*
gen significance = 1 if p_values<=0.05


cd `"`tab5_dir'"'

dataout, save(annual_case_adv_cadiff_col2_boot) tex replace










clear

***** LET'S BOOTSTRP THE ERRORS COL. 3

*Load bootstrap data
use "data_heterogeneity_boots_y_sum.dta", clear
capture drop quantili
sort bootstrap

capture drop quantili
sort bootstrap

*****Defining "Mineral (05)" and "Cement (13)" as Metals.
ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = "." if `var' == "NA"
} 


gen industrymode_aggregated = industry_mode
replace industrymode_aggregated = "Metals" if industry_mode=="Mineral (05)"|industry_mode=="Cement (13)"

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

*egen quantili=xtile(TE_indiv_boots), n(10) by(bootstrap)
*save "data_heterogeneity_boots.dta", replace
*sort bootstrap quantili
*keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

gen TE_indiv_boots = export_futurex - pred_sum
**CODE IF WE WANT 25% value
capture drop quantili
egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap)
*save "data_heterogeneity_boots.dta", replace
sort bootstrap quantili
keep if quantili ==1 |quantili ==4

********************************************************************************
********************************************************************************

gen Special=0
replace Special= 1 if strpos(industrymode_aggregated,"Arms (19)")
replace Special= 1 if strpos(industrymode_aggregated,"Art (21)")
replace Special= 1 if strpos(industrymode_aggregated,"Precis. inst. (18)")
replace Special= 1 if strpos(industrymode_aggregated,"Special (22)")



gen Agriculture=0
replace Agriculture = 1 if strpos(industrymode_aggregated, "Animal (01)")
replace Agriculture = 1 if strpos(industrymode_aggregated, "Vegetable (02)")
replace Agriculture = 1 if strpos(industrymode_aggregated, "Prep. food (04)")
replace Agriculture = 1 if strpos(industrymode_aggregated, "Fats/oils (03)")


gen Chemicals=0
replace Chemicals= 1 if strpos(industrymode_aggregated,"Chemical (06)")
replace Chemicals= 1 if strpos(industrymode_aggregated,"Plastics (07)")
replace Chemicals= 1 if strpos(industrymode_aggregated,"Mineral (05)")
replace Chemicals= 1 if strpos(industrymode_aggregated,"Cement (13)")

gen Manufacturing=0
replace Manufacturing= 1 if strpos(industrymode_aggregated,"Manuf. (20)")
replace Manufacturing= 1 if strpos(industrymode_aggregated,"Machinery (16)")

replace Manufacturing= 1 if strpos(industrymode_aggregated,"Vehicles (17)")


gen Metals=0
replace Metals= 1 if strpos(industrymode_aggregated,"Metals")
replace Metals= 1 if strpos(industrymode_aggregated,"Metals (15)")
replace Metals= 1 if strpos(industrymode_aggregated,"Jewel (14)")
replace Metals= 1 if strpos(industrymode_aggregated,"Mineral (05)")
replace Metals= 1 if strpos(industrymode_aggregated,"Cement (13)")

gen Textile=0
replace Textile= 1 if strpos(industrymode_aggregated, "Textile (11)")
replace Textile= 1 if strpos(industrymode_aggregated, "Leather (08)")
replace Textile= 1 if strpos(industrymode_aggregated, "Footwear (12)")

gen Wood=0
replace Wood= 1 if strpos(industrymode_aggregated,"Wood (09)")
replace Wood= 1 if strpos(industrymode_aggregated,"Paper (10)")



gen Land=0
replace Land = 1 if via_mode=="Land"
gen Air=0
replace Air = 1 if via_mode=="Air"
gen Sea=0
replace Sea = 1 if via_mode=="Sea"

gen Jan = 0
replace Jan = 1 if month==1
gen Feb = 0
replace Feb = 1 if month==2
gen Mar = 0
replace Mar = 1 if month==3
gen Apr = 0
replace Apr = 1 if month==4
gen May = 0
replace May = 1 if month==5
gen Jun = 0
replace Jun = 1 if month==6
gen Jul = 0
replace Jul = 1 if month==7
gen Aug = 0
replace Aug = 1 if month==8
gen Sep = 0
replace Sep = 1 if month==9
gen Oct = 0
replace Oct = 1 if month==10
gen Nov = 0
replace Nov = 1 if month==11
gen Dec = 0
replace Dec = 1 if month==12

gen dummy = 0
*bys bootstrap: replace dummy=1 if quantili ==10
bys bootstrap: replace dummy=1 if quantili ==4

****** SAVING A DATA FOR EACH BOOT. SAMPLE:


forval q = 1/100 {
	preserve
        keep if bootstrap==`q'
        save "boots_Y_SUM_mar26/col3_adv_boot`q'.dta", replace
restore
     
}


*SAVING COEFFICIENTS AND STANDARD ERRORS FOR EACH BOOTSTRAP


forvalues q = 1/100 {

    clear
    tempfile col3all_results_boot_`q'
    save `col3all_results_boot_`q'', emptyok

    local allvars  TE_indiv_boots lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land Agriculture Chemicals Manufacturing Metals Wood Special Textile
    local keepvars TE_indiv_boots lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land
    local controls Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec Agriculture Chemicals Manufacturing Metals Wood Special Textile

    capture confirm file "boots_Y_SUM_mar26/col3_adv_boot`q'.dta"
    if _rc continue

    use "boots_Y_SUM_mar26/col3_adv_boot`q'.dta", clear

    count
    if r(N) == 0 continue

    local success = 0

    foreach v of varlist `allvars' {
        local variables `controls'
        local not `v'
        local variables : list variables - not

        capture confirm variable `v'
        if _rc continue

        tempfile `v'
        capture noisily statsby, saving(``v'', replace): reg `v' dummy `variables'
    }

    foreach v of varlist `keepvars' {
        capture confirm file ``v''
        if _rc continue

        clear
        use ``v'', clear
        gen Variable = "`v'"
        append using `col3all_results_boot_`q''
        save `col3all_results_boot_`q'', replace
        local success = 1
    }

    if `success' == 0 continue

    use `col3all_results_boot_`q'', clear
    capture confirm variable _b_dummy
    if !_rc rename _b_dummy Coeff
    capture confirm variable _b_cons
    if !_rc drop _b_cons

    save "advregs_Y_SUM_mar26/col3_adv_regression_`q'.dta", replace
}



clear
cd "advregs_Y_SUM_mar26"
local fnames: dir "." files "col3_adv_regression_*.dta"
drop _all

foreach f of local fnames {
    append using `"`f'"'
}   
sort Variable
keep Coeff Variable
cd `"`tab5_dir'"'
save "adv_regressions_all_together_col3", replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id) 
drop id
sort Variable
gen id = _n

replace Variable = "TE_indiv_original_SAM_SUM" if Variable == "TE_indiv_boots"

save "adv_boot_coefficients_col3.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 Variable id using "original_coefficients_advcadiff_control_months_sec_y_sum.dta"
gen t_stat = Coeff_orig/coeff_sd
gen significance_five = 0
**significnce 5%
replace significance_five = 1 if abs(t_stat)>1.96
**significance 10% (reproducing the paper)
gen significance_ten = 0
replace significance_ten = 1 if abs(t_stat)>1.645
******* COMPUTING p-values:
gen t_stat_abs = abs(t_stat)
gen p_values = 2*ttail(99, t_stat_abs[_n])

order Variable Coeff_orig p_values
drop Coeff coeff_sd id _merge t_stat* significance*
gen significance = 1 if p_values<=0.05


cd `"`tab5_dir'"'

dataout, save(annual_case_adv_cadiff_col3_boot) tex replace
















































































