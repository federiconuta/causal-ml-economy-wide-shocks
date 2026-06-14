clear

local tab4_dir = subinstr("`c(pwd)'", "\", "/", .)
capture confirm file "data_heterogeneity_boots.csv"
if _rc {
    capture cd "Tab_4"
    local tab4_dir = subinstr("`c(pwd)'", "\", "/", .)
}
capture confirm file "data_heterogeneity_boots.csv"
if _rc {
    display as error "Run this do-file from replication_code_online/Tab_4 or from replication_code_online."
    exit 601
}
capture mkdir "`tab4_dir'/boots_mar26"
capture mkdir "`tab4_dir'/advregs_mar26"
capture mkdir "`tab4_dir'/bootstrap_data"
capture mkdir "`tab4_dir'/bootstrap_data/regression_boots"
capture mkdir "`tab4_dir'/bootstrap_data_quarter"
capture mkdir "`tab4_dir'/bootstrap_data_quarter/regression_boots_qtr"
capture mkdir "`tab4_dir'/bootstrap_data_month"
capture mkdir "`tab4_dir'/bootstrap_data_month/regression_boots_month"

import delimited "`tab4_dir'/data_heterogeneity_boots.csv", clear 

save "`tab4_dir'/data_heterogeneity_boots.dta", replace





clear

********************************************** FROM HERE WE START THE BOOTSTRAP PART **********************************************

***** LET'S BOOTSTRP THE ERRORS COL. 1

*Load bootstrap data
use "`tab4_dir'/data_heterogeneity_boots.dta", clear

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
*save "`tab4_dir'/data_heterogeneity_boots.dta", replace
*sort bootstrap quantili
*keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

gen TE_indiv_boots = pred_sam - pred_sum
***CODE IF WE WANT 25% value
capture drop quantili
egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap)
**save "`tab4_dir'/data_heterogeneity_boots.dta", replace
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
        save "`tab4_dir'/boots_mar26/col1_adv_boot`q'.dta", replace
restore
     
}


*SAVING COEFFICIENTS AND STANDARD ERRORS FOR EACH BOOTSTRAP

forvalues q = 1/100 {

    clear
    tempfile col1all_results_boot_`q'
    save `col1all_results_boot_`q'', emptyok

    capture confirm file "`tab4_dir'/boots_mar26/col1_adv_boot`q'.dta"
    if _rc {
        di as error "Bootstrap file q=`q' not found"
        continue
    }

    use "`tab4_dir'/boots_mar26/col1_adv_boot`q'.dta", clear

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

        use "`tab4_dir'/boots_mar26/col1_adv_boot`q'.dta", clear
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

    save "`tab4_dir'/advregs_mar26/col1_adv_regression_`q'.dta", replace
}



clear
cd "`tab4_dir'/advregs_mar26/"
local fnames: dir "." files "col1_adv_regression_*.dta"
drop _all

foreach f of local fnames {
    append using `"`f'"'
}   
sort Variable
keep Coeff Variable
save "`tab4_dir'/adv_regressions_all_together_col1", replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id) 
drop id
sort Variable
gen id = _n

replace Variable = "TE_indiv_original_SAM_SUM" if strpos(Variable, "TE_indiv_boots")

save "`tab4_dir'/adv_boot_coefficients_col1.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 Variable id using "`tab4_dir'/original_coefficients_advcadiff_nocontrol.dta"
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

cd "`tab4_dir'"

dataout, save(annual_case_adv_cadiff_col1_boot) tex replace





clear

***** LET'S BOOTSTRP THE ERRORS COL. 2

*Load bootstrap data
use "`tab4_dir'/data_heterogeneity_boots.dta", clear

capture drop quantili
sort bootstrap

*****Defining "Mineral (05)" and "Cement (13)" as Metals.
ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = "." if `var' == "NA"
} 


gen industrymode_aggregated = industry
replace industrymode_aggregated = "Metals" if industry_mode=="Mineral (05)"|industry_mode=="Cement (13)"

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

*egen quantili=xtile(TE_indiv_boots), n(10) by(bootstrap)
*save "`tab4_dir'/data_heterogeneity_boots.dta", replace
*sort bootstrap quantili
*keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

gen TE_indiv_boots = pred_sam - pred_sum
**CODE IF WE WANT 25% value
capture drop quantili
egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap)
*save "`tab4_dir'/data_heterogeneity_boots.dta", replace
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
        save "`tab4_dir'/boots_mar26/col2_adv_boot`q'.dta", replace
restore
     
}


*SAVING COEFFICIENTS AND STANDARD ERRORS FOR EACH BOOTSTRAP


forvalues q = 1/100 {

    clear
    tempfile col2all_results_boot_`q'
    save `col2all_results_boot_`q'', emptyok

    * Check that the input file exists
    capture confirm file "`tab4_dir'/boots_mar26/col2_adv_boot`q'.dta"
    if _rc {
        di as error "Bootstrap file q=`q' not found"
        continue
    }

    use "`tab4_dir'/boots_mar26/col2_adv_boot`q'.dta", clear

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

    save "`tab4_dir'/advregs_mar26/col2_adv_regression_`q'.dta", replace
}



clear
cd "`tab4_dir'/advregs_mar26/"
local fnames: dir "." files "col2_adv_regression_*.dta"
drop _all

foreach f of local fnames {
    append using `"`f'"'
}   
sort Variable
keep Coeff Variable
save "`tab4_dir'/adv_regressions_all_together_col2", replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id) 
drop id
sort Variable
gen id = _n
save "`tab4_dir'/adv_boot_coefficients_col2.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 id using "`tab4_dir'/original_coefficients_advcadiff_control_sec.dta"
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

cd "`tab4_dir'"

dataout, save(annual_case_adv_cadiff_col2_boot) tex replace

















clear

***** LET'S BOOTSTRP THE ERRORS COL. 3

*Load bootstrap data
use "`tab4_dir'/data_heterogeneity_boots.dta", clear
capture drop quantili
sort bootstrap

capture drop quantili
sort bootstrap

*****Defining "Mineral (05)" and "Cement (13)" as Metals.
ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = "." if `var' == "NA"
} 


gen industrymode_aggregated = industry
replace industrymode_aggregated = "Metals" if industry_mode=="Mineral (05)"|industry_mode=="Cement (13)"

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

*egen quantili=xtile(TE_indiv_boots), n(10) by(bootstrap)
*save "`tab4_dir'/data_heterogeneity_boots.dta", replace
*sort bootstrap quantili
*keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

gen TE_indiv_boots = pred_sam - pred_sum
**CODE IF WE WANT 25% value
capture drop quantili
egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap)
*save "`tab4_dir'/data_heterogeneity_boots.dta", replace
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
        save "`tab4_dir'/boots_mar26/col3_adv_boot`q'.dta", replace
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

    capture confirm file "`tab4_dir'/boots_mar26/col3_adv_boot`q'.dta"
    if _rc continue

    use "`tab4_dir'/boots_mar26/col3_adv_boot`q'.dta", clear

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

    save "`tab4_dir'/advregs_mar26/col3_adv_regression_`q'.dta", replace
}



clear
cd "`tab4_dir'/advregs_mar26/"
local fnames: dir "." files "col3_adv_regression_*.dta"
drop _all

foreach f of local fnames {
    append using `"`f'"'
}   
sort Variable
keep Coeff Variable
save "`tab4_dir'/adv_regressions_all_together_col3", replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id) 
drop id
sort Variable
gen id = _n
save "`tab4_dir'/adv_boot_coefficients_col3.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 id using "`tab4_dir'/original_coefficients_advcadiff_control_months_sec.dta"
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

cd "`tab4_dir'"

dataout, save(annual_case_adv_cadiff_col3_boot) tex replace


******* RUN UNTIL HERE. IF YOU WANT TO RUN THE SUBSEQUENT CODE MAKE SURE YOU ADJUST THE PATHS 

















































































































************************** OTHER ROB. CHECKS (NOT IN THE PAPER) ********************************** 


********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************

                    **** QUARTERLY ANALYSIS:  *****

********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************



clear
tempfile all_results_quarter
save `all_results_quarter', emptyok

use "`tab4_dir'/data_heterogeneity_original.dta", clear

drop quarter
gen quarter = 1 if month==1|month==2|month==3
replace quarter = 2 if month==4|month==5|month==6
replace quarter = 3 if month==7|month==8|month==9
replace quarter = 4 if month==10|month==11|month==12

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

*egen quantili=xtile(TE_indiv_original), n(10) by(quarter)

*keep if quantili ==1 |quantili ==10
*gen dummy = 0
*replace dummy=1 if quantili ==10

********************************************************************************
********************************************************************************
**CODE IF WE WANT 25% value

egen quantili=xtile(TE_indiv_original), n(4) by(quarter)
keep if quantili ==1 |quantili ==4
gen dummy = 0
replace dummy=1 if quantili ==4

********************************************************************************
********************************************************************************

decode industrymode_aggregated, gen (industrymode_aggregated_decoded)

gen Agriculture=0
replace Agriculture = 1 if industrymode_aggregated_decoded=="Agriculture"
gen Chemicals=0
replace Chemicals= 1 if industrymode_aggregated_decoded=="Chemicals"
gen Manufacturing=0
replace Manufacturing= 1 if industrymode_aggregated_decoded=="Heavy Industry"
gen Metals=0
replace Metals= 1 if industrymode_aggregated_decoded=="Metals"
gen Special=0
replace Special= 1 if industrymode_aggregated_decoded=="Special"
gen Textile=0
replace Textile= 1 if industrymode_aggregated_decoded=="Textile_industry"
gen Wood=0
replace Wood= 1 if industrymode_aggregated_decoded=="Wood_preparations"

decode via_mode, gen (via_mode_decoded)
gen Land=0
replace Land = 1 if via_mode_decoded=="Land"
gen Air=0
replace Air = 1 if via_mode_decoded=="Air"
gen Sea=0
replace Sea = 1 if via_mode_decoded=="Sea"

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


foreach v of varlist TE_indiv_original lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec{
	local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile
    tempfile `v'
    statsby, by(quarter) saving(``v''): reg `v' dummy `variables'
}

foreach v of varlist TE_indiv_original lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec{
	clear
    use ``v''
    gen Variable = "`v'"
    append using `all_results_quarter'
    save "`all_results_quarter'", replace
}

cd "`tab4_dir'"
rename _b_dummy Coeff_original
drop _b_cons
sort Variable quarter
egen id = group(Variable quarter)
keep if Coeff !=0
keep id quarter Variable Coeff_orig
replace Variable ="TE" if Variable =="TE_indiv_original"
save "original_advcoefficients_cadiff_quarterly", replace


********* QUARTERLY BOOTSTRAP:
clear
use "`tab4_dir'/data_heterogeneity_boots.dta", clear
order quarter
gen quarter_num = 1 if quarter == "qt1"
replace quarter_num =2 if quarter == "qt2"
replace quarter_num = 3 if quarter == "qt3"
replace quarter_num = 4 if quarter == "qt4"

sort bootstrap quarter
drop quantili*

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value
*egen quantili=xtile(TE_indiv_boots), n(10) by(bootstrap quarter_num)
*save "`tab4_dir'/adv_boot_aware_unaware_quarter.dta", replace
*sort bootstrap quarter_n quantili

*keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

********************************************************************************
********************************************************************************
**CODE IF WE WANT 25% value

egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap quarter_num)
save "`tab4_dir'/boot_aware_unaware_quarter.dta", replace
sort bootstrap quarter_n quantili

keep if quantili ==1 |quantili ==4 

********************************************************************************
********************************************************************************


decode industrymode_aggregated, gen(industrymode_aggregated_dec)
gen Agriculture=0
replace Agriculture = 1 if industrymode_aggregated_dec=="Agriculture"
gen Chemicals=0
replace Chemicals= 1 if industrymode_aggregated_dec=="Chemicals"
gen Manufacturing=0
replace Manufacturing= 1 if industrymode_aggregated_dec=="Heavy Industry"
gen Metals=0
replace Metals= 1 if industrymode_aggregated_dec=="Metals"
gen Special=0
replace Special= 1 if industrymode_aggregated_dec=="Special"
gen Textile=0
replace Textile= 1 if industrymode_aggregated_dec=="Textile_industry"
gen Wood=0
replace Wood= 1 if industrymode_aggregated_dec=="Wood_preparations"

decode via_mode, gen (via_mode_decoded)
gen Land=0
replace Land = 1 if via_mode_decoded=="Land"
gen Air=0
replace Air = 1 if via_mode_decoded=="Air"
gen Sea=0
replace Sea = 1 if via_mode_decoded=="Sea"

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
*bys bootstrap quarter_num: replace dummy=1 if quantili ==10
bys bootstrap quarter_num: replace dummy=1 if quantili ==4



forval q = 1/100 {
	forval qtr = 1/4 {
	preserve
        keep if bootstrap==`q' & quarter_num ==`qtr'
        save "`tab4_dir'/bootstrap_data_quarter/adv_boot`q'_quarter_`qtr'.dta", replace
	restore
	}
}


*SAVING COEFFICIENTS AND STANDARD ERRORS FOR EACH BOOTSTRAP (QUARTER)
* Be aware of the quoting here! It seems that there are more quoting in saving data, but these are fine :)

clear 
forval q = 1/100 {
	forval qtr = 1/4 {
		clear
		tempfile all_results_boot_`q'_`qtr'
		save `all_results_boot_`q'_`qtr'', emptyok
		use "`tab4_dir'/bootstrap_data_quarter/adv_boot`q'_quarter_`qtr'.dta", clear
		
		foreach v of varlist TE_indiv_boots lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec{
			local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile
			tempfile `v'
			statsby, saving(``v''): reg `v' dummy `variables'
		}
	
		foreach v of varlist TE_indiv_boots lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec{
			clear
			use ``v''
			gen Variable = "`v'"
			append using `all_results_boot_`q'_`qtr''
			save "`all_results_boot_`q'_`qtr''" , replace
		}

		rename _b_dummy Coeff
		drop _b_cons
	
		save "`tab4_dir'/bootstrap_data_quarter/regression_boots_qtr/adv_regression_`q'_`qtr''" , replace
	}
}

**GENERATING A VARIABLE TO RECOGNIZE EAACH QUARTER-BOOT COUPLE:

clear 
forval q = 1/100 {
	forval qtr = 1/4 {
		use "`tab4_dir'/bootstrap_data_quarter/regression_boots_qtr/adv_regression_`q'_`qtr''", clear
		gen id_boot = `q'
		gen id_qtr = `qtr'
		save "`tab4_dir'/bootstrap_data_quarter/regression_boots_qtr/adv_regression_`q'_`qtr''" , replace
	}
}

clear
cd "`tab4_dir'/bootstrap_data_quarter/regression_boots_qtr"
local files: dir . files "adv_regression_*.dta"
foreach file in `files' {
    append using "`file'"
}

save regressions_all_together_quarterly, replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
egen id_boot_qtr = group(id_boot id_qtr)
sort id id_boot_qtr
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id id_qtr) 
drop if Coeff==0
drop id
sort Variable id_qtr
gen id = _n
rename id_qtr quarter
replace Variable ="TE" if Variable =="TE_indiv_boots"
save "`tab4_dir'/adv_boot_coefficients_quarterly.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 Variable quarter using "`tab4_dir'/original_advcoefficients_cadiff_quarterly.dta"
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

dataout, save(adv_quarter_case) tex replace
save "`tab4_dir'/adv_final_significance_quarterly_advanced.dta", replace





********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************

                    **** MONTHLY ANALYSIS:  *****

********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************



clear
tempfile all_results_month
save `all_results_month', emptyok

use "`tab4_dir'/data_heterogeneity_original.dta", clear

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

*egen quantili=xtile(TE_indiv_original), n(10) by(month)

*keep if quantili ==1 |quantili ==10
*gen dummy = 0
*replace dummy=1 if quantili ==10

********************************************************************************
********************************************************************************
**CODE IF WE WANT 25% value

egen quantili=xtile(TE_indiv_original), n(4) by(month)
keep if quantili ==1 |quantili ==4
gen dummy = 0
replace dummy=1 if quantili ==4

********************************************************************************
********************************************************************************

decode industrymode_aggregated, gen (industrymode_aggregated_decoded)

gen Agriculture=0
replace Agriculture = 1 if industrymode_aggregated_decoded=="Agriculture"
gen Chemicals=0
replace Chemicals= 1 if industrymode_aggregated_decoded=="Chemicals"
gen Manufacturing=0
replace Manufacturing= 1 if industrymode_aggregated_decoded=="Heavy Industry"
gen Metals=0
replace Metals= 1 if industrymode_aggregated_decoded=="Metals"
gen Special=0
replace Special= 1 if industrymode_aggregated_decoded=="Special"
gen Textile=0
replace Textile= 1 if industrymode_aggregated_decoded=="Textile_industry"
gen Wood=0
replace Wood= 1 if industrymode_aggregated_decoded=="Wood_preparations"

decode via_mode, gen (via_mode_decoded)
gen Land=0
replace Land = 1 if via_mode_decoded=="Land"
gen Air=0
replace Air = 1 if via_mode_decoded=="Air"
gen Sea=0
replace Sea = 1 if via_mode_decoded=="Sea"


foreach v of varlist TE_indiv_original lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land{
    tempfile `v'
	local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile
    statsby, by(month) saving(``v''): reg `v' dummy `variables'
}

foreach v of varlist TE_indiv_original lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land{
	clear
    use ``v''
    gen Variable = "`v'"
    append using `all_results_month'
    save "`all_results_month'", replace
}

cd "`tab4_dir'"
rename _b_dummy Coeff_original
drop _b_cons
sort Variable month
egen id = group(Variable month)
keep id Variable month Coeff_original 
save "adv_original_coefficients_cadiff_monthly", replace

********* MONTHLY BOOTSTRAP:
clear
use "`tab4_dir'/data_heterogeneity_boots.dta", clear
order quarter
gen quarter_num = 1 if quarter == "qt1"
replace quarter_num =2 if quarter == "qt2"
replace quarter_num = 3 if quarter == "qt3"
replace quarter_num = 4 if quarter == "qt4"

sort bootstrap month
drop quantili*

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

*egen quantili=xtile(TE_indiv_boots), n(10) by(bootstrap month)
*save "`tab4_dir'/boot_aware_unaware_month.dta", replace
*sort bootstrap quarter_n quantili

*keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

********************************************************************************
********************************************************************************
**CODE IF WE WANT 25% value

egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap month)
save "`tab4_dir'/boot_aware_unaware_month.dta", replace
sort bootstrap quarter_n quantili

keep if quantili ==1 |quantili ==4 

********************************************************************************
********************************************************************************


decode industrymode_aggregated, gen(industrymode_aggregated_dec)
gen Agriculture=0
replace Agriculture = 1 if industrymode_aggregated_dec=="Agriculture"
gen Chemicals=0
replace Chemicals= 1 if industrymode_aggregated_dec=="Chemicals"
gen Manufacturing=0
replace Manufacturing= 1 if industrymode_aggregated_dec=="Heavy Industry"
gen Metals=0
replace Metals= 1 if industrymode_aggregated_dec=="Metals"
gen Special=0
replace Special= 1 if industrymode_aggregated_dec=="Special"
gen Textile=0
replace Textile= 1 if industrymode_aggregated_dec=="Textile_industry"
gen Wood=0
replace Wood= 1 if industrymode_aggregated_dec=="Wood_preparations"

decode via_mode, gen (via_mode_decoded)
gen Land=0
replace Land = 1 if via_mode_decoded=="Land"
gen Air=0
replace Air = 1 if via_mode_decoded=="Air"
gen Sea=0
replace Sea = 1 if via_mode_decoded=="Sea"


gen dummy = 0
*bys bootstrap quarter_num: replace dummy=1 if quantili ==10
bys bootstrap quarter_num: replace dummy=1 if quantili ==4



forval q = 1/100 {
	forval qtr = 1/12 {
	preserve
        keep if bootstrap==`q' & month ==`qtr'
        save "`tab4_dir'/bootstrap_data_month/adv_boot`q'_month_`qtr'.dta", replace
	restore
	}
}


*SAVING COEFFICIENTS AND STANDARD ERRORS FOR EACH BOOTSTRAP (QUARTER)
* Be aware of the quoting here! It seems that there are more quoting in saving data, but these are fine :)

clear 
forval q = 1/100 {
	forval qtr = 1/12 {
		clear
		tempfile all_results_boot_`q'_`qtr'
		save `all_results_boot_`q'_`qtr'', emptyok
		use "`tab4_dir'/bootstrap_data_month/adv_boot`q'_month_`qtr'.dta", clear
		
		foreach v of varlist TE_indiv_boots lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land {
			tempfile `v'
			local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile
			statsby, saving(``v''): reg `v' dummy `variables'
		}
	
		foreach v of varlist TE_indiv_boots lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land {
			clear
			use ``v''
			gen Variable = "`v'"
			append using `all_results_boot_`q'_`qtr''
			save "`all_results_boot_`q'_`qtr''" , replace
		}

		capture rename _b_dummy Coeff
		drop _b_cons
	
		save "`tab4_dir'/bootstrap_data_month/regression_boots_month/adv_regression_`q'_`qtr''" , replace
	}
}

**GENERATING A VARIABLE TO RECOGNIZE EAACH QUARTER-BOOT COUPLE:

clear 
forval q = 1/100 {
	forval qtr = 1/12 {
		use "`tab4_dir'/bootstrap_data_month/regression_boots_month/adv_regression_`q'_`qtr''", clear
		gen id_boot = `q'
		gen id_qtr = `qtr'
		save "`tab4_dir'/bootstrap_data_month/regression_boots_month/adv_regression_`q'_`qtr''" , replace
	}
}

clear
cd "`tab4_dir'/bootstrap_data_month/regression_boots_month"
local files: dir . files "adv_regression_*.dta"
foreach file in `files' {
    append using "`file'"
}

save regressions_all_together_monthly, replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
egen id_boot_qtr = group(id_boot id_qtr)
sort id id_boot_qtr
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id id_qtr) 
drop id
sort Variable id_qtr
gen id = _n
save "`tab4_dir'/adv_boot_coefficients_monthly.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 id using "`tab4_dir'/adv_original_coefficients_cadiff_monthly.dta"
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

dataout, save(monthly_advcase_10pcent) tex replace
save "`tab4_dir'/adv_final_significance_monthly.dta", replace



























******* ADVANCED SECTORS + MONTH:



clear
tempfile all_results
save `all_results', emptyok

use "`tab4_dir'/data_heterogeneity_original.dta", clear
decode industrymode_aggregated, gen(industrymode_aggregated_decoded)

********************************************************************************
********************************************************************************

**CODE IF WE WANT 10% value

*egen quartile2=xtile(TE_indiv_original), n(10) 

*keep if quartile2 ==1 |quartile2 ==10
*gen dummy = 0
*replace dummy=1 if quartile2 ==10

********************************************************************************
********************************************************************************

*** **CODE IF WE WANT 25% value: (could be genralized)
centile (TE_indiv_original), centile (25 75) // search 25 and 75 percentiles
gen centiles = 25 if TE_indiv_original <=`r(c_1)'
replace centiles = 75 if TE_indiv_original >=`r(c_2)'
keep if centiles~=.

gen dummy = 0
replace dummy=1 if centiles==75

********************************************************************************
********************************************************************************


gen Agriculture=0
replace Agriculture = 1 if industrymode_aggregated_decoded=="Agriculture"
gen Chemicals=0
replace Chemicals= 1 if industrymode_aggregated_decoded=="Chemicals"
gen Manufacturing=0
replace Manufacturing= 1 if industrymode_aggregated_decoded=="Heavy Industry"
gen Metals=0
replace Metals= 1 if industrymode_aggregated_decoded=="Metals"
gen Special=0
replace Special= 1 if industrymode_aggregated_decoded=="Special"
gen Textile=0
replace Textile= 1 if industrymode_aggregated_decoded=="Textile_industry"
gen Wood=0
replace Wood= 1 if industrymode_aggregated_decoded=="Wood_preparations"

decode via_mode, gen (via_mode_decoded)
gen Land=0
replace Land = 1 if via_mode_decoded=="Land"
gen Air=0
replace Air = 1 if via_mode_decoded=="Air"
gen Sea=0
replace Sea = 1 if via_mode_decoded=="Sea"

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



foreach v of varlist TE_indiv_original lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land{
	local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
	local not `v'
	local variables: list variables- not
	*di `"`variables'"'
	tempfile `v'
	statsby, saving(``v''): reg `v' dummy `variables' 
	
}

foreach v of varlist TE_indiv_original lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land {
	clear
    use ``v''
    gen Variable = "`v'"
    append using `all_results'
    save "`all_results'", replace
}


cd "`tab4_dir'"
rename _b_dummy Coeff
drop _b_cons
sort Variable
gen id = _n
rename Coeff Coeff_orig
keep id Variable Coeff_orig
save "original_coefficients_advcadiff", replace



***** LET'S BOOTSTRP THE ERRORS

*Load bootstrap data
use "`tab4_dir'/data_heterogeneity_boots.dta", clear
capture drop quantili
sort bootstrap

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

*egen quantili=xtile(TE_indiv_boots), n(10) by(bootstrap)
*save "`tab4_dir'/data_heterogeneity_boots.dta", replace
*sort bootstrap quantili
*keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

**CODE IF WE WANT 25% value
capture drop quantili
egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap)
save "`tab4_dir'/data_heterogeneity_boots.dta", replace
sort bootstrap quantili
keep if quantili ==1 |quantili ==4

********************************************************************************
********************************************************************************

decode industrymode_aggregated, gen(industrymode_aggregated_dec)
gen Agriculture=0
replace Agriculture = 1 if industrymode_aggregated_dec=="Agriculture"
gen Chemicals=0
replace Chemicals= 1 if industrymode_aggregated_dec=="Chemicals"
gen Manufacturing=0
replace Manufacturing= 1 if industrymode_aggregated_dec=="Heavy Industry"
gen Metals=0
replace Metals= 1 if industrymode_aggregated_dec=="Metals"
gen Special=0
replace Special= 1 if industrymode_aggregated_dec=="Special"
gen Textile=0
replace Textile= 1 if industrymode_aggregated_dec=="Textile_industry"
gen Wood=0
replace Wood= 1 if industrymode_aggregated_dec=="Wood_preparations"

decode via_mode, gen (via_mode_decoded)
gen Land=0
replace Land = 1 if via_mode_decoded=="Land"
gen Air=0
replace Air = 1 if via_mode_decoded=="Air"
gen Sea=0
replace Sea = 1 if via_mode_decoded=="Sea"

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
        save "`tab4_dir'/bootstrap_data/adv_boot`q'.dta", replace
restore
     
}


*SAVING COEFFICIENTS AND STANDARD ERRORS FOR EACH BOOTSTRAP


forval q = 1/100 {
	clear
	tempfile all_results_boot_`q'
	save `all_results_boot_`q'', emptyok
	use "`tab4_dir'/bootstrap_data/adv_boot`q'.dta", clear
	foreach v of varlist TE_indiv_boots lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land {
		local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
	local not `v'
	local variables: list variables- not
	tempfile `v'
	statsby, saving(``v''): reg `v' dummy `variables' 

	}

	foreach v of varlist TE_indiv_boots lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land {
		clear
		use ``v''
		gen Variable = "`v'"
		append using `all_results_boot_`q''
		save "`all_results_boot_`q''" , replace
	}

	cd "`tab4_dir'"
	capture rename _b_dummy Coeff
	drop _b_cons
	
	save "`tab4_dir'/bootstrap_data/regression_boots/adv_regression_`q''" , replace
}


clear
cd "`tab4_dir'/bootstrap_data/regression_boots/"
local fnames: dir "." files "adv_regression_*.dta"
drop _all

foreach f of local fnames {
    append using `"`f'"'
}   
sort Variable
keep Coeff Variable
save adv_regressions_all_together, replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id) 
drop id
sort Variable
gen id = _n
save "`tab4_dir'/adv_boot_coefficients.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 id using "`tab4_dir'/original_coefficients_advcadiff.dta"
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

dataout, save(annual_case_adv_cadiff) tex replace






































********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************

                    **** QUARTERLY ANALYSIS:  *****

********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************



clear
tempfile all_results_quarter
save `all_results_quarter', emptyok

use "`tab4_dir'/data_heterogeneity_original.dta", clear

drop quarter
gen quarter = 1 if month==1|month==2|month==3
replace quarter = 2 if month==4|month==5|month==6
replace quarter = 3 if month==7|month==8|month==9
replace quarter = 4 if month==10|month==11|month==12

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value

egen quantili=xtile(TE_indiv_original), n(10) by(quarter)

keep if quantili ==1 |quantili ==10
gen dummy = 0
replace dummy=1 if quantili ==10

********************************************************************************
********************************************************************************
**CODE IF WE WANT 25% value

*egen quantili=xtile(TE_indiv_original), n(4) by(quarter)
*keep if quantili ==1 |quantili ==4
*gen dummy = 0
*replace dummy=1 if quantili ==4

********************************************************************************
********************************************************************************

decode industrymode_aggregated, gen (industrymode_aggregated_decoded)

gen Agriculture=0
replace Agriculture = 1 if industrymode_aggregated_decoded=="Agriculture"
gen Chemicals=0
replace Chemicals= 1 if industrymode_aggregated_decoded=="Chemicals"
gen Manufacturing=0
replace Manufacturing= 1 if industrymode_aggregated_decoded=="Heavy Industry"
gen Metals=0
replace Metals= 1 if industrymode_aggregated_decoded=="Metals"
gen Special=0
replace Special= 1 if industrymode_aggregated_decoded=="Special"
gen Textile=0
replace Textile= 1 if industrymode_aggregated_decoded=="Textile_industry"
gen Wood=0
replace Wood= 1 if industrymode_aggregated_decoded=="Wood_preparations"

decode via_mode, gen (via_mode_decoded)
gen Land=0
replace Land = 1 if via_mode_decoded=="Land"
gen Air=0
replace Air = 1 if via_mode_decoded=="Air"
gen Sea=0
replace Sea = 1 if via_mode_decoded=="Sea"

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


foreach v of varlist TE_indiv_original lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land {
	local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
    tempfile `v'
    statsby, by(quarter) saving(``v''): reg `v' dummy `variables'
}

foreach v of varlist TE_indiv_original lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land {
	clear
    use ``v''
    gen Variable = "`v'"
    append using `all_results_quarter'
    save "`all_results_quarter'", replace
}

cd "`tab4_dir'"
rename _b_dummy Coeff_original
drop _b_cons
sort Variable quarter
egen id = group(Variable quarter)
keep if Coeff !=0
keep id quarter Variable Coeff_orig
replace Variable ="TE" if Variable =="TE_indiv_original"
save "original_advcoefficients_cadiff_quarterly", replace


********* QUARTERLY BOOTSTRAP:
clear
use "`tab4_dir'/data_heterogeneity_boots.dta", clear
order quarter
gen quarter_num = 1 if quarter == "qt1"
replace quarter_num =2 if quarter == "qt2"
replace quarter_num = 3 if quarter == "qt3"
replace quarter_num = 4 if quarter == "qt4"

sort bootstrap quarter
drop quantili*

********************************************************************************
********************************************************************************
**CODE IF WE WANT 10% value
egen quantili=xtile(TE_indiv_boots), n(10) by(bootstrap quarter_num)
save "`tab4_dir'/adv_boot_aware_unaware_quarter.dta", replace
sort bootstrap quarter_n quantili

keep if quantili ==1 |quantili ==10 

********************************************************************************
********************************************************************************

********************************************************************************
********************************************************************************
**CODE IF WE WANT 25% value

*egen quantili=xtile(TE_indiv_boots), n(4) by(bootstrap quarter_num)
*save "`tab4_dir'/boot_aware_unaware_quarter.dta", replace
*sort bootstrap quarter_n quantili

*keep if quantili ==1 |quantili ==4 

********************************************************************************
********************************************************************************


decode industrymode_aggregated, gen(industrymode_aggregated_dec)
gen Agriculture=0
replace Agriculture = 1 if industrymode_aggregated_dec=="Agriculture"
gen Chemicals=0
replace Chemicals= 1 if industrymode_aggregated_dec=="Chemicals"
gen Manufacturing=0
replace Manufacturing= 1 if industrymode_aggregated_dec=="Heavy Industry"
gen Metals=0
replace Metals= 1 if industrymode_aggregated_dec=="Metals"
gen Special=0
replace Special= 1 if industrymode_aggregated_dec=="Special"
gen Textile=0
replace Textile= 1 if industrymode_aggregated_dec=="Textile_industry"
gen Wood=0
replace Wood= 1 if industrymode_aggregated_dec=="Wood_preparations"

decode via_mode, gen (via_mode_decoded)
gen Land=0
replace Land = 1 if via_mode_decoded=="Land"
gen Air=0
replace Air = 1 if via_mode_decoded=="Air"
gen Sea=0
replace Sea = 1 if via_mode_decoded=="Sea"

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
bys bootstrap quarter_num: replace dummy=1 if quantili ==10
*bys bootstrap quarter_num: replace dummy=1 if quantili ==4



forval q = 1/100 {
	forval qtr = 1/4 {
	preserve
        keep if bootstrap==`q' & quarter_num ==`qtr'
        save "`tab4_dir'/bootstrap_data_quarter/adv_boot`q'_quarter_`qtr'.dta", replace
	restore
	}
}


*SAVING COEFFICIENTS AND STANDARD ERRORS FOR EACH BOOTSTRAP (QUARTER)
* Be aware of the quoting here! It seems that there are more quoting in saving data, but these are fine :)

clear 
forval q = 1/100 {
	forval qtr = 1/4 {
		clear
		tempfile all_results_boot_`q'_`qtr'
		save `all_results_boot_`q'_`qtr'', emptyok
		use "`tab4_dir'/bootstrap_data_quarter/adv_boot`q'_quarter_`qtr'.dta", clear
		
		foreach v of varlist TE_indiv_boots lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land {
			local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
			tempfile `v'
			statsby, saving(``v''): reg `v' dummy `variables'
		}
	
		foreach v of varlist TE_indiv_boots lnX lnX_import index_stringency_w index_stringency_w_import ND NO NP Air Sea Land {
			clear
			use ``v''
			gen Variable = "`v'"
			append using `all_results_boot_`q'_`qtr''
			save "`all_results_boot_`q'_`qtr''" , replace
		}

		rename _b_dummy Coeff
		drop _b_cons
	
		save "`tab4_dir'/bootstrap_data_quarter/regression_boots_qtr/adv_regression_`q'_`qtr''" , replace
	}
}

**GENERATING A VARIABLE TO RECOGNIZE EAACH QUARTER-BOOT COUPLE:

clear 
forval q = 1/100 {
	forval qtr = 1/4 {
		use "`tab4_dir'/bootstrap_data_quarter/regression_boots_qtr/adv_regression_`q'_`qtr''", clear
		gen id_boot = `q'
		gen id_qtr = `qtr'
		save "`tab4_dir'/bootstrap_data_quarter/regression_boots_qtr/adv_regression_`q'_`qtr''" , replace
	}
}

clear
cd "`tab4_dir'/bootstrap_data_quarter/regression_boots_qtr"
local files: dir . files "adv_regression_*.dta"
foreach file in `files' {
    append using "`file'"
}

save regressions_all_together_quarterly, replace

**** obtaining mean and sd from the final data:
egen id = group(Variable)
egen id_boot_qtr = group(id_boot id_qtr)
sort id id_boot_qtr
gen coeff_sd = Coeff
collapse (mean) Coeff (sd) coeff_sd (first) Variable, by(id id_qtr) 
drop if Coeff==0
drop id
sort Variable id_qtr
gen id = _n
rename id_qtr quarter
replace Variable ="TE" if Variable =="TE_indiv_boots"
save "`tab4_dir'/adv_boot_coefficients_quarterly.dta", replace
***merging with ORIGINAL coefficients:
merge 1:1 Variable quarter using "`tab4_dir'/original_advcoefficients_cadiff_quarterly.dta"
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

dataout, save(adv_quarter_case) tex replace
save "`tab4_dir'/adv_final_significance_quarterly_advanced.dta", replace





























