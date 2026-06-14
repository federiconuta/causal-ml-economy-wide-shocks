
********************************* TABELLA 5 RIPRODUZIONE COEFFICIENTI ************************************

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

import delimited "data_heterogeneity_original_y_sum.csv", clear 

save "data_heterogeneity_original_y_sum.dta", replace




******************* COLONNA 1


clear

********************************************** FROM HERE WE JUST REPEAT THE PROCEURE TO OBTAIN ORIGINAL COEFFICIENTS **********************************************
tempfile all_results
save `all_results', emptyok

use "data_heterogeneity_original_y_sum.dta", clear

ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = "." if `var' == "NA"
} 


gen industrymode_aggregated = industry_mode
replace industrymode_aggregated = "Metals" if industry_mode=="Mineral (05)"|industry_mode=="Cement (13)"

********************************************************************************
********************************************************************************

**CODE IF WE WANT 10% value

*egen quartile2=xtile(TE_indiv_original), n(10) 

*keep if quartile2 ==1 |quartile2 ==10
*gen dummy = 0
*replace dummy=1 if quartile2 ==10

********************************************************************************
********************************************************************************

rename te_indiv TE_indiv_original_Y_SUM

*** **CODE IF WE WANT 25% value: (could be genralized)
centile (TE_indiv_original_Y_SUM), centile (25 75) // search 25 and 75 percentiles
gen centiles = 25 if TE_indiv_original_Y_SUM <=`r(c_1)'
replace centiles = 75 if TE_indiv_original_Y_SUM >=`r(c_2)'
keep if centiles~=.

gen dummy = 1
replace dummy=0 if centiles==75




********************************************************************************
********************************************************************************

*** Generate dummies for industry mode:


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


*** Check months and sectors;
foreach v of varlist TE_indiv_original_Y_SUM lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec Agriculture Chemicals Manufacturing Metals Wood Special Textile{
	*local variables 
	*local not `v'
	*local variables: list variables- not
	**di `"`variables'"'
	tempfile `v'
	statsby, saving(``v''): reg `v' dummy 
	
}

foreach v of varlist TE_indiv_original_Y_SUM lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec Agriculture Chemicals Manufacturing Metals Wood Special Textile{
	clear
    use ``v''
    gen Variable = "`v'"
    append using `all_results'
    save "`all_results'", replace
}


cd `"`tab5_dir'"'
rename _b_dummy Coeff
drop _b_cons
sort Variable
gen id = _n
rename Coeff Coeff_orig
**Only the dummy coefficient (Coeff_orig) is needed, not the other regression coefficients.
keep id Variable Coeff_orig
save "original_coefficients_advcadiff_nocontrol_y_sum", replace
















******************* COLONNA 2


clear

********************************************** FROM HERE WE JUST REPEAT THE PROCEURE TO OBTAIN ORIGINAL COEFFICIENTS **********************************************
tempfile all_results
save `all_results', emptyok

use "data_heterogeneity_original_y_sum.dta", clear

ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = "." if `var' == "NA"
} 


gen industrymode_aggregated = industry
replace industrymode_aggregated = "Metals" if industry_mode=="Mineral (05)"|industry_mode=="Cement (13)"
********************************************************************************
********************************************************************************

**CODE IF WE WANT 10% value

*egen quartile2=xtile(TE_indiv_original), n(10) 

*keep if quartile2 ==1 |quartile2 ==10
*gen dummy = 0
*replace dummy=1 if quartile2 ==10

********************************************************************************
********************************************************************************

rename te_indiv TE_indiv_original_Y_SUM

*** **CODE IF WE WANT 25% value: (could be genralized)
centile (TE_indiv_original_Y_SUM), centile (25 75) // search 25 and 75 percentiles
gen centiles = 25 if TE_indiv_original_Y_SUM <=`r(c_1)'
replace centiles = 75 if TE_indiv_original_Y_SUM >=`r(c_2)'
keep if centiles~=.

gen dummy = 1
replace dummy=0 if centiles==75

********************************************************************************
********************************************************************************

*** Generate dummies for industry mode:


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


*** Check months and sectors;
foreach v of varlist TE_indiv_original lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec{
	local variables Agriculture Chemicals Manufacturing Metals Wood Special Textile  
	local not `v'
	local variables: list variables- not
	*di `"`variables'"'
	tempfile `v'
	statsby, saving(``v''): reg `v' dummy `variables' 
	
}

foreach v of varlist TE_indiv_original lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec{
	clear
    use ``v''
    gen Variable = "`v'"
    append using `all_results'
    save "`all_results'", replace
}


cd `"`tab5_dir'"'
rename _b_dummy Coeff
drop _b_cons
sort Variable
gen id = _n
rename Coeff Coeff_orig
**Only the dummy coefficient (Coeff_orig) is needed, not the other regression coefficients.
keep id Variable Coeff_orig
save "original_coefficients_advcadiff_control_sec_y_sum", replace







******************* COLONNA 3


clear

********************************************** FROM HERE WE JUST REPEAT THE PROCEURE TO OBTAIN ORIGINAL COEFFICIENTS **********************************************
tempfile all_results
save `all_results', emptyok

use "data_heterogeneity_original_y_sum.dta", clear

ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = "." if `var' == "NA"
} 


gen industrymode_aggregated = industry
replace industrymode_aggregated = "Metals" if industry_mode=="Mineral (05)"|industry_mode=="Cement (13)"
********************************************************************************
********************************************************************************

**CODE IF WE WANT 10% value

*egen quartile2=xtile(TE_indiv_original), n(10) 

*keep if quartile2 ==1 |quartile2 ==10
*gen dummy = 0
*replace dummy=1 if quartile2 ==10

********************************************************************************
********************************************************************************

rename te_indiv TE_indiv_original_Y_SUM

*** **CODE IF WE WANT 25% value: (could be genralized)
centile (TE_indiv_original_Y_SUM), centile (25 75) // search 25 and 75 percentiles
gen centiles = 25 if TE_indiv_original_Y_SUM <=`r(c_1)'
replace centiles = 75 if TE_indiv_original_Y_SUM >=`r(c_2)'
keep if centiles~=.

gen dummy = 1
replace dummy=0 if centiles==75

********************************************************************************
********************************************************************************

*** Generate dummies for industry mode:


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

*** Check months and sectors;
foreach v of varlist TE_indiv_original lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land {
	local variables Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec Agriculture Chemicals Manufacturing Metals Wood Special Textile
	local not `v'
	local variables: list variables- not
	*di `"`variables'"'
	tempfile `v'
	statsby, saving(``v''): reg `v' dummy `variables' 
	
}

foreach v of varlist TE_indiv_original lnx lnx_import index_stringency_w index_stringency_w_import nd np Air Sea Land {
	clear
    use ``v''
    gen Variable = "`v'"
    append using `all_results'
    save "`all_results'", replace
}


cd `"`tab5_dir'"'

rename _b_dummy Coeff
drop _b_cons
sort Variable
gen id = _n
rename Coeff Coeff_orig
**Only the dummy coefficient (Coeff_orig) is needed, not the other regression coefficients.
keep id Variable Coeff_orig
save "original_coefficients_advcadiff_control_months_sec_y_sum", replace
