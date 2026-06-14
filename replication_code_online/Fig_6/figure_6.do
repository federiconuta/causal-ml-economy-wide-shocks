version 19
clear all
set more off
capture set scheme modern
if _rc != 0 {
    set scheme s2color
}

local tab4_dir : environment OBES_TAB4_DIR
if "`tab4_dir'" == "" {
    local tab4_dir "../Tab_4"
}

local out_dir : environment OBES_FIG6_OUT_DIR
if "`out_dir'" == "" {
    local out_dir "."
}
capture mkdir "`out_dir'"

capture confirm file "`tab4_dir'/data_heterogeneity_original.dta"
if _rc == 0 {
    use "`tab4_dir'/data_heterogeneity_original.dta", clear
}
else {
    capture confirm file "`tab4_dir'/data_heterogeneity_original.csv"
    if _rc != 0 {
        display as error "Missing data_heterogeneity_original.dta or data_heterogeneity_original.csv in `tab4_dir'"
        exit 601
    }
    import delimited "`tab4_dir'/data_heterogeneity_original.csv", clear varnames(1)
}

capture confirm variable pred_sam
if _rc != 0 {
    capture rename pred_SAM pred_sam
}
capture confirm variable pred_sum
if _rc != 0 {
    capture rename pred_SUM pred_sum
}
capture confirm variable export_future
if _rc != 0 {
    capture rename export_future_x export_future
}

gen effect_sam_sum = pred_sam - pred_sum
gen effect_y_sum = export_future - pred_sum

label variable effect_sam_sum "Average(SAM-SUM)"
label variable effect_y_sum "Average(Y-SUM)"

local qstarts 1 4 7 10
local qends 3 6 9 12

forvalues q = 1/2 {
    local qstart : word `q' of `qstarts'
    local qend : word `q' of `qends'
    if `q' == 1 {
        local qlabel "Jan-Mar 2020"
    }
    if `q' == 2 {
        local qlabel "Apr-Jun 2020"
    }
   

    preserve
        keep if month >= `qstart' & month <= `qend'
        keep if !missing(effect_y_sum, effect_sam_sum)
        xtile percentile = effect_y_sum, nq(100)

        collapse (mean) effect_sam_sum effect_y_sum, by(percentile)
        label variable effect_sam_sum "Average(SAM-SUM)"
        label variable effect_y_sum "Average(Y-SUM)"
        export delimited using "`out_dir'/ppercentilesq`q'_values.csv", replace

        twoway ///
            (connected effect_sam_sum percentile, sort msymbol(diamond) color(red)) ///
            (connected effect_y_sum percentile, sort color(blue)), ///
            ytitle("Avg. Estimated treatment effect in `qlabel'") ///
            xtitle("Percentiles of the distribution of the SUM estimation error in `qlabel'") ///
            ylabel(-1 (.25) 1.015) ///
            xlabel(0 (20) 105) ///
            legend(ring(0) position(4))

        graph export "`out_dir'/ppercentilesq`q'.png", replace
    restore
}
