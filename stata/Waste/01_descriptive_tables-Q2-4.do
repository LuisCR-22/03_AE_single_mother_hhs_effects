/*******************************************************************************
DESCRIPTIVE STATISTICS TABLES - SINGLE MOTHER HOUSEHOLD HEADS
Author: Luis Castellanos
Project: Anchors in the Storm - Descriptive Statistics

PURPOSE: 
Create descriptive statistics tables for single mothers who are household heads,
comparing means and standard deviations by eligibility status.

THREE TABLES:
1. Full sample (no bandwidth restriction) - mother's characteristics
2. Optimal bandwidth sample (using pea) - mother's characteristics  
3. Full sample - household average characteristics

SAMPLE: Single mother household heads (jefe==1 & hh_madre_soltera_rob_2020==1)
TIME PERIOD: 2020, quarters 2-4
*******************************************************************************/

clear all
set more off
cls

/*==============================================================================
PATH SETUP
==============================================================================*/

global project_dir "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Transfers causal analysis BRA"
global output_dir "$project_dir\Outputs\01_New_Outputs_2025\Excel\RD_results\24_Descr_table"

capture mkdir "$output_dir"

noi di ""
noi di "==================================================================="
noi di "=== DESCRIPTIVE STATISTICS TABLES: SINGLE MOTHER HOUSEHOLD HEADS ==="
noi di "==================================================================="
noi di ""

/*==============================================================================
DATA LOADING AND SAMPLE PREPARATION
==============================================================================*/

noi di "=== LOADING AND PREPARING DATA ==="

use "$project_dir\Data\03_SEDLAC_panel_2019-2020_with_single_mother.dta", clear

* Time restrictions
keep if ano == 2020                    
keep if inlist(trimestre, 2, 3, 4)     
keep if cohh == 1                      

noi di "Sample after time filters: " _N

* Define eligibility threshold and running variable
local threshold_2020 = 522.5
gen running_var = ipcf - `threshold_2020'
label var running_var "Per capita income minus eligibility threshold"

gen eligible = (running_var < 0) if !missing(running_var)
label var eligible "Eligible for Auxilio Emergencial (below threshold)"

cap drop region_1 region_2 region_3 region_4 region_5

* Create region dummies if they don't exist
capture confirm variable region_1
if _rc != 0 {
    noi di "Creating region dummy variables..."
    capture drop region_num
    encode(region_est1), gen(region_num)
    
    forvalues r = 1/5 {
        gen region_`r' = (region_num == `r') if !missing(region_num)
    }
}

* Label region variables
capture label var region_1 "Region: North"
capture label var region_2 "Region: Northeast"
capture label var region_3 "Region: Southeast"
capture label var region_4 "Region: South"
capture label var region_5 "Region: Central-West"

* Create household head education variable if not exists
capture confirm variable nivedu_hh
if _rc != 0 {
    bysort idp_h: egen nivedu_hh = max(nivedu * (jefe==1))
}
label var nivedu_hh "Household head education level"

* Ensure region_num exists for clustering
capture confirm variable region_num
if _rc != 0 {
    encode(region_est1), gen(region_num)
}

/*==============================================================================
DEFINE VARIABLE LABELS (with capture to avoid errors if var doesn't exist)
==============================================================================*/

* Demographics
capture label var hombre "Male"
capture label var edad "Age"
capture label var asiste "Educational attendance"
capture label var nro_hijos "Number of children"
capture label var miembros "Household members"

* Education levels
capture label var prii "Incomplete primary"
capture label var pric "Complete primary"
capture label var seci "Incomplete secondary"
capture label var secc "Complete secondary"
capture label var supi "Incomplete tertiary"
capture label var supc "Complete tertiary"
capture label var max_edu "Maximum education level"

* Income
capture label var ipcf "Per capita household income"

* Labor market
capture label var pea "Labor force participation"
capture label var ocupado "Employed"
capture label var n_ocu_h "Number employed in household"

* Geographic
capture label var urbano "Urban"

/*==============================================================================
BUILD COVARIATE LIST - ONLY INCLUDE EXISTING VARIABLES WITH NON-MISSING VALUES
==============================================================================*/

* Define sample condition
global sample_cond "hh_madre_soltera_rob_2020 == 1 & jefe == 1"

* List of potential covariates
local potential_covs hombre edad asiste nro_hijos miembros urbano ///
    nivedu_hh prii pric seci secc supi supc max_edu ///
    ipcf pea ocupado n_ocu_h ///
    region_1 region_2 region_3 region_4 region_5

* Build list of valid covariates (exist and have non-missing values)
global covariate_list ""
global covariate_list_ftest ""

noi di ""
noi di "Checking available covariates..."

foreach var of local potential_covs {
    capture confirm variable `var'
    if _rc == 0 {
        * Variable exists, check if it has non-missing values in our sample
        quietly count if $sample_cond & !missing(`var')
        if r(N) > 0 {
            global covariate_list "$covariate_list `var'"
            
            * For F-test, also check variance > 0
            quietly sum `var' if $sample_cond
            if r(sd) > 0 {
                global covariate_list_ftest "$covariate_list_ftest `var'"
            }
            noi di "  [OK] `var' - " r(N) " non-missing obs"
        }
        else {
            noi di "  [SKIP] `var' - all missing in sample"
        }
    }
    else {
        noi di "  [SKIP] `var' - variable not found"
    }
}

noi di ""
noi di "Covariates for descriptives: $covariate_list"
noi di "Covariates for F-test: $covariate_list_ftest"

count if $sample_cond
noi di ""
noi di "Total sample (single mother HH heads): " %12.0fc r(N)

/*==============================================================================
PROGRAM: CREATE DESCRIPTIVE TABLE (NO BANDWIDTH RESTRICTION)
==============================================================================*/

capture program drop create_descriptive_table
program define create_descriptive_table
    syntax, OUTfile(string) [USEbw(real 0) SAMPLEtype(string)]
    
    * Default sample type
    if "`sampletype'" == "" {
        local sampletype "individual"
    }
    
    noi di ""
    noi di "==================================================================="
    if `usebw' > 0 {
        noi di "=== DESCRIPTIVE TABLE: OPTIMAL BANDWIDTH = `usebw' BRL ==="
    }
    else {
        noi di "=== DESCRIPTIVE TABLE: FULL SAMPLE ==="
    }
    noi di "=== Sample type: `sampletype' ==="
    noi di "==================================================================="
    
    * Create bandwidth indicator if needed
    if `usebw' > 0 {
        capture drop temp_in_bw
        gen temp_in_bw = (abs(running_var) <= `usebw') if !missing(running_var)
        local bw_cond "& temp_in_bw == 1"
    }
    else {
        local bw_cond ""
    }
    
    * Count observations
    count if $sample_cond `bw_cond'
    local n_total = r(N)
    count if $sample_cond `bw_cond' & eligible == 1
    local n_eligible = r(N)
    count if $sample_cond `bw_cond' & eligible == 0
    local n_noneligible = r(N)
    
    noi di "Total observations: " %10.0fc `n_total'
    noi di "Eligible (below threshold): " %10.0fc `n_eligible'
    noi di "Non-eligible (above threshold): " %10.0fc `n_noneligible'
    
    /*==========================================================================
    CALCULATE STATISTICS
    ==========================================================================*/
    
    * Store results in temporary file
    tempname memhold
    tempfile temp_results
    postfile `memhold' str50 varlabel ///
        total_mean total_sd total_n ///
        eligible_mean eligible_sd eligible_n ///
        noneligible_mean noneligible_sd noneligible_n ///
        difference se_diff t_stat str5 stars ///
        using "`temp_results'", replace
    
    foreach var of global covariate_list {
        
        * Get variable label
        local var_label : variable label `var'
        if "`var_label'" == "" {
            local var_label "`var'"
        }
        
        * Total sample stats
        quietly sum `var' if $sample_cond `bw_cond'
        local t_mean = r(mean)
        local t_sd = r(sd)
        local t_n = r(N)
        
        * Handle case where no observations
        if `t_n' == 0 {
            local t_mean = .
            local t_sd = .
        }
        
        * Eligible stats (below threshold)
        quietly sum `var' if $sample_cond `bw_cond' & eligible == 1
        local e_mean = r(mean)
        local e_sd = r(sd)
        local e_n = r(N)
        
        if `e_n' == 0 {
            local e_mean = .
            local e_sd = .
        }
        
        * Non-eligible stats (above threshold)
        quietly sum `var' if $sample_cond `bw_cond' & eligible == 0
        local ne_mean = r(mean)
        local ne_sd = r(sd)
        local ne_n = r(N)
        
        if `ne_n' == 0 {
            local ne_mean = .
            local ne_sd = .
        }
        
        * Difference and SE (Eligible - Non-eligible)
        if `e_n' > 0 & `ne_n' > 0 & !missing(`e_mean') & !missing(`ne_mean') {
            local diff = `e_mean' - `ne_mean'
            if `e_sd' != . & `ne_sd' != . & `e_sd' > 0 & `ne_sd' > 0 {
                local se = sqrt((`e_sd'^2/`e_n') + (`ne_sd'^2/`ne_n'))
                if `se' > 0 {
                    local t = `diff' / `se'
                }
                else {
                    local t = .
                    local se = .
                }
            }
            else {
                local se = .
                local t = .
            }
        }
        else {
            local diff = .
            local se = .
            local t = .
        }
        
        * Significance stars
        local sig_stars ""
        if !missing(`t') {
            if abs(`t') >= 2.576 {
                local sig_stars "***"
            }
            else if abs(`t') >= 1.96 {
                local sig_stars "**"
            }
            else if abs(`t') >= 1.645 {
                local sig_stars "*"
            }
        }
        
        post `memhold' ("`var_label'") ///
            (`t_mean') (`t_sd') (`t_n') ///
            (`e_mean') (`e_sd') (`e_n') ///
            (`ne_mean') (`ne_sd') (`ne_n') ///
            (`diff') (`se') (`t') ("`sig_stars'")
    }
    
    postclose `memhold'
    
    /*==========================================================================
    JOINT F-TEST FOR COVARIATE BALANCE
    ==========================================================================*/
    
    noi di ""
    noi di "Computing joint significance test..."
    
    * Check if we have valid covariates for F-test
    local ftest_covs "$covariate_list_ftest"
    local n_ftest_covs : word count `ftest_covs'
    
    local f_stat = .
    local f_df = .
    local f_p = .
    local chi2 = .
    
    if `n_ftest_covs' > 0 {
        capture {
            quietly regress eligible `ftest_covs' if $sample_cond `bw_cond', ///
                vce(cluster region_num)
            quietly test `ftest_covs'
            local f_stat = r(F)
            local f_df = r(df)
            local f_p = r(p)
            local chi2 = `f_stat' * `f_df'
        }
        if _rc != 0 {
            noi di "Warning: F-test failed. Trying without clustering..."
            capture {
                quietly regress eligible `ftest_covs' if $sample_cond `bw_cond', robust
                quietly test `ftest_covs'
                local f_stat = r(F)
                local f_df = r(df)
                local f_p = r(p)
                local chi2 = `f_stat' * `f_df'
            }
            if _rc != 0 {
                noi di "Warning: F-test could not be computed."
            }
        }
    }
    else {
        noi di "Warning: No valid covariates for F-test."
    }
    
    if !missing(`f_stat') {
        noi di "Joint F-test: F = " %8.3f `f_stat' ", df = " %3.0f `f_df' ", p = " %6.4f `f_p'
    }
    
    /*==========================================================================
    EXPORT TO EXCEL
    ==========================================================================*/
    
    noi di ""
    noi di "Exporting to Excel: `outfile'"
    
    preserve
    use "`temp_results'", clear
    
    * Build formatted difference column
    gen diff_formatted = string(difference, "%9.3f") + stars + " (" + string(se_diff, "%9.3f") + ")"
    replace diff_formatted = "." if missing(difference)
    
    * Export using putexcel
    putexcel set "`outfile'", replace
    
    * Title and headers
    putexcel A1 = "Table: Descriptive Statistics - Single Mother Household Heads", bold
    if `usebw' > 0 {
        local bw_text "Sample: Within optimal bandwidth (`usebw' BRL)"
        putexcel A2 = "`bw_text'"
    }
    else {
        putexcel A2 = "Sample: Full sample (no bandwidth restriction)"
    }
    putexcel A3 = "Sample type: `sampletype'"
    
    * Column headers
    putexcel A5 = "Variable", bold
    putexcel B5 = "Total Mean", bold
    putexcel C5 = "Total SD", bold
    putexcel D5 = "Total N", bold
    putexcel E5 = "Eligible Mean", bold
    putexcel F5 = "Eligible SD", bold
    putexcel G5 = "Eligible N", bold
    putexcel H5 = "Non-elig Mean", bold
    putexcel I5 = "Non-elig SD", bold
    putexcel J5 = "Non-elig N", bold
    putexcel K5 = "Difference (E-NE)", bold
    
    * Export data rows
    local nobs = _N
    forvalues i = 1/`nobs' {
        local row = `i' + 5
        
        local lab = varlabel[`i']
        local t_m = total_mean[`i']
        local t_s = total_sd[`i']
        local t_n = total_n[`i']
        local e_m = eligible_mean[`i']
        local e_s = eligible_sd[`i']
        local e_n = eligible_n[`i']
        local ne_m = noneligible_mean[`i']
        local ne_s = noneligible_sd[`i']
        local ne_n = noneligible_n[`i']
        local diff_text = diff_formatted[`i']
        
        putexcel A`row' = "`lab'"
        putexcel B`row' = `t_m', nformat(number_d3)
        putexcel C`row' = `t_s', nformat(number_d3)
        putexcel D`row' = `t_n', nformat(number)
        putexcel E`row' = `e_m', nformat(number_d3)
        putexcel F`row' = `e_s', nformat(number_d3)
        putexcel G`row' = `e_n', nformat(number)
        putexcel H`row' = `ne_m', nformat(number_d3)
        putexcel I`row' = `ne_s', nformat(number_d3)
        putexcel J`row' = `ne_n', nformat(number)
        putexcel K`row' = "`diff_text'"
    }
    
    * Add summary info
    local row = `nobs' + 7
    putexcel A`row' = "Joint Significance Test (Covariate Balance)", bold
    
    local row = `row' + 1
    putexcel A`row' = "F-statistic:"
    if !missing(`f_stat') {
        putexcel B`row' = `f_stat', nformat(number_d3)
    }
    else {
        putexcel B`row' = "N/A"
    }
    
    local row = `row' + 1
    putexcel A`row' = "Degrees of freedom:"
    if !missing(`f_df') {
        putexcel B`row' = `f_df', nformat(number)
    }
    else {
        putexcel B`row' = "N/A"
    }
    
    local row = `row' + 1
    putexcel A`row' = "P-value:"
    if !missing(`f_p') {
        putexcel B`row' = `f_p', nformat(number_d4)
    }
    else {
        putexcel B`row' = "N/A"
    }
    
    local row = `row' + 1
    putexcel A`row' = "Chi-squared:"
    if !missing(`chi2') {
        putexcel B`row' = `chi2', nformat(number_d3)
    }
    else {
        putexcel B`row' = "N/A"
    }
    
    local row = `row' + 2
    putexcel A`row' = "Sample Sizes", bold
    
    local row = `row' + 1
    putexcel A`row' = "Total:"
    putexcel B`row' = `n_total', nformat(number)
    
    local row = `row' + 1
    putexcel A`row' = "Eligible (below threshold):"
    putexcel B`row' = `n_eligible', nformat(number)
    
    local row = `row' + 1
    putexcel A`row' = "Non-eligible (above threshold):"
    putexcel B`row' = `n_noneligible', nformat(number)
    
    local row = `row' + 2
    putexcel A`row' = "Notes:", bold
    local row = `row' + 1
    putexcel A`row' = "Standard errors in parentheses. * p<0.10, ** p<0.05, *** p<0.01"
    local row = `row' + 1
    putexcel A`row' = "Sample: Single mother household heads (jefe==1 & hh_madre_soltera_rob_2020==1)"
    local row = `row' + 1
    putexcel A`row' = "Eligible = Below threshold (income < 522.5 BRL); Non-eligible = Above threshold"
    local row = `row' + 1
    putexcel A`row' = "Time period: 2020, Q2-Q4"
    local row = `row' + 1
    putexcel A`row' = "Difference = Eligible mean - Non-eligible mean"
    
    restore
    
    * Clean up
    if `usebw' > 0 {
        capture drop temp_in_bw
    }
    
    noi di "Done!"
    
end

/*==============================================================================
TABLE 1: FULL SAMPLE - MOTHER'S CHARACTERISTICS
==============================================================================*/

noi di ""
noi di "*** CREATING TABLE 1: FULL SAMPLE - MOTHER'S CHARACTERISTICS ***"

create_descriptive_table, outfile("$output_dir\01_Descriptive_all.xlsx") ///
    sampletype("Individual (mother's characteristics)")

/*==============================================================================
TABLE 2: OPTIMAL BANDWIDTH - MOTHER'S CHARACTERISTICS
==============================================================================*/

noi di ""
noi di "*** EXTRACTING OPTIMAL BANDWIDTH USING PEA ***"

* Calculate optimal bandwidth using pea as dependent variable
preserve
keep if $sample_cond

* Check if pea has enough variation
quietly sum pea
if r(sd) > 0 {
    quietly rdrobust pea running_var, c(0) all masspoints(adjust) ///
        vce(cluster region_num)
    
    local bw_optimal = (e(h_l) + e(h_r)) / 2
    noi di "Optimal bandwidth (using pea): " %8.3f `bw_optimal' " BRL"
}
else {
    noi di "Warning: pea has no variation, using default bandwidth of 100 BRL"
    local bw_optimal = 100
}

restore

noi di ""
noi di "*** CREATING TABLE 2: OPTIMAL BANDWIDTH - MOTHER'S CHARACTERISTICS ***"

create_descriptive_table, outfile("$output_dir\02_Descriptive_all_h.xlsx") ///
    usebw(`bw_optimal') sampletype("Individual (mother's characteristics)")

/*==============================================================================
TABLE 3: FULL SAMPLE - HOUSEHOLD AVERAGE CHARACTERISTICS
==============================================================================*/

noi di ""
noi di "*** CREATING TABLE 3: HOUSEHOLD AVERAGE CHARACTERISTICS ***"
noi di "Computing household-level averages..."

* Reload data to compute household averages
use "$project_dir\Data\03_SEDLAC_panel_2019-2020_with_single_mother.dta", clear

* Time restrictions
keep if ano == 2020                    
keep if inlist(trimestre, 2, 3, 4)     
keep if cohh == 1                      

* Define eligibility threshold and running variable
local threshold_2020 = 522.5
gen running_var = ipcf - `threshold_2020'
label var running_var "Per capita income minus eligibility threshold"

gen eligible = (running_var < 0) if !missing(running_var)
label var eligible "Eligible for Auxilio Emergencial (below threshold)"

* Create region dummies if they don't exist
capture confirm variable region_1
if _rc != 0 {
    capture drop region_num
    encode(region_est1), gen(region_num)
    
    forvalues r = 1/5 {
        gen region_`r' = (region_num == `r') if !missing(region_num)
    }
}

* Ensure region_num exists for clustering
capture confirm variable region_num
if _rc != 0 {
    encode(region_est1), gen(region_num)
}

/*------------------------------------------------------------------------------
STEP 1: Identify single mother households (using household head info)
------------------------------------------------------------------------------*/

* First, identify which households are single mother households
* by checking if the household head meets the criteria
bysort idp_h: egen is_sm_hh = max(hh_madre_soltera_rob_2020 == 1 & jefe == 1)

* Keep only members of single mother households
keep if is_sm_hh == 1

noi di "Observations in single mother households: " _N

/*------------------------------------------------------------------------------
STEP 2: Compute household-level averages for all covariates
------------------------------------------------------------------------------*/

* List of variables to compute household averages
local vars_to_avg hombre edad asiste nro_hijos miembros urbano ///
    prii pric seci secc supi supc max_edu ///
    ipcf pea ocupado n_ocu_h ///
    region_1 region_2 region_3 region_4 region_5

* Create household-level averages for each variable
foreach var of local vars_to_avg {
    capture confirm variable `var'
    if _rc == 0 {
        capture drop hh_avg_`var'
        bysort idp_h: egen hh_avg_`var' = mean(`var')
    }
}

* For nivedu_hh, it's already at household level (head's education)
* but compute mean anyway for consistency
capture confirm variable nivedu_hh
if _rc != 0 {
    bysort idp_h: egen nivedu_hh = max(nivedu * (jefe==1))
}
capture drop hh_avg_nivedu_hh
bysort idp_h: egen hh_avg_nivedu_hh = mean(nivedu_hh)

/*------------------------------------------------------------------------------
STEP 3: Collapse to household level (keep one obs per household)
------------------------------------------------------------------------------*/

* Keep only household heads (one obs per household)
keep if jefe == 1

noi di "Households (one obs per HH): " _N

/*------------------------------------------------------------------------------
STEP 4: Replace original variables with household averages
------------------------------------------------------------------------------*/

* Replace variables with their household averages
local vars_to_replace hombre edad asiste nro_hijos miembros urbano ///
    nivedu_hh prii pric seci secc supi supc max_edu ///
    ipcf pea ocupado n_ocu_h ///
    region_1 region_2 region_3 region_4 region_5

foreach var of local vars_to_replace {
    capture confirm variable hh_avg_`var'
    if _rc == 0 {
        replace `var' = hh_avg_`var'
    }
}

/*------------------------------------------------------------------------------
STEP 5: Update variable labels for household averages
------------------------------------------------------------------------------*/

capture label var hombre "HH avg: Proportion male"
capture label var edad "HH avg: Age"
capture label var asiste "HH avg: Educational attendance"
capture label var nro_hijos "HH avg: Number of children"
capture label var miembros "HH avg: Household members"
capture label var urbano "HH avg: Urban"
capture label var nivedu_hh "HH avg: Head education level"
capture label var prii "HH avg: Incomplete primary"
capture label var pric "HH avg: Complete primary"
capture label var seci "HH avg: Incomplete secondary"
capture label var secc "HH avg: Complete secondary"
capture label var supi "HH avg: Incomplete tertiary"
capture label var supc "HH avg: Complete tertiary"
capture label var max_edu "HH avg: Maximum education level"
capture label var ipcf "HH avg: Per capita income"
capture label var pea "HH avg: Labor force participation"
capture label var ocupado "HH avg: Employed"
capture label var n_ocu_h "HH avg: Number employed in HH"
capture label var region_1 "HH avg: Region North"
capture label var region_2 "HH avg: Region Northeast"
capture label var region_3 "HH avg: Region Southeast"
capture label var region_4 "HH avg: Region South"
capture label var region_5 "HH avg: Region Central-West"

/*------------------------------------------------------------------------------
STEP 6: Rebuild covariate list for this dataset
------------------------------------------------------------------------------*/

* Define sample condition
global sample_cond "hh_madre_soltera_rob_2020 == 1 & jefe == 1"

* List of potential covariates
local potential_covs hombre edad asiste nro_hijos miembros urbano ///
    nivedu_hh prii pric seci secc supi supc max_edu ///
    ipcf pea ocupado n_ocu_h ///
    region_1 region_2 region_3 region_4 region_5

* Build list of valid covariates
global covariate_list ""
global covariate_list_ftest ""

noi di ""
noi di "Checking available covariates for HH averages..."

foreach var of local potential_covs {
    capture confirm variable `var'
    if _rc == 0 {
        quietly count if $sample_cond & !missing(`var')
        if r(N) > 0 {
            global covariate_list "$covariate_list `var'"
            
            quietly sum `var' if $sample_cond
            if r(sd) > 0 {
                global covariate_list_ftest "$covariate_list_ftest `var'"
            }
            noi di "  [OK] `var' - " r(N) " non-missing obs"
        }
        else {
            noi di "  [SKIP] `var' - all missing in sample"
        }
    }
    else {
        noi di "  [SKIP] `var' - variable not found"
    }
}

count if $sample_cond
noi di ""
noi di "Final sample for HH averages table: " %12.0fc r(N)

* Create the table
create_descriptive_table, outfile("$output_dir\03_Descriptive_avg_hh.xlsx") ///
    sampletype("Household averages (all members)")

/*==============================================================================
COMPLETION MESSAGE
==============================================================================*/

noi di ""
noi di "==================================================================="
noi di "=== ALL TABLES COMPLETED ==="
noi di "==================================================================="
noi di ""
noi di "Output files saved to:"
noi di "$output_dir"
noi di ""
noi di "Files created:"
noi di "  1. 01_Descriptive_all.xlsx    - Full sample, mother's characteristics"
noi di "  2. 02_Descriptive_all_h.xlsx  - Optimal bandwidth, mother's characteristics"
noi di "  3. 03_Descriptive_avg_hh.xlsx - Full sample, household averages"
noi di ""
noi di "==================================================================="
