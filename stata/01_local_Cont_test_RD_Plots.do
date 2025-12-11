/*******************************************************************************
COVARIATE BALANCE PLOTS - HOUSEHOLD HEADS IN SINGLE MOTHER HOUSEHOLDS
Author: Luis Castellanos
Project: Anchors in the Storm - Manipulation Testing

PURPOSE: 
Generate RD plots for covariate balance testing around the Auxilio Emergencial
eligibility threshold.

OUTPUTS:
- Two plots per covariate (quadratic p=2 and linear p=1)
- 26 total plots (13 covariates × 2 polynomials)

TIME PERIOD: 2020, quarters 2-4
SAMPLE: Household heads in single mother households
*******************************************************************************/

clear all
set more off
cls

/*==============================================================================
PATH SETUP
==============================================================================*/

* PROJECT DIRECTORY
global project_dir "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Transfers causal analysis BRA"

* OUTPUT DIRECTORY FOR PLOTS
global plots_dir "$project_dir\Outputs\01_New_Outputs_2025\Images\01_New_outputs\18_manipulation"

* Create output directory if it doesn't exist
capture mkdir "$project_dir\Outputs\01_New_Outputs_2025\Images"
capture mkdir "$project_dir\Outputs\01_New_Outputs_2025\Images\01_New_outputs"
capture mkdir "$plots_dir"

noi di ""
noi di "=== COVARIATE BALANCE RD PLOTS GENERATION ==="
noi di "Sample: Household heads in single mother households"
noi di "Plots output: $plots_dir"
noi di ""

/*==============================================================================
DATA LOADING AND SAMPLE PREPARATION
==============================================================================*/

noi di "=== LOADING AND PREPARING DATA ==="

use "$project_dir\Data\03_SEDLAC_panel_2019-2020_with_single_mother.dta", clear

* Apply time and sample restrictions
keep if ano == 2020
keep if inlist(trimestre, 2, 3, 4)
keep if cohh == 1

noi di "Sample after time/consistency filters: " _N

* Create running variable
local threshold_2020 = 522.5
gen running_var = ipcf - `threshold_2020'
label var running_var "Per capita income minus eligibility threshold (522.5)"

* Create region numeric variable for clustering
encode(region_est1), gen(region_num)

* Define sample
local sample_condition "hh_madre_soltera_rob_2020 == 1 & jefe == 1"

count if `sample_condition'
noi di "Sample size (household heads in single mother HHs): " r(N)

/*==============================================================================
DEFINE COVARIATES
==============================================================================*/

local cov_nums "01 02 03 04 05 06 07 08 09 10 11 12 13"

* Short names for file naming
local name_01 "hombre"
local name_02 "nro_hijos"
local name_03 "miembros"
local name_04 "urbano"
local name_05 "edad"
local name_06 "prii"
local name_07 "pric"
local name_08 "seci"
local name_09 "secc"
local name_10 "supi"
local name_11 "supc"
local name_12 "max_edu"
local name_13 "nivedu"

* Variable names
local varname_01 "hombre"
local varname_02 "nro_hijos"
local varname_03 "miembros"
local varname_04 "urbano"
local varname_05 "edad"
local varname_06 "prii"
local varname_07 "pric"
local varname_08 "seci"
local varname_09 "secc"
local varname_10 "supi"
local varname_11 "supc"
local varname_12 "max_edu"
local varname_13 "nivedu"

* Display names for plot titles
local desc_01 "Gender (Male=1)"
local desc_02 "Number of Children"
local desc_03 "Household Size"
local desc_04 "Urban Area"
local desc_05 "Age"
local desc_06 "Primary Incomplete"
local desc_07 "Primary Complete"
local desc_08 "Secondary Incomplete"
local desc_09 "Secondary Complete"
local desc_10 "Superior Incomplete"
local desc_11 "Superior Complete"
local desc_12 "Maximum Education Level"
local desc_13 "HH Head Education Level"

* Y-axis labels
local ylabel_01 "Proportion Male"
local ylabel_02 "Number of Children"
local ylabel_03 "Household Size"
local ylabel_04 "Proportion Urban"
local ylabel_05 "Age (years)"
local ylabel_06 "Proportion Primary Incomplete"
local ylabel_07 "Proportion Primary Complete"
local ylabel_08 "Proportion Secondary Incomplete"
local ylabel_09 "Proportion Secondary Complete"
local ylabel_10 "Proportion Superior Incomplete"
local ylabel_11 "Proportion Superior Complete"
local ylabel_12 "Maximum Education Level"
local ylabel_13 "Education Level"

/*==============================================================================
MAIN LOOP - GENERATE PLOTS FOR ALL COVARIATES
==============================================================================*/

foreach cov_num of local cov_nums {
    local cov_name = "`name_`cov_num''"
    local cov_var = "`varname_`cov_num''"
    local cov_desc = "`desc_`cov_num''"
    local y_label = "`ylabel_`cov_num''"
    
    noi di ""
    noi di "==============================================================================="
    noi di "=== COVARIATE `cov_num': `cov_desc' (`cov_var') ==="
    noi di "==============================================================================="
    
    * Check sample size
    count if `sample_condition' & !missing(`cov_var') & !missing(running_var)
    local valid_n = r(N)
    noi di "Sample size with valid data: `valid_n'"
    
    if `valid_n' < 50 {
        noi di "WARNING: Insufficient observations for `cov_var', skipping"
        continue
    }
    
    /*==========================================================================
    STEP 1: RUN RDROBUST TO GET OPTIMAL BANDWIDTHS AND COEFFICIENT
    ==========================================================================*/
    
    noi di ""
    noi di "--- Running rdrobust to obtain optimal bandwidths and coefficient ---"
    
    * Run rdrobust (no covariates for balance test)
    capture rdrobust `cov_var' running_var if `sample_condition', ///
        c(0) all masspoints(adjust) vce(cluster region_num)
    
    if _rc != 0 {
        noi di "ERROR: rdrobust failed for `cov_var'"
        continue
    }
    
    * Extract bias-corrected bandwidths and coefficient
    local hbc_left = e(b_l)
    local hbc_right = e(b_r)
    local coef_bc = e(tau_bc)
    local se_bc = e(se_tau_rb)
    
    noi di "Bias-corrected bandwidth left: `hbc_left'"
    noi di "Bias-corrected bandwidth right: `hbc_right'"
    noi di "Bias-corrected coefficient: `coef_bc'"
    noi di "Bias-corrected SE: `se_bc'"
    
    * Check if bandwidths are valid
    if missing(`hbc_left') | missing(`hbc_right') | missing(`coef_bc') {
        noi di "ERROR: Could not extract valid bandwidths or coefficient for `cov_var'"
        continue
    }
    
    * Round values for display
    local coef_bc_round = round(`coef_bc', 0.001)
    local hbc_left_round = round(`hbc_left', 0.01)
    local hbc_right_round = round(`hbc_right', 0.01)
    
    /*==========================================================================
    STEP 2: GENERATE PLOT WITH QUADRATIC POLYNOMIAL (p=2)
    ==========================================================================*/
    
    noi di ""
    noi di "--- Generating quadratic plot (p=2) ---"
    
    preserve
    
    * Restrict to optimal bandwidth
    drop if running_var < (`hbc_left' * -1) | running_var > `hbc_right'
    
    noi di "Observations within bandwidth: " _N
    
    * Generate quadratic RD plot
    capture rdplot `cov_var' running_var if `sample_condition', ///
        c(0) all masspoints(adjust) vce(cluster region_num) ///
        kernel(tri) ci(90) p(2) binselect(qspr) ///
        graph_options(title("Balance Test - `cov_desc'") ///
                     xtitle("Per Capita Income - Eligibility Threshold") ///
                     ytitle("`y_label'") ///
                     note("Bias-corrected coefficient: `coef_bc_round'  |  Polynomial: p=2  |  Bandwidths: [`hbc_left_round', `hbc_right_round']", size(small)) ///
                     legend(off))
    
    if _rc != 0 {
        noi di "ERROR: rdplot (p=2) failed for `cov_var'"
        restore
        continue
    }
    
    * Save plot
    local plot_file_p2 "`cov_num'_RD_`cov_name'_balance_p2_qspr.png"
    local plot_path_p2 "$plots_dir\\`plot_file_p2'"
    
    graph export "`plot_path_p2'", replace as(png) width(1200)
    noi di "Saved: `plot_path_p2'"
    
    restore
    
    /*==========================================================================
    STEP 3: GENERATE PLOT WITH LINEAR POLYNOMIAL (p=1)
    ==========================================================================*/
    
    noi di ""
    noi di "--- Generating linear plot (p=1) ---"
    
    preserve
    
    * Restrict to optimal bandwidth
    drop if running_var < (`hbc_left' * -1) | running_var > `hbc_right'
    
    * Generate linear RD plot
    capture rdplot `cov_var' running_var if `sample_condition', ///
        c(0) all masspoints(adjust) vce(cluster region_num) ///
        kernel(tri) ci(90) p(1) binselect(qspr) ///
        graph_options(title("Balance Test - `cov_desc'") ///
                     xtitle("Per Capita Income - Eligibility Threshold") ///
                     ytitle("`y_label'") ///
                     note("Bias-corrected coefficient: `coef_bc_round'  |  Polynomial: p=1  |  Bandwidths: [`hbc_left_round', `hbc_right_round']", size(small)) ///
                     legend(off))
    
    if _rc != 0 {
        noi di "ERROR: rdplot (p=1) failed for `cov_var'"
        restore
        continue
    }
    
    * Save plot
    local plot_file_p1 "`cov_num'_RD_`cov_name'_balance_p1_qspr.png"
    local plot_path_p1 "$plots_dir\\`plot_file_p1'"
    
    graph export "`plot_path_p1'", replace as(png) width(1200)
    noi di "Saved: `plot_path_p1'"
    
    restore
    
    noi di ""
    noi di "=== COVARIATE `cov_num' PLOTS COMPLETED ==="
    noi di "Plots saved:"
    noi di "  - Quadratic (p=2): `plot_file_p2'"
    noi di "  - Linear (p=1): `plot_file_p1'"
    noi di ""
}

/*==============================================================================
FINAL SUMMARY
==============================================================================*/

noi di ""
noi di "============================================================================="
noi di "=== ALL COVARIATE BALANCE PLOTS COMPLETED ==="
noi di "============================================================================="
noi di "Output location: $plots_dir"
noi di ""
noi di "Covariates plotted:"
noi di "  01. Gender (hombre)"
noi di "  02. Number of Children (nro_hijos)"
noi di "  03. Household Size (miembros)"
noi di "  04. Urban Area (urbano)"
noi di "  05. Age (edad)"
noi di "  06. Primary Incomplete (prii)"
noi di "  07. Primary Complete (pric)"
noi di "  08. Secondary Incomplete (seci)"
noi di "  09. Secondary Complete (secc)"
noi di "  10. Superior Incomplete (supi)"
noi di "  11. Superior Complete (supc)"
noi di "  12. Maximum Education Level (max_edu)"
noi di "  13. HH Head Education Level (nivedu)"
noi di ""
noi di "Two plots generated per covariate (p=1 and p=2)"
noi di "Total expected plots: 26"
noi di ""