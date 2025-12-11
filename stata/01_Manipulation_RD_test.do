/*******************************************************************************
REGRESSION DISCONTINUITY ANALYSIS: MANIPULATION TEST
Author: Luis Castellanos (le.castellanos10@uniandes.edu.co)
Project: Anchors in the Storm

PURPOSE: 
Tests for manipulation of the running variable (household per capita income) 
around the Auxilio Emergencial eligibility threshold using density discontinuity 
methods.

METHODOLOGY:
- Density discontinuity test using local polynomial density estimation
- Running variable: Household per capita income (ipcf) centered at threshold
- Specification: Second-order local polynomial (p=2)
- Inference: Bias-corrected with jackknife standard errors

SAMPLE:
- Population: Single mother households in Brazil, 2020 Q2-Q4
- Eligibility threshold: 522.5 Brazilian reais per capita income
*******************************************************************************/

clear all
set more off
cls

/*==============================================================================
SECTION 1: ENVIRONMENT SETUP
==============================================================================*/

* Define project directory
global project_dir "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Transfers causal analysis BRA"

* Define output directories
global base_docs_dir "$project_dir\Outputs\01_New_Outputs_2025\Docs"
global base_excel_dir "$project_dir\Outputs\01_New_Outputs_2025\Excel\RD_results"
global base_plots_dir "$project_dir\Outputs\01_New_Outputs_2025\Images\01_New_outputs"

/*==============================================================================
SECTION 2: DATA PREPARATION
==============================================================================*/

noi di "=== LOADING AND PREPARING SAMPLE ==="

* Load panel data
use "$project_dir\Data\03_SEDLAC_panel_2019-2020_with_single_mother.dta", clear

* Apply sample restrictions
keep if ano == 2020                          // Year 2020 only
keep if inlist(trimestre, 2, 3, 4)          // Post-program quarters
keep if cohh == 1                            // Income consistency check passed

noi di "Sample size after restrictions: " _N " observations"

/*==============================================================================
SECTION 3: PREPARE RUNNING VARIABLE
==============================================================================*/

noi di ""
noi di "=== PREPARING RUNNING VARIABLE ==="

* Construct running variable centered at threshold
local threshold_2020 = 522.5
gen running_var = ipcf - `threshold_2020'
label var running_var "Per capita income minus eligibility threshold"

noi di "Running variable preparation completed successfully"

/*==============================================================================
SECTION 4: MANIPULATION TEST
==============================================================================*/

noi di ""
noi di "=== CONDUCTING MANIPULATION TEST ==="

* Run density discontinuity test
rddensity running_var

* Store test statistics with 3 decimal precision
local bc_t = string(e(T_q), "%9.3f")
local bc_p = string(e(pv_q), "%9.3f")

* Generate manipulation test plot
rddensity running_var, plot plot_range(-200 200) hist_range(-200 200) ///
    plot_grid(qs) hist_n(15 15) plotl_estype(none) plotr_estype(none) ///
    graph_opt(title("RD Manipulation Test") ///
              xtitle("Per Capita Income - Eligibility Threshold") ///
              ytitle("Density") ///
              xline(0, lpattern(dot) lcolor(black)) ///
              note("Bias-corrected confidence intervals | Polynomial: p=2 | bc-t-statistic [`bc_t'] | bc-p-value [`bc_p']", size(small)) ///
              legend(off))
              
graph export "$base_plots_dir/18_maniputation/01_rddensity.png", replace as(png) width(1200)

noi di "Manipulation test plot saved"

/*==============================================================================
SECTION 5: KERNEL DENSITY PLOT
==============================================================================*/

noi di ""
noi di "=== GENERATING KERNEL DENSITY PLOT ==="

* Restrict to window of interest for kernel density
preserve
keep if running_var >= -200 & running_var <= 200 & !missing(running_var)

* Generate kernel density plot with Epanechnikov kernel
kdensity running_var, ///
    kernel(epanechnikov) ///
    xline(0, lpattern(dot) lcolor(black)) ///
    title("Kernel Density of Running Variable") ///
    xtitle("Per Capita Income - Eligibility Threshold") ///
    ytitle("Density") ///
    note("Epanechnikov kernel | Sample restricted to [-200, 200]", size(small))
    
graph export "$base_plots_dir/18_maniputation/02_kernel_distr.png", replace as(png) width(1200)

restore

noi di "Kernel density plot saved"
noi di ""
noi di "=== MANIPULATION TESTING COMPLETED ==="

-

/*
The confidence intervals/bands may not be centered at the point
        estimates because they have been bias−corrected.
rddensity implements manipulation testing procedures using the local polynomial
        density estimators proposed in Cattaneo, Jansson and Ma (2020), and implements
        graphical procedures with valid confidence bands using the results in
        Cattaneo, Jansson and Ma (2024).  In addition, the command provides
        complementary manipulation testing based on finite sample exact binomial
        testing following the results in Cattaneo, Frandsen and Titiunik (2015)
        Cattaneo, Frandsen and Vazquez−Bare (2017).  For an introduction to
        manipulation testing see McCrary (2008).
*/