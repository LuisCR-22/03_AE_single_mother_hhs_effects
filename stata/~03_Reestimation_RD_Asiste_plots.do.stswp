/*******************************************************************************
HOUSEHOLD-LEVEL RD PLOTS GENERATION - AUXILIO EMERGENCIAL IMPACT
Author: Luis Castellanos - (le.castellanos10@uniandes.edu.co)
Project: Anchors in the Storm, the causal effect of Auxilio Emergencial

PURPOSE: 
Generate household-level RD plots for 9 demographic groups showing the effect 
of Auxilio Emergencial eligibility on school attendance using regression 
discontinuity design.

OUTPUTS:
- Two plots per group (quadratic p=2 and linear p=1)
- 18 total plots saved as PNG files

GROUPS:
Ages 18-24: Young sons, young daughters, young children, young men, young women, young adults
Ages <18: Boys, girls, kids

SAMPLE FILTER: cohh==1, hh_madre_soltera_rob_2020==1 (single-mother households)
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
global plots_dir "$project_dir\Outputs\02_Dissertation_checks\PNG"

* Create output directory if it doesn't exist
capture mkdir "$project_dir\Outputs\02_Dissertation_checks"
capture mkdir "$plots_dir"

noi di ""
noi di "=== HOUSEHOLD-LEVEL RD PLOTS GENERATION ==="
noi di "Generating plots for 9 demographic groups"
noi di "Plots output: $plots_dir"
noi di ""

/*==============================================================================
DATA LOADING AND PREPARATION
==============================================================================*/

noi di "=== LOADING AND PREPARING DATA ==="

use "$project_dir\Data\03_SEDLAC_panel_2019-2020_with_single_mother.dta", clear

* Apply time and sample restrictions
keep if ano == 2020
keep if inlist(trimestre, 2, 3, 4)
keep if cohh == 1
keep if hh_madre_soltera_rob_2020 == 1

noi di "Individual observations after filters: " _N

* Create running variable (household-level)
local threshold_2020 = 522.5
gen running_var = ipcf - `threshold_2020'
label var running_var "Per capita income minus eligibility threshold"

* Create household education variable
bysort idp_h: egen nivedu_hh = max(nivedu * (jefe==1))
label var nivedu_hh "Education level of household head"

* Create region numeric variable
encode(region_est1), gen(region_num)

/*==============================================================================
CREATE HOUSEHOLD-LEVEL AGGREGATED VARIABLES
==============================================================================*/

noi di "=== CREATING HOUSEHOLD-LEVEL VARIABLES ==="

* Create age restriction indicators
gen young = (edad >= 18 & edad < 25)
gen child = (edad < 18)

* Step 1: Create indicators for each demographic group
* Young adults (18-24)
gen is_young_son = (hijo==1 & hombre==1 & young==1)
gen is_young_daughter = (hijo==1 & hombre==0 & young==1)
gen is_young_child = (hijo==1 & young==1)
gen is_young_man = (hombre==1 & young==1)
gen is_young_woman = (hombre==0 & young==1)
gen is_young_all = (young==1)

* Children (<18)
gen is_boy = (hijo==1 & hombre==1 & child==1)
gen is_girl = (hijo==1 & hombre==0 & child==1)
gen is_kid = (hijo==1 & child==1)

* Step 2: Create attendance values for each group (missing if not in group)
* Young adults (18-24)
gen asiste_young_son = asiste if is_young_son==1
gen asiste_young_daughter = asiste if is_young_daughter==1
gen asiste_young_child = asiste if is_young_child==1
gen asiste_young_man = asiste if is_young_man==1
gen asiste_young_woman = asiste if is_young_woman==1
gen asiste_young_all = asiste if is_young_all==1

* Children (<18)
gen asiste_boy = asiste if is_boy==1
gen asiste_girl = asiste if is_girl==1
gen asiste_kid = asiste if is_kid==1

* Step 3: Calculate household-level means (will be . if no group members)
* Young adults (18-24)
bysort idp_h: egen mean_asiste_young_sons = mean(asiste_young_son)
bysort idp_h: egen mean_asiste_young_daughters = mean(asiste_young_daughter)
bysort idp_h: egen mean_asiste_young_children = mean(asiste_young_child)
bysort idp_h: egen mean_asiste_young_men = mean(asiste_young_man)
bysort idp_h: egen mean_asiste_young_women = mean(asiste_young_woman)
bysort idp_h: egen mean_asiste_young_all = mean(asiste_young_all)

* Children (<18)
bysort idp_h: egen mean_asiste_boys = mean(asiste_boy)
bysort idp_h: egen mean_asiste_girls = mean(asiste_girl)
bysort idp_h: egen mean_asiste_kids = mean(asiste_kid)

* Create household-level control: proportion male
bysort idp_h: egen prop_male = mean(hombre)
label var prop_male "Proportion of household members who are male"

/*==============================================================================
COLLAPSE TO HOUSEHOLD LEVEL
==============================================================================*/

noi di "=== COLLAPSING TO HOUSEHOLD LEVEL ==="

* Keep one observation per household
collapse (first) running_var pc_elegibilidad_2020 region_num nivedu_hh miembros urbano prop_male ///
         mean_asiste_young_sons mean_asiste_young_daughters mean_asiste_young_children ///
         mean_asiste_young_men mean_asiste_young_women mean_asiste_young_all ///
         mean_asiste_boys mean_asiste_girls mean_asiste_kids, ///
         by(idp_h)

noi di "Total households in dataset: " _N

/*==============================================================================
DEFINE GROUP CHARACTERISTICS
==============================================================================*/

* Group numbers
local group_nums "01 02 03 04 05 06 07 08 09"

* Outcome variables for each group
local outcome_01 "mean_asiste_young_sons"
local outcome_02 "mean_asiste_young_daughters"
local outcome_03 "mean_asiste_young_children"
local outcome_04 "mean_asiste_young_men"
local outcome_05 "mean_asiste_young_women"
local outcome_06 "mean_asiste_young_all"
local outcome_07 "mean_asiste_boys"
local outcome_08 "mean_asiste_girls"
local outcome_09 "mean_asiste_kids"

* Group names for file naming
local name_01 "young_sons_18-24"
local name_02 "young_daughters_18-24"
local name_03 "young_children_18-24"
local name_04 "young_men_18-24"
local name_05 "young_women_18-24"
local name_06 "young_all_18-24"
local name_07 "boys_under18"
local name_08 "girls_under18"
local name_09 "kids_under18"

* Group descriptions for display and graph titles
local desc_01 "Young Sons (18-24)"
local desc_02 "Young Daughters (18-24)"
local desc_03 "Young Children (18-24)"
local desc_04 "Young Men (18-24)"
local desc_05 "Young Women (18-24)"
local desc_06 "Young Adults (18-24)"
local desc_07 "Boys (<18)"
local desc_08 "Girls (<18)"
local desc_09 "Kids (<18)"

* Control variables (same for all groups - household level)
local controls "miembros urbano prop_male nivedu_hh"

/*==============================================================================
MAIN LOOP - GENERATE PLOTS FOR ALL GROUPS
==============================================================================*/

foreach group_num of local group_nums {
    local group_name = "`name_`group_num''"
    local outcome = "`outcome_`group_num''"
    
    noi di ""
    noi di "==============================================================================="
    noi di "=== GROUP `group_num': `desc_`group_num'' ==="
    noi di "==============================================================================="
    
    * Check how many households have data for this group
    count if !missing(`outcome')
    local sample_size = r(N)
    noi di "Households with data for this group: `sample_size'"
    
    if `sample_size' < 50 {
        noi di "WARNING: Small sample size (`sample_size'), skipping this group"
        continue
    }
    
    /*==========================================================================
    STEP 1: RUN RDROBUST TO GET OPTIMAL BANDWIDTHS AND COEFFICIENT
    ==========================================================================*/
    
    noi di ""
    noi di "--- Running rdrobust to obtain optimal bandwidths and coefficient ---"
    
    * Run rdrobust with household-level controls and default mserd bandwidth
    capture rdrobust `outcome' running_var, ///
        c(0) all vce(cluster region_num) ///
        covs(`controls')
    
    if _rc != 0 {
        noi di "ERROR: rdrobust failed for group `group_num'"
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
        noi di "ERROR: Could not extract valid bandwidths or coefficient for group `group_num'"
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
    
    * Restrict to optimal bandwidth and non-missing outcomes
    drop if missing(`outcome')
    drop if running_var < (`hbc_left' * -1) | running_var > `hbc_right'
    
    noi di "Households within bandwidth: " _N
    
    * Generate quadratic RD plot with custom options
    capture rdplot `outcome' running_var, ///
        c(0) kernel(tri) ci(90) p(2) binselect(qspr) ///
        graph_options(title("RD Plot - `desc_`group_num'' (Household-Level)") ///
                     xtitle("Per Capita Income - Eligibility Threshold") ///
                     ytitle("Mean School Attendance Rate") ///
                     note("Bias-corrected coefficient: `coef_bc_round'  |  Polynomial: p=2  |  Bandwidths: [`hbc_left_round', `hbc_right_round']", size(small)) ///
                     legend(off))
    
    if _rc != 0 {
        noi di "ERROR: rdplot (p=2) failed for group `group_num'"
        restore
        continue
    }
    
    * Save plot
    local plot_file_p2 "`group_num'_HH_RD_`group_name'_asiste_p2_qspr.png"
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
    
    * Restrict to optimal bandwidth and non-missing outcomes
    drop if missing(`outcome')
    drop if running_var < (`hbc_left' * -1) | running_var > `hbc_right'
    
    * Generate linear RD plot with custom options
    capture rdplot `outcome' running_var, ///
        c(0) kernel(tri) ci(90) p(1) binselect(qspr) ///
        graph_options(title("RD Plot - `desc_`group_num'' (Household-Level)") ///
                     xtitle("Per Capita Income - Eligibility Threshold") ///
                     ytitle("Mean School Attendance Rate") ///
                     note("Bias-corrected coefficient: `coef_bc_round'  |  Polynomial: p=1  |  Bandwidths: [`hbc_left_round', `hbc_right_round']", size(small)) ///
                     legend(off))
    
    if _rc != 0 {
        noi di "ERROR: rdplot (p=1) failed for group `group_num'"
        restore
        continue
    }
    
    * Save plot
    local plot_file_p1 "`group_num'_HH_RD_`group_name'_asiste_p1_qspr.png"
    local plot_path_p1 "$plots_dir\\`plot_file_p1'"
    
    graph export "`plot_path_p1'", replace as(png) width(1200)
    noi di "Saved: `plot_path_p1'"
    
    restore
    
    noi di ""
    noi di "=== GROUP `group_num' COMPLETED ==="
    noi di "Plots saved:"
    noi di "  - Quadratic (p=2): `plot_file_p2'"
    noi di "  - Linear (p=1): `plot_file_p1'"
    noi di ""
	
	/*==========================================================================
    STEP 4: GENERATE PLOT WITH LINEAR POLYNOMIAL (p=3)
    ==========================================================================*/
    
    noi di ""
    noi di "--- Generating linear plot (p=3) ---"
    
    preserve
    
    * Restrict to optimal bandwidth and non-missing outcomes
    drop if missing(`outcome')
    drop if running_var < (`hbc_left' * -1) | running_var > `hbc_right'
    
    * Generate linear RD plot with custom options
    capture rdplot `outcome' running_var, ///
        c(0) kernel(tri) ci(90) p(3) binselect(qspr) ///
        graph_options(title("RD Plot - `desc_`group_num'' (Household-Level)") ///
                     xtitle("Per Capita Income - Eligibility Threshold") ///
                     ytitle("Mean School Attendance Rate") ///
                     note("Bias-corrected coefficient: `coef_bc_round'  |  Polynomial: p=3  |  Bandwidths: [`hbc_left_round', `hbc_right_round']", size(small)) ///
                     legend(off))
    
    if _rc != 0 {
        noi di "ERROR: rdplot (p=3) failed for group `group_num'"
        restore
        continue
    }
    
    * Save plot
    local plot_file_p3 "`group_num'_HH_RD_`group_name'_asiste_p3_qspr.png"
    local plot_path_p3 "$plots_dir\\`plot_file_p3'"
    
    graph export "`plot_path_p3'", replace as(png) width(1200)
    noi di "Saved: `plot_path_p3'"
    
    restore
    
    noi di ""
    noi di "=== GROUP `group_num' COMPLETED ==="
    noi di "Plots saved:"
    noi di ""
}

/*==============================================================================
FINAL SUMMARY
==============================================================================*/

noi di ""
noi di "============================================================================="
noi di "=== ALL HOUSEHOLD-LEVEL RD PLOTS GENERATION COMPLETED ==="
noi di "============================================================================="
noi di "Output location: $plots_dir"
noi di ""
noi di "Groups analyzed (household-level averages in single-mother households):"
noi di "  Ages 18-24:"
noi di "    01. Young sons (hijo==1, male)"
noi di "    02. Young daughters (hijo==1, female)"
noi di "    03. Young children (hijo==1)"
noi di "    04. Young men (male, no hijo restriction)"
noi di "    05. Young women (female, no hijo restriction)"
noi di "    06. Young adults (all 18-24)"
noi di "  Ages <18:"
noi di "    07. Boys (hijo==1, male)"
noi di "    08. Girls (hijo==1, female)"
noi di "    09. Kids (hijo==1)"
noi di ""
noi di "Three plots generated per group (p=1, p=2 and p=3)"