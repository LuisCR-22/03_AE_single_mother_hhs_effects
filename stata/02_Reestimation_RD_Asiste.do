/*******************************************************************************
HOUSEHOLD-LEVEL REGRESSION DISCONTINUITY ANALYSIS
Author: Luis Castellanos
Modified for: Household-level analysis of young adults (18-24) AND children (<18)

PURPOSE: 
Estimate causal effect of Auxilio Emergencial on school attendance using
household-level averages for demographic groups within single-mother households.
Estimates with polynomial orders p=1, p=2, and p=3.

ANALYSIS GROUPS in single-mother households:
Ages 18-24:
1. Young sons (hijo==1 & hombre==1)
2. Young daughters (hijo==1 & hombre==0)  
3. Young children (hijo==1)
4. Young men (hombre==1, no hijo restriction)
5. Young women (hombre==0, no hijo restriction)
6. Young adults (all 18-24, no restrictions)

Ages <18:
7. Boys (hijo==1 & hombre==1)
8. Girls (hijo==1 & hombre==0)
9. Kids (hijo==1)

HOUSEHOLD-LEVEL CONTROLS: miembros, urbano, prop_male, nivedu_hh
*******************************************************************************/

clear all
set more off
cls

/*==============================================================================
PATH SETUP
==============================================================================*/

global project_dir "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Transfers causal analysis BRA"
global excel_dir "$project_dir\Outputs\02_Dissertation_checks\Excel"

capture mkdir "$excel_dir"

noi di ""
noi di "=== HOUSEHOLD-LEVEL RD ANALYSIS: MULTIPLE AGE GROUPS & POLYNOMIAL ORDERS ==="
noi di "Excel output: $excel_dir"
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
DEFINE ANALYSIS GROUPS
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

* Group descriptions
local desc_01 "Young sons (18-24, hijo==1, male)"
local desc_02 "Young daughters (18-24, hijo==1, female)"
local desc_03 "Young children (18-24, hijo==1)"
local desc_04 "Young men (18-24, male, no hijo restriction)"
local desc_05 "Young women (18-24, female, no hijo restriction)"
local desc_06 "Young adults (18-24, all)"
local desc_07 "Boys (<18, hijo==1, male)"
local desc_08 "Girls (<18, hijo==1, female)"
local desc_09 "Kids (<18, hijo==1)"

* Control variables (same for all groups - household level)
local controls "miembros urbano prop_male"

/*==============================================================================
MAIN ANALYSIS LOOP - ITERATE THROUGH ALL GROUPS AND POLYNOMIAL ORDERS
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
    LOOP THROUGH POLYNOMIAL ORDERS: p=1, p=2, p=3
    ==========================================================================*/
    
    foreach p_order in 1 2 3 {
        
        noi di ""
        noi di "--- Polynomial order p = `p_order' ---"
        
        /*======================================================================
        PREPARE EXCEL OUTPUT FILE FOR THIS GROUP AND POLYNOMIAL ORDER
        ======================================================================*/
        
        * Construct filename based on polynomial order
        if `p_order' == 1 {
            local excel_file "`group_num'_HH_RD_`group_name'_asiste.xlsx"
        }
        else {
            local excel_file "`group_num'_HH_RD_`group_name'_asiste_p`p_order'.xlsx"
        }
        
        local excel_path "$excel_dir\\`excel_file'"
        
        noi di "Creating Excel file: `excel_path'"
        
        putexcel set "`excel_path'", replace
        
        * Set up column headers
        putexcel A1 = "Statistic"
        putexcel B1 = "No Controls"
        putexcel C1 = "With Controls"
        putexcel D1 = "Cluster No Controls"
        putexcel E1 = "Cluster With Controls"
        putexcel F1 = "Cluster With Controls Edu"
        putexcel G1 = "MSEtwo No Controls"
        putexcel H1 = "MSEtwo With Controls"
        putexcel I1 = "MSEtwo Cluster No Controls"
        putexcel J1 = "MSEtwo Cluster With Controls"
        putexcel K1 = "MSEtwo Cluster With Controls Edu"
        
        * Set up row labels
        putexcel A2 = "Original N"
        putexcel A3 = "Original N left"
        putexcel A4 = "Original N right"
        putexcel A5 = "Effective N left (h)"
        putexcel A6 = "Effective N right (h)"
        putexcel A7 = "Effective N left (b)"
        putexcel A8 = "Effective N right (b)"
        putexcel A9 = "Cutoff"
        putexcel A10 = "Polynomial order (p)"
        putexcel A11 = "Polynomial order (q)"
        putexcel A12 = "Bandwidth left"
        putexcel A13 = "Bandwidth right"
        putexcel A14 = "BC Bandwidth left"
        putexcel A15 = "BC Bandwidth right"
        putexcel A16 = "Conventional estimate"
        putexcel A17 = "Conventional estimate left"
        putexcel A18 = "Conventional estimate right"
        putexcel A19 = "BC estimate"
        putexcel A20 = "BC estimate left"
        putexcel A21 = "BC estimate right"
        putexcel A22 = "Conventional SE"
        putexcel A23 = "BC SE"
        putexcel A24 = "Mean above (BC)"
        putexcel A25 = "Mean below (BC)"
        putexcel A26 = "Mean above (Conv)"
        putexcel A27 = "Mean below (Conv)"
        
        /*======================================================================
        RD ESTIMATIONS FOR CURRENT GROUP AND POLYNOMIAL ORDER
        ======================================================================*/
        
        local col_letters "B C D E F G H I J K"
        local col_count = 0
        
        * Loop through 10 regression specifications
        forvalues reg_num = 1/10 {
            local ++col_count
            local col_letter : word `col_count' of `col_letters'
            
            noi di "--- Running Regression `reg_num' (p=`p_order') ---"
            
            * Determine specification
            local use_controls = 0
            local use_clustering = 0
            local use_education = 0
            local use_msetwo = 0
            local reg_title = ""
            
            if `reg_num' == 1 {
                local reg_title "No Controls"
            }
            else if `reg_num' == 2 {
                local use_controls = 1
                local reg_title "With Controls"
            }
            else if `reg_num' == 3 {
                local use_clustering = 1
                local reg_title "Cluster No Controls"
            }
            else if `reg_num' == 4 {
                local use_controls = 1
                local use_clustering = 1
                local reg_title "Cluster With Controls"
            }
            else if `reg_num' == 5 {
                local use_controls = 1
                local use_clustering = 1
                local use_education = 1
                local reg_title "Cluster With Controls Edu"
            }
            else if `reg_num' == 6 {
                local use_msetwo = 1
                local reg_title "MSEtwo No Controls"
            }
            else if `reg_num' == 7 {
                local use_controls = 1
                local use_msetwo = 1
                local reg_title "MSEtwo With Controls"
            }
            else if `reg_num' == 8 {
                local use_clustering = 1
                local use_msetwo = 1
                local reg_title "MSEtwo Cluster No Controls"
            }
            else if `reg_num' == 9 {
                local use_controls = 1
                local use_clustering = 1
                local use_msetwo = 1
                local reg_title "MSEtwo Cluster With Controls"
            }
            else if `reg_num' == 10 {
                local use_controls = 1
                local use_clustering = 1
                local use_education = 1
                local use_msetwo = 1
                local reg_title "MSEtwo Cluster With Controls Edu"
            }
            
            * Build rdrobust command WITH POLYNOMIAL ORDER
            local rd_cmd "rdrobust `outcome' running_var, c(0) all p(`p_order')"
            
            * Add bandwidth selector
            if `use_msetwo' == 1 {
                local rd_cmd "`rd_cmd' bwselect(msetwo)"
            }
            
            * Add clustering (at region level)
            if `use_clustering' == 1 {
                local rd_cmd "`rd_cmd' vce(cluster region_num)"
            }
            
            * Add covariates
            if `use_controls' == 1 {
                if `use_education' == 1 {
                    local rd_cmd "`rd_cmd' covs(`controls' nivedu_hh)"
                }
                else {
                    local rd_cmd "`rd_cmd' covs(`controls')"
                }
            }
            
            * Execute regression
            capture `rd_cmd'
            
            if _rc != 0 {
                noi di "ERROR in regression `reg_num' (p=`p_order') for group `group_num': `rd_cmd'"
                continue
            }
            
            * Store comprehensive results
            local orig_n = e(N)
            local orig_nl = e(N_l)
            local orig_nr = e(N_r)
            local e_n_r = e(N_h_r)
            local e_n_l = e(N_h_l)
            local e_bn_r = e(N_b_r)
            local e_bn_l = e(N_b_l)
            local cut = e(c)
            local poli = e(p)
            local qpoli = e(q)
            local h_left = e(h_l)
            local h_right = e(h_r)
            local hbc_left = e(b_l)
            local hbc_right = e(b_r)
            local coef_conv = e(tau_cl)
            local coef_conv_r = e(tau_cl_r)
            local coef_conv_l = e(tau_cl_l)
            local coef_bc = e(tau_bc)
            local coef_bc_r = e(tau_bc_r)
            local coef_bc_l = e(tau_bc_l)
            local se_coef_conv = e(se_tau_cl)
            local se_coef_bc = e(se_tau_rb)
            
            * Calculate means using bias-corrected bandwidth
            capture {
                sum `outcome' if running_var < `hbc_right' & running_var > 0
                local mean_ab_bc = r(mean)
                sum `outcome' if running_var > -`hbc_left' & running_var < 0
                local mean_be_bc = r(mean)
            }
            if _rc != 0 {
                local mean_ab_bc = .
                local mean_be_bc = .
            }
            
            * Calculate means using conventional bandwidth
            capture {
                sum `outcome' if running_var < `h_right' & running_var > 0
                local mean_ab_conv = r(mean)
                sum `outcome' if running_var > -`h_left' & running_var < 0
                local mean_be_conv = r(mean)
            }
            if _rc != 0 {
                local mean_ab_conv = .
                local mean_be_conv = .
            }
            
            * Export comprehensive results to Excel
            capture {
                putexcel `col_letter'2 = `orig_n'
                putexcel `col_letter'3 = `orig_nl'
                putexcel `col_letter'4 = `orig_nr'
                putexcel `col_letter'5 = `e_n_l'
                putexcel `col_letter'6 = `e_n_r'
                putexcel `col_letter'7 = `e_bn_l'
                putexcel `col_letter'8 = `e_bn_r'
                putexcel `col_letter'9 = `cut'
                putexcel `col_letter'10 = `poli'
                putexcel `col_letter'11 = `qpoli'
                putexcel `col_letter'12 = `h_left'
                putexcel `col_letter'13 = `h_right'
                putexcel `col_letter'14 = `hbc_left'
                putexcel `col_letter'15 = `hbc_right'
                putexcel `col_letter'16 = `coef_conv'
                putexcel `col_letter'17 = `coef_conv_l'
                putexcel `col_letter'18 = `coef_conv_r'
                putexcel `col_letter'19 = `coef_bc'
                putexcel `col_letter'20 = `coef_bc_l'
                putexcel `col_letter'21 = `coef_bc_r'
                putexcel `col_letter'22 = `se_coef_conv'
                putexcel `col_letter'23 = `se_coef_bc'
                putexcel `col_letter'24 = `mean_ab_bc'
                putexcel `col_letter'25 = `mean_be_bc'
                putexcel `col_letter'26 = `mean_ab_conv'
                putexcel `col_letter'27 = `mean_be_conv'
            }
            
            if _rc != 0 {
                noi di "WARNING: Excel export failed for regression `reg_num' (p=`p_order') in group `group_num'"
            }
        }
        
        noi di "Excel file completed: `excel_file'"
        
    } // End polynomial order loop
    
    noi di ""
    noi di "=== GROUP `group_num' COMPLETED (all polynomial orders) ==="
    noi di "Households with data: `sample_size'"
    noi di ""
}

/*==============================================================================
FINAL SUMMARY
==============================================================================*/

noi di ""
noi di "============================================================================="
noi di "=== ALL HOUSEHOLD-LEVEL RD ANALYSES COMPLETED ==="
noi di "============================================================================="
noi di "Output location: $excel_dir"
noi di ""
noi di "Groups analyzed in single-mother households:"
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
noi di "Polynomial orders analyzed: p=1 (linear), p=2 (quadratic), p=3 (cubic)"
noi di "Total Excel files created: 27 (9 groups × 3 polynomial orders)"
noi di ""
noi di "File naming convention:"
noi di "  p=1: ##_HH_RD_[groupname]_asiste.xlsx"
noi di "  p=2: ##_HH_RD_[groupname]_asiste_p2.xlsx"
noi di "  p=3: ##_HH_RD_[groupname]_asiste_p3.xlsx"
noi di ""
noi di "Each analysis uses household-level means as outcomes"
noi di "Controls: miembros, urbano, prop_male, nivedu_hh (when specified)"
noi di "Clustering: region_num (when specified)"
noi di ""