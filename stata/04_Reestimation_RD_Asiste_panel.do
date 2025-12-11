/*******************************************************************************
HOUSEHOLD-LEVEL PANEL RD ANALYSIS - TRANSITIONS FOR YOUNG MEMBERS
Author: Luis Castellanos
Modified for: Household-level transition analysis for ages 18-24 AND under 18

PURPOSE: 
Estimate causal effect of Auxilio Emergencial on educational transitions using
household-level averages for young members within single-mother households.
Analyzes both start_asist and stopped_asist transitions.
Estimates with polynomial order p=1 only (for speed).

ANALYSIS GROUPS:
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

OUTCOMES: start_asist, stopped_asist

HOUSEHOLD-LEVEL CONTROLS: miembros, urbano, prop_male, nivedu_hh
*******************************************************************************/

clear all
set more off
cls

/*==============================================================================
PATH SETUP
==============================================================================*/

global project_dir "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Transfers causal analysis BRA"
global excel_dir "$project_dir\Outputs\02_Dissertation_checks\Excel\02_asiste_panel"

capture mkdir "$excel_dir"

noi di ""
noi di "=== HOUSEHOLD-LEVEL PANEL RD ANALYSIS: ALL YOUNG MEMBERS TRANSITIONS ==="
noi di "Excel output: $excel_dir"
noi di ""

/*==============================================================================
DATA LOADING AND PANEL PREPARATION
==============================================================================*/

noi di "=== LOADING AND PREPARING PANEL DATA ==="

use "$project_dir\Data\03_SEDLAC_panel_2019-2020_with_single_mother.dta", clear

noi di "Initial observations: " _N

* Verify panel variable exists
capture confirm variable idp_match_individual
if _rc != 0 {
    noi di as error "ERROR: Variable idp_match_individual not found"
    exit 111
}

* Keep only panel individuals
keep if idp_match_individual == 1

noi di "After keeping panel individuals: " _N

/*==============================================================================
PREPARE DATA FOR TRANSITION ANALYSIS
==============================================================================*/

noi di ""
noi di "=== RESHAPING DATA FOR TRANSITION ANALYSIS ==="

* Keep relevant variables
keep idp_i idp_h ano trimestre ipcf asiste pea ///
     hh_madre_soltera_rob_2020 edad hombre jefe hijo miembros urbano ///
     nivedu region_est1 cohh idp_match_individual v3002 v3002a pondera

* Sort data
sort idp_i ano trimestre

/*==============================================================================
SEPARATE 2019 AND 2020 DATA
==============================================================================*/

* For 2019: Take any observation
preserve
keep if ano == 2019
bysort idp_i: keep if _n == 1
foreach var in asiste pea {
    rename `var' `var'_2019
}
keep idp_i asiste_2019 pea_2019
tempfile data_2019
save `data_2019'
restore

* For 2020: Keep Q2-Q4 observations
keep if ano == 2020 & inlist(trimestre, 2, 3, 4)

* Apply income consistency filter
keep if cohh == 1

* Apply single mother household filter
keep if hh_madre_soltera_rob_2020 == 1

* Merge with 2019 data
merge m:1 idp_i using `data_2019'
keep if _merge == 3
drop _merge

noi di "After merging 2019 and 2020 data: " _N

if _N == 0 {
    noi di as error "ERROR: No observations remain after merging"
    exit 2000
}

/*==============================================================================
CREATE TRANSITION VARIABLES AT INDIVIDUAL LEVEL
==============================================================================*/

noi di ""
noi di "=== CREATING TRANSITION VARIABLES ==="

* Recode missing values to zero for asiste
replace asiste = 0 if asiste == .
replace asiste_2019 = 0 if asiste_2019 == .

* START_ASIST: From not attending (0) in 2019 to attending (1) in 2020
gen start_asist = 0
replace start_asist = 1 if asiste_2019 == 0 & asiste == 1
label var start_asist "Started attending school between 2019 and 2020"

* STOPPED_ASIST: From attending (1) in 2019 to not attending (0) in 2020
gen stopped_asist = 0
replace stopped_asist = 1 if asiste_2019 == 1 & asiste == 0
label var stopped_asist "Stopped attending school between 2019 and 2020"

* Display summary
noi di ""
noi di "Transition variable summary:"
foreach var in start_asist stopped_asist {
    sum `var'
    noi di "`var': Mean = " %6.4f r(mean) ", N = " r(N)
}

/*==============================================================================
CREATE RUNNING VARIABLE AND CONTROLS
==============================================================================*/

noi di ""
noi di "=== PREPARING CONTROL VARIABLES ==="

* Create running variable using 2020 threshold and income
local threshold_2020 = 522.5
gen running_var = ipcf - `threshold_2020'

noi di "Running variable created: ipcf - `threshold_2020'"

* Create household education variable
bysort idp_h: egen nivedu_hh = max(nivedu * (jefe==1))

* Create region numeric variable
capture drop region_num
encode(region_est1), gen(region_num)

noi di "Control variables prepared"

/*==============================================================================
CREATE AGE RESTRICTIONS AND DEMOGRAPHIC INDICATORS FOR ALL AGE GROUPS
==============================================================================*/

noi di ""
noi di "=== CREATING HOUSEHOLD-LEVEL VARIABLES FOR ALL YOUNG MEMBERS ==="

* Age restrictions
gen young = (edad >= 18 & edad < 25)
gen child = (edad < 18)

* Keep only young OR children for this analysis
keep if young == 1 | child == 1

noi di "Observations after restricting to ages <25: " _N
noi di "Sample size for analysis: " _N

* Create indicators for each demographic group
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

/*==============================================================================
CREATE TRANSITION VALUES FOR EACH GROUP
==============================================================================*/

* For start_asist - Young adults (18-24)
gen start_young_son = start_asist if is_young_son==1
gen start_young_daughter = start_asist if is_young_daughter==1
gen start_young_child = start_asist if is_young_child==1
gen start_young_man = start_asist if is_young_man==1
gen start_young_woman = start_asist if is_young_woman==1
gen start_young_all = start_asist if is_young_all==1

* For start_asist - Children (<18)
gen start_boy = start_asist if is_boy==1
gen start_girl = start_asist if is_girl==1
gen start_kid = start_asist if is_kid==1

* For stopped_asist - Young adults (18-24)
gen stopped_young_son = stopped_asist if is_young_son==1
gen stopped_young_daughter = stopped_asist if is_young_daughter==1
gen stopped_young_child = stopped_asist if is_young_child==1
gen stopped_young_man = stopped_asist if is_young_man==1
gen stopped_young_woman = stopped_asist if is_young_woman==1
gen stopped_young_all = stopped_asist if is_young_all==1

* For stopped_asist - Children (<18)
gen stopped_boy = stopped_asist if is_boy==1
gen stopped_girl = stopped_asist if is_girl==1
gen stopped_kid = stopped_asist if is_kid==1

tab start_young_child v3002a[iw=pondera], row
tab start_young_child v3002a, row

/*==============================================================================
CALCULATE HOUSEHOLD-LEVEL MEANS
==============================================================================*/

* For start_asist - Young adults (18-24)
bysort idp_h: egen mean_start_young_sons = mean(start_young_son)
bysort idp_h: egen mean_start_young_daughters = mean(start_young_daughter)
bysort idp_h: egen mean_start_young_children = mean(start_young_child)
bysort idp_h: egen mean_start_young_men = mean(start_young_man)
bysort idp_h: egen mean_start_young_women = mean(start_young_woman)
bysort idp_h: egen mean_start_young_all = mean(start_young_all)

* For start_asist - Children (<18)
bysort idp_h: egen mean_start_boys = mean(start_boy)
bysort idp_h: egen mean_start_girls = mean(start_girl)
bysort idp_h: egen mean_start_kids = mean(start_kid)

* For stopped_asist - Young adults (18-24)
bysort idp_h: egen mean_stopped_young_sons = mean(stopped_young_son)
bysort idp_h: egen mean_stopped_young_daughters = mean(stopped_young_daughter)
bysort idp_h: egen mean_stopped_young_children = mean(stopped_young_child)
bysort idp_h: egen mean_stopped_young_men = mean(stopped_young_man)
bysort idp_h: egen mean_stopped_young_women = mean(stopped_young_woman)
bysort idp_h: egen mean_stopped_young_all = mean(stopped_young_all)

* For stopped_asist - Children (<18)
bysort idp_h: egen mean_stopped_boys = mean(stopped_boy)
bysort idp_h: egen mean_stopped_girls = mean(stopped_girl)
bysort idp_h: egen mean_stopped_kids = mean(stopped_kid)

* Create household-level control: proportion male
bysort idp_h: egen prop_male = mean(hombre)
label var prop_male "Proportion of young household members who are male"

* CREATE ELIGIBILITY DUMMY BASED ON THRESHOLD
gen pc_elegibilidad_2020 = (ipcf >= 522.5)
label var pc_elegibilidad_2020 "Eligible for Auxilio Emergencial (income >= 522.5)"

/*==============================================================================
COLLAPSE TO HOUSEHOLD LEVEL
==============================================================================*/

noi di "=== COLLAPSING TO HOUSEHOLD LEVEL ==="

collapse (first) running_var pc_elegibilidad_2020 region_num nivedu_hh miembros urbano prop_male ///
         mean_start_young_sons mean_start_young_daughters mean_start_young_children ///
         mean_start_young_men mean_start_young_women mean_start_young_all ///
         mean_start_boys mean_start_girls mean_start_kids ///
         mean_stopped_young_sons mean_stopped_young_daughters mean_stopped_young_children ///
         mean_stopped_young_men mean_stopped_young_women mean_stopped_young_all ///
         mean_stopped_boys mean_stopped_girls mean_stopped_kids, ///
         by(idp_h)

noi di "Total households in dataset: " _N

/*==============================================================================
DEFINE ANALYSIS GROUPS AND OUTCOMES
==============================================================================*/

* Group numbers (now includes all 9 groups)
local group_nums "01 02 03 04 05 06 07 08 09"

* Outcome types
local outcome_types "start stopped"

* Outcome variables for start_asist
local outcome_start_01 "mean_start_young_sons"
local outcome_start_02 "mean_start_young_daughters"
local outcome_start_03 "mean_start_young_children"
local outcome_start_04 "mean_start_young_men"
local outcome_start_05 "mean_start_young_women"
local outcome_start_06 "mean_start_young_all"
local outcome_start_07 "mean_start_boys"
local outcome_start_08 "mean_start_girls"
local outcome_start_09 "mean_start_kids"

* Outcome variables for stopped_asist
local outcome_stopped_01 "mean_stopped_young_sons"
local outcome_stopped_02 "mean_stopped_young_daughters"
local outcome_stopped_03 "mean_stopped_young_children"
local outcome_stopped_04 "mean_stopped_young_men"
local outcome_stopped_05 "mean_stopped_young_women"
local outcome_stopped_06 "mean_stopped_young_all"
local outcome_stopped_07 "mean_stopped_boys"
local outcome_stopped_08 "mean_stopped_girls"
local outcome_stopped_09 "mean_stopped_kids"

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
MAIN ANALYSIS LOOP - ALL GROUPS AND OUTCOMES (p=1 ONLY)
==============================================================================*/

local p_order = 1

foreach outcome_type of local outcome_types {
    
    noi di ""
    noi di "==============================================================================="
    noi di "=== ANALYZING OUTCOME: `outcome_type'_asist ==="
    noi di "==============================================================================="
    
    foreach group_num of local group_nums {
        local group_name = "`name_`group_num''"
        local outcome = "`outcome_`outcome_type'_`group_num''"
        
        noi di ""
        noi di "==============================================================================="
        noi di "=== GROUP `group_num': `desc_`group_num'' ==="
        noi di "==============================================================================="
        
        * Check sample size
        count if !missing(`outcome')
        local sample_size = r(N)
        noi di "Households with data for this group: `sample_size'"
        
        if `sample_size' < 50 {
            noi di "WARNING: Small sample size (`sample_size'), skipping this group"
            continue
        }
        
        /*======================================================================
        PREPARE EXCEL OUTPUT FILE (p=1 only)
        ======================================================================*/
        
        local excel_file "`group_num'_HH_RD_`group_name'_`outcome_type'_asist_panel.xlsx"
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
        
        /*==================================================================
        RD ESTIMATIONS - 10 SPECIFICATIONS
        ==================================================================*/
        
        local col_letters "B C D E F G H I J K"
        local col_count = 0
        
        forvalues reg_num = 1/10 {
            local ++col_count
            local col_letter : word `col_count' of `col_letters'
            
            noi di "--- Running Regression `reg_num' (p=`p_order') ---"
            
            * Determine specification
            local use_controls = 0
            local use_clustering = 0
            local use_education = 0
            local use_msetwo = 0
            
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
            
            * Build rdrobust command with polynomial order
            local rd_cmd "rdrobust `outcome' running_var, c(0) all p(`p_order')"
            
            * Add bandwidth selector
            if `use_msetwo' == 1 {
                local rd_cmd "`rd_cmd' bwselect(msetwo)"
            }
            
            * Add clustering
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
                noi di "ERROR in regression `reg_num' (p=`p_order'): `rd_cmd'"
                continue
            }
            
            * Store results
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
            
            * Calculate means
            capture {
                sum `outcome' if running_var < `hbc_right' & running_var > 0
                local mean_ab_bc = r(mean)
                sum `outcome' if running_var > -`hbc_left' & running_var < 0
                local mean_be_bc = r(mean)
                sum `outcome' if running_var < `h_right' & running_var > 0
                local mean_ab_conv = r(mean)
                sum `outcome' if running_var > -`h_left' & running_var < 0
                local mean_be_conv = r(mean)
            }
            if _rc != 0 {
                local mean_ab_bc = .
                local mean_be_bc = .
                local mean_ab_conv = .
                local mean_be_conv = .
            }
            
            * Export to Excel
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
                noi di "WARNING: Excel export failed for regression `reg_num'"
            }
        }
        
        noi di "Excel file completed: `excel_file'"
        noi di ""
        noi di "=== GROUP `group_num' COMPLETED ==="
        noi di "Households with data: `sample_size'"
        noi di ""
        
    } // End group loop
    
} // End outcome type loop

/*==============================================================================
FINAL SUMMARY
==============================================================================*/

noi di ""
noi di "============================================================================="
noi di "=== ALL HOUSEHOLD-LEVEL PANEL RD ANALYSES COMPLETED ==="
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
noi di "Outcomes analyzed:"
noi di "  - start_asist: Started attending school"
noi di "  - stopped_asist: Stopped attending school"
noi di ""
noi di "Polynomial order: p=1 (linear) only"
noi di "Total Excel files created: 18 (9 groups × 2 outcomes × 1 polynomial order)"
noi di ""
noi di "File naming convention:"
noi di "  ##_HH_RD_[groupname]_[outcome]_asist_panel.xlsx"
noi di ""
noi di "Each analysis uses household-level transition means as outcomes"
noi di "Controls: miembros, urbano, prop_male, nivedu_hh (when specified)"
noi di "Clustering: region_num (when specified)"
noi di ""