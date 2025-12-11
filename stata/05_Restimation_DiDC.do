/*******************************************************************************
HOUSEHOLD-LEVEL PANEL DIFFERENCE-IN-DISCONTINUITIES (DiDC) ANALYSIS
Author: Luis Castellanos

APPROACH: RDD of Differences (Panel Data) - Uses household-level mean 
          of Delta Attendance as outcome and Delta Household Income as RV.
*******************************************************************************/

clear all
set more off
cls

/*==============================================================================
PATH SETUP
==============================================================================*/

global project_dir "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Transfers causal analysis BRA"
* OUTPUT DIRECTORIES FOR HOUSEHOLD DiDC APPROACH
global docs_dir_hh "$project_dir\Outputs\01_New_Outputs_2025\Docs\25_DiDC_HH"
global excel_dir_hh "$project_dir\Outputs\01_New_Outputs_2025\Excel\RD_results\25_DiDC_HH"

* Create directories
capture mkdir "`docs_dir_hh'"
capture mkdir "`excel_dir_hh'"

noi di ""
noi di "==============================================================================="
noi di "=== HOUSEHOLD-LEVEL PANEL DIFFERENCE-IN-DISCONTINUITIES ANALYSIS ==="
noi di "=== RDD of Differences (Delta Income as RV) ==="
noi di "==============================================================================="
noi di ""

/*==============================================================================
DEFINE THRESHOLDS FOR EACH PERIOD
==============================================================================*/
local threshold_2019 = 499
local threshold_2020 = 522.5
noi di "Thresholds defined:"
noi di "  2019: `threshold_2019'"
noi di "  2020: `threshold_2020'"
noi di ""

/*==============================================================================
DEFINE GROUPS AND SPECIFICATIONS (REPLICATING CODE 1 STRUCTURE)
==============================================================================*/
* Group numbers (all 9 groups from Code 1)
local group_nums "01 02 03 04 05 06 07 08 09"

* Group names for file naming
local name_01 "young_sons"      // was "young_sons_18-24"
local name_02 "young_daughters" // was "young_daughters_18-24"
local name_03 "young_children"  // was "young_children_18-24"
local name_04 "young_men"       // was "young_men_18-24"
local name_05 "young_women"     // was "young_women_18-24"
local name_06 "young_all"       // was "young_all_18-24"
local name_07 "boys"            // was "boys_under18"
local name_08 "girls"           // was "girls_under18"
local name_09 "kids"            // was "kids_under18"

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

* Control variables (same as Code 1 - household level)
local controls "miembros urbano prop_male"
local controls_edu "`controls' nivedu_hh"

/*==============================================================================
DATA LOADING AND PANEL PREPARATION (REPLICATING CODE 1)
==============================================================================*/

noi di "=== LOADING AND PREPARING PANEL DATA ==="
use "$project_dir\Data\03_SEDLAC_panel_2019-2020_with_single_mother.dta", clear
noi di "Initial observations: " _N

* Keep only panel individuals
keep if idp_match_individual == 1
noi di "After keeping panel individuals: " _N

/*------------------------------------------------------------------------------
STEP 1: Extract 2019 Outcomes and Income (Same Logic as Code 1)
------------------------------------------------------------------------------*/
noi di ""
noi di "Step 1: Extracting 2019 outcomes and income..."

* Keep relevant variables
keep idp_i idp_h ano trimestre ipcf asiste ///
     hh_madre_soltera_rob_2020 edad hombre jefe hijo miembros urbano ///
     nivedu region_est1 cohh idp_match_individual

sort idp_i ano trimestre

* For 2019: Take any observation
preserve
keep if ano == 2019
bysort idp_i: keep if _n == 1
foreach var in asiste ipcf {
    rename `var' `var'_2019
}
keep idp_i asiste_2019 ipcf_2019
tempfile data_2019
save `data_2019'
noi di "  2019 data extracted: " _N " individuals"
restore

/*------------------------------------------------------------------------------
STEP 2: Prepare 2020 Data and Merge with 2019 (Same Logic as Code 1)
------------------------------------------------------------------------------*/
noi di ""
noi di "Step 2: Preparing 2020 data and merging..."

* For 2020: Keep Q2-Q4 observations
keep if ano == 2020 & inlist(trimestre, 2, 3, 4)

* Apply income consistency filter
keep if cohh == 1

* Apply single mother household filter (required for Code 1 groups)
keep if hh_madre_soltera_rob_2020 == 1

* Merge with 2019 data
merge m:1 idp_i using `data_2019'
keep if _merge == 3
drop _merge

noi di "  After merging 2019 and 2020 data: " _N

if _N == 0 {
    noi di as error "ERROR: No observations remain after merging years"
    exit 2000
}

/*------------------------------------------------------------------------------
STEP 3: Create Individual-Level DiD Outcome and Running Variable
------------------------------------------------------------------------------*/
noi di ""
noi di "Step 3: Creating DiD outcome (Delta Attendance) and RVs..."

* Recode missing values to zero for asiste (if needed)
replace asiste = 0 if asiste == .
replace asiste_2019 = 0 if asiste_2019 == .

* INDIVIDUAL-LEVEL DiD OUTCOME: Delta Attendance
gen delta_asiste = asiste - asiste_2019
label var delta_asiste "Change in school attendance (2020 - 2019)"

* RUNNING VARIABLE COMPONENTS (from Code 2 logic)
* Individual-level income normalized by period-specific thresholds
gen rv_2019 = ipcf_2019 - `threshold_2019'
gen rv_2020 = ipcf - `threshold_2020'

* INDIVIDUAL-LEVEL DiDC RUNNING VARIABLE: Delta RV (Difference in normalized income)
gen delta_rv = rv_2020 - rv_2019
label var delta_rv "Delta Running Variable (normalized income difference)"

/*------------------------------------------------------------------------------
STEP 4: Create Individual-Level Demographic Groups (REPLICATING CODE 1)
------------------------------------------------------------------------------*/
noi di ""
noi di "Step 4: Creating demographic group indicators..."

* Age restrictions
gen young = (edad >= 18 & edad < 25)
gen child = (edad < 18)

* Keep only young OR children for this analysis (Code 1 restriction)
keep if young == 1 | child == 1

noi di "  Observations after restricting to ages <25: " _N

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

/*------------------------------------------------------------------------------
STEP 5: Create Delta Attendance Values for Each Group
------------------------------------------------------------------------------*/
noi di ""
noi di "Step 5: Preparing Delta Attendance for each group..."

* Delta Attendance for Young adults (18-24)
gen delta_young_son = delta_asiste if is_young_son==1
gen delta_young_daughter = delta_asiste if is_young_daughter==1
gen delta_young_child = delta_asiste if is_young_child==1
gen delta_young_man = delta_asiste if is_young_man==1
gen delta_young_woman = delta_asiste if is_young_woman==1
gen delta_young_all = delta_asiste if is_young_all==1

* Delta Attendance for Children (<18)
gen delta_boy = delta_asiste if is_boy==1
gen delta_girl = delta_asiste if is_girl==1
gen delta_kid = delta_asiste if is_kid==1

/*------------------------------------------------------------------------------
STEP 6: Calculate HOUSEHOLD-LEVEL MEANS and CONTROLS
------------------------------------------------------------------------------*/
noi di ""
noi di "Step 6: Calculating household-level means and controls..."

* Household-level mean of Delta Attendance (DiD Outcome)
bysort idp_h: egen mean_delta_young_sons = mean(delta_young_son)
bysort idp_h: egen mean_delta_young_daughters = mean(delta_young_daughter)
bysort idp_h: egen mean_delta_young_children = mean(delta_young_child)
bysort idp_h: egen mean_delta_young_men = mean(delta_young_man)
bysort idp_h: egen mean_delta_young_women = mean(delta_young_woman)
bysort idp_h: egen mean_delta_young_all = mean(delta_young_all)

bysort idp_h: egen mean_delta_boys = mean(delta_boy)
bysort idp_h: egen mean_delta_girls = mean(delta_girl)
bysort idp_h: egen mean_delta_kids = mean(delta_kid)

* Household-level control: proportion male
bysort idp_h: egen prop_male = mean(hombre)
label var prop_male "Proportion of young household members who are male"

* Household education variable (using 2020 value - assumed fixed in Code 1)
bysort idp_h: egen nivedu_hh = max(nivedu * (jefe==1))

* Region numeric variable
capture drop region_num
encode(region_est1), gen(region_num)

/*------------------------------------------------------------------------------
STEP 7: Collapse to Household Level
------------------------------------------------------------------------------*/
noi di ""
noi di "Step 7: Collapsing to household level..."

* Collapse, keeping the mean DiD outcomes and one observation for shared household variables
collapse (first) delta_rv nivedu_hh miembros urbano prop_male region_num ///
         mean_delta_young_sons mean_delta_young_daughters mean_delta_young_children ///
         mean_delta_young_men mean_delta_young_women mean_delta_young_all ///
         mean_delta_boys mean_delta_girls mean_delta_kids, ///
         by(idp_h)

noi di "Total households in dataset for DiDC: " _N

/*==============================================================================
DiDC ANALYSIS LOOP (RDD of DIFFERENCES)
==============================================================================*/
noi di ""
noi di "==============================================================================="
noi di "=== STARTING DiDC ANALYSIS (RDD of Differences) ==="
noi di "==============================================================================="

local p_order = 1
local col_letters "B C D E F G H I J K"
local DiDC_rv "delta_rv"
local cutoff = 0 // The cutoff for delta_rv is 0 (0-0 = 0)
local col_count = 0

foreach group_num of local group_nums {
    local group_name = "`name_`group_num''"
    local outcome = "mean_delta_`group_name'"
    local group_desc = "`desc_`group_num''"

    noi di ""
    noi di "==============================================================================="
    noi di "=== DiDC GROUP `group_num': `group_desc' ==="
    noi di "==============================================================================="

    * Check sample size
    count if !missing(`outcome') & !missing(`DiDC_rv')
    local sample_size = r(N)
    noi di "Households with data for this group: `sample_size'"

    if `sample_size' < 50 {
        noi di "WARNING: Small sample size (`sample_size'), skipping this group"
        continue
    }

    /*--------------------------------------------------------------------------
    PREPARE EXCEL OUTPUT FILE
    --------------------------------------------------------------------------*/

    local excel_file "`group_num'_HH_DiDC_`group_name'_panel_V2.xlsx"
    local excel_path "$excel_dir_hh\\`excel_file'"

    noi di "Creating Excel file: `excel_path'"

    putexcel set "`excel_path'", replace

    * Set up column headers (Simplified to match Code 1 structure)
    putexcel A1 = "Statistic"
    putexcel B1 = "No Controls"
    putexcel C1 = "With Controls"
    putexcel D1 = "Cluster No Controls"
    putexcel E1 = "Cluster With Controls"
    putexcel F1 = "Cluster With Controls Edu"

    * Set up row labels (Only relevant subset of Code 1 labels)
    putexcel A2 = "Original N"
    putexcel A3 = "Original N left"
    putexcel A4 = "Original N right"
    putexcel A5 = "Bandwidth left (h)"
    putexcel A6 = "Bandwidth right (h)"
    putexcel A7 = "BC Bandwidth left (b)"
    putexcel A8 = "BC Bandwidth right (b)"
    putexcel A9 = "Cutoff"
    putexcel A10 = "Polynomial order (p)"
    putexcel A11 = "Conventional estimate"
    putexcel A12 = "Conventional SE"
    putexcel A13 = "BC estimate"
    putexcel A14 = "BC SE"

    /*--------------------------------------------------------------------------
    DiDC ESTIMATIONS (6 Specifications for DiDC)
    --------------------------------------------------------------------------*/
    local col_letters_DiDC "B C D E F"
    local col_count = 0

    forvalues reg_num = 1/5 {
        local ++col_count
        local col_letter : word `col_count' of `col_letters_DiDC'

        noi di "--- Running Regression `reg_num' (p=`p_order') ---"

        * Determine specification (Simplified from Code 1, keeping main types)
        local use_controls = 0
        local use_clustering = 0
        local use_education = 0

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

        * Build rdrobust command for DiDC
        local rd_cmd "rdrobust `outcome' `DiDC_rv', c(`cutoff') all p(`p_order') bwselect(mserd)"
        
        * Add clustering
        if `use_clustering' == 1 {
            local rd_cmd "`rd_cmd' vce(cluster region_num)"
        }

        * Add covariates
        if `use_controls' == 1 {
            if `use_education' == 1 {
                local rd_cmd "`rd_cmd' covs(`controls_edu')"
            }
            else {
                local rd_cmd "`rd_cmd' covs(`controls')"
            }
        }

        * Execute regression
        capture `rd_cmd'

        if _rc != 0 {
            noi di "ERROR in DiDC regression `reg_num' (p=`p_order'): `rd_cmd'"
            continue
        }

        * Store results (adapted for DiDC output)
        local orig_n = e(N)
        local orig_nl = e(N_l)
        local orig_nr = e(N_r)
        local h_left = e(h_l)
        local h_right = e(h_r)
        local hbc_left = e(b_l)
        local hbc_right = e(b_r)
        local cut = e(c)
        local poli = e(p)
        local coef_conv = e(tau_cl)
        local se_coef_conv = e(se_tau_cl)
        local coef_bc = e(tau_bc)
        local se_coef_bc = e(se_tau_rb)

        * Export to Excel
        capture {
            putexcel `col_letter'2 = `orig_n'
            putexcel `col_letter'3 = `orig_nl'
            putexcel `col_letter'4 = `orig_nr'
            putexcel `col_letter'5 = `h_left'
            putexcel `col_letter'6 = `h_right'
            putexcel `col_letter'7 = `hbc_left'
            putexcel `col_letter'8 = `hbc_right'
            putexcel `col_letter'9 = `cut'
            putexcel `col_letter'10 = `poli'
            putexcel `col_letter'11 = `coef_conv'
            putexcel `col_letter'12 = `se_coef_conv'
            putexcel `col_letter'13 = `coef_bc'
            putexcel `col_letter'14 = `se_coef_bc'
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

/*==============================================================================
FINAL SUMMARY
==============================================================================*/

noi di ""
noi di "============================================================================="
noi di "=== ALL HOUSEHOLD-LEVEL PANEL DiDC ANALYSES COMPLETED ==="
noi di "============================================================================="
noi di "Output location: $excel_dir_hh"
noi di ""
noi di "Groups analyzed in single-mother households (Delta Attendance):"
noi di "  Ages 18-24: 01-06"
noi di "  Ages <18: 07-09"
noi di ""
noi di "RDD of Differences (DiDC) Approach used."
noi di "Running Variable: Delta Household Income (normalized by period-specific thresholds)"
noi di ""