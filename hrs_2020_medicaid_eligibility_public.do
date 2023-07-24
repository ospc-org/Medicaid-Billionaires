* the followinf STATA code uses the 2020 HRS RAND Longitudinal data to produce
* estimates found in "Billionaires on Medicaid"
clear all

* SET CURRENT DIRECTORY TO LOCATION OF HRS DATA
cd ""

* Longitudinal file
use "Data\Raw\randhrs1992_2020v1.dta", clear
sort hhidpn

keep if r15iwstat == 1 // drop missing observations for 2020
keep hhidpn ragender r15* h15* s15* *wt* r*dcbal* // drop variables for other waves

* look at just 65+ and disabled population
keep if r15agey_b >= 65 | r15issdi > 0 // disability strictly defined

* generate age group buckets
recode r15agey_b min/64 = 999 65/69 = 0 70/74 = 1 75/79 = 2 80/84=3 85/89=4 90/94 = 5 95/max = 6, gen(agegrp)

* clean some variable names
rename h15atran tot_veh_val
* don't double-count mobile homes
replace tot_veh_val = 0 if tot_veh_val == h15atoth & h15atoth > 0 
* Ignore vehicle/business values greater than $30,000
replace tot_veh_val = 0 if tot_veh_val > 30000
replace h15absns = 0 if h15absns > 30000

* Compute total retirement assets - defined controbution plan plus IRA
gen tot_dc_holdings = r15dcbal1
replace tot_dc_holdings = 0 if missing(tot_dc_holdings)

gen n_tot_ret_assets = tot_dc_holdings + h15aira

* Winsorize variables of interest
winsor h15atotb, p(0.005) gen(tot_assets)
winsor h15atoth, p(0.005) gen(tot_prim_res)
winsor h15itot, p(0.005) gen(tot_hh_inc)
winsor h15atotn, p(0.005) gen(tot_no_h_assets)
winsor n_tot_ret_assets, p(0.005) gen(tot_ret_assets)
winsor h15absns, p(0.005) gen(tot_bus_assets)

* Income test - In-home care
gen in_home_income_test = 0
replace in_home_income_test = 1 if tot_hh_inc < 20000 & r15mstat > 2
replace in_home_income_test = 1 if tot_hh_inc < 70000 & r15mstat <= 2

* Income test - nursing home care
gen nh_income_test = 0
replace nh_income_test = 1 if tot_hh_inc < 100000 & r15mstat > 2
replace nh_income_test = 1 if tot_hh_inc < 200000 & r15mstat <= 2

******* Most restrictive
* Asset test
gen most_restrictive_asset = 0
replace most_restrictive_asset = 1 if tot_assets < 2000 & r15mstat > 2
replace most_restrictive_asset = 1 if tot_assets < 3000 & r15mstat <= 2

* Combined tests
gen ih_most_restrictive_ovr = (most_restrictive_asset == 1 & in_home_income_test == 1)
gen nh_most_restrictive_ovr = (most_restrictive_asset == 1 & nh_income_test == 1)

******* More restrictive - subtract some housing
* Qualify for residential deduction?
gen mr_prim_res_ind = (tot_prim_res < 585000)
gen new_asset_tot_mr = tot_assets - mr_prim_res_ind*tot_prim_res - tot_veh_val - tot_bus_assets

gen more_restrictive_asset = (new_asset_tot_mr < 2000) & (r15mstat > 2) | (new_asset_tot_mr < 3000) & (r15mstat <= 2)

* Combined test
gen ih_more_restrictive_ovr = (more_restrictive_asset == 1 & in_home_income_test == 1)
gen nh_more_restrictive_ovr = (more_restrictive_asset == 1 & nh_income_test == 1)

******* Less restrictive - subtract more housing & retirement assets
gen lr_prim_res_ind = (tot_prim_res < 878000)
gen new_asset_tot_lr = tot_assets - lr_prim_res_ind*tot_prim_res - tot_ret_assets - tot_veh_val - tot_bus_assets
gen less_restrictive_asset = (new_asset_tot_lr < 2000) & (r15mstat > 2) | (new_asset_tot_lr < 3000) & (r15mstat <= 2)

* Combined test
gen ih_less_restrictive_ovr = (less_restrictive_asset == 1 & in_home_income_test == 1)
gen nh_less_restrictive_ovr = (less_restrictive_asset == 1 & nh_income_test == 1)

******* Very liberal
gen new_asset_tot_vl = tot_assets - tot_prim_res - tot_ret_assets - tot_veh_val - tot_bus_assets
gen more_liberal_asset = (new_asset_tot_vl < 2000) & (r15mstat > 2) | (new_asset_tot_vl < 3000) & (r15mstat <= 2)

* Combined test
gen ih_more_liberal_ovr = (more_liberal_asset == 1 & in_home_income_test == 1)
gen nh_more_liberal_ovr = (more_liberal_asset == 1 & nh_income_test == 1)

******* California liberal
gen cali_liberal_asset = (new_asset_tot_vl < 130000) & (r15mstat > 2) | (new_asset_tot_vl < 195000) & (r15mstat <= 2)

* Combined test
gen ih_cali_liberal_ovr = (cali_liberal_asset == 1 & in_home_income_test == 1)
gen nh_cali_liberal_ovr = (cali_liberal_asset == 1 & nh_income_test == 1)

*********************
* Now, compute shares of population which qualify (using weights)
* in-home test
tab ih_most_restrictive_ovr [aweight = r14wtcrnh]
tab ih_more_restrictive_ovr [aweight = r14wtcrnh]
tab ih_less_restrictive_ovr [aweight = r14wtcrnh]
tab ih_more_liberal_ovr [aweight = r14wtcrnh]
tab ih_cali_liberal_ovr [aweight = r14wtcrnh]
tab in_home_income_test [aweight = r14wtcrnh]

tab nh_most_restrictive_ovr [aweight = r14wtcrnh]
tab nh_more_restrictive_ovr [aweight = r14wtcrnh]
tab nh_less_restrictive_ovr [aweight = r14wtcrnh]
tab nh_more_liberal_ovr [aweight = r14wtcrnh]
tab nh_cali_liberal_ovr [aweight = r14wtcrnh]
tab nh_income_test [aweight = r14wtcrnh]

* Compute shares by age group and gender
bysort agegrp ragender: asgen most_restrictive_numerator = nh_most_restrictive_ovr
bysort agegrp ragender: egen most_restrictive = pc(nh_most_restrictive_ovr) [aweight = r14wtcrnh]

bysort ragender agegrp: tab nh_most_restrictive_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab nh_more_restrictive_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab nh_less_restrictive_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab nh_more_liberal_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab nh_cali_liberal_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab nh_income_test [aweight = r14wtcrnh]

bysort ragender agegrp: tab ih_most_restrictive_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab ih_more_restrictive_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab ih_less_restrictive_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab ih_more_liberal_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab ih_cali_liberal_ovr [aweight = r14wtcrnh]
bysort ragender agegrp: tab in_home_income_test [aweight = r14wtcrnh]