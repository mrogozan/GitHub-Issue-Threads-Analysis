*****************************************  DATA PREPARATION  *********************************************
clear
set more off

*************If you want to run this, replace with your own path to short_comment_issue_merge.dta below*****************
use "C:\Users\20200604\OneDrive - TU Eindhoven\Documents\Master\year2\Untitled Folder\short_comment_issue_merge.dta"
**************************************************************************************************************
encode consequence_category, generate(cat_consequence_category)
encode tbdf, generate(cat_tbdf)
encode trigger, generate(cat_trigger)
encode target, generate(cat_target)
drop consequence_category tbdf trigger target
rename cat_consequence_category consequence_category
rename cat_tbdf comment_label
rename cat_trigger trigger
rename cat_target target

generate positive_consequence = 0
replace positive_consequence = 1 if consequence_category == 4

generate negative_consequence = 0
replace negative_consequence = 1 if consequence_category == 2

generate neutral_consequence = 0
replace neutral_consequence = 1 if consequence_category == 3

generate balanced_consequence = 0
replace balanced_consequence = 1 if consequence_category == 1



*****************************************  ASSUMPTION CHECKING  *********************************************


/*

mlogit consequence_category i.comment_label i.trigger i.target



logit positive_consequence i.comment_label i.trigger i.target, cluster(issue_id) 
vif, uncentered
logit negative_consequence i.comment_label i.trigger i.target, cluster(issue_id) 
estat classification
estat gof, group(10)
logit neutral_consequence i.comment_label i.trigger i.target, cluster(issue_id) 

logit balanced_consequence i.comment_label i.trigger i.target, cluster(issue_id) 



xtset issue_id
xtlogit positive_consequence i.comment_label i.trigger i.target, re
scalar intercept_variance = -1628.657 
scalar ICC = intercept_variance / (intercept_variance + (3.14159^2 / 3))
display ICC

xtset issue_id
xtlogit negative_consequence i.comment_label i.trigger i.target, re
predict xb_hat, xb
histogram xb_hat, normal
swilk xb_hat
sktest xb_hat

gsem (positive_consequence <- i.comment_label i.trigger i.target), ///
    logit latent(issue_id)
vif, uncentered

bysort issue_id (positive_consequence): gen obs_per_cluster = _N
tabulate obs_per_cluster

*/
*****************************************  RQ1  *********************************************
gsem (positive_consequence <- i.comment_label i.trigger i.target), ///
    logit latent(issue_id) // Log likelihood = -2626.7759
scalar ll_full = e(ll)
gsem (positive_consequence <- ), logit latent(issue_id)
scalar ll_null = e(ll)
scalar r2_mcfadden = 1 - (ll_full / ll_null)
display "McFadden's Pseudo-R²: " r2_mcfadden //.1229


gsem (negative_consequence <- i.comment_label i.trigger i.target), ///
    logit latent(issue_id) // Log likelihood = -2658.106
	
scalar ll_full = e(ll)
gsem (negative_consequence <- ), logit latent(issue_id)
scalar ll_null = e(ll)
scalar r2_mcfadden = 1 - (ll_full / ll_null)
display "McFadden's Pseudo-R²: " r2_mcfadden //.1206
	
gsem (neutral_consequence <- i.comment_label i.trigger i.target), ///
    logit latent(issue_id) // Log likelihood = -3098.6712
	
scalar ll_full = e(ll)
gsem (neutral_consequence <- ), logit latent(issue_id)
scalar ll_null = e(ll)
scalar r2_mcfadden = 1 - (ll_full / ll_null)
display "McFadden's Pseudo-R²: " r2_mcfadden //.2466
	
/*	
gsem (balanced_consequence <- i.comment_label i.trigger i.target), ///
    logit latent(issue_id)
*/


*****************************************  RQ2  *********************************************


*****************************************within issue corr*****************************************

preserve
gen cramer_v_trigger = .
gen cramer_v_target = .
levelsof issue_id, local(issue_list)

foreach issue in `issue_list' {
    
    keep if issue_id == `issue'

    * Compute Cramér's V for comment_label and trigger
    scalar chi2_trigger = r(chi2)
    scalar n_trigger = r(N)
    scalar min_dim_trigger = min(r(r), r(c))
    replace cramer_v_trigger = sqrt(chi2_trigger / (n_trigger * (min_dim_trigger - 1))) if issue_id == `issue'

    * Compute Cramér's V for comment_label and target

    scalar chi2_target = r(chi2)
    scalar n_target = r(N)
    scalar min_dim_target = min(r(r), r(c))
    replace cramer_v_target = sqrt(chi2_target / (n_target * (min_dim_target - 1))) if issue_id == `issue'

    
}

summarize cramer_v_trigger cramer_v_target
restore




*****************************************between issue corr*****************************************
preserve
* Collapse data to issue level
collapse (first) trigger target, by(issue_id)

* Generate a contingency table and compute chi-squared statistic
tabulate trigger target, cchi2 V


*****************************************interaction effects in gsem*****************************************
restore

gsem (positive_consequence <- i.comment_label 5.trigger 6.trigger 1.trigger##4.target 2.trigger##1.target 3.trigger##3.target  4.trigger##4.target 7.trigger##1.target 8.trigger##5.target), logit latent(issue_id)

gsem (negative_consequence <- i.comment_label 5.trigger 6.trigger 1.trigger##4.target 2.trigger##1.target 3.trigger##3.target  4.trigger##4.target 7.trigger##1.target 8.trigger##5.target), logit latent(issue_id)

gsem (neutral_consequence <- i.comment_label 5.trigger 6.trigger 1.trigger##4.target 2.trigger##1.target 3.trigger##3.target  4.trigger##4.target 7.trigger##1.target 8.trigger##5.target), logit latent(issue_id)

sum comment_label
sum trigger
sum target