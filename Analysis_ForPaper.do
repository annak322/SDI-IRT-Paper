/*
Author: Anna Konstantinova
Last edited: June 25th, 2018
*/

*****************************************************************************
* Initializing
*****************************************************************************

	global root "/Users/bbdaniels/GitHub/SDI-IRT-Paper/SDI-Paper"
	global data "$root/data"
	global figures "$root/figures"
	global tables "$root/tables"
	global gphfiles "C:\Users\annak\Box Sync\WB Work\DECRG Github Repo\outputs\figs\CrossCountry\All10\TheGraphs"
	global recodeAfter = "0"

*****************************************************************************
/* Regression results using wide data */
*****************************************************************************

	use "$data/SDI_Vignette_IRT_Analysis_AfterTo${recodeAfter}_withAge.dta", clear

	areg percent_correctd, a(country)
	areg percent_correctd comp_mle, a(country)
	margins, at(comp_mle=(-2.12651  -0.66355 0.621643 1.58764))

	areg percent_correctt, a(country)
	areg percent_correctt comp_mle, a(country)
	margins, at(comp_mle=(-2.12651  -0.66355 0.621643 1.58764))

	areg percent_antibiotict, a(country)
	areg percent_antibiotict comp_mle, a(country)
	margins, at(comp_mle=(-2.12651  -0.66355 0.621643 1.58764))

	eststo clear

	replace provider_cadre=. if provider_cadre==1
	replace provider_agedisc = . if provider_agedisc>=75 | provider_agedisc<15
	gen countryfac_id = country + "_" + facility_id
	areg comp_mle i.provider_cadre provider_agedisc i.facility_level i.ruralurban i.publicprivate, ab(country) cluster(survey_id)
	eststo comp_mle1
	areg comp_mle i.provider_cadre provider_agedisc , ab(countryfac_id) cluster(survey_id)
	eststo comp_mle2
	areg comp_mle i.provider_cadre provider_agedisc , ab(survey_id) cluster(survey_id)

*****************************************************************************
/* Regression results using long data */
*****************************************************************************

	use "$data/SDI_Vignette_IRT_Analysis_AfterTo${recodeAfter}_withAge.dta", clear
	replace provider_agedisc = . if provider_agedisc>=75 | provider_agedisc<15
	replace provider_cadre=. if provider_cadre==1
	gen countryfac_id = country + "_" + facility_id

	keep country survey_id countryfac_id *_antibiotic *_correctt provider_cadre provider_agedisc facility_level ruralurban publicprivate
	drop pneumonia_treat_other_antibiotic num_correctt percent_correctt

	rename *_correctt correctt_*
	rename *_antibiotic antibiotic_*

	reshape long correctt antibiotic , i(survey_id) j(disease) string
	replace disease = subinstr(disease, "_", "", .)

	replace antibiotic = 1 if antibiotic==100
	replace correctt = 1 if correctt==100

	areg antibiotic i.provider_cadre provider_agedisc i.facility_level i.ruralurban i.publicprivate, ab(country) cluster(survey_id)
	eststo antibiotic1
	areg antibiotic i.provider_cadre provider_agedisc , ab(countryfac_id) cluster(survey_id)
	eststo antibiotic2
	areg antibiotic i.provider_cadre provider_agedisc , ab(survey_id) cluster(survey_id)
	eststo antibiotic3

	areg correctt i.provider_cadre provider_agedisc i.facility_level i.ruralurban i.publicprivate, ab(country) cluster(survey_id)
	eststo correctt1
	areg correctt i.provider_cadre provider_agedisc , ab(countryfac_id) cluster(survey_id)
	eststo correctt2
	areg correctt i.provider_cadre provider_agedisc , ab(survey_id) cluster(survey_id)
	eststo correctt3

*****************************************************************************
/* Output regression results */
*****************************************************************************

	esttab comp_mle1 comp_mle2 ///
		correctt1 correctt2 correctt3 ///
		antibiotic1 antibiotic2 antibiotic3 ///
		using "$tables\Regression_Results.csv", replace ///
		stats(N r2,  fmt(0 3) labels("Observations" "R2")) ///
		csv label se(3) collabels(none) ///
		nodepvars  star( * 0.1 ** 0.05 *** 0.01)

*****************************************************************************
/* Figures using wide data */
*****************************************************************************

	use "$data/SDI_Vignette_IRT_Analysis_AfterTo${recodeAfter}_withAge.dta", clear

	regress percent_correctt comp_mle
	margins, at(comp_mle = (-3(1)3))
		cap mat drop theResults
		mat theResults = r(b)
		mat theResults = theResults'
	forvalues i = 1/7 {
		local treat`i' = round(theResults[`i',1], 1)
		if `treat`i'' > 100 local treat`i' = 100
		if `treat`i'' < 0 local treat`i' = 0
	}

	regress percent_correctd comp_mle
	margins, at(comp_mle = (-3(1)3))
		cap mat drop theResults
		mat theResults = r(b)
		mat theResults = theResults'
	forvalues i = 1/7 {
		local diag`i' = round(theResults[`i',1], 1)
		if `diag`i'' > 100 local diag`i' = 100
		if `diag`i'' < 0 local diag`i' = 0
	}

	regress percent_antibiotict comp_mle
	margins, at(comp_mle = (-3(1)3))
		cap mat drop theResults
		mat theResults = r(b)
		mat theResults = theResults'
	forvalues i = 1/7 {
		local ab`i' = round(theResults[`i',1], 1)
		if `ab`i'' > 100 local ab`i' = 100
		if `ab`i'' < 0 local ab`i' = 0
	}

	local outcome_axis " -5 `" "Knowledge Score" " " "Correct Diagnoses" " " "Correct Treatment" " " "Inapprop. Antibiotics" "' -3 `" "-3" " " "`diag1'%" " " "`treat1'%" " " "`ab1'%" "' -2 `" "-2" " " "`diag2'%" " " "`treat2'%" " " "`ab2'%" "' -1 `" "-1" " " "`diag3'%" " " "`treat3'%" " " "`ab3'%" "' 0 `" "0" " " "`diag4'%" " " "`treat4'%" " " "`ab4'%" "' 1 `" "1" " " "`diag5'%" " " "`treat5'%" " " "`ab5'%" "' 2 `" "2" " " "`diag6'%" " " "`treat6'%" " " "`ab6'%" "' 3 `" "3" " " "`diag7'%" " " "`treat7'%" " " "`ab7'%" "' "

	egen med = median(comp_mle)
	egen lqt = pctile(comp_mle), p(25)
	egen uqt = pctile(comp_mle), p(75)
	egen ls = pctile(comp_mle), p(5)
	egen us = pctile(comp_mle), p(95)
	gen height = 1

	graph twoway ///
		(histogram comp_mle, yaxis(2) start(-5) width(0.25) color(gs13)) ///
		(lowess percent_correctd comp_mle if comp_mle<3, yaxis(1) lcolor(purple) lwidth(1) legend(label(2 "Conditions Diagnosed Correctly"))) ///
		(lowess percent_correctt comp_mle if comp_mle<3, yaxis(1) lcolor(midblue) lwidth(1) legend(label(3 "Conditions Treated Correctly"))) ///
		(lowess percent_antibiotict comp_mle if comp_mle<3, yaxis(1) lcolor(dkorange) lwidth(1) legend(label(4 "Conditions Given Inappropriate Antibiotics"))) ///
		(rbar lqt med height, horizontal barwidth(10) fcolor(none) lcolor(black) lwidth(0.5)) ///
		(rbar uqt med height, horizontal barwidth(10) fcolor(none) lcolor(black) lwidth(0.5)) ///
		(rspike lqt ls height, horizontal lcolor(black) lwidth(0.5)) ///
		(rspike uqt us height, horizontal lcolor(black) lwidth(0.5)) ///
		(rcap us us height, horizontal lcolor(black) lwidth(0.5) msize(large)) ///
		(rcap ls ls height, horizontal lcolor(black) lwidth(0.5) msize(large)) ///
		, ///
		xtitle("") xscale(range(-5 3) noli titlegap(2)) ///
		ytitle("", axis(1)) yscale(alt) yscale(noli axis(1)) yscale(axis(2) off)  ///
		ylabel(0 "0%" 20 "20%" 40 "40%" 60 "60%" 80 "80%" 100 "100%", angle(0) nogrid axis(1)) ///
		xlabel(`outcome_axis', labsize(small)) ///
		note(`" "Correct treatment" include all vignettes except pregnancy and pre-eclampsia."' ///
			`" "Correct diagnoses" include all vignettes. "' ///
			`" "Prescribed inappropriate antibiotics" include diarrhea+dehydration, pneumonia, tuberculosis, pelvic inflammatory disease vignettes."', size(vsmall)) ///
 		legend(order(2 3 4) size(small) symy(2) symx(4) pos(11) ring(0) cols(1)) graphregion(color(white)) legend(region(lc(none) fc(none))) bgcolor(white)
		graph export "$figures/Histogram.png", width(2000) replace

	preserve
		replace provider_cadre=. if provider_cadre==1
		graph box comp_mle, ///
			over(provider_cadre, reverse axis(noli) label(nolabel)) ///
			over(country, axis(noli) label(labsize(small))) ///
			noout box(1, fcolor(none) lcolor(navy*0.6)) ///
			box(2, fcolor(none) lcolor(navy*0.9)) ///
			box(3, fcolor(none) lcolor(navy*1.3)) ///
			title(, size(medium) justification(left) color(black) span pos(11)) ///
			graphregion(color(white)) ytitle(, placement(left) justification(left)) ylabel(, angle(0) nogrid) ///
			legend(label(3 "Doctor") label(2 "Clinical Officer") label(1 "Nurse") order(3 2 1) pos(1) ring(0) cols(1) region(lwidth(0.2) fc(none)) symx(4) symy(2) size(vsmall)) ///
			yscale(range(-3 3) titlegap(2)) bgcolor(white) asyvars showyvars horizontal ///
			ylabel(-3 "-3" -1 "-1" 0 "0" 1 "1" 3 "3" , labsize(small)) ///
			ytitle("Provider knowledge score {&rarr} ", size(small)) note("") allcategories
			graph export "$figures/Provider_Cadre_Box.png", width(2000) replace
	restore

	graph box comp_mle, ///
		over(country, sort(1) descending axis(noli) ///
		relabel(1 "Kenya 2012" 2 "Madagascar 2016" 3 "Mozambique 2014" 4 "Niger 2015" 5 "Nigeria 2013" ///
			6 "Senegal 2010" 7 "Tanzania 2014" 8 "Tanzania 2016" 9 "Togo 2013" 10 "Uganda 2013") label(labsize(small))) ///
		box(1, fcolor(none) lcolor(navy) lwidth(0.4)) marker(1, msize(vsmall) mcolor(navy)) ///
		box(2, fcolor(none) lcolor(cranberry) lwidth(0.4)) marker(2, msize(vsmall) mcolor(cranberry)) ///
		box(3, fcolor(none) lcolor(gold*1.2) lwidth(0.4)) marker(3, msize(vsmall) mcolor(gold*1.2)) ///
		box(4, fcolor(none) lcolor(purple) lwidth(0.4)) marker(4, msize(vsmall) mcolor(purple)) ///
		box(5, fcolor(none) lcolor(chocolate) lwidth(0.4)) marker(5, msize(vsmall) mcolor(chocolate)) ///
		box(6, fcolor(none) lcolor(orange) lwidth(0.4)) marker(6, msize(vsmall) mcolor(orange)) ///
		box(7, fcolor(none) lcolor(midgreen) lwidth(0.4)) marker(7, msize(vsmall) mcolor(midgreen)) ///
		box(8, fcolor(none) lcolor(midblue) lwidth(0.4)) marker(8, msize(vsmall) mcolor(midblue)) ///
		box(9, fcolor(none) lcolor(emerald) lwidth(0.4)) marker(9, msize(vsmall) mcolor(emerald)) ///
		box(10, fcolor(none) lcolor(lavender) lwidth(0.4)) marker(10, msize(vsmall) mcolor(lavender)) ///
		yline(0, lwidth(0.3) lcolor(gs12) lpattern(dash)) ///
		ylabel(-5(1)5, labsize(small) angle(0) nogrid) ///
		ytitle("Provider's knowledge score {&rarr}", placement(left) justification(left) size(small)) ///
		legend(off) yscale(range(-5 5) titlegap(2)) bgcolor(white) graphregion(color(white)) asyvars showyvars horizontal
 		graph export "$figures/BoxPlot.png", width(2000) replace

	tw  (scatter percent_correctt comp_mle, jitter(10) m(x) mc(navy%10) lw(thick)) ///
		(scatter percent_antibiotict comp_mle, jitter(10) m(x) mc(maroon%10) lw(thick)) ///
		(lpoly percent_correctt comp_mle, degree(1) lw(thick) lcolor(navy)) ///
		(lpoly percent_antibiotict comp_mle, degree(1) lw(thick) lcolor(maroon)) ///
		, ///
		graphregion(color(white)) ///
		legend(order(3 4) pos(5) region(lc(none) fc(none)) label(3 "Treated Condition Correctly") label(4 "Gave Inappropriate Antibiotics") cols(1)) ///
		title("", size(medium) justification(left) color(black) span pos(11)) ///
		xtitle("Provider's knowledge score {&rarr} ", placement(left) justification(left)) xscale(titlegap(2)) ///
		ylab(0 "0" 20 "20%" 40 "40%" 60 "60%" 80 "80%" 100 "100%",angle(0) nogrid) yscale(noli) bgcolor(white) ytitle("") ///
		note(" ") xlabel(-5 (1) 5) xscale(noli)
		graph export "$figures/Treatment_Antibiotic_Knowledge_Scatter.png", width(2000) replace

	lpoly percent_antibiotict comp_mle, ///
		degree(1) jitter(10) m(x) mc(black%10) lineopts(lw(thick)) graphregion(color(white)) ///
		title("Percent of Conditions Given Inappropriate Antibiotics", size(medium) justification(left) color(black) span pos(11)) ///
		xtitle("Provider's knowledge score {&rarr} ", placement(left) justification(left)) xscale(titlegap(2)) ///
		ylab(0 "0" 20 "20%" 40 "40%" 60 "60%" 80 "80%" 100 "100%", angle(0) nogrid) yscale(noli) bgcolor(white) ytitle("") ///
		note(" ") xlabel(-5 (1) 5) xscale(noli)
		graph export "$figures/Antibiotics_Knowledge_Scatter.png", width(2000) replace

	replace provider_agedisc = . if provider_agedisc>=75 | provider_agedisc<15
	lpoly percent_correctt provider_agedisc, ///
		degree(1) jitter(10) m(x) mc(black%10) lineopts(lw(thick)) graphregion(color(white)) ///
		title("Percent of Conditions Treated Correctly", size(medium) justification(left) color(black) span pos(11)) ///
		xtitle("Provider's age", placement(left) justification(left)) xscale(titlegap(2)) ///
		ylab(0 "0" 20 "20%" 40 "40%" 60 "60%" 80 "80%" 100 "100%", angle(0) nogrid) yscale(noli) bgcolor(white) ytitle("") ///
		note(" ") xscale(noli)
		graph export "$figures/Treatment_Age_Scatter.png", width(2000) replace

	tw 	(lowess percent_correctt pctile_bycountry if country=="Mozambique-2014", lcolor(gs10) lwidth(0.5)) ///
		(lowess percent_correctt pctile_bycountry if country=="Niger-2015", lcolor(gs10) lwidth(0.5)) ///
		(lowess percent_correctt pctile_bycountry if country=="Senegal-2010", lcolor(gs10) lwidth(0.5)) ///
		(lowess percent_correctt pctile_bycountry if country=="Tanzania-2014", lcolor(gs10) lwidth(0.5)) ///
		(lowess percent_correctt pctile_bycountry if country=="Tanzania-2016", lcolor(gs10) lwidth(0.5)) ///
		(lowess percent_correctt pctile_bycountry if country=="Togo-2013", lcolor(gs10) lwidth(0.5)) ///
		(lowess percent_correctt pctile_bycountry if country=="Uganda-2013", lcolor(gs10) lwidth(0.5)) ///
		(lowess percent_correctt pctile_bycountry if country=="Kenya-2012", lcolor(navy) lwidth(0.8)) ///
		(lowess percent_correctt pctile_bycountry if country=="Madagascar-2016", lcolor(cranberry) lwidth(0.8)) ///
		(lowess percent_correctt pctile_bycountry if country=="Nigeria-2013", lcolor(chocolate) lwidth(0.8)) ///
		, ///
		legend(order(8 "Kenya 2012" 9 "Madagascar 2016" 10 "Nigeria 2013" 1 "Mozambique 2014" ///
			2 "Niger 2015"  3 "Senegal 2010" 4 "Tanzania 2014" ///
			5 "Tanzania 2016" 6 "Togo 2013" 7 "Uganda 2013" ) symy(2) symx(4) size(small) c(1) ring(1) pos(3) region(lc(none) fc(none))) ///
		title(" ", size(medium) justification(left) color(black) span pos(11)) ///
		ytitle("Percent of Conditions Treated Correctly") ylabel(0 "0%" 20 "20%" 40 "40%" 60 "60%" 80 "80%" 100 "100%", angle(0) nogrid)  ///
		xlabel(1 "1st" 25 "25th" 50 "50th" 75 "75th" 99 "99th") ///
		xtitle("Rank of Provider in Country", placement(left)) ///
		xsize(7) graphregion(color(white)) xscale(noli titlegap(2)) yscale(noli) ///
		bgcolor(white)
		graph export "$figures/TreatmentLowess.png", width(2000) replace

	graph combine "$gphfiles/diarrhea_correctt_bar.gph" ///
		"$gphfiles/pneumonia_correctt_bar.gph" ///
		"$gphfiles/malaria_correctt_bar.gph" ///
		"$gphfiles/tb_correctt_bar.gph" ///
		"$gphfiles/diabetes_correctt_bar.gph" ///
		"$gphfiles/pph_correctt_bar.gph" ///
		"$gphfiles/asphyxia_correctt_bar.gph" ///
		, ///
		xcommon ///
		title("Fraction Who Correctly Treated Condition", placement(left) justification(left) color(black) size(medium)) ///
		graphregion(color(white) lcolor(white) lwidth(10))
		graph export "$figures/TreatmentBars.png", width(2000) replace


*****************************************************************************
/* Figures using long data */
*****************************************************************************

	use "$data/SDI_Vignette_IRT_Analysis_AfterTo${recodeAfter}_withAge.dta", clear

	keep country survey_id *_questions_num *_exams_num *_tests_num *_antibiotic *_correctd *_correctt
	drop pneumonia_treat_other_antibiotic num_correctt num_correctd percent_correctt percent_correctd

	rename *_questions_num questions_num_*
	rename *_exams_num exams_num_*
	rename *_tests_num tests_num_*
	rename *_correctd correctd_*
	rename *_correctt correctt_*
	rename *_antibiotic antibiotic_*

	reshape long questions_num exams_num tests_num correctd correctt antibiotic , i(survey_id) j(disease) string

	replace disease = subinstr(disease, "_", "", .)
	drop if disease == "pid" | disease == "pregnant" | disease == "eclampsia"
	replace disease = proper(disease)
	replace disease = "Type II Diabetes" if disease=="Diabetes"
	replace disease = upper(disease) if disease=="Pph" | disease=="Tb"
	replace disease = "Neonatal Asphyxia" if disease=="Asphyxia"
	replace disease = "Diarrhea with Dehydration" if disease=="Diarrhea"
	replace disease = "Malaria with Anemia" if disease=="Malaria"

	graph bar questions_num exams_num tests_num, over(disease, relabel(1 `" "Diarrhea" "with" "Dehydration" "' 2 `" "Malaria" "with" "Anemia" "' 3 `" "Neonatal" "Asphyxia" "' 7 `" "Type II" "Diabetes" "')) blab(bar, format(%9.1f)) ///
		graphregion(color(white)) ylab(,angle(0) nogrid) yscale(noli) ///
		legend(label(1 "Number of History Questions Asked") ///
			label(2 "Number of Examinations Done") label(3 "Number of Tests Ordered") cols(1) region(lc(none) fc(none)) pos(1) ring(0)) ///
		bgcolor(white) xsize(7) ylabel(0(1)8)
		graph export "$figures/ChecklistItems.png", width(2000) replace

	graph bar correctd correctt antibiotic, over(disease, sort(correctt) descending relabel(1 `" "Diarrhea" "with" "Dehydration" "' 2 `" "Malaria" "with" "Anemia" "' 3 `" "Neonatal" "Asphyxia" "' 7 `" "Type II" "Diabetes" "')) blab(bar, format(%9.0f) size(vsmall)) ///
		graphregion(color(white)) ylab(,angle(0) nogrid) yscale(noli) ///
		bar(1, fcolor(eltblue) fintensity(inten100) lcolor(eltblue%100)) ///
		bar(2, fcolor(green) fintensity(inten100) lcolor(green%100)) ///
		bar(3, fcolor(cranberry) fintensity(inten100) lcolor(cranberry%100)) ///
		legend(label(1 "Correctly Diagnosed Condition") ///
			label(2 "Correctly Treated Condition") label(3 "Prescribed Inappropriate Antibiotics") cols(1) region(lc(none) fc(none)) pos(1) ring(0)) ///
		bgcolor(white) xsize(7) ylabel(0 "0%" 20 "20%" 40 "40%" 60 "60%" 80 "80%" 100 "100%")
		local nb=`.Graph.plotregion1.barlabels.arrnels'
		forval i=1/`nb' {
		  di "`.Graph.plotregion1.barlabels[`i'].text[1]'"
		  .Graph.plotregion1.barlabels[`i'].text[1]="`.Graph.plotregion1.barlabels[`i'].text[1]'%"
		}
		.Graph.drawgraph
		graph export "$figures/TreatmentOutcomes.png", width(2000) replace


//collapse (mean) diarrhea_correctt pneumonia_correctt diabetes_correctt tb_correctt malaria_correctt pph_correctt asphyxia_correctt, by(country)

/*
gen quintile = .
replace quintile = 1 if pctile_bycountry < 20
replace quintile = 2 if pctile_bycountry >= 20 & pctile_bycountry < 40
replace quintile = 3 if pctile_bycountry >= 40 & pctile_bycountry < 60
replace quintile = 4 if pctile_bycountry >= 60 & pctile_bycountry < 80
replace quintile = 5 if pctile_bycountry >= 80 & pctile_bycountry < 100
collapse (mean) percent_correctt, by(country quintile)
*/
