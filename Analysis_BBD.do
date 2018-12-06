* Ben version


*****************************************************************************
* Initializing
*****************************************************************************

	**global root "/Users/bbdaniels/GitHub/SDI-IRT-Paper/"
	global root "C:/Users/annak/Documents/GitHub/SDI-IRT-Paper/"
	global data "$root//SDI-Health/harmonizedData/"
	global figures "$root/figures/"
	global tables "$root/tables/"

	* Graph options

		global graph_opts title(, justification(left) color(black) span pos(11)) graphregion(color(white)) ylab(,angle(0) nogrid) xtit(,placement(left) justification(left)) legend(region(lc(none) fc(none)))
		global graph_opts_1 title(, justification(left) color(black) span pos(11)) graphregion(color(white)) ylab(,angle(0) nogrid) yscale(noline) legend(region(lc(none) fc(none)))
		global comb_opts graphregion(color(white))
		global hist_opts ylab(, angle(0) axis(2)) yscale(noline alt axis(2)) ytit(, axis(2)) ytit(, axis(1)) yscale(off axis(2)) yscale(alt)
		global note_opts justification(left) color(black) span pos(7)
		global pct `" 0 "0%" .25 "25%" .5 "50%" .75 "75%" 1 "100%" "'
		global pctile `" 0 "1st" .25 "25th" .5 "50th" .75 "75th" 1 "99th" "'
		global bar lc(white) lw(thin) la(center)

	* Ado

		qui do "${root}/ado/LabelCollapse/labelcollapse.ado"
		qui do "${root}/ado/EasyIRT/easyirt.ado"
		qui do "${root}/ado/OpenIRT/openirt.ado"

	* Enter the list of countries to analyze: 
	
		global theCountries ///
			`" "Kenya-2012" "Madagascar-2016" "Nigeria-2013" "Tanzania-2014" "Tanzania-2016" "Uganda-2013" "Mozambique-2014" "Niger-2015" "Senegal-2010" "Togo-2013" "'

	* Recalculate IRT scores? (Set "yes" or "no"):

		global doIRT = "no"

	* Recode "Would do if in ideal world" to 0 or 1? (Set "0" or "1"):

		global recodeAfter = "0"

*****************************************************************************
* Constructing dataset
*****************************************************************************

	if "$doIRT" == "yes" {

		do "${root}/Do_IRT.do"

	}

	qui do "${root}/Make_Variables.do"

*****************************************************************************
*****************************************************************************	

/****BEN****/

* Long data construction

	use "${root}/SDI_Vignette_IRT_Analysis_AfterTo0.dta" , clear

	* keep country survey_id countryfac_id *_antibiotic *_correctt provider_cadre provider_agedisc facility_level ruralurban publicprivate
	drop pneumonia_treat_other_antibiotic num_* percent_* total_*

	rename *_correctt correct*
	rename *_correctd diagnosis*
	rename *_antibiotic antibiotic*
	rename *_exams_num exams*
	rename *_questions_num questions*
	rename *_tests_num tests*

	keep provider_age1 provider_educ1 country survey_id comp_mle correct* diagnosis* antibiotic* exams* questions* tests*

	reshape long correct diagnosis antibiotic exams questions tests ///
		, i(survey_id) j(disease) string

	replace antibiotic = 1 if antibiotic==100
	replace correct = 1 if correct==100
	replace diagnosis = 1 if diagnosis==100

	bys country: gen weight = 1/_N

	drop if disease == "pid" | disease == "pregnant" | disease == "asphyxia"

	save "${root}/vignettes_long.dta" , replace

* Section 1 – Aggregate Measures

	* Figure: Diagnostic and Treatment Knowledge

		use "${root}/vignettes_long.dta", clear

			replace disease = proper(disease)
				replace disease = "TB" if disease == "Tb"
				replace disease = "PPH" if disease == "Pph"
				encode disease, gen(case)

			label var diagnosis "Correct Diagnosis"
			label var correct "Correct Treatment"
			label var antibiotic "Antibiotics"


		* Panel A – Diagnostics

			graph bar ///
				questions exams tests ///
				[pweight = weight] ///
			,  ${graph_opts_1} over(case) xsize(8) title("Panel A: Diagnostic Knowledge") ///
				bar(1, ${bar} fc(ebblue)) bar(2, ${bar} fc(dkgreen)) bar(3, ${bar} fc(maroon))  ///
				legend(ring(2) pos(12) r(1) symxsize(small) symysize(small) ///
					order(1 "History Questions" 2 "Physical Exams" 3 "Laboratory Tests"))

				graph save "${figures}/outcomes_a.gph" , replace

		* Panel B – Treatments

			graph bar ///
				diagnosis correct antibiotic ///
				[pweight = weight] ///
			,  ${graph_opts_1} over(case) xsize(8) title("Panel B: Management Knowledge") ///
				ylab(${pct}) bar(1, ${bar} fc(ebblue)) bar(2, ${bar} fc(dkgreen)) bar(3, ${bar} fc(maroon))  ///
				legend(ring(2) pos(12) r(1) symxsize(small) symysize(small) ///
					order(1 "Correct Diagnosis" 2 "Correct Treatment" 3 "Unnecessary Antibiotics"))

				graph save "${figures}/outcomes_b.gph" , replace

		* Combine

			graph combine ///
				"${figures}/outcomes_a.gph" ///
				"${figures}/outcomes_b.gph" ///
			, c(1) ${comb_opts} ysize(5)

				graph export "${figures}/outcomes.eps" , replace

	* Figure: Boxplot

		use "${root}/vignettes_long.dta", clear

		collapse (mean) comp_mle (firstnm) country, by(survey_id) fast

		replace comp_mle = comp_mle + 5

		graph hbox comp_mle ///
		, over(country, sort(1) descending axis(noline)) ${graph_opts_1} ///
			box(1, lc(black) fc(none)) noout ylab(0(1)10) note(" ") ///
			ytitle("Diagnostic Domain Competence Score")

			graph export "${figures}/competence.eps" , replace

	* Figure: Correlation of Diagnostic and Treatment Knowledge

		use "${root}/vignettes_long.dta", clear

		collapse (mean) correct diagnosis antibiotic weight comp_mle, by(survey_id) fast

		replace comp_mle = comp_mle+5

		tw ///
			(histogram comp_mle , yaxis(2) fc(gs12) lc(gs12) la(center)) ///
			(lpoly diagnosis comp_mle [aweight = weight], degree(1) lw(thick) lcolor(ebblue)) ///
			(lpoly correct comp_mle [aweight = weight], degree(1) lw(thick) lcolor(dkgreen)) ///
			(lpoly antibiotic comp_mle [aweight = weight], degree(1)  lw(thick) lcolor(maroon)) ///
		, ${graph_opts} ${hist_opts} legend(pos(11) ring(0) c(1) symxsize(small) ///
				order(1 "Distribution" 2 "Correct Diagnosis" 3 "Correct Treatment" 4 "Unnecessary Antibiotics")) ///
			ylab(${pct}) xlab(0(1)10) xtitle("Diagnostic Domain Competence Score {&rarr}")

			graph export "${figures}/correlations.eps" , replace

	* Figure: Age and Education

		use "${root}/vignettes_long.dta", clear

		labelcollapse (mean) comp_mle provider_age1 provider_educ1 weight ///
			(firstnm) country , by(survey_id) fast vallab(provider_age1 provider_educ1)

		replace comp_mle = comp_mle +5

		areg comp_mle i.provider_age1##i.provider_educ1 [pweight = weight], a(country)

		margins , over(provider_age1 provider_educ1)
			mat a = r(table)
			local 1 = a[1,10]
			local 2 = a[1,11]
			local 3 = a[1,12]
		marginsplot ///
			, ${graph_opts} title("") xtit("Provider Age {&rarr}") ytit("{&larr} Predicted Competence {&rarr}") ///
			plotopts(msize(large) lc(black) mlc(black)) ciopts(lc(black)) ///
			xlab(1 "Under 25" 2 "25-34" 3 "35-44" 4 "45+" 5 " ") ///
			legend(off) /// ring(1) pos(3) c(1)) ///
			addplot(scatteri `1' 4.1 "{&larr} Primary"  ///
			 `2' 4.1 "{&larr} Secondary" ///
		 	 `3' 4.1 "{&larr} Post-Secondary" ///
			  5 5.2 "" , m(none) mlabc(black) mlabs(med))

			  graph export "${figures}/ageedu.eps" , replace

* Section 2 – Sensitivity to alternate definitions

	* Table: Diagnosis

	* Table: Treatment

* Section 3 – Issues with Comparability

	* Figure: Rank Stability by Condition

		use "${root}/vignettes_long.dta", clear

		collapse (mean) correct [pweight = weight] , by(disease country) fast
		bys disease: egen rank = rank(correct) , unique
			drop if rank == .
			bys disease : gen tot = _N
			replace rank = tot - rank + 1

		levelsof country, local(states)

		local lines ""
		local scatter ""
		foreach state in `states' {
			local thick "lc(gray)"
				if "`state'" == "Nigeria-2013" local thick "lw(thick) lc(black)"
			local lines "`lines' (line rank case if country == "`state'" , `thick')"
			local scatter "`scatter' (scatter rank case if country == "`state'" , mlc(black) msize(vlarge) )"
		}

		cap gen five = 7
		gen pos = 6.1
		cap encode disease , gen(case)

		tw `lines' `scatter' ///
			(scatter rank pos if case == 6 ///
					, m(none) mlabc(black) mlp(3) mlabel(country)) ///
			(scatter rank five , m(none)) ///
		, ${graph_opts} legend(off) yscale(reverse) xsize(8) ///
			yscale(noline) ytit("Rank for Condition") ylab(1 "Best" 10 "Worst") ///
			xscale(noline) xlab(1 "Diabetes" 2 "Diarrhea" 3 "Malaria" 4 "Pneumonia" 5 "PPH" 6 "Tuberculosis")

			graph export "${figures}/ranks_conditions.eps" , replace
			graph save "${figures}/ranks_conditions.gph" , replace

	* Figure: Rank Stability by Position

		use "${root}/vignettes_long.dta", clear

		collapse (mean) correct comp_mle weight (firstnm) country , by(survey_id) fast

		levelsof country, local(states)

		local x = 0
		foreach state in `states' {
			local ++x
			xtile rank_`x' = comp_mle if country == "`state'" , n(10)
		}

		egen quant = rowmin(rank*)
			drop rank_*

		collapse (mean) correct comp_mle , by(country quant) fast

		bys quant: egen rank = rank(correct) , unique
			drop if rank == .
			bys quant : gen tot = _N
			replace rank = tot - rank + 1

		levelsof country, local(states)

		local lines ""
		local scatter ""
		foreach state in `states' {
			local thick "lc(gray)"
				if "`state'" == "Nigeria-2013" local thick "lw(thick) lc(black)"
			local lines "`lines' (line rank quant if country == "`state'" , `thick')"
			local scatter "`scatter' (scatter rank quant if country == "`state'" , mlc(black) msize(vlarge) )"
		}

		cap gen five = 11
		cap gen pos = 10.1

		tw `lines' `scatter' ///
			(scatter rank pos if quant == 10 ///
					, m(none) mlabc(black) mlp(3) mlabel(country)) ///
			(scatter rank five , m(none)) ///
		, ${graph_opts} legend(off) yscale(reverse) xsize(8) ///
			yscale(noline) ytit("Rank for Decile") ylab(1 "Best" 10 "Worst") ///
			xscale(noline) xtit("Provider Decile {&rarr}") xlab(1 "Lowest" 2(1)9 10 "Highest")

			graph export "${figures}/ranks_positions.eps" , replace
			graph save "${figures}/ranks_positions.gph" , replace

	* Combine

		graph combine ///
			"${figures}/ranks_conditions.gph" ///
			"${figures}/ranks_positions.gph" ///
		, ${comb_opts}

		* Combine

			graph combine ///
				"${figures}/ranks_conditions.gph" ///
				"${figures}/ranks_positions.gph" ///
			, ${comb_opts} c(1)

			graph export "${figures}/rank.eps" , replace

*****************************************************************************
*****************************************************************************	

/****ANNA****/

* Construct alternate treatment definitions

	use "${root}/SDI_Vignette_IRT_Analysis_AfterTo0.dta" , 

	bys country: gen weight = 1/_N

	egen diarrhea_correctt1 = anymatch(diarrhea_treat_ivfluids diarrhea_treat_ngtube diarrhea_treat_tubedose diarrhea_refer_facility diarrhea_refer_clinician diarrhea_treat_ors diarrhea_educate_ors), val(1)
	
	egen diarrhea_correctt2 = anymatch(diarrhea_treat_ivfluids diarrhea_treat_ngtube diarrhea_treat_tubedose diarrhea_treat_ors diarrhea_educate_ors), val(1)
	replace diarrhea_correctt2 = 1 if (diarrhea_refer_facility==1 | diarrhea_refer_clinician==1) & facility_level==1

	egen diarrhea_correctt3 = anymatch(diarrhea_treat_ivfluids diarrhea_treat_ngtube diarrhea_treat_tubedose diarrhea_treat_ors diarrhea_educate_ors), val(1)
	replace diarrhea_correctt3 = 0 if diarrhea_treat_zinc==0
	replace diarrhea_correctt3 = 1 if (diarrhea_refer_facility==1 | diarrhea_refer_clinician==1) & facility_level==1

	egen diarrhea_correctt4 = anymatch(diarrhea_treat_ivfluids diarrhea_treat_ngtube diarrhea_treat_tubedose), val(1)
	replace diarrhea_correctt4 = 1 if (diarrhea_refer_facility==1 | diarrhea_refer_clinician==1) & facility_level==1
	
	egen diarrhea_correctt5 = anymatch(diarrhea_treat_ivfluids diarrhea_treat_ngtube diarrhea_treat_tubedose), val(1)
	replace diarrhea_correctt5 = 0 if diarrhea_treat_zinc==0
	replace diarrhea_correctt5 = 1 if (diarrhea_refer_facility==1 | diarrhea_refer_clinician==1) & facility_level==1

	egen pneumonia_correctt1 = anymatch(pneumonia_treat_amoxy_dose pneumonia_treat_amoxycillin pneumonia_treat_xpen pneumonia_treat_cotrimox pneumonia_refer_facility pneumonia_refer_clinician), val(1)
	
	egen pneumonia_correctt2 = anymatch(pneumonia_treat_amoxy_dose pneumonia_treat_amoxycillin pneumonia_treat_xpen pneumonia_treat_cotrimox), val(1)
	replace pneumonia_correctt2 = 1 if (pneumonia_refer_facility==1 | pneumonia_refer_clinician==1) & facility_level==1
	
	egen pneumonia_correctt3 = anymatch(pneumonia_treat_amoxy_dose pneumonia_treat_amoxycillin), val(1)
	replace pneumonia_correctt3 = 1 if (pneumonia_refer_facility==1 | pneumonia_refer_clinician==1) & facility_level==1

	egen tb_correctt1 = anymatch(tb_treat_ctdurdose tb_treat_ctdose tb_treat_ctdrugs tb_treat_ctdur tb_refer_tbclinic tb_refer_facility tb_refer_clinician tb_test_chest_xray tb_test_sputum tb_test_idr), val(1)
	
	egen tb_correctt2 = anymatch(tb_treat_ctdurdose tb_treat_ctdose tb_treat_ctdrugs tb_treat_ctdur tb_refer_tbclinic tb_refer_facility tb_refer_clinician tb_test_sputum), val(1)
	
	egen tb_correctt3 = anymatch(tb_treat_ctdurdose tb_treat_ctdose tb_treat_ctdrugs tb_treat_ctdur tb_refer_tbclinic tb_refer_facility tb_refer_clinician), val(1)

	egen malaria_correctt1 = anymatch(malaria_treat_al_wdose malaria_treat_al_dose malaria_treat_artemisinin malaria_treat_al malaria_treat_artesunateam malaria_refer_facility malaria_refer_clinician), val(1)
	
	egen malaria_correctt2 = anymatch(malaria_treat_al_wdose malaria_treat_al_dose malaria_treat_artemisinin malaria_treat_al malaria_treat_artesunateam), val(1)
	replace malaria_correctt2 = 1 if (malaria_refer_facility==1 | malaria_refer_clinician==1) & facility_level==1 

	egen malaria_correctt3 = anymatch(malaria_treat_al_wdose malaria_treat_al_dose malaria_treat_artemisinin malaria_treat_al malaria_treat_artesunateam), val(1)
	replace malaria_correctt3 = 0 if malaria_treat_iron_folicacid==0
	replace malaria_correctt3 = 1 if (malaria_refer_facility==1 | malaria_refer_clinician==1) & facility_level==1 

	foreach x in "diarrhea" "pneumonia" "tb" "malaria" {
		foreach y of varlist `x'_correctt? {
			replace `y' = . if skip_`x'==1
			replace `y' = 0 if skip_`x'==0 & `y'==.
		}
	}

	collapse (mean) diarrhea_correctt? pneumonia_correctt? tb_correctt? malaria_correctt? [pweight=weight]
	xpose, clear varname
	gen id = .
	local counter = 0
	forvalues i = 1/3 {
		replace id = .25 + 0.25*`counter' if _varname=="pneumonia_correctt`i'"
		local ++counter
	}
	local counter = 0 
	forvalues i = 1/3 {
		replace id = 1.75 + 0.25*`counter' if _varname=="malaria_correctt`i'"
		local ++counter
	}
	local counter = 0 
	forvalues i = 1/3 {
		replace id = 3.25 + 0.25*`counter' if _varname=="tb_correctt`i'"
		local ++counter
	}
	local counter = 0 
	forvalues i = 1/5 {
		replace id = 4.75 + 0.25*`counter' if _varname=="diarrhea_correctt`i'"
		local ++counter
	}

	forvalues i = 1/5 {
		replace _varname = subinstr(_varname, "`i'", "", .)
	}

	tw (scatter id v1 if _varname=="diarrhea_correctt") ///
		(scatter id v1 if _varname=="tb_correctt") ///
		(scatter id v1 if _varname=="malaria_correctt") ///
		(scatter id v1 if _varname=="pneumonia_correctt"), ///
		yline(.25 .5 .75 1.75 2 2.25 3.25 3.5 3.75 4.75 5 5.25 5.5 5.75, lpattern(dash) lcolor(gs12)) ///
		xtitle("Fraction who correctly managed condition") xlabel(, format(%9.1f))  ///
		legend(label(1 "Diarrhea") label(2 "TB") label(3 "Malaria") label(4 "Pneumonia") cols(4) pos(12) ) ///
		ylabel(5.75 "Strictest" 4.75 "Least Strict" 3.75 "Strictest" 3.25 "Least Strict" ///
		2.25 "Strictest" 1.757 "Least Strict" 0.75 "Strictest" 0.25 "Least Strict", labsize(medsmall)) ytitle("") ///
		$graph_opts

	graph export "${figures}/treatment_definitions.eps" , replace
	graph save "${figures}/treatment_definitions.gph" , replace
	graph save "${figures}/treatment_definitions.png" , replace

* Ok!
