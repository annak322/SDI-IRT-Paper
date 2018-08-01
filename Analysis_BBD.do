* Ben version


*****************************************************************************
* Initializing
*****************************************************************************

	global root "/Users/bbdaniels/GitHub/SDI-IRT-Paper/"
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

	keep country survey_id comp_mle correct* diagnosis* antibiotic* exams* questions* tests*

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

* Ok!
