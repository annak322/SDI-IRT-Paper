* Ben version


*****************************************************************************
* Initializing
*****************************************************************************

	global root "/Users/bbdaniels/GitHub/SDI-IRT-Paper/"
	global data "$root//SDI-Health/harmonizedData/"
	global figures "$root/figures/"
	global tables "$root/tables/"

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

	save "${root}/vignettes_long.dta" , replace

* Section 1 – Aggregate Measures

	* Figure 1: Diagnostic and Treatment Knowledge

		use "${root}/vignettes_long.dta", clear

		* Panel A – Diagnostics

			graph hbar ///
				diagnosis correct antibiotic ///
				[pweight = weight]///
			, blab(bar) over(disease)

		* Panel B – Treatments

			graph hbar ///
				questions exams tests ///
				[pweight = weight]///
			, blab(bar) over(disease)

	* Figure 2: Correlation of Diagnostic and Treatment Knowledge

		use "${root}/vignettes_long.dta", clear

		collapse (mean) correct antibiotic weight comp_mle, by(survey_id) fast

		tw ///
			(scatter correct comp_mle , m(x) mcolor(navy%5) jitter(10)) ///
			(scatter antibiotic comp_mle , m(x) mcolor(maroon%5) jitter(10)) ///
			(lpoly correct comp_mle , lw(thick) lcolor(navy)) ///
			(lpoly antibiotic comp_mle , lw(thick) lcolor(maroon))

* Section 2 – Sensitivity to alternate definitions

	* Table – Diagnosis

	* Table – Treatment

* Section 3 – Issues with Comparability

	* Figure – Rank Stability by Condition

		use "${root}/vignettes_long.dta", clear

		collapse (mean) correct [pweight = weight] , by(disease country) fast
		bys disease: egen rank = rank(correct) , unique
			drop if rank == .
			bys disease : gen tot = _N
			replace rank = tot - rank + 1

		drop if disease == "pid"

		levelsof country, local(states)

		foreach state in `states' {
			local thick "lc(gray)"
				if "`state'" == "Nigeria-2013" local thick "lw(thick) lc(black)"
			local lines "`lines' (line rank case if country == "`state'" , `thick')"
			local scatter "`scatter' (scatter rank case if country == "`state'" , mlc(black) msize(vlarge) )"
		}

		cap gen five = 8
		gen pos = 7.1
		cap encode disease , gen(case)

		tw `lines' `scatter' ///
			(scatter rank pos if case == 7 ///
					, m(none) mlabc(black) mlp(3) mlabel(country)) ///
			(scatter rank five , m(none)) ///
		, ${graph_opts} legend(off) yscale(reverse) xsize(8) ///
			yscale(noline) ytit("Rank for Condition") ylab(1 "Best" 10 "Worst") ///
			xscale(noline) xlab(1 "Asphyxia" 2 "Diabetes" 3 "Diarrhea" 4 "Malaria" 5 "Pneumonia" 6 "PPH" 7 "Tuberculosis")

			graph export "${directory}/outputs/2_m1_vignettes/ranks.eps" , replace

	* Figure – Rank Stability by Position

		use "${root}/vignettes_long.dta", clear
			drop if disease == "pid"

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
			yscale(noline) ytit("Rank for Condition") ylab(1 "Best" 10 "Worst") ///
			xscale(noline) xtit("Provider Decile {&rarr}") xlab(1 "Lowest" 2(1)9 10 "Highest")

* Ok!
