* Ben version


*****************************************************************************
* Initializing
*****************************************************************************

	global root "/Users/bbdaniels/GitHub/SDI-IRT-Paper/"
	global data "$root//SDI-Health/harmonizedData/"
	global figures "$root/figures/"
	global tables "$root/tables/"
	global gphfiles "C:\Users\annak\Box Sync\WB Work\DECRG Github Repo\outputs\figs\CrossCountry\All10\TheGraphs"
	global recodeAfter = "0"


/****BEN****/

	use "${root}/SDI_Vignette_IRT_Analysis_AfterTo0.dta" , clear


	* Ranks

		use "${directory}/Constructed/M2_Vignettes_long.dta" , clear

		collapse (mean) treat [pweight = weight_vig] , by(case statename) fast
		bys case: egen rank = rank(treat) , unique
			replace rank = 20 - rank

		levelsof statename, local(states)

		foreach state in `states' {
			local lines "`lines' (line rank case if statename == "`state'" , lc(black))"
			local scatter "`scatter' (scatter rank case if statename == "`state'" , mlc(black))"
		}

		gen five = 5

		tw `lines' `scatter' ///
			(scatter rank case if case == 4 ///
					, m(none) mlabc(black) mlp(3) mlabel(statename)) ///
			(scatter rank five , m(none)) ///
		, ${graph_opts} legend(off) yscale(reverse) ///
			yscale(noline) ytit("Rank for Condition") ylab(1 "Best" 19 "Worst") ///
			xscale(noline) xlab(1 "Tuberculosis" 2 "Preeclampsia" 3 "Dysentery" 4 "Diarrhea")

			graph export "${directory}/outputs/2_m1_vignettes/ranks.eps" , replace


-







* Ok!
