* SAS https://healthpolicy.ucla.edu/chis/analyze/Pages/sample-code-pooling.aspx

data combined; /*this step concatenates the data files*/
	set libname.chis_2017 (in=in17) libname.chis_2018 (in=in18);
	
	if in17 then year=2017;
	else if in18 then year=2018;
	
	***Create new weight variables;
	fnwgt0 = rakedw0/2;
	array a_origwgts[80] rakedw1-rakedw80;
	array a_newwgts[160] fnwgt1-fnwgt160;
	do i = 1 to 80;
		if year=2017 then do;
			a_newwgts[i] = a_origwgts[i]/2;
			a_newwgts[i+80] = rakedw0/2;
		end;
		else if year=2018 then do;
			a_newwgts[i]    = rakedw0/2;
			a_newwgts[i+80] = a_origwgts[i]/2;
		end;

	end;
run;

proc surveyfreq data = combined varmethod=jackknife;
	weight fnwgt0;
	repweight fnwgt1-fnwgt160/jkcoefs=1;
	table ins;
run;
