/*Multiple Logistic Regression: Diagnosis of Apple Juice*/
options ls = 80 nodate;
data Apple;
infile '/folders/myfolders/Apple.txt';
input pHs Nisin Tem Brix Growth;
if pHs < 4 then y = 0;
if pHs >= 4 then y = 1;
/*proc print data=Apple;
	title 'Multiple Logistic Regression of Apple Juice Data';
run;*/
proc format;
	value G 1 = 'Yes' 0 = 'No';
    value Y 0 = 'Very acidic' 1 = 'acidic';
run;
/*Finding the correct link function*/
/*proc genmod;
  model Growth = Tem / dist = poi link = log;
  title 'Poisson Regression: Log Link';
run;
proc genmod;
  model Growth = Tem / dist = poi link = identity;
  title 'Poisson Regression: Identity Link';
run;
proc genmod;
  model Growth = Tem / dist = negbin link = log;
  title 'Negative Binomial Regression: Log Link';
run;*/
/*Model selection for logistic regression*/
/*proc reg data = Apple;
  model Growth = pHs Nisin Tem Brix / selection = forward sle = 0.05 sls = 0.05;
run;*/
proc reg data = Apple;
  model Growth = pHs Nisin Tem Brix / selection = cp;
run;
/*Using proc catmod on continuous variables*/
proc catmod order=data;
  direct pHs Nisin Tem Brix;
  model Growth = pHs Nisin Tem Brix / ml nogls pred = prob;
  format Growth G.;
  title 'Logistic Regression of the Apple Juice Data using CATMOD Procedure';
run;



















/*proc logistic data = Apple;
  class y;
  model Growth = y Nisin Tem Brix / selection = forward details;
  format Growth G.;
  format y Y.;
  title 'Forward Stepwise Logistic Regression.';
run;
proc logistic data = Apple;
  class y;
  model Growth = y Nisin Tem Brix / selection = backward details;
  format Growth G.;
  format y Y.;
  title 'Forward Stepwise Logistic Regression.';
run;*/
/*Logistic diagnostics*/
/*proc logistic data = Apple;
  class y;
  model Growth = y Nisin Tem Brix / rsq lackfit plcl plrl risklimits influence iplots;
  output out = masteco p = predprob;
  format Growth G.;
  format y Y.;
  title 'Final Logistic Regression Model for the Mastectomy Data';
run;*/
