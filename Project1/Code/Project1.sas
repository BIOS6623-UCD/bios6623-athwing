** This is the analysis code for Project 1;
** This file is stored on SAS Academic on demand, it can be created
** using the instructions in the Code_cleanup.R file

FILENAME REFFILE '/home/anniethwing1/sasuser.v94/ADA/HIV_cleaned_SAS.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV REPLACE
	OUT=HIV_DATA;
	GETNAMES=YES;
RUN;

** First, Run crude models for every outcome;
** CRUDE;
PROC MCMC DATA=HIV_DATA NBI = 10000 NMC = 100000 THIN = 15 DIC MISSING = CC DIAGNOSTICS = GEWEKE;
   PARMS betaInt 0; PARMS betaBaseline 0; PARMS betahard_drugs 0;
   PARMS sigma2 1;
   PRIOR betaInt ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBaseline ~ NORMAL(mean = 0, var = 1000);
   PRIOR betahard_drugs ~ NORMAL(mean = 0, var = 1000);
   PRIOR sigma2 ~ igamma(shape=2.001,scale=1.001);
   mu = betaInt + betaBaseline*LEU3N + betahard_drugs*hard_drugs;
   MODEL LEU3NDiff ~ normal(mu, var = sigma2);
   TITLE "Model 1: Outcome LEU3N Crude";
RUN;
** Then run full models for each outcome;
** FULL;
PROC MCMC DATA=HIV_DATA NBI = 10000 NMC = 100000 THIN = 15 DIC MISSING = CC DIAGNOSTICS = GEWEKE;
   PARMS betaInt 0; PARMS betaBaseline 0; PARMS betaHASHV 0; PARMS betaincome 0;
   PARMS betaBMI 0; PARMS betaSMOKE 0; PARMS betaDKGRP 0; PARMS betaADH 0;
   PARMS betaRACE 0; PARMS betaEDUCBAS 0; PARMS betaage 0; PARMS betahard_drugs 0;
   PARMS sigma2 1;
   PRIOR betaInt ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBaseline ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaHASHV ~ NORMAL(mean = 0, var = 1000); 
   PRIOR betaincome ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBMI ~ NORMAL(mean = 0, var = 10000);
   PRIOR betaSMOKE ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaDKGRP ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaADH ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaRACE ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaEDUCBAS ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaage ~ NORMAL(mean = 0, var = 10000);
   PRIOR betahard_drugs ~ NORMAL(mean = 0, var = 1000);
   PRIOR sigma2 ~ igamma(shape=2.001,scale=1.001);
   mu = betaInt + betaBaseline*LEU3N + betaHASHV*HASHV + betaincome*income + betaBMI*BMI + betaSMOKE*SMOKE
   + betaDKGRP*DKGRP + betaADH*ADH + betaRACE*RACE + betaEDUCBAS*EDUCBAS + betaage*age 
   + betahard_drugs*hard_drugs;
   MODEL LEU3NDiff ~ normal(mu, var = sigma2);
   TITLE "Model 1: Outcome LEU3N Full";
RUN;

** CRUDE;
PROC MCMC DATA=HIV_DATA NBI = 10000 NMC = 100000 THIN = 15 DIC MISSING = CC DIAGNOSTICS = GEWEKE;
   PARMS betaInt 0; PARMS betaBaseline 0; PARMS betahard_drugs 0;
   PARMS sigma2 1;
   PRIOR betaInt ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBaseline ~ NORMAL(mean = 0, var = 1000);
   PRIOR betahard_drugs ~ NORMAL(mean = 0, var = 1000);
   PRIOR sigma2 ~ igamma(shape=2.001,scale=1.001);
   mu = betaInt + betaBaseline*VLOAD + betahard_drugs*hard_drugs;
   MODEL VLOADDiff ~ normal(mu, var = sigma2);
   TITLE "Model 1: Outcome VLOAD Crude";
RUN;

** FULL;
PROC MCMC DATA=HIV_DATA NBI = 10000 NMC = 100000 THIN = 15 DIC MISSING = CC DIAGNOSTICS = GEWEKE;
   PARMS betaInt 0; PARMS betaBaseline 0; PARMS betaHASHV 0; PARMS betaincome 0;
   PARMS betaBMI 0; PARMS betaSMOKE 0; PARMS betaDKGRP 0; PARMS betaADH 0;
   PARMS betaRACE 0; PARMS betaEDUCBAS 0; PARMS betaage 0; PARMS betahard_drugs 0;
   PARMS sigma2 1;
   PRIOR betaInt ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBaseline ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaHASHV ~ NORMAL(mean = 0, var = 1000); 
   PRIOR betaincome ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBMI ~ NORMAL(mean = 0, var = 10000);
   PRIOR betaSMOKE ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaDKGRP ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaADH ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaRACE ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaEDUCBAS ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaage ~ NORMAL(mean = 0, var = 10000);
   PRIOR betahard_drugs ~ NORMAL(mean = 0, var = 1000);
   PRIOR sigma2 ~ igamma(shape=2.001,scale=1.001);
   mu = betaInt + betaBaseline*VLOAD + betaHASHV*HASHV + betaincome*income + betaBMI*BMI + betaSMOKE*SMOKE
   + betaDKGRP*DKGRP + betaADH*ADH + betaRACE*RACE + betaEDUCBAS*EDUCBAS + betaage*age 
   + betahard_drugs*hard_drugs;
   MODEL VLOADDiff ~ normal(mu, var = sigma2);
   TITLE "Model 1: Outcome VLOAD Full";
RUN;

** CRUDE;
PROC MCMC DATA=HIV_DATA NBI = 10000 NMC = 100000 THIN = 15 DIC MISSING = CC DIAGNOSTICS = GEWEKE;
   PARMS betaInt 0; PARMS betaBaseline 0; PARMS betahard_drugs 0;
   PARMS sigma2 1;
   PRIOR betaInt ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBaseline ~ NORMAL(mean = 0, var = 1000);
   PRIOR betahard_drugs ~ NORMAL(mean = 0, var = 1000);
   PRIOR sigma2 ~ igamma(shape=2.001,scale=1.001);
   mu = betaInt + betaBaseline*AGG_MENT + betahard_drugs*hard_drugs;
   MODEL AGG_MENTDiff ~ normal(mu, var = sigma2);
   TITLE "Model 1: Outcome AGG_MENT Crude";
RUN;

** FULL;
PROC MCMC DATA=HIV_DATA NBI = 10000 NMC = 100000 DIC THIN = 15 MISSING = CC DIAGNOSTICS = GEWEKE;
   PARMS betaInt 0; PARMS betaBaseline 0; PARMS betaHASHV 0; PARMS betaincome 0;
   PARMS betaBMI 0; PARMS betaSMOKE 0; PARMS betaDKGRP 0; PARMS betaADH 0;
   PARMS betaRACE 0; PARMS betaEDUCBAS 0; PARMS betaage 0; PARMS betahard_drugs 0;
   PARMS sigma2 1;
   PRIOR betaInt ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBaseline ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaHASHV ~ NORMAL(mean = 0, var = 1000); 
   PRIOR betaincome ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBMI ~ NORMAL(mean = 0, var = 10000);
   PRIOR betaSMOKE ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaDKGRP ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaADH ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaRACE ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaEDUCBAS ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaage ~ NORMAL(mean = 0, var = 10000);
   PRIOR betahard_drugs ~ NORMAL(mean = 0, var = 1000);
   PRIOR sigma2 ~ igamma(shape=2.001,scale=1.001);
   mu = betaInt + betaBaseline*AGG_MENT + betaHASHV*HASHV + betaincome*income + betaBMI*BMI + betaSMOKE*SMOKE
   + betaDKGRP*DKGRP + betaADH*ADH + betaRACE*RACE + betaEDUCBAS*EDUCBAS + betaage*age 
   + betahard_drugs*hard_drugs;
   MODEL AGG_MENTDiff ~ normal(mu, var = sigma2);
   TITLE "Model 1: Outcome AGG_MENT Full";
RUN;

** CRUDE;
PROC MCMC DATA=HIV_DATA NBI = 10000 NMC = 100000 DIC THIN = 15 MISSING = CC DIAGNOSTICS = GEWEKE;
   PARMS betaInt 0; PARMS betaBaseline 0; PARMS betahard_drugs 0;
   PARMS sigma2 1;
   PRIOR betaInt ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBaseline ~ NORMAL(mean = 0, var = 1000);
   PRIOR betahard_drugs ~ NORMAL(mean = 0, var = 1000);
   PRIOR sigma2 ~ igamma(shape=2.001,scale=1.001);
   mu = betaInt + betaBaseline*AGG_PHYS + betahard_drugs*hard_drugs;
   MODEL AGG_PHYSDiff ~ normal(mu, var = sigma2);
   TITLE "Model 1: Outcome AGG_PHYS Crude";
RUN;

** FULL;
PROC MCMC DATA=HIV_DATA NBI = 10000 NMC = 100000 DIC THIN = 15 MISSING = CC DIAGNOSTICS = GEWEKE;
   PARMS betaInt 0; PARMS betaBaseline 0; PARMS betaHASHV 0; PARMS betaincome 0;
   PARMS betaBMI 0; PARMS betaSMOKE 0; PARMS betaDKGRP 0; PARMS betaADH 0;
   PARMS betaRACE 0; PARMS betaEDUCBAS 0; PARMS betaage 0; PARMS betahard_drugs 0;
   PARMS sigma2 1;
   PRIOR betaInt ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBaseline ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaHASHV ~ NORMAL(mean = 0, var = 1000); 
   PRIOR betaincome ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaBMI ~ NORMAL(mean = 0, var = 10000);
   PRIOR betaSMOKE ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaDKGRP ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaADH ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaRACE ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaEDUCBAS ~ NORMAL(mean = 0, var = 1000);
   PRIOR betaage ~ NORMAL(mean = 0, var = 10000);
   PRIOR betahard_drugs ~ NORMAL(mean = 0, var = 1000);
   PRIOR sigma2 ~ igamma(shape=2.001,scale=1.001);
   mu = betaInt + betaBaseline*AGG_PHYS + betaHASHV*HASHV + betaincome*income + betaBMI*BMI + betaSMOKE*SMOKE
   + betaDKGRP*DKGRP + betaADH*ADH + betaRACE*RACE + betaEDUCBAS*EDUCBAS + betaage*age 
   + betahard_drugs*hard_drugs;
   MODEL AGG_PHYSDiff ~ normal(mu, var = sigma2);
   TITLE "Model 1: Outcome AGG_PHYS Full";
RUN;

