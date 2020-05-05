library(dplyr)
library(mice)
library(PropCIs)
library(questionr)
library(exact2x2)
library(regclass)
library(fmsb)

covid <- read.csv(file = "~/Downloads/70.csv", header = TRUE, na.strings=c("Unknown"))

covid <- covid %>%
    mutate(
        Gender = as.factor(Gender),
        Race = as.factor(Race),
        SNF. = as.factor(SNF.),
        Hospitalized.for.COVID = as.factor(Hospitalized.for.COVID),
        Diabetes = as.factor(Diabetes),
        HTN = as.factor(HTN),
        COPD = as.factor(COPD),
        Asthma = as.factor(Asthma),
        Hx.of.DVT = as.factor(Hx.of.DVT),
        CKD = as.factor(CKD),
        Cancer = as.factor(Cancer),
        Hx.of.MI = as.factor(Hx.of.MI),
        CVD = as.factor(CVD),
        CHF = as.factor(CHF),
        Hypo.thyroidism = as.factor(Hypo.thyroidism),
        Steroids.or.IMT = as.factor(Steroids.or.IMT),
        Baseline.Plaquenil = as.factor(Baseline.Plaquenil),
        ACEI.ARB = as.factor(ACEI.ARB),
        Smoking.History. = as.factor(Smoking.History.),
        Anticoagulation. = as.factor(Anticoagulation.)
    )

### Perform multiple imputation
set.seed(3249)
num_imputations <- 25
init <- mice(covid, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix

meth[c("SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Anticoagulation.")]="logreg" 
meth[c("Race", "Smoking.History.")]="polyreg"

predM <- ifelse(predM < 0, 1, 0)

#Pick which factors should be involved in imputation
predM["Race", c("Age", "Gender", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["Diabetes", c("Age", "Gender", "Race", "SNF.", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["HTN", c("Age", "Gender", "Race", "SNF.", "Diabetes", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["COPD", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["Asthma", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["Hx.of.DVT", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["CKD", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["Cancer", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["Hx.of.MI", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["CVD", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["CHF", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["Hypo.thyroidism", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["Steroids.or.IMT", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["Baseline.Plaquenil", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "ACEI.ARB", "Smoking.History.", "Anticoagulation.")] = 1
predM["ACEI.ARB", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "Smoking.History.", "Anticoagulation.")] = 1
predM["Smoking.History.", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Anticoagulation.")] = 1
predM["Anticoagulation.", c("Age", "Gender", "Race", "SNF.", "Diabetes", "HTN", "COPD", "Asthma", "Hx.of.DVT", "CKD", "Cancer", "Hx.of.MI", "CVD", "CHF", "Hypo.thyroidism", "Steroids.or.IMT", "Baseline.Plaquenil", "ACEI.ARB", "Smoking.History.")] = 1


imputed <- mice(covid, method=meth, predictorMatrix=predM, m=num_imputations, maxit=50)



### Do univariate analysis
# Using median-P from https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-017-0404-7
test_matrices <- list(
c("Gender", "M", "F"),
c("SNF.", "Yes", "No"),
c("Race", "White", "White"),
c("Race", "Asian", "White"),
c("Race", "African American", "White"),
c("Race", "American Indian or Alaskan Native", "White"),
c("Race", "Native Hawaiian or Pacific Islander", "White"),
c("Race", "Other", "White"),
c("Diabetes", "Yes", "No"),
c("HTN", "Yes", "No"),
c("COPD", "Yes", "No"),
c("Asthma", "Yes", "No"),
c("CVD", "Yes", "No"),
c("CHF", "Yes", "No"),
c("CKD", "Yes", "No"),
c("Cancer", "Yes", "No"),
c("Hx.of.DVT", "Yes", "No"),
c("Hypo.thyroidism", "Yes", "No"),
c("Hx.of.MI", "Yes", "No"),
c("Smoking.History.", "Former", "Never"),
c("Smoking.History.", "Never", "Never"),
c("Smoking.History.", "Active", "Never"),
c("Steroids.or.IMT", "Yes", "No"),
c("Baseline.Plaquenil", "Yes", "No"),
c("ACEI.ARB", "Yes", "No"),
c("Anticoagulation.", "Yes", "No"))

adjusted_risk_ratios <- pool(with(imputed, glm(Hospitalized.for.COVID ~ Gender + SNF. + Race + Diabetes + HTN + COPD + Asthma + CVD + CHF + CKD + Cancer + Hx.of.DVT + Hypo.thyroidism + Hx.of.MI + Smoking.History. + Steroids.or.IMT + Baseline.Plaquenil + ACEI.ARB + Anticoagulation., family = binomial)))
print(adjusted_risk_ratios)

print(paste("value,num,total-num,percentage,95-CI-pct-low,95-CI-pct-high,num-hospitalized,total-hospitalized,pct-hospitalized,95-CI-pct-hospitalized-low,95-CI-pct-hospitalized-high,num-non-hospitalized,total-non-hospitalized,pct-non-hospitalized,95-CI-pct-non-hospitalized-low,95-CI-pct-non-hospitalized-high,odds-ratio,95-CI-odds-ratio-low,95-CI-odds-ratio-high,fisher-p-val,chisq-p-val", sep=","))
for (test_matrix in test_matrices) {
	if (test_matrix[2] == test_matrix[3]) {
		curr <- matrix(c(
			nrow(subset(covid, eval(parse(text = test_matrix[1])) == test_matrix[2] & Hospitalized.for.COVID == 'Yes')),
			nrow(subset(covid, eval(parse(text = test_matrix[1])) == test_matrix[2] & Hospitalized.for.COVID == 'No')),
			nrow(subset(covid, eval(parse(text = test_matrix[1])) != test_matrix[3] & Hospitalized.for.COVID == 'Yes')),
			nrow(subset(covid, eval(parse(text = test_matrix[1])) != test_matrix[3] & Hospitalized.for.COVID == 'No'))), nrow = 2, ncol = 2 )
	} else {
		curr <- matrix(c(
			nrow(subset(covid, eval(parse(text = test_matrix[1])) == test_matrix[2] & Hospitalized.for.COVID == 'Yes')),
			nrow(subset(covid, eval(parse(text = test_matrix[1])) == test_matrix[2] & Hospitalized.for.COVID == 'No')),
			nrow(subset(covid, eval(parse(text = test_matrix[1])) == test_matrix[3] & Hospitalized.for.COVID == 'Yes')),
			nrow(subset(covid, eval(parse(text = test_matrix[1])) == test_matrix[3] & Hospitalized.for.COVID == 'No'))), nrow = 2, ncol = 2 )
	}

	all_total <- (curr[1][1] + curr[2][1] + curr[3][1] + curr[4][1])
	all <- (curr[1][1] + curr[2][1])
	all_ci <- exactci(all, all_total, conf.level = 0.95)
	all_str <- paste(all, all_total, all / all_total, all_ci$conf.int[1][1], all_ci$conf.int[2][1], sep=",")

	hosp_total <- curr[1][1] + curr[3][1]
	hosp <- curr[1][1]
	hosp_ci <- exactci(hosp, hosp_total, conf.level = 0.95)
	hosp_str <- paste(hosp, hosp_total, hosp / hosp_total, hosp_ci$conf.int[1][1], hosp_ci$conf.int[2][1], sep=",")

	non_total <- curr[2][1] + curr[4][1]
	non <- curr[2][1]
	non_ci <- exactci(non, non_total, conf.level = 0.95)
	non_str <- paste(non, non_total, non / non_total, non_ci$conf.int[1][1], non_ci$conf.int[2][1], sep=",")

	invisible(capture.output(odds <- oddsratio(curr)))

	fisher_p_vals <- c()
	chisq_p_vals <- c()
	for (i in 1:num_imputations) {
		data <- complete(imputed, i)
		if (test_matrix[2] == test_matrix[3]) {
			curr <- matrix(c(
				nrow(subset(data, eval(parse(text = test_matrix[1])) == test_matrix[2] & Hospitalized.for.COVID == 'Yes')),
				nrow(subset(data, eval(parse(text = test_matrix[1])) == test_matrix[2] & Hospitalized.for.COVID == 'No')),
				nrow(subset(data, eval(parse(text = test_matrix[1])) != test_matrix[3] & Hospitalized.for.COVID == 'Yes')),
				nrow(subset(data, eval(parse(text = test_matrix[1])) != test_matrix[3] & Hospitalized.for.COVID == 'No'))), nrow = 2, ncol = 2 )
		} else {
			curr <- matrix(c(
				nrow(subset(data, eval(parse(text = test_matrix[1])) == test_matrix[2] & Hospitalized.for.COVID == 'Yes')),
				nrow(subset(data, eval(parse(text = test_matrix[1])) == test_matrix[2] & Hospitalized.for.COVID == 'No')),
				nrow(subset(data, eval(parse(text = test_matrix[1])) == test_matrix[3] & Hospitalized.for.COVID == 'Yes')),
				nrow(subset(data, eval(parse(text = test_matrix[1])) == test_matrix[3] & Hospitalized.for.COVID == 'No'))), nrow = 2, ncol = 2 )
		}

		fisher <- uncondExact2x2(curr[1][1], curr[1][1]+curr[2][1], curr[3][1], curr[3][1] + curr[4][1], parmtype = "oddsratio", conf.int = FALSE, midp = TRUE)
		chisq <- chisq.test(curr)
		fisher_p_vals <- c(fisher_p_vals, fisher$p.value)
		chisq_p_vals <- c(chisq_p_vals, chisq$p.value)
	}

	print(paste(paste(test_matrix[1], test_matrix[2], sep="-"), all_str, hosp_str, non_str, odds$estimate, odds$conf.int[1], odds$conf.int[2], median(fisher_p_vals), median(chisq_p_vals), sep=","))
}


### Perform LOO calibration
model_formulation <- as.formula("Hospitalized.for.COVID ~ Age + SNF.")
# model_formulation <- Hospitalized.for.COVID ~ Age + SNF. + HTN + Hx.of.DVT + CKD + Cancer + CVD + CHF + Steroids.or.IMT + ACEI.ARB + Anticoagulation.

patients <- unique(covid$pat_num)
mse = 0
for (patient in patients) {
	curr_row <- covid %>% filter(pat_num == patient)
	sum = 0
	for (i in 1:num_imputations) {
		data_set <- complete(imputed, i)
		train_data <- data_set %>% filter(pat_num != patient)
		test_data <- data_set %>% filter(pat_num == patient)
		model <- glm(model_formulation, family = binomial, data = train_data)
		response <- predict(model, newdata = test_data, type="response")
		sum = sum + response
	}
	avg = sum/num_imputations
	if (curr_row$Hospitalized.for.COVID[1] == "Yes") {
		print(paste(patient, 1, avg, sep=","))
		mse = mse + ((1 - avg) * (1 - avg))
	} else {
		print(paste(patient, 0, avg, sep=","))
		mse = mse + (avg * avg)
	}
}
print(paste("MSE: ", mse / length(patients)))

adjusted_risk_ratios <- with(imputed, glm(model_formulation, family = binomial))
