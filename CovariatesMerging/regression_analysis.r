library(survey)
library(tidyverse)
library(multcomp)

df_all <- read.csv("CleanedData.csv", strip.white = T)

adj_name_lst <- c("RIDAGEYR", "gender", "education_dichotomous", "marital_status", "race", "smoking_status", "alcohol")
#adj_type_lst <- c("n", "f", "f", "f", "f", "f", "n")
#adj_ref_lst <- c("", "Male", "", "Not Married", "Other", "0", "1")
#for(i in 1:length(adj_name_lst)){
 #   if(adj_type_lst[i] == "f"){
  #      df_all[, adj_name_lst[i]] <- relevel(factor(df_all[, adj_name_lst[i]]), ref = adj_ref_lst[i])
   # }
#}

data_info <- list(
    "RIDAGEYR" = list(type = "n", weight_var = "WTINT2YR"),
    "gender" = list(type = "f", weight_var = "WTINT2YR", ref = "Male"),
    "race" = list(type = "f", weight_var = "WTINT2YR", ref = "Other"),
    "marital_status" = list(type = "f", weight_var = "WTINT2YR", ref = ""),
    "education_dichotomous" = list(type = "f", weight_var = "WTINT2YR", ref = ""),
    "smoking_status" = list(type = "f", weight_var = "WTMEC2YR", ref = "0"),
    "alcohol" = list(type = "n", weight_var = "WTMEC2YR"),
    "sleep_duration" = list(type = "n", weight_var = "WTINT2YR"),
    "sleep_disorder" = list(type = "f", weight_var = "WTINT2YR", ref = "0"),
    "BMXBMI" = list(type = "n", weight_var = "WTMEC2YR"),
    "Obesity" = list(type = "f", weight_var = "WTMEC2YR", ref = "0"),
    "BMXWAIST" = list(type = "n", weight_var = "WTMEC2YR"),
    "central_adiposity" = list(type = "f", weight_var = "WTMEC2YR", ref = "0"),
    "LBXGLU" = list(type = "n", weight_var = "WTSAF2YR"),
    "diabetes_status" = list(type = "f", weight_var = "WTSAF2YR", ref = "0"),
    "systolic_BP" = list(type = "n", weight_var = "WTMEC2YR"),
    "diastolic_BP" = list(type = "n", weight_var = "WTMEC2YR"),
    "HTN_status" = list(type = "f", weight_var = "WTMEC2YR", ref = "0"),
    "HTN_old_status" = list(type = "f", weight_var = "WTMEC2YR", ref = "0"),
    "cvd_status" = list(type = "f", weight_var = "WTINT2YR", ref = "0"),
    "physical_function" = list(type = "f", weight_var = "WTMEC2YR", ref = "0"),
    "IS60" = list(type = "n", weight_var = "WTINT2YR"),
    "IV60" = list(type = "n", weight_var = "WTINT2YR"),
    "M10_midpoint" = list(type = "n", weight_var = "WTINT2YR"),
    "M10_cnt" = list(type = "n", weight_var = "WTINT2YR"),
    "L5_midpoint" = list(type = "n", weight_var = "WTINT2YR"),
    "L5_cnt" = list(type = "n", weight_var = "WTINT2YR"),
    "RA" = list(type = "n", weight_var = "WTINT2YR")
)

df_all$M10_cnt_100 <- df_all$M10_cnt / 100

for(key in names(data_info)){
    if(data_info[[key]]$type == "f" & key %in% colnames(df_all)){
        df_all[, key] <- relevel(factor(df_all[, key]), ref = data_info[[key]]$ref)
    }
}

digits <- 3

get_weight_var <- function(var_lst){
    weight_var_lst <- c()
    for(var in var_lst){
        weight_var <- data_info[[var]][["weight_var"]]
        weight_var_lst <- c(weight_var_lst, weight_var)
    }
    weight_var_lst <- unique(weight_var_lst)
    weight_var_name <- "WTINT2YR"
    if("WTSAF2YR" %in% weight_var_lst){
        weight_var_name <- "WTSAF2YR"
    } else if("WTMEC2YR" %in% weight_var_lst){
        weight_var_name <- "WTMEC2YR"
    }
    return(weight_var_name)
}

#design <- svydesign(data = df_all, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, nest = T)
#df <- subset(design, inAnalysis == "True")

#weight_var <- "WTMEC2YR"

reg_func <- function(df, input_name_lst, output_name_lst, adj_name_lst, model_name){
    input_ret <- c()
    output_ret <- c()
    adj_ret <- c()
    model_ret <- c()
    est_ret <- c()
    std_ret <- c()
    pval_ret <- c()
    est_t1_ret <- c()
    std_t1_ret <- c()
    pval_t1_ret <- c()
    est_t2_ret <- c()
    std_t2_ret <- c()
    pval_t2_ret <- c()
    est_t3_ret <- c()
    std_t3_ret <- c()
    pval_t3_ret <- c()
    sample_size_ret <- c()
    for(input in input_name_lst){
        for(output in output_name_lst){
            for(adj in c(1, 2, 3, 4)){
                weight_var <- ""
                push <- T
                adj_curr <- adj
                if(adj == 1){
                    weight_var <- get_weight_var(c(input, output))
                } else if(adj == 2){
                    weight_var <- get_weight_var(c(input, output, adj_name_lst))
                } else if(adj == 3){
                    if(output %in% c("HTN_status", "HTN_old_status", "cvd_status", "systolic_BP", "diastolic_BP")){
                      weight_var <- get_weight_var(c(input, output, c(adj_name_lst, "BMXBMI")))
                    } else{
                      push <- F
                    }
                } else{
                    weight_var <- get_weight_var(c(input, output, adj_name_lst, "sleep_disorder"))
                    if(!(output %in% c("HTN_status", "HTN_old_status", "cvd_status", "systolic_BP", "diastolic_BP"))){
                      adj_curr <- 3
                    }
                }
                if(push){
                    f_weight <- as.formula(paste0("~", weight_var))
                    df_curr <- df[!is.na(df[, weight_var]),]
                    sample_size_curr <- nrow(df_curr)
                    sample_size_ret <- c(sample_size_ret, sample_size_curr)
                    dsg <- svydesign(data = df_curr, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = f_weight, nest = T)
                    df_sub <- subset(dsg, inAnalysis == "True")
                    if(model_name == "linear"){
                        formula_str <- paste0(output, " ~ ", input)
                        if(adj == 1){
                            formula_m <- as.formula(formula_str)
                        } else if(adj == 2){
                            formula_m <- as.formula(paste0(formula_str, " + ", paste0(adj_name_lst, collapse="+")))
                        } else if (adj == 3){
                            formula_m <- as.formula(paste0(formula_str, " + ", paste0(c(adj_name_lst, "BMXBMI"), collapse="+")))
                        } else{
                            formula_m <- as.formula(paste0(formula_str, " + ", paste0(c(adj_name_lst, "sleep_disorder"), collapse="+")))
                        }
                        input_name <- input
                        model <- svyglm(formula_m, df_sub, family = "gaussian") #lm(formula_m, data = df, weights = df[, weight_var])
                        
                        est_t1_ret <- c(est_t1_ret, NA)
                        std_t1_ret <- c(std_t1_ret, NA)
                        pval_t1_ret <- c(pval_t1_ret, NA)
                        est_t2_ret <- c(est_t2_ret, NA)
                        std_t2_ret <- c(std_t2_ret, NA)
                        pval_t2_ret <- c(pval_t2_ret, NA)
                        est_t3_ret <- c(est_t3_ret, NA)
                        std_t3_ret <- c(std_t3_ret, NA)
                        pval_t3_ret <- c(pval_t3_ret, NA)
                    } else{
                        formula_str <- paste0(output, " ~ ", input)
                        formula_str_ordered <- paste0(output, " ~ ordered(", input, ")")
                        if(adj == 1){
                            formula_m <- as.formula(formula_str)
                            formula_m_ordered <- as.formula(formula_str_ordered)
                        } else if(adj == 2){
                            formula_m <- as.formula(paste0(formula_str, " + ", paste0(adj_name_lst, collapse="+")))
                            formula_m_ordered <- as.formula(paste0(formula_str_ordered, " + ", paste0(adj_name_lst, collapse="+")))
                        } else if(adj == 3){
                            formula_m <- as.formula(paste0(formula_str, " + ", paste0(c(adj_name_lst, "BMXBMI"), collapse="+")))
                            formula_m_ordered <- as.formula(paste0(formula_str_ordered, " + ", paste0(c(adj_name_lst, "BMXBMI"), collapse="+")))
                        } else{
                            formula_m <- as.formula(paste0(formula_str, " + ", paste0(c(adj_name_lst, "sleep_disorder"), collapse="+")))
                            formula_m_ordered <- as.formula(paste0(formula_str_ordered, " + ", paste0(c(adj_name_lst, "sleep_disorder"), collapse="+")))
                        }
                        input_name <- paste0("ordered(", input, ").L")
                        #design_obj <- svydesign(ids = ~0, data = df, weights = df[, weight_var])
                        model_tertiles <- svyglm(formula_m, df_sub, family = binomial(link = "logit")) #glm(formula_m, data = df, weights = df[, weight_var], family="binomial")
                        model <- svyglm(formula_m_ordered, df_sub, family = binomial(link = "logit")) #glm(formula_m_ordered, data = df, weights = df[, weight_var], family="binomial")
                        coef_model_tertiles <- summary(model_tertiles)$coefficients
                        est_t1_ret <- c(est_t1_ret, coef_model_tertiles[1, 1])
                        std_t1_ret <- c(std_t1_ret, coef_model_tertiles[1, 2])
                        pval_t1_ret <- c(pval_t1_ret, coef_model_tertiles[1, 4])
                        est_t2_ret <- c(est_t2_ret, coef_model_tertiles[2, 1])
                        std_t2_ret <- c(std_t2_ret, coef_model_tertiles[2, 2])
                        pval_t2_ret <- c(pval_t2_ret, coef_model_tertiles[2, 4])
                        est_t3_ret <- c(est_t3_ret, coef_model_tertiles[3, 1])
                        std_t3_ret <- c(std_t3_ret, coef_model_tertiles[3, 2])
                        pval_t3_ret <- c(pval_t3_ret, coef_model_tertiles[3, 4])
                    }
                    coef_model <- summary(model)$coefficients
    
                    input_ret <- c(input_ret, input)
                    output_ret <- c(output_ret, output)
                    adj_ret <- c(adj_ret, adj_curr)
                    model_ret <- c(model_ret, model_name)
                    est_ret <- c(est_ret, coef_model[input_name, 1])
                    std_ret <- c(std_ret, coef_model[input_name, 2])
                    if(model_name == "linear"){
                        pval_ret <- c(pval_ret, coef_model[input_name, 4])
                    } else{
                        trend_model <- glht(model_tertiles, matrix(c(0, 0, 1, rep(0, length(coefficients(model_tertiles)) - 3)), nrow = 1))
                        pval_ret <- c(pval_ret, summary(trend_model)$test$pvalue[[1]])
                    }
                }
            }
        }
    }
    df_ret <- data.frame(Input = input_ret, Output = output_ret, AdjModel = adj_ret, RegModel = model_ret, Estimates = est_ret, Std.Err = std_ret, P.Value = pval_ret, EstimatesTertile1 = est_t1_ret, Std.ErrTertile1 = std_t1_ret, P.ValueTertile1 = pval_t1_ret, EstimatesTertile2 = est_t2_ret, Std.ErrTertile2 = std_t2_ret, P.ValueTertile2 = pval_t2_ret, EstimatesTertile3 = est_t3_ret, Std.ErrTertile3 = std_t3_ret, P.ValueTertile3 = pval_t3_ret, SampleSize = sample_size_ret)
    return(df_ret)
}

compare_func <- function(df, comp_name_lst, comp_type_lst){
    compute_meanStd <- function(data, var_name){
        f <- formula(paste0("~", var_name))
        comp_obj <- svymean(f, data, na.rm = T)
        mu <- round(comp_obj[[1]], 2)
        #std <- round(SE(comp_obj)[1], digits)
        comp_obj <- svyvar(f, data, na.rm = T)
        std <- round((comp_obj[[1]])^0.5, 2)
        return(paste0(mu, " (", std, ")"))
    }
    compute_freq <- function(data, var_name){
        f <- formula(paste0("~", var_name))
        comp_obj <- svymean(f, data, na.rm = T)
        mu_mat <- round(as.matrix(comp_obj), digits) * 100
        total_obj <- svytotal(f, data, na.rm = T)
        total_mat <- round(as.matrix(total_obj), digits)
        return(list(mu=mu_mat, total=total_mat))
    }
    
    cutoff <- median(df$RA, na.rm = T)
    print(cutoff)
    char_lst <- c()
    overall_lst <- c()
    below_lst <- c()
    above_lst <- c()
    pval_lst <- c()
    ra_binary <- as.numeric(df$RA > cutoff)
    df["RA_above"] <- ra_binary
    for(i in 1:length(comp_name_lst)){
        if(comp_name_lst[i] %in% colnames(df)){
            if(comp_type_lst[i] == "n"){
                weight_var <- get_weight_var(c("RA", comp_name_lst[i]))
                #print(paste(comp_name_lst[i], weight_var))
                f_weight <- as.formula(paste0("~", weight_var))
                df_curr <- df[!is.na(df[, weight_var]),]
                dsg <- svydesign(data = df_curr, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = f_weight, nest = T)
                df_sub <- subset(dsg, inAnalysis == "True")
                f_ttest <- as.formula(paste0(comp_name_lst[i], "~ RA_above"))
                t_test <- svyttest(f_ttest, df_sub)
                muStd_all <- compute_meanStd(df_sub, comp_name_lst[i])
                muStd_above <- compute_meanStd(subset(dsg, RA_above == 1), comp_name_lst[i])
                muStd_below <- compute_meanStd(subset(dsg, RA_above == 0), comp_name_lst[i])
                #var_x <- df[df[, "RA"] <= cutoff, comp_name_lst[i]]
                #var_y <- df[df[, "RA"] > cutoff, comp_name_lst[i]]
                #t_test <- t.test(var_x, var_y)
                
                char_lst <- c(char_lst, comp_name_lst[i])
                overall_lst <- c(overall_lst, muStd_all)
                below_lst <- c(below_lst, muStd_below)
                above_lst <- c(above_lst, muStd_above)
                pval_lst <- c(pval_lst, t_test$p.value)
            } else{
                #chisq_test <- chisq.test(df[, comp_name_lst[i]], ra_binary)
                var <- factor(df[, comp_name_lst[i]])
                #anova_test <- lm(ra_binary ~ var)
                #anova_coef <- summary(anova_test)$coefficients
                weight_var <- get_weight_var(c("RA", comp_name_lst[i]))
                #print(paste(comp_name_lst[i], weight_var))
                f_weight <- as.formula(paste0("~", weight_var))
                df_curr <- df[!is.na(df[, weight_var]),]
                dsg <- svydesign(data = df_curr, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = f_weight, nest = T)
                df_sub <- subset(dsg, inAnalysis == "True")
                f_chisq <- as.formula(paste0("~ RA_above + ", comp_name_lst[i]))
                p_val <- svychisq(f_chisq, df_sub, statistics = "Chisq")$p.value[[1]]
                
                freq_all <- compute_freq(df_sub, comp_name_lst[i])
                freq_above <- compute_freq(subset(dsg, RA_above == 1), comp_name_lst[i])
                freq_below <- compute_freq(subset(dsg, RA_above == 0), comp_name_lst[i])
                for(j in 2:length(levels(var))){
                    char_lst <- c(char_lst, paste0(comp_name_lst[i], "-", levels(var)[j]))
                    #overall_lst <- c(overall_lst, nrow(df[df[, comp_name_lst[i]] == levels(var)[j],]))
                    #below_lst <- c(below_lst, nrow(df[(df[, comp_name_lst[i]] == levels(var)[j]) & (df$RA <= cutoff),]))
                    #above_lst <- c(above_lst, nrow(df[(df[, comp_name_lst[i]] == levels(var)[j]) & (df$RA > cutoff),]))
                    keyname <- paste0(comp_name_lst[i], levels(var)[j])
                    overall_percent <- freq_all$mu[keyname,][[1]]
                    below_percent <- freq_below$mu[keyname,][[1]]
                    above_percent <- freq_above$mu[keyname,][[1]]
                    overall_total <- freq_all$total[keyname,][[1]]
                    below_total <- freq_below$total[keyname,][[1]]
                    above_total <- freq_above$total[keyname,][[1]]
                    overall_res <- paste0(overall_percent, "%") #paste0(overall_total, " (", overall_percent, ")")
                    below_res <- paste0(below_percent, "%") #paste0(below_total, " (", below_percent, ")")
                    above_res <- paste0(above_percent, "%") #paste0(above_total, " (", above_percent, ")")
                    
                    overall_lst <- c(overall_lst, overall_res)
                    below_lst <- c(below_lst, below_res)
                    above_lst <- c(above_lst, above_res)
                    pval_lst <- c(pval_lst, p_val)
                }
            }
        }
    }
    df_ret <- data.frame(Characteristics = char_lst, OverallSample = overall_lst, RABelow = below_lst, RAAbove = above_lst, P.Value = pval_lst, RACutoff = cutoff, SampleSize = nrow(df_all))
    return(df_ret)
}

input_name_lst <- c("IS60", "IV60", "M10_midpoint", "M10_cnt_100", "L5_midpoint", "L5_cnt", "RA")
output_name_lst_linear <- c("systolic_BP", "diastolic_BP", "BMXBMI", "BMXWAIST", "LBXGLU")
output_name_lst_logist <- c("HTN_status", "HTN_old_status", "cvd_status", "Obesity", "central_adiposity", "diabetes_status")

df_linear <- reg_func(df_all, input_name_lst, output_name_lst_linear, adj_name_lst, model_name = "linear")

tertile_input_lst <- c()
tertile_cutoff1_lst <- c()
tertile_cutoff2_lst <- c()
for(input in input_name_lst){
    vec <- df_all[, input]
    cutoffs <- quantile(vec, c(1/3, 2/3), na.rm = T)
    df_all[, paste0(input, "_tertile")] <- as.factor((vec > cutoffs[[1]]) + (vec > cutoffs[[2]]) + 1)
    tertile_input_lst <- c(tertile_input_lst, input)
    tertile_cutoff1_lst <- c(tertile_cutoff1_lst, cutoffs[[1]])
    tertile_cutoff2_lst <- c(tertile_cutoff2_lst, cutoffs[[2]])
}
df_cutoff <- data.frame(Input = tertile_input_lst, Cutoff1 = tertile_cutoff1_lst, Cutoff2 = tertile_cutoff2_lst)
write.csv(df_cutoff, "cutoff_results.csv", row.names = F)

df_logist <- reg_func(df_all, paste0(input_name_lst, "_tertile"), output_name_lst_logist, adj_name_lst, model_name = "logist")

df_comb <- data.frame(rbind(df_linear, df_logist))
#print(df_comb)
write.csv(df_comb, "regression_results.csv", row.names = F)

# Missing: sleep_duration, Obesity
comp_name_lst <- c("RIDAGEYR", "gender", "race", "marital_status", "education_dichotomous", "smoking_status", "alcohol", "sleep_duration", "sleep_disorder", "BMXBMI", "Obesity", "BMXWAIST", "central_adiposity", "LBXGLU", "diabetes_status", "systolic_BP", "diastolic_BP", "HTN_status", "cvd_status", "IS60", "IV60", "M10_midpoint", "M10_cnt", "L5_midpoint", "L5_cnt", "RA")
comp_type_lst <- c("n", "f", "f", "f", "f", "f", "n", "n", "f", "n", "f", "n", "f", "n", "f", "n", "n", "f", "f", "n", "n", "n", "n", "n", "n", "n")
df_compare <- compare_func(df_all, comp_name_lst, comp_type_lst)
write.csv(df_compare, "descriptive_results.csv", row.names = F)
