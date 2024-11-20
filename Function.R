## Function to exclude observations from dataset
multiple_exclude <- function(data, conditions) {
  # fit in the tidyverse filter function
  # condition is a string that can be evaluated
  # e.g. "exclude_calor == 50"
  data_new <- data
  n_array <- c()
  exclude_table <- data.frame(Condition = "Original dataset", N = nrow(data))
  for (condition in conditions) {
    now_nrow <- nrow(data_new)
    data_new <- data_new %>% filter(!(!!parse_expr(condition)))
    n_array <- c(n_array, nrow(data_new) - now_nrow)
  }
  exclude_table_add <- data.frame(Condition = conditions, N = n_array)
  exclude_table <- rbind(exclude_table, exclude_table_add)
  exclude_table <- exclude_table %>% add_row(Condition = "Final dataset", N = nrow(data_new))
  return(list(data_new, exclude_table))
}



## Function to calculate HR and 95% CI using COX model
cox_model <- function(followyear, exposure, outcome, variable_cox, dataframe, cont = TRUE) {
  multivariable <- exposure
  for (i in variable_cox) {
    multivariable <- paste0(multivariable, "+", i)
  }
  
  FML_mul <- as.formula(paste0("Surv(", followyear, ",", outcome, "== 1) ~", multivariable))
  model <- coxph(FML_mul, data = dataframe)
  
  if (cont) {
    out_df <- data.frame(coef = summary(model)$coefficients[1, "coef"],
                         exp.coef. = summary(model)$coefficients[1, "exp(coef)"],
                         se.coef. = summary(model)$coefficients[1, "se(coef)"],
                         z = summary(model)$coefficients[1, "z"],
                         p.value = summary(model)$coefficients[1, "Pr(>|z|)"],
                         exposure = exposure,
                         outcome = outcome,
                         category = NA,
                         n = NA,
                         case = NA,
                         person_years = NA)
  } else {
    nlevel <- length(unique(dataframe[[exposure]]))
    py_df <- dataframe %>%
      group_by(!!sym(exposure)) %>% 
      summarise(n = n(), 
                case = sum(!!sym(outcome)),
                person_years = sum(!!sym(followyear))) %>% 
      mutate(person_years = sprintf("%.1f", person_years)) %>% 
      data.frame() %>% 
      select(-!!sym(exposure))
    
    out_df <- data.frame(coef = summary(model)$coefficients[1:(nlevel-1), "coef"],
                         exp.coef. = summary(model)$coefficients[1:(nlevel-1), "exp(coef)"],
                         se.coef. = summary(model)$coefficients[1:(nlevel-1), "se(coef)"],
                         z = summary(model)$coefficients[1:(nlevel-1), "z"],
                         p.value = summary(model)$coefficients[1:(nlevel-1), "Pr(>|z|)"],
                         exposure = exposure,
                         outcome = outcome,
                         category = 1:(nlevel-1))
    out_df <- rbind(c(0, 1, 0, 0, NA, exposure, outcome, 0), out_df)
    out_df <- cbind(out_df, py_df)
    names(out_df)[names(out_df) == "Pr(>|z|)"] <- "p.value"
  }
  out_df <- out_df %>%
    mutate(coef = as.numeric(coef),
           se.coef. = as.numeric(se.coef.),
           HR = paste0(sprintf("%.2f", exp(coef)), " (", 
                       sprintf("%.2f", exp(coef - 1.96 * se.coef.)), ", ", 
                       sprintf("%.2f", exp(coef + 1.96 * se.coef.)), ")"),
           HR = ifelse(se.coef. == 0, "1.00 (Reference)", HR),
           P = p.value %>% as.numeric() %>% sprintf("%.3f", .))
  return(out_df)
}




## Function to calculate HR and 95% CI in subgroup analysis using COX model
cox_subgroup <- function(followyear, exposure, outcome, variable_cox, dataframe, subgroup, cont = TRUE){
  multivariable <- exposure
  for (i in variable_cox) {
    multivariable <- paste0(multivariable, "+", i)
  }
  
  dataframe$subgroup <- pull(dataframe, subgroup) %>% as.character()
  stratas <- unique(dataframe$subgroup) %>% as.character()
  FML_sg <- as.formula(paste0("Surv(", followyear, ",", outcome, "== 1) ~ ", subgroup, "*", multivariable))
  sg_model <- coxph(FML_sg, data = dataframe)
  aov <- anova(sg_model)
  pint <- aov$`Pr(>|Chi|)`[nrow(aov)]
  
  if (cont){
    FML_mul <- as.formula(paste0("Surv(", followyear, ",", outcome, "== 1) ~", multivariable))
    coefs <- map_df(stratas, ~ {
      model_summary <- summary(coxph(FML_mul, data = dataframe %>% filter(subgroup == .x)))
      data.frame(coef = model_summary$coefficients[1, "coef"],
                 se.coef. = model_summary$coefficients[1, "se(coef)"],
                 exp.coef. = model_summary$coefficients[1, "exp(coef)"],
                 z = model_summary$coefficients[1, "z"],
                 p.value = model_summary$coefficients[1, "Pr(>|z|)"])
    })
    sgcox_df <- bind_cols(coefs, 
                          exposure = exposure,
                          outcome = outcome,
                          category = NA,
                          n = NA,
                          case = NA,
                          person_years = NA,
                          subgroup = subgroup,
                          strata = stratas,
                          p_int = sprintf("%.3f", pint))
  } else {
    sgcox_df <- data.frame()
    for (j in stratas) {
      FML_mul <- as.formula(paste0("Surv(", followyear, ",", outcome, "== 1) ~", multivariable))
      model <- coxph(FML_mul, data = dataframe %>% filter(subgroup == j))
      nlevel <- length(unique(dataframe[[exposure]]))
      py_df <- dataframe %>%
        filter(subgroup == j) %>% 
        group_by(!!sym(exposure)) %>% 
        summarise(n = n(), 
                  case = sum(!!sym(outcome)),
                  person_years = sum(!!sym(followyear))) %>% 
        mutate(person_years = sprintf("%.1f", person_years)) %>% 
        data.frame() %>% 
        select(-!!sym(exposure))
      
      out_df <- data.frame(coef = summary(model)$coefficients[1:(nlevel-1), "coef"],
                           exp.coef. = summary(model)$coefficients[1:(nlevel-1), "exp(coef)"],
                           se.coef. = summary(model)$coefficients[1:(nlevel-1), "se(coef)"],
                           z = summary(model)$coefficients[1:(nlevel-1), "z"],
                           p.value = summary(model)$coefficients[1:(nlevel-1), "Pr(>|z|)"],
                           exposure = exposure,
                           outcome = outcome,
                           category = 1:(nlevel-1))
      
      out_df <- rbind(c(0, 1, 0, 0, NA, exposure, outcome, 0), out_df)
      out_df <- cbind(out_df, py_df)
      out_df <- cbind(out_df,
                      subgroup = subgroup,
                      strata = j,
                      p_int = sprintf("%.3f", pint))
      sgcox_df <- rbind(sgcox_df, out_df)
    }
  }
  sgcox_df <- sgcox_df %>%
    mutate(coef = as.numeric(coef),
           se.coef. = as.numeric(se.coef.),
           HR = paste0(sprintf("%.2f", exp(coef)), " (", 
                       sprintf("%.2f", exp(coef - 1.96 * se.coef.)), ", ", 
                       sprintf("%.2f", exp(coef + 1.96 * se.coef.)), ")"),
           HR = ifelse(se.coef. == 0, "1.00 (Reference)", HR),
           P = p.value %>% as.numeric() %>% sprintf("%.3f", .))
  return(sgcox_df)