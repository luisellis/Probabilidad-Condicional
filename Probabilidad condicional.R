#Guides:
#https://community.rstudio.com/t/conditional-probability-with-dplyr/5117/4
#https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html


#	
#Experiment

#For first experiment we use the avg mean to group for this value the conditional probabilities(since we need to make the continuous variable a categorical one)
mean(diabetes$age)
diabetes %>%
  mutate(age_cat = ifelse(age >= 45, "at least 45", "less than 45")) %>%
  count(age_cat, gender) %>%
  spread(gender, n) %>%
  mutate(prop = female / (female + male))

#We then do it again with two continuous variables
diabetes%>%
  filter(
    !is.na(chol), 
    !is.na(hdl)
  ) %>% 
	mutate(chol_cat = ifelse(chol >=240, "High cholesterol", "Borderline or Good cholesterol")) %>%
	mutate(hdl_cat = ifelse(hdl >= 50, "High Density Lipoprotein over 50", "Under 50")) %>%
	count(chol_cat, hdl_cat) %>%
	spread(hdl_cat, n) %>%
	mutate(prop = `High Density Lipoprotein over 50` / (`High Density Lipoprotein over 50` + `Under 50`))

#And lastly, for 2 categorical variables:
diabetes %>% 
	count(location, gender) %>% 
	spread(gender,n) %>%
	mutate(prop = female / (female + male))



#Regression analysis 

#Cleaning
diabetes_cat <- dummy.data.frame(diabetes, names = c("location","gender","frame"), sep="_")
diabetes_cat <- diabetes_cat[,- c(1,20,21)]
#Model building
model <- lm(chol ~ ., data = diabetes_cat)
k <- ols_step_backward_aic(model)
k

formula <- as.formula("chol ~ hdl + ratio + glyhb + location_Buckingham + location_Louisa + gender_male + height + weight + frame_large +
						frame_medium + bp.1d + hip + time.ppn")
diabetes_model <- lm(formula, data = diabetes_cat)
summary(diabetes_model)
