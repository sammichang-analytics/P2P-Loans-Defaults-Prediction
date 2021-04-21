library(lubridate)

library(fiftystater)

install.packages("dplyr")
library(dplyr)

data <- read.csv("lc_loan.csv", header = TRUE)


#delete useless variable
lc_loan1 <- subset(data, select = -c(url,funded_amnt, desc,zip_code,pymnt_plan, addr_state,initial_list_status,out_prncp,
                                        out_prncp_inv,total_pymnt,total_pymnt_inv,total_rec_prncp,total_rec_int,
                                        total_rec_late_fee,collection_recovery_fee,last_pymnt_d,last_pymnt_amnt,
                                        next_pymnt_d,last_credit_pull_d, recoveries, dti_joint, open_il_6m, funded_amnt_inv, sub_grade))

#delete NA variable 
lc_loan2 <- subset(lc_loan1, select = -c(verification_status_joint,annual_inc_joint, open_acc_6m,
                                         open_il_24m,mths_since_rcnt_il,total_bal_il,open_rv_12m,
                                         total_cu_tl,inq_last_12m, open_il_12m, il_util, open_rv_24m, max_bal_bc,all_util, inq_fi))


#delete variables we can't use for decision tree 
lc_loan3 <- subset(lc_loan2, select = -c(emp_title, id, member_id, title, policy_code))

##delete the rows that reflect the loans that are in progress
del = (lc_loan3$loan_status == "Current" | lc_loan3$loan_status == "Late (31-120 days)" | lc_loan3$loan_status == 'In Grace Period'
        | lc_loan3$loan_status == 'Issued' | lc_loan3$loan_status == 'Late (16-30 days)' 
            | lc_loan3$loan_status == 'Does not meet the credit policy. Status:Fully Paid' | lc_loan3$loan_status == 'Does not meet the credit policy. Status:Charged Off')

lc_loan4 = subset(lc_loan3, del == FALSE)


##make dummy variable for Default,Charged Off(1) and Fully Paid(0)
lc_loan4$Default = ifelse(lc_loan4$loan_status == 'Fully Paid', "N", "Y")
#delete original loan_status
lc_loan5 = subset(lc_loan4, select = -c(loan_status))
lc_loan5$default = ifelse(lc_loan5$Default == "Y",1,0)

lc_loan5 = subset(lc_loan5, select = -c(other))
lc_loan5 = subset(lc_loan5, select = -c(mortagage, Default11))



revbal_to_inc = (lc_loan5$annual_inc/12)/(lc_loan5$revol_bal)
lc_loan5$rev_bal_to_inc = c(revbal_to_inc)
lc_loan5$rev_bal_to_inc[which(lc_loan5$rev_bal_to_inc == "Inf")] = 20

##make a new variable: monthly income to monthly payment ratio
inc_to_installment  = (lc_loan5$annual_inc/12)/(lc_loan5$installment)
lc_loan5$inc_to_installment = c(inc_to_installment)



##calculate new variable for new DTI
lc_loan5$newDTI = (lc_loan5$dti*(lc_loan5$annual_inc/12) + lc_loan5$installment) / (lc_loan5$annual_inc/12)

lc_loan5$dti_diff = lc_loan5$newDTI - lc_loan5$dti

num_vars <- 
  lc_loan5 %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()

library(corrplot)
corrplot::corrplot(cor(lc_loan5[, num_vars], use = "complete.obs"), 
                   method = "square", type = "upper")



#make dummy variable for rent 
lc_loan5$rent = ifelse(lc_loan5$home_ownership == 'RENT', 1, 0)
#make dummy variable for own
lc_loan5$own = ifelse(lc_loan5$home_ownership == 'OWN', 1, 0)
#make dummy variable for mortgage 
lc_loan5$mortgage = ifelse(lc_loan5$home_ownership == 'MORTGAGE', 1, 0)
#make dummy variable for other
lc_loan5$other = ifelse(lc_loan5$home_ownership == 'OTHER', 1, 0)


#delete original home_ownership
lc_loan6 = subset(lc_loan5, select = -c(home_ownership))




##manipulate  emp_length

lc_loan6$emp_length_lessthan1 = ifelse(lc_loan6$emp_length == "< 1 year", 1,0)

lc_loan6$emp_length_1to5 = ifelse(lc_loan6$emp_length == "1 year" |lc_loan6$emp_length == "2 year"|
                                   lc_loan6$emp_length == "3 years" |lc_loan6$emp_length == "4 years"|
                                   lc_loan6$emp_length == "5 years", 1,0)

lc_loan6$emp_length_6to9 = ifelse(lc_loan6$emp_length == "6 years" |lc_loan6$emp_length == "7 years"|
                                   lc_loan6$emp_length == "8 years" |lc_loan6$emp_length == "9 years", 1,0)
lc_loan6$emp_length_10plus = ifelse(lc_loan6$emp_length == "10+ years", 1, 0)
lc_loan6$emp_length_is_NA = ifelse(lc_loan6$emp_length == "NA years", 1, 0)

#delete original emp_length
lc_loan7 = subset(lc_loan6, select = -c(emp_length))



#make dummy variable for grade_a
lc_loan7$grade_a = ifelse(lc_loan7$grade == 'A', 1, 0)
#make dummy variable for grade_b
lc_loan7$grade_b = ifelse(lc_loan7$grade == 'B', 1, 0)
#make dummy variable for grade_c
lc_loan7$grade_c = ifelse(lc_loan7$grade == 'C', 1, 0)
#make dummy variable for grade_e
lc_loan7$grade_e = ifelse(lc_loan7$grade == 'E', 1, 0)
#make dummy variable for grade_f
lc_loan7$grade_f = ifelse(lc_loan7$grade == 'F', 1, 0)
#make dummy variable for grade_g
lc_loan7$grade_g = ifelse(lc_loan7$grade == 'G', 1, 0)

#delete original grade
lc_loan8 = subset(lc_loan7, select = -c(grade))



#make dummy variable for purpose-credit_card
lc_loan8$Credit_Card = ifelse(lc_loan8$purpose == 'credit_card', 1, 0)
#make dummy variable for purpose-car
lc_loan8$Car = ifelse(lc_loan8$purpose == 'car', 1, 0)
#make dummy variable for purpose-small_business
lc_loan8$Small_Business = ifelse(lc_loan8$purpose == 'small_business', 1, 0)
#make dummy variable for purpose-other
lc_loan8$Purpose_Other = ifelse(lc_loan8$purpose == 'other', 1, 0)
#make dummy variable for purpose-wedding 
lc_loan8$Wedding = ifelse(lc_loan8$purpose == 'wedding', 1, 0)
#make dummy variable for purpose-debt_consolidation
lc_loan8$Debt_Consolidation = ifelse(lc_loan8$purpose == 'debt_consolidation', 1, 0)
#make dummy variable for purpose-home_improvement
lc_loan8$Home_Improvement = ifelse(lc_loan8$purpose == 'home_improvement', 1, 0)
#make dummy variable for purpose-major_purchase
lc_loan8$Major_Purchase = ifelse(lc_loan8$purpose == 'major_purchase', 1, 0)
#make dummy variable for purpose-medical
lc_loan8$Medical = ifelse(lc_loan8$purpose == 'medical', 1, 0)
#make dummy variable for purpose-moving
lc_loan8$Moving = ifelse(lc_loan8$purpose == 'moving', 1, 0)
#make dummy variable for purpose-vacation
lc_loan8$Vacation = ifelse(lc_loan8$purpose == 'vacation', 1, 0)
#make dummy variable for purpose-house
lc_loan8$House = ifelse(lc_loan8$purpose == 'house', 1, 0)
#make dummy variable for purpose-renewable_energy
lc_loan8$Renewable = ifelse(lc_loan8$purpose == 'renewable_energy', 1, 0)

lc_loan9 = subset(lc_loan8, select = -c(purpose))



###### make variable for the age of the credit line at the loan issue date 

##convert the loan issue date to date format
c <-parse_date_time(lc_loan9$issue_d, "by")
issue_date <- as.Date(c, format =  "%b-%Y")
##convert the earliest credit line date to date format 
d <- parse_date_time(lc_loan9$earliest_cr_line, "by")
cr_date <- as.Date(d, format =  "%b-%Y")
##find age of the credit line
cr_age = difftime(issue_date, cr_date)
lc_loan9$credit_age = cr_age/12


lc_loan10 = subset(lc_loan9, select = -c(earliest_cr_line))




##dummy for delinq
lc_loan10$Delinq=ifelse(is.na(lc_loan10$mths_since_last_delinq), median(lc_loan10$mths_since_last_delinq,na.rm=T), lc_loan10$mths_since_last_delinq)
#lc_loan10$Delinq = ifelse(is.na(lc_loan10$mths_since_last_delinq), -1, lc_loan10$mths_since_last_delinq)
#lc_loan10$Delinq_gr50 = ifelse(lc_loan10$Delinq > 50, 1, 0)
#lc_loan10$Delinq_lss50 = ifelse(lc_loan10$Delinq < 50 & lc_loan10$Delinq > 0, 1, 0)
lc_loan11 = subset(lc_loan10, select = -c(mths_since_last_delinq))


##dummy for derog 
lc_loan11$Derog = ifelse(is.na(lc_loan11$mths_since_last_major_derog), median(lc_loan11$mths_since_last_major_derog,na.rm=T), lc_loan11$mths_since_last_major_derog)
#lc_loan11$Derog_gr25 = ifelse(lc_loan11$Derog > 25, 1, 0)
#lc_loan11$Derog_lss25 = ifelse(lc_loan11$Derog < 25 & lc_loan11$Derog > 0, 1, 0)
lc_loan12 = subset(lc_loan11, select = -c(mths_since_last_major_derog))

#make dummy variable for application_type
lc_loan12$application_type_dummy = ifelse(lc_loan12$application_type == 'INDIVIDUAL', 1, 0)
lc_loan13=subset(lc_loan12, select = -c(application_type))

##dummy for term 
lc_loan13$thirtysix_mo = ifelse(lc_loan13$term == " 36 months", 1, 0)
lc_loan14=subset(lc_loan13, select = -c(term))

lc_loan14$Last_Record=ifelse(is.na(lc_loan14$mths_since_last_record), median(lc_loan14$mths_since_last_record,na.rm=T), lc_loan14$mths_since_last_record)
lc_loan15 = subset(lc_loan14, select = -c(mths_since_last_record))


##remove any rows with NA in any column 
lc_loan16 = na.omit(lc_loan15)



##feature engineering 

##make a new variable: monthly revolving balance to monthly income
revbal_to_inc = (lc_loan16$annual_inc/12)/(lc_loan16$revol_bal)
lc_loan16$revbal_to_inc1 = c(revbal_to_inc)
lc_loan16$revbal_to_inc1[which(lc_loan16$revbal_to_inc1 == "Inf")] = 20

##make a new variable: monthly income to monthly payment ratio
inc_to_installment  = (lc_loan16$annual_inc/12)/(lc_loan16$installment)
lc_loan16$inc_to_installment1 = c(inc_to_installment)

##calculate new variable for new DTI
lc_loan16$newDTI = (lc_loan16$dti*(lc_loan16$annual_inc/12) + lc_loan16$installment) / (lc_loan16$annual_inc/12)

lc_loan16$dti_diff = lc_loan16$newDTI - lc_loan16$dti

#write.csv(lc_loan16, "proj24_273.csv")

           
           
