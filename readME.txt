This project aimed to predict whether a loan would default given characertistics such as the loan amount and borrower financial history.
Prediction accuracy topped 65% on the test set, which is higher than any research studies that have previously been done on the same 
P2P Lending dataset (so far, at least in the studies that I have seen). Our research utilized a new, engineered variable named dti_diff, 
which is the difference between the new debt-to-income ratio and the old debt-to-income ratio.
It effectively characterizes the change in financial burden of the borrower.  This study used ML techniques: decision trees, random forest, 
naive bayes and logistic regression with oversampling to remedy the problem of class imbalance.
