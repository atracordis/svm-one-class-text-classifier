drug_string="I'm selling some pretty sweet zacateca 
if you're interested call me it's gonna be xtreme, got some weird seed, oxycontin
potent and pure heroin, good kush coming straight from holland man"
drug_string_2="zacateca"
non_drug_string_1="I want to sell some food pizza chawarma love  cream"
non_drug_string_2="Potato?"



predict(svm.model,clean_normalize(drug_string))
predict(svm.model,clean_normalize(drug_string_2))
predict(svm.model,clean_normalize(non_drug_string_1))
predict(svm.model,clean_normalize(non_drug_string_2))


