#Mincer Regression Work - Meeting 3 ====

#Goal: Recode education variable from Categorical to Numeric ----

#Internet Source:
#We can check the class of each column of our data table with the sapply function:
  
  sapply(data, class)                          # Get classes of all columns
#       x1          x2          x3 
# "factor" "character"   "integer"

data$x1 <- as.numeric(as.character(data$x1))  # Convert one variable to numeric

#GHS: 
sapply(append_GHSNew, class)
##Education is character variable, not categorical 


#New Internet Source: 
Bank$Gender.Dummy <- 0
##Define new variable 

Bank$Gender.Dummy<-ifelse(Bank$Gender=="Male",1,0)
If Bank$Gender equals “Male” then the ifelse function returns a value of 1. Otherwise it returns a value of zero. In this way, a textual binary variable can be recoded as a numeric binary variable.

#Or: 
We could use that condition (JobGrade >) in an ifelse() statement to create a new variable:
  
  Bank$Mgmt.Dummy <- ifelse(Bank$JobGrade > 4, 1, 0)
But lets use a more flexible “list membership” approach instead. Create a list of non-management job grades using the combine operator:
  
  nonmgmt=c(1,2,3,4)
Now we can use that list plus the is.element() function to distinguish managers from non-managers:
  
  Bank$Mgmt.Dummy <- ifelse(is.element(Bank$JobGrade, nonmgmt),"non-mgmt", "mgmt")
The ifelse function checks to see whether Bank$JobGrade is an element of the list nonmgnt (as defined above). If it is, it returns the string “non-mgmt”; otherwise, it returns the string “mgmt”. Of course, we could have used 0 and 1 in place of the strings “non-mgmt” and “mgmt”.

#Or, using the dplyr package: 
Bank %>% mutate(Manager = recode(JobGrade, 
                                 "1" = "Non-mgmt",
                                 "2" = "Non-mgmt",
                                 "3" = "Non-mgmt",
                                 "4" = "Non-mgmt",
                                 .default = "Mgmt"))  %>% 
  select(Employee, JobGrade, Gender, Manager)

The statement does the following:
  
  It starts with the Bank data frame (since Bank is piped into mutate())
It creates a new variable called “Manager” and sets its value based on the recode function.
The first argument in the recode function is the source, JobGrade. The other arguments are the mappings from old values to new values. The special mapping “.default” means “everything else”. So here, JobGrade={1,2,3,4} are mapped to “Non-mgmt” and all other values of JobGrade are mapped to “Mgmt”.
The results of the mutate function are piped to the select function. Select simply limits the columns so the new Manager column shows in the output. Without it, the tibble is too wide to show in the console.


#GHS: 
append_GHSNew <- mutate(append_GHSNew, educnum = recode(education, 
                                 "No schooling" = "0",
                                 "Grade R/0" = "0",
                                 "Grade 1/Sub A/Class 1" = "1",
                                 "Sub A/Grade 1" = "1",
                                 "Grade 2/Sub B/Class 2" = "2",
                                 "Sub B/Grade 2" = "2",
                                 "Grade 3/Standard 1/ABET/AET 1" = "3",
                                 "Grade 3/Standard 1" = "3",
                                 "Grade 4/Standard 2" = "4",
                                 "Grade 5/Standard 3/ABET/AET 2" = "5",
                                 "Grade 5/Standard 3" = "5",
                                 "Grade 6/Standard 4" = "6",
                                 "Grade 7/Standard 5" = "7",
                                 "Grade 7/Standard 5/ABET/AET 3" = "7",
                                 "Grade 8/Standard 6/Form 1" = "8",
                                 "Grade 9/Standard 7/Form 2" = "9",
                                 "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate-NQF Level 1" = "9",
                                 "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate-NQF Level 2" = "10",
                                 "Grade 10/Standard 8/Form 3" = "10",                                                                                
                                 "Grade 11/Standard 9/Form 4" = "11",
                                 "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate-NQF Level 3" = "11",
                                 "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate-NQF Level 4" = "12",
                                 "Grade 12/Standard 10/Form 5/Matric" = "12",
                                 "NTC I" = "10",
                                 "NTC I/N1" = "10",
                                 "NTC II" = "11",
                                 "NTC II/N2" = "11",
                                 "NTC III" = "11",
                                 "NTC III/N3" = "11",
                                 "N4/NTC 4/Occupational Certificate-NQF Level 5" = "13",
                                 "N5/NTC 5/Occupational Certificate-NQF Level 5" = "13",
                                 "N6/NTC 6/Occupational Certificate-NQF Level 5" = "13",
                                 "Diploma/certificate with less than Grade 12/Std 10" = "11",
                                 "Diploma with less than Grade 12/Standard 10" = "11",
                                 "Certificate with less than Grade 12/Standard 10" = "11",
                                 "Diploma/certificate with Grade 12/Std 10" = "13",
                                 "Diploma with Grade 12/Standard 10/Occupational Certificate-NQF Level 6" = "14",
                                 "Higher/National/Advance certificate with Grade 12/Std 10/Occupational Certificate-NQF Level 5" = "13",
                                 "Higher Diploma/Occupational Certificate (B-Tech Diploma)-NQF Level 7" = "15",
                                 "Degree" = "15",
                                 "Bachelors Degree/Occupational Certificate-NQF Level 7" = "15",
                                 "Postgraduate degree or diploma" = "16", 
                                 "Honours Degree/Postgraduate Diploma/Occupational Certificate-NQF Level 8" = "16",
                                 "Post Higher Diploma (Masters Diploma and Masters Degree)-NQF Level 9" = "17",
                                 "Doctoral Degrees (Doctoral Diploma and PhD)-NQF Level 10" = "18",
                                 "Other" = "NA",
                                 "Don't know" = "NA", 
                                 "Do not know" = "NA",
                                 "Unspecified" = "NA"))

get_labels(
  append_GHSNew$education,
  attr.only = FALSE,
  values = NULL,
  non.labelled = FALSE,
  drop.na = TRUE,
  drop.unused = FALSE
)

append_GHSNew$educnum <- as.numeric(as.character(append_GHSNew$education))

append_GHSNew$educnum <- as.numeric(append_GHSNew$educnum)

unique_educ <- unique(append_GHSNew$education)

#delete feature: 
append_GHSNew$educnumeric <- NULL



#Rerun Mincer with numeric education feature: 
GHSMincerNew <- lm(logsal ~ educnum + age + age2 + Sex + Race + lastyear + lastyear*Race, data=append_GHSNew)
GHSMincerNew


tab <- huxreg(GHSMincerNew)
flextable(tab)
quick_docx(tab)



# Salary Variable 
## Many Unspecified/Not Applicable's
append_GHSNew <- append_GHSNew %>% 
  mutate(salNA = recode(
    lab_sal, "Not applicable" = "NA", "Unspecified" = "NA")
    )


#Make new salary variable numeric: 
sapply(append_GHSNew, class)   

##salNA and educnum are characters
append_GHSNew$salNA <- as.numeric(append_GHSNew$salNA)
append_GHSNew$educnum <- as.numeric(append_GHSNew$educnum)

##New log of salary variable: 
append_GHSNew <- mutate(append_GHSNew, logsalNA = log(salNA))

#Rerun Mincer with numeric-education and log(salary with NA's) features: 
GHSMincerNew2 <- lm(logsalNA ~ educnum + age + age2 + Sex + Race + lastyear + lastyear*Race, data=append_GHSNew)
GHSMincerNew2


tab2 <- huxreg(GHSMincerNew2)
flextable(tab2)
quick_docx(tab2)

GHSMincerNew3 <- lm(logsalNA ~ educnum, data=append_GHSNew %>% filter(lastyear == 0))
GHSMincerNew3

#To save dataset: 
save(append_GHSNew, file = "append_GHSNew")

write.csv(append_GHSNew, "~/Desktop/Masters/Thesis Datawork/append_GHSNewcsv.csv", row.names = FALSE)

