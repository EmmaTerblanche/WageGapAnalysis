# Meeting 6 Work

# Packages ----
library(dplyr)
library(vtable)
library(haven)

# Read in new data ----

Worker2005 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/data/GHS2005Data/ghs-2005-worker-v1.4.csv", header = TRUE)

Person2005 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/data/GHS2005Data/ghs-2005-person-v1.4.csv", header = TRUE)

# Create Data Set ----

Worker2005 <- Worker2005 %>% 
  arrange(UqNr,PersonNR)

Person2005 <- Person2005 %>% 
  arrange(UqNr,PersonNR)

Person2005WAP <- Person2005[ which(Person2005$Age > 14 & Person2005$Age < 65),]

Person2005Educ <- Person2005WAP[,26]%>% 
  as.data.frame(Person2005Educ) 

# Further data restriction ----
WEduc2005 <- merge(Worker2005, Person2005Educ)
## 66k+ observations

Worker2005WAPE <- WEduc2005[ which(WEduc2005$Status1 == "Employed"),]
## 24k+ observations

Worker2005WAPES <- Worker2005WAPE[ which(Worker2005WAPE$Q29Salto != "Unspecified" & Worker2005WAPE$Q29Salto != "Not applicable"),]
## 16k+ observations


# New salary variable ----
class(Worker2005WAPES$Q29Salto)

Worker2005WAPES$Q29Salto <- as.numeric(Worker2005WAPES$Q29Salto)

Worker2005WAPES <- Worker2005WAPES %>% 
  mutate(msal = ifelse(
    Q210Salp == "Per week", Q29Salto *4.2, ifelse(
      Q210Salp == "Annually", Q29Salto /12, Q29Salto)))

Worker2005WAPES$logsal <- log(Worker2005WAPES$msal)

# Education Variable ----
Worker2005WAPES <- mutate(Worker2005WAPES, educnum = recode(Educ, 
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
                                                            "NTC l" = "10",
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
                                                            "Unspecified" = "NA",
                                                            "Honours Degree" = "16",
                                                            "Certificate with less than grade 12/STD 10" = "11",
                                                            "Bachelors Degree" = "15",
                                                            "Bachelors Degree and Diploma" = "16",
                                                            "Higher Degree (Masters, Doctorate)" = "17",       
                                                            "Diploma with less than grade 12/STD 10" = "11",
                                                            "Certificate with grade 12/STD 10" = "13",
                                                            "Diploma with grade 12/STD 10" = "13"))

class(Worker2005WAPES$educnum)

Worker2005WAPES$educnum <- as.numeric(Worker2005WAPES$educnum)

# Age-squared ----
Worker2005WAPES$AgeSq <- Worker2005WAPES$Age ^2

# Gender Variable ----
Worker2005WAPES <- Worker2005WAPES %>% 
  mutate(Worker2005WAPES, Gender1 = recode(Gender, 
                                           "Male" = 0, 
                                           "Female" = 1))

Worker2005WAPES$Gender1 <- as.numeric(Worker2005WAPES$Gender1)


# Regression ----
mod1 <- lm(logsal ~ Race + educnum + Age + AgeSq + Gender1, data = Worker2005WAPES)
mod1 %>% 
  summary()

mod2 <- lm(logsal ~ Race + bs(educnum, knots = c(7,12), degree = 1) + Age + AgeSq + Gender1, data = Worker2005WAPES)
mod2 %>% 
  summary()

library(ggplot2)
Worker2005WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()


library(splines)

# Create subset

GHS2005 <- Worker2005WAPES[,c("UqNr", "PersonNR", "Prov", "Gender1", "Age", "Race", "Worker_wgt", "logsal", "AgeSq", "educnum")]

GHS2005 <- GHS2005 %>%
  rename(Weight = Worker_wgt) %>% 
  rename(Gender = Gender1) %>% 
  rename(PersonNr = PersonNR)

# Last Year Dummy 

GHS2005$lastyear <- c(0)

# 2002 Dummy 
GHS2005$Y2002 <- c(0)

# 2005 Dummy 
GHS2005$Y2005 <- c(1)

# 2009 Dummy 
GHS2005$Y2009 <- c(0)

# 2016 Dummy 
GHS2005$Y2016 <- c(0)
