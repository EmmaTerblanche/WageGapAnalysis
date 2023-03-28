# Holiday Data Work

# Packages ----
library(dplyr)
library(vtable)
library(haven)

# Read in new data ----

Worker2002 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/ghs-2002-v1/ghs-2002-worker-v1.3.csv", header = TRUE)

Person2002 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/ghs-2002-v1/ghs-2002-person-v1.3.csv", header = TRUE)

# Create Data Set ----

Worker2002 <- Worker2002 %>%
  arrange(UqNr,PersonNr)

Person2002 <- Person2002 %>%
  arrange(UqNr,PersonNr)

Person2002WAP <- Person2002[ which(Person2002$Age > 14 & Person2002$Age < 65),]

Person2002Educ <- Person2002WAP[,23] %>%
  as.data.frame(Person2002Educ) %>%
  mutate(UqNr = Person2002WAP$UqNr) %>%
  mutate(PersonNr = Person2002WAP$PersonNr) %>%
  rename("Educ" = ".")

# Descriptives ----
sumtable(WEduc2002, vars = 'Q27Salto', title = "Salary")

# Cross tab
unique(WEduc2002$Status1)

empltab <- table(WEduc2002$Q27Salto, WEduc2002$Status1)
empltab

# Further data restriction ----
WEduc2002 <- merge(Worker2002, Person2002Educ)
## 63k+ observations

Worker2002WAPE <- WEduc2002[ which(WEduc2002$Status1 == "Employed"),]
## 24k+ observations

Worker2002WAPES <- Worker2002WAPE ##[ which(Worker2002WAPE$Q27Salto != "Unspecified" & Worker2002WAPE$Q27Salto != "Not applicable"),]
## 15k+ observations

# New salary variable ----
class(Worker2002WAPES$Q27Salto)

Worker2002WAPES$Q27Salto <- as.numeric(Worker2002WAPES$Q27Salto)

Worker2002WAPES <- Worker2002WAPES %>%
  mutate(msal = ifelse(
    Q28Salpe == "Per week", Q27Salto *4.2, ifelse(
      Q28Salpe == "Annually", Q27Salto /12, Q27Salto)))

Worker2002WAPES$logsal <- log(Worker2002WAPES$msal)

# Education Variable ----
Worker2002WAPES <- mutate(Worker2002WAPES, educnum = recode(Educ,
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
                                                        "Unspecified" = "NA"))

class(Worker2002WAPES$educnum)

Worker2002WAPES$educnum <- as.numeric(Worker2002WAPES$educnum)

# Age-squared ----
Worker2002WAPES$AgeSq <- Worker2002WAPES$Age ^2

# Gender Variable ----
Worker2002WAPES <- Worker2002WAPES %>%
  mutate(Worker2002WAPES, Gender1 = recode(Gender,
                                           "Male" = 0,
                                           "Female" = 1))

Worker2002WAPES$Gender1 <- as.numeric(Worker2002WAPES$Gender1)


# Regression ----
mod1 <- lm(logsal ~ Race + educnum + Age + AgeSq + Gender1, data = Worker2002WAPES)
mod1 %>%
  summary()

mod2 <- lm(logsal ~ Race + bs(educnum, knots = c(7,12), degree = 1) + Age + AgeSq + Gender1, data = Worker2002WAPES)
mod2 %>%
  summary()

Worker2002WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()


library(splines)

# Create subset

GHS2002 <- Worker2002WAPES[,c("UqNr", "PersonNr", "Prov", "Gender1", "Age", "Race", "Worker_wgt", "AgeSq", "educnum", "Q27Salto", "Q28Salpe", "Q29Salca")]

GHS2002 <- GHS2002 %>%
  rename(Weight = Worker_wgt) %>%
  rename(Gender = Gender1) %>%
    rename(TotSal = Q27Salto) %>%
    rename(SalPeriod = Q28Salpe) %>%
    rename(Interval = Q29Salca)

# Last Year Dummy
GHS2002$lastyear <- c(0)

# 2002 Dummy
GHS2002$Y2002 <- c(1)

# 2003 Dummy
GHS2002$Y2003 <- c(0)

# 2004 Dummy
GHS2002$Y2004 <- c(0)

# 2005 Dummy
GHS2002$Y2005 <- c(0)

# 2006 Dummy
GHS2002$Y2006 <- c(0)

# 2007 Dummy
GHS2002$Y2007 <- c(0)

# 2008 Dummy
GHS2002$Y2008 <- c(0)

# 2009 Dummy
GHS2002$Y2009 <- c(0)

# 2010 Dummy
GHS2002$Y2010 <- c(0)

# 2011 Dummy
GHS2002$Y2011 <- c(0)

# 2012 Dummy
GHS2002$Y2012 <- c(0)

# 2013 Dummy
GHS2002$Y2013 <- c(0)

# 2014 Dummy
GHS2002$Y2014 <- c(0)

# 2015 Dummy
GHS2002$Y2015 <- c(0)

# 2016 Dummy
GHS2002$Y2016 <- c(0)

# 2017 Dummy
GHS2002$Y2017 <- c(0)

# 2018 Dummy
GHS2002$Y2018 <- c(0)

# 2019 Dummy
GHS2002$Y2019 <- c(0)

# 2020 Dummy
GHS2002$Y2020 <- c(0)

# 2021 Dummy
GHS2002$Y2021 <- c(0)


class(GHS2002$TotSal)
class(GHS2002$Interval)
class(GHS2002$SalPeriod)



