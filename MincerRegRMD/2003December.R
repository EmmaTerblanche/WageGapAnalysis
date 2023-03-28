# 2003

# Packages ----
library(dplyr)
library(vtable)
library(haven)

# Read in new data ----

Worker2003 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS 2003 Data/ghs2003worker.csv", header = TRUE, fileEncoding="latin1")

Person2003 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS 2003 Data/ghs2003person.csv", header = TRUE)

# Create Data Set ----

Worker2003 <- Worker2003 %>%
    arrange(UqNr,PersonNr)

Person2003 <- Person2003 %>%
    arrange(UqNr,PersonNr)

Person2003WAP <- Person2003[ which(Person2003$Age > 14 & Person2003$Age < 65),]

Person2003Educ <- Person2003WAP

Person2003Educ <- Person2003WAP[,24] %>%
    as.data.frame(Person2003Educ) %>%
    mutate(UqNr = Person2003WAP$UqNr) %>%
    mutate(PersonNr = Person2003WAP$PersonNr) %>%
    rename("Educ" = ".")

# Further data restriction ----
WEduc2003 <- merge(Worker2003, Person2003Educ)
## 62k+ observations

Worker2003WAPE <- WEduc2003[ which(WEduc2003$Status1 == "Employed"),]
## 23k+ observations

Worker2003WAPES <- Worker2003WAPE ##[ which(Worker2003WAPE$Q28Salto != "Unspecified" & Worker2003WAPE$Q28Salto != "Not applicable"),]
## 15k+ observations

# # New salary variable ----
# class(Worker2003WAPES$Q28Salto)
#
# Worker2003WAPES$Q28Salto <- as.numeric(Worker2003WAPES$Q28Salto)
#
# Worker2003WAPES <- Worker2003WAPES %>%
#     mutate(msal = ifelse(
#         Q29Salpe == "Per week", Q28Salto *4.2, ifelse(
#             Q29Salpe == "Annually", Q28Salto /12, Q28Salto)))
#
# Worker2003WAPES$logsal <- log(Worker2003WAPES$msal)

# Education Variable ----
Worker2003WAPES <- mutate(Worker2003WAPES, educnum = recode(Educ,
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
                                                            "Diploma/Certificate with less than grade 12/STD 10 " = "11",
                                                            "Certificate with less than Grade 12/Standard 10" = "11",
                                                            "Diploma/certificate with Grade 12/Std 10" = "13",
                                                            "Diploma/Certificate with grade 12/STD 10" = "13",
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
                                                            "Other " = "NA",
                                                            "Don't know" = "NA",
                                                            "Do not know" = "NA",
                                                            "Unspecified" = "NA"))

class(Worker2003WAPES$educnum)

Worker2003WAPES$educnum <- as.numeric(Worker2003WAPES$educnum)

# Age-squared ----
Worker2003WAPES$AgeSq <- Worker2003WAPES$Age ^2

# Gender Variable ----
Worker2003WAPES <- Worker2003WAPES %>%
    mutate(Worker2003WAPES, Gender1 = recode(Gender,
                                             "Male" = 0,
                                             "Female" = 1))

Worker2003WAPES$Gender1 <- as.numeric(Worker2003WAPES$Gender1)


# # Regression ----
# library(splines)
# mod1 <- lm(logsal ~ Race + educnum + Age + AgeSq + Gender1, data = Worker2003WAPES)
# mod1 %>%
#     summary()
#
# mod2 <- lm(logsal ~ Race + bs(educnum, knots = c(7,12), degree = 1) + Age + AgeSq + Gender1, data = Worker2003WAPES)
# mod2 %>%
#     summary()
#
# library(ggplot2)
# Worker2003WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()


# Create subset

GHS2003 <- Worker2003WAPES[,c("UqNr", "PersonNr", "Prov", "Gender1", "Age", "Race", "Worker_wgt", "AgeSq", "educnum", "Q28Salto", "Q29Salpe", "Q210Salc")]

GHS2003 <- GHS2003 %>%
    rename(Weight = Worker_wgt) %>%
    rename(Gender = Gender1) %>%
    rename(TotSal = Q28Salto) %>%
    rename(SalPeriod = Q29Salpe) %>%
    rename(Interval = Q210Salc)

# Last Year Dummy
GHS2003$lastyear <- c(0)

# 2002 Dummy
GHS2003$Y2002 <- c(0)

# 2003 Dummy
GHS2003$Y2003 <- c(1)

# 2004 Dummy
GHS2003$Y2004 <- c(0)

# 2005 Dummy
GHS2003$Y2005 <- c(0)

# 2006 Dummy
GHS2003$Y2006 <- c(0)

# 2007 Dummy
GHS2003$Y2007 <- c(0)

# 2008 Dummy
GHS2003$Y2008 <- c(0)

# 2009 Dummy
GHS2003$Y2009 <- c(0)

# 2010 Dummy
GHS2003$Y2010 <- c(0)

# 2011 Dummy
GHS2003$Y2011 <- c(0)

# 2012 Dummy
GHS2003$Y2012 <- c(0)

# 2013 Dummy
GHS2003$Y2013 <- c(0)

# 2014 Dummy
GHS2003$Y2014 <- c(0)

# 2015 Dummy
GHS2003$Y2015 <- c(0)

# 2016 Dummy
GHS2003$Y2016 <- c(0)

# 2017 Dummy
GHS2003$Y2017 <- c(0)

# 2018 Dummy
GHS2003$Y2018 <- c(0)

# 2019 Dummy
GHS2003$Y2019 <- c(0)

# 2020 Dummy
GHS2003$Y2020 <- c(0)

# 2021 Dummy
GHS2003$Y2021 <- c(0)


class(GHS2003$TotSal)
class(GHS2003$Interval)
class(GHS2003$SalPeriod)
