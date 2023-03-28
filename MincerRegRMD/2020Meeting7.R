# Meeting 7 work

library(dplyr)
library(ggplot2)

# Read in new data ----

Person2020 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/data/GHS2020Data/ghs-2020-person-v1.csv")

# Create Data Set ----

Person2020WAP <- Person2020[ which(Person2020$age > 14 & Person2020$age < 65),]
## 21k+ observations

Person2020WAPE <- Person2020WAP[ which(Person2020WAP$lab_wge == "Yes"),]
## 5k+ observations

Person2020WAPES <- Person2020WAPE ##[ which(Person2020WAPE$lab_sto != "Do not know" & Person2020WAPE$lab_sto != "Unspecified"),]
## 2k+ observations


# # New salary variable ----
# class(Person2020WAPES$lab_sto)
#
# Person2020WAPES$lab_sto <- as.numeric(Person2020WAPES$lab_sto)
#
# Person2020WAPES <- Person2020WAPES %>%
#     mutate(msal = ifelse(
#         lab_salper == "Per week", lab_sto *4.2, ifelse(
#             lab_salper == "Annually", lab_sto /12, lab_sto)))
#
# Person2020WAPES$logsal <- log(Person2020WAPES$msal)

# Age-squared -----
Person2020WAPES$AgeSq <- Person2020WAPES$age ^2

# Education variable
Person2020WAPES <- mutate(Person2020WAPES, educnum = recode(education,
                                                            "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate \x96 NQF Level 4" = "12",
                                                            "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate \x96 NQF Level 1" = "9",
                                                            "Grade 7/Standard 5/ABET/AET 3" = "7",
                                                            "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate \x96 NQF Level 2" = "10",
                                                            "Bachelor\x92s Degree/Occupational Certificate \x96 NQF Level 7" = "15",
                                                            "Diploma with Grade 12/Standard 10/Occupational Certificate \x96 NQF Level 6" = "13",
                                                            "Grade 4/Standard 2" = "4",
                                                            "Grade 5/Standard 3/ABET/AET 2" = "5",
                                                            "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate \x96 NQF Level 3" = "11",
                                                            "Grade 6/Standard 4" = "6",
                                                            "Grade 8/Standard 6/Form 1" = "8",
                                                            "No schooling" = "0",
                                                            "Grade 3/Standard 1/ABET/AET 1" = "3",
                                                            "Higher Diploma/Occupational Certificate (B-Tech Diploma) \x96 NQF Level 7" = "15",
                                                            "N6/NTC 6/Occupational Certificate \x96 NQF Level 5" = "13",
                                                            "DO NOT KNOW" = "NA",
                                                            "Higher/National/Advance certificate with Grade 12/Std 10/Occupational Certificate \x96 NQF Level 5" = "13",
                                                            "Honours Degree/Postgraduate Diploma/Occupational Certificate \x96 NQF Level 8" = "16",
                                                            "N5/NTC 5/Occupational Certificate \x96 NQF Level 5" = "13",
                                                            "Doctoral Degrees (D-Tech and PhD) \x96 NQF Level 10" = "18",
                                                            "Other" = "NA",
                                                            "Grade 2/Sub B/Class 2" = "2",
                                                            "Certificate with less than Grade 12/Standard 10" = "10",
                                                            "N4/NTC 4/Occupational Certificate \x96 NQF Level 5" = "13",
                                                            "Grade 1/Sub A/Class 1" = "1",
                                                            "NTC lll/N3/NQF 3" = "11",
                                                            "Post Higher Diploma (M-Tech and Master's Degree) \x96 NQF Level 9" = "17",
                                                            "NTC ll/N2/NQF 2" = "11",
                                                            "Diploma with less than Grade 12/Standard 10" = "10" ))

Person2020WAPES$educnum <- as.numeric(Person2020WAPES$educnum)

# Gender Variable

Person2020WAPES <- Person2020WAPES %>%
    mutate(Person2020WAPES, Gender = recode(Sex,
                                             "Male" = 0,
                                             "Female" = 1))

Person2020WAPES$Gender <- as.numeric(Person2020WAPES$Gender)

#
# # Run regressions with msal variable -----
#
# r1 <- lm(logsal ~ Population + educnum + AgeSq + age + Gender, data = Person2020WAPES)
# r1 %>%
#     summary()
#
# r2 <- lm(logsal ~ Population + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + age + Gender, data = Person2020WAPES)
# r2 %>%
#     summary()
#
# Person2020WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()

# Create Subset ----
GHS2020 <- Person2020WAPES[,c("uqnr", "personnr", "prov", "Gender", "age", "person_wgt", "AgeSq", "Population", "educnum", "lab_sto", "lab_salper", "lab_salc")]

GHS2020 <- GHS2020 %>%
    rename(Race = Population) %>%
    rename(UqNr = uqnr) %>%
    rename(PersonNr = personnr) %>%
    rename(Weight = person_wgt) %>%
    rename(Age = age) %>%
    rename(Prov = prov) %>%
    rename(TotSal = lab_sto) %>%
    rename(SalPeriod = lab_salper) %>%
    rename(Interval = lab_salc)

# Last Year Dummy
GHS2020$lastyear <- c(0)

# 2002 Dummy
GHS2020$Y2002 <- c(0)

# 2003 Dummy
GHS2020$Y2003 <- c(0)

# 2004 Dummy
GHS2020$Y2004 <- c(0)

# 2005 Dummy
GHS2020$Y2005 <- c(0)

# 2006 Dummy
GHS2020$Y2006 <- c(0)

# 2007 Dummy
GHS2020$Y2007 <- c(0)

# 2008 Dummy
GHS2020$Y2008 <- c(0)

# 2009 Dummy
GHS2020$Y2009 <- c(0)

# 2010 Dummy
GHS2020$Y2010 <- c(0)

# 2011 Dummy
GHS2020$Y2011 <- c(0)

# 2012 Dummy
GHS2020$Y2012 <- c(0)

# 2013 Dummy
GHS2020$Y2013 <- c(0)

# 2014 Dummy
GHS2020$Y2014 <- c(0)

# 2015 Dummy
GHS2020$Y2015 <- c(0)

# 2016 Dummy
GHS2020$Y2016 <- c(0)

# 2017 Dummy
GHS2020$Y2017 <- c(0)

# 2018 Dummy
GHS2020$Y2018 <- c(0)

# 2019 Dummy
GHS2020$Y2019 <- c(0)

# 2020 Dummy
GHS2020$Y2020 <- c(1)

# 2021 Dummy
GHS2020$Y2021 <- c(0)


class(GHS2020$TotSal)

class(GHS2020$Interval)

class(GHS2020$SalPeriod)

