# 2021

# Read in new data ----

Person2021 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS2021Data/ghs-2021-person-v1.csv")

# Create Data Set ----

Person2021WAP <- Person2021[ which(Person2021$age > 14 & Person2021$age < 65),]
## 22k+ observations

Person2021WAPE <- Person2021WAP[ which(Person2021WAP$lab_wge == "Yes"),]
## 6k+ observations

Person2021WAPES <- Person2021WAPE ##[ which(Person2021WAPE$lab_sto != "Do not know" & Person2021WAPE$lab_sto != "Unspecified"),]
## 6k+ observations


# # New salary variable ----
# class(Person2021WAPES$lab_sto)
#
# Person2021WAPES$lab_sto <- as.numeric(Person2021WAPES$lab_sto)
#
# Person2021WAPES <- Person2021WAPES %>%
#     mutate(msal = ifelse(
#         lab_salper == "Per week", lab_sto *4.2, ifelse(
#             lab_salper == "Annually", lab_sto /12, lab_sto)))
#
# Person2021WAPES$logsal <- log(Person2021WAPES$msal)

# Age-squared -----
Person2021WAPES$AgeSq <- Person2021WAPES$age ^2

# Education variable
Person2021WAPES <- mutate(Person2021WAPES, educnum = recode(education,
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
                                                            "Diploma with less than Grade 12/Standard 10" = "10",
                                                            "Grade R/0" = "0",
                                                            "NTC l/N1/NQF 1" = "10"))

Person2021WAPES$educnum <- as.numeric(Person2021WAPES$educnum)

# Gender Variable

Person2021WAPES <- Person2021WAPES %>%
    mutate(Person2021WAPES, Gender = recode(Sex,
                                            "Male" = 0,
                                            "Female" = 1))

Person2021WAPES$Gender <- as.numeric(Person2021WAPES$Gender)


# # Run regressions with msal variable -----
#
# r1 <- lm(logsal ~ Population + educnum + AgeSq + age + Gender, data = Person2021WAPES)
# r1 %>%
#     summary()
#
# r2 <- lm(logsal ~ Population + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + age + Gender, data = Person2021WAPES)
# r2 %>%
#     summary()
#
# Person2021WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()

# Create Subset ----
GHS2021 <- Person2021WAPES[,c("uqnr", "personnr", "prov", "Gender", "age", "person_wgt", "AgeSq", "Population", "educnum", "lab_sto", "lab_salper", "lab_salc")]

GHS2021 <- GHS2021 %>%
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
GHS2021$lastyear <- c(1)

# 2002 Dummy
GHS2021$Y2002 <- c(0)

# 2003 Dummy
GHS2021$Y2003 <- c(0)

# 2004 Dummy
GHS2021$Y2004 <- c(0)

# 2005 Dummy
GHS2021$Y2005 <- c(0)

# 2006 Dummy
GHS2021$Y2006 <- c(0)

# 2007 Dummy
GHS2021$Y2007 <- c(0)

# 2008 Dummy
GHS2021$Y2008 <- c(0)

# 2009 Dummy
GHS2021$Y2009 <- c(0)

# 2010 Dummy
GHS2021$Y2010 <- c(0)

# 2011 Dummy
GHS2021$Y2011 <- c(0)

# 2012 Dummy
GHS2021$Y2012 <- c(0)

# 2013 Dummy
GHS2021$Y2013 <- c(0)

# 2014 Dummy
GHS2021$Y2014 <- c(0)

# 2015 Dummy
GHS2021$Y2015 <- c(0)

# 2016 Dummy
GHS2021$Y2016 <- c(0)

# 2017 Dummy
GHS2021$Y2017 <- c(0)

# 2018 Dummy
GHS2021$Y2018 <- c(0)

# 2019 Dummy
GHS2021$Y2019 <- c(0)

# 2020 Dummy
GHS2021$Y2020 <- c(0)

# 2021 Dummy
GHS2021$Y2021 <- c(1)


class(GHS2021$TotSal)

class(GHS2021$Interval)

class(GHS2021$SalPeriod)


