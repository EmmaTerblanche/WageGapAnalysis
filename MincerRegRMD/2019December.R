# 2019

# Read in new data ----

Person2019 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS 2019 Data/zaf-statssa-ghs-2019-person-v1-csv.csv")

# Create Data Set ----

Person2019WAP <- Person2019[ which(Person2019$age > 14 & Person2019$age < 65),]
## 43k+ observations

Person2019WAPE <- Person2019WAP[ which(Person2019WAP$LAB_WGE == "Yes"),]
## 15k+ observations

Person2019WAPES <- Person2019WAPE ##[ which(Person2019WAPE$LAB_STO != "Unspecified" & Person2019WAPE$LAB_STO != "Do not know" & Person2019WAPE$LAB_STO > "0"),]
## 7k+ observations

# # Log salary variable ----
# class(Person2019WAPES$LAB_STO)
#
# Person2019WAPES$LAB_STO <- as.numeric(Person2019WAPES$LAB_STO)
#
# Person2019WAPES <- Person2019WAPES %>%
#     mutate(msal = ifelse(
#         LAB_SALPER == "Per week", LAB_STO *4.2, ifelse(
#             LAB_SALPER == "Annually", LAB_STO /12, LAB_STO)))
#
# Person2019WAPES$logsal <- log(Person2019WAPES$msal)

# Age-squared -----
Person2019WAPES$AgeSq <- Person2019WAPES$age ^2

# Education variable
Person2019WAPES <- mutate(Person2019WAPES, educnum = recode(Education,
                                                            "NTC 3/ N3/NC (V)/Level 4" = "11",
                                                            "Grade 9/Standard 7/Form 2/AET 4" = "9",
                                                            "Grade 6/Standard 4" = "6",
                                                            "Grade 2/Sub B/Class 2" = "2",
                                                            "N6/NTC 6" = "13",
                                                            "Grade 12/Standard 10/Form 5/Matric (No Exemption)" = "12",
                                                            "Grade 8/Standard 6/Form 1" = "8",
                                                            "Diploma with Grade 12/Std 10" = "13",
                                                            "Grade 11/Standard 9/ Form 4" = "11",
                                                            "Bachelor's Degree and post-graduate diploma" ="16",
                                                            "No schooling" = "0",
                                                            "Grade 5/Standard 3/AET 2" = "5",
                                                            "Grade 10/Standard 8/ Form 3" = "10",
                                                            "Grade 4/Standard 2" = "4",
                                                            "Grade 7/Standard 5/AET 3" = "7",
                                                            "Grade 3/Standard 1/AET 1(Kha Ri Gude, Sanli)" = "3",
                                                            "Bachelor's Degree" = "15",
                                                            "Higher Diploma (Technikon/University of Technology)" = "15",
                                                            "Grade 12/Standard 10/Form 5/Matric (Exemption *)" = "12",
                                                            "Do not know" = "0",
                                                            "Diploma with less than Grade 12/Std 10" = "11",
                                                            "Grade 1/Sub A/Class 1" = "1",
                                                            "Certificate with less than Grade 12/Std 10" = "11",
                                                            "Certificate with Grade 12/Std 10" = "13",
                                                            "NTC 1 N1/NC (V) Level 2" = "10",
                                                            "Unspecified" = "0",
                                                            "Post Higher Diploma (Technikon/University of Technology Masters, Doctoral)" = "17",
                                                            "Other (specify in the box below)" = "0",
                                                            "Honours Degree" = "16",
                                                            "N4/NTC 4" = "13",
                                                            "Higher degree (Masters, Doctorate)" = "17",
                                                            "Grade R/0" = "0",
                                                            "NTC 2/N2/ NC (V) Level 3" = "11",
                                                            "N5/NTC 5" = "13",
                                                            "Grade 5/ Standard 3/ABET 2" = "5",
                                                            "Grade 9/Standard 7/Form 2/ABET 4" = "9",
                                                            "Grade 4/ Standard 2" = "4",
                                                            "Bachelors Degree" = "15",
                                                            "Grade 3/Standard 1/ABET 1(Kha Ri Gude; Sanli)" = "3",
                                                            "Grade 2 / Sub B/Class 2" = "2",
                                                            "Grade 10/ Standard 8/ Form 3" = "10",
                                                            "Grade 7/Standard 5/ABET 3" = "7",
                                                            "Grade 11/ Standard 9/ Form 4" = "11",
                                                            "NTC 1/ N1/NC (V) Level 2" = "10",
                                                            "Bachelors Degree and post-graduate diploma" = "16",
                                                            "Grade 1/ Sub A/Class 1" = "1",
                                                            "Post Higher Diploma (Technikon/University of Technology Masters; Doctoral)" = "17",
                                                            "Higher degree (Masters; Doctorate)" = "18",
                                                            "NTC 2/ N2/ NC (V) Level 3" = "11",
                                                            "Other" = "NA",
                                                            "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate \x96 NQF Level 2" = "10",
                                                            "Grade 7/Standard 5/ABET/AET 3" = "7",
                                                            "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate \x96 NQF Level 4" = "12",
                                                            "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate \x96 NQF Level 1" = "9",
                                                            "Bachelor\x92s Degree/Occupational Certificate \x96 NQF Level 7" = "15",
                                                            "N6/NTC 6/Occupational Certificate \x96 NQF Level 5" = "13",
                                                            "NTC lll/N3/NQF 3" = "11",
                                                            "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate \x96 NQF Level 3" = "11",
                                                            "NTC l/N1/NQF 1" = "10",
                                                            "Grade 5/Standard 3/ABET/AET 2" = "5",
                                                            "Diploma with Grade 12/Standard 10/Occupational Certificate \x96 NQF Level 6" = "13",
                                                            "Honours Degree/Postgraduate Diploma/Occupational Certificate \x96 NQF Level 8" = "16",
                                                            "N4/NTC 4/Occupational Certificate \x96 NQF Level 5" = "13",
                                                            "Certificate with less than Grade 12/Standard 10" = "11",
                                                            "Grade 3/Standard 1/ABET/AET 1" = "3",
                                                            "DO NOT KNOW" = "NA",
                                                            "Higher/National/Advance certificate with Grade 12/Std 10/Occupational Certificate \x96 NQF Level 5" = "13",
                                                            "Higher Diploma/Occupational Certificate (B-Tech Diploma) \x96 NQF Level 7" = "15",
                                                            "Post Higher Diploma (M-Tech and Master's Degree) \x96 NQF Level 9" = "17",
                                                            "Diploma with less than Grade 12/Standard 10" = "11",
                                                            "N5/NTC 5/Occupational Certificate \x96 NQF Level 5" = "13",
                                                            "NTC ll/N2/NQF 2" = "11",
                                                            "Doctoral Degrees (D-Tech and PhD) \x96 NQF Level 10" = "18"))

Person2019WAPES$educnum <- as.numeric(Person2019WAPES$educnum)

# Gender Variable

Person2019WAPES <- Person2019WAPES %>%
    mutate(Person2019WAPES, Gender1 = recode(Sex,
                                             "Male" = 0,
                                             "Female" = 1))

Person2019WAPES$Gender1 <- as.numeric(Person2019WAPES$Gender1)


# # Run regressions with msal variable -----
#
# r1 <- lm(logsal ~ Population + educnum + AgeSq + age + Gender1, data = Person2019WAPES)
# r1 %>%
#     summary()
#
# r2 <- lm(logsal ~ Population + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + age + Gender1, data = Person2019WAPES)
# r2 %>%
#     summary()
#
# Person2019WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()
#

# Create Subset ----
GHS2019 <- Person2019WAPES[,c("uqnr", "personnr", "prov", "Gender1", "age", "person_wgt", "AgeSq", "Population", "educnum", "LAB_STO", "LAB_SALPER", "LAB_SALC")]

GHS2019 <- GHS2019 %>%
    rename(Race = Population) %>%
    rename(PersonNr = personnr) %>%
    rename(Weight = person_wgt) %>%
    rename(Gender = Gender1) %>%
    rename(Prov = prov) %>%
    rename(TotSal = LAB_STO) %>%
    rename(SalPeriod = LAB_SALPER) %>%
    rename(Interval = LAB_SALC)

GHS2019 <- GHS2019 %>%
    rename(UqNr = uqnr)

GHS2019 <- GHS2019 %>%
    rename(Age = age)

# Last Year Dummy
GHS2019$lastyear <- c(0)

# 2002 Dummy
GHS2019$Y2002 <- c(0)

# 2003 Dummy
GHS2019$Y2003 <- c(0)

# 2004 Dummy
GHS2019$Y2004 <- c(0)

# 2005 Dummy
GHS2019$Y2005 <- c(0)

# 2006 Dummy
GHS2019$Y2006 <- c(0)

# 2007 Dummy
GHS2019$Y2007 <- c(0)

# 2008 Dummy
GHS2019$Y2008 <- c(0)

# 2009 Dummy
GHS2019$Y2009 <- c(0)

# 2010 Dummy
GHS2019$Y2010 <- c(0)

# 2011 Dummy
GHS2019$Y2011 <- c(0)

# 2012 Dummy
GHS2019$Y2012 <- c(0)

# 2013 Dummy
GHS2019$Y2013 <- c(0)

# 2014 Dummy
GHS2019$Y2014 <- c(0)

# 2015 Dummy
GHS2019$Y2015 <- c(0)

# 2016 Dummy
GHS2019$Y2016 <- c(0)

# 2017 Dummy
GHS2019$Y2017 <- c(0)

# 2018 Dummy
GHS2019$Y2018 <- c(0)

# 2019 Dummy
GHS2019$Y2019 <- c(1)

# 2020 Dummy
GHS2019$Y2020 <- c(0)

# 2021 Dummy
GHS2019$Y2021 <- c(0)


class(GHS2019$TotSal)

class(GHS2019$Interval)

class(GHS2019$SalPeriod)

