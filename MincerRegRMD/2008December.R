# 2008

# Read in new data ----

Worker2008 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS 2008 Data/ghs-2008-worker-v1.4.csv", header = TRUE, fileEncoding="latin1")
Person2008 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS 2008 Data/ghs-2008-person-v1.4.csv", header = TRUE)

# Create Data Set ----

Worker2008 <- Worker2008 %>%
    arrange(UqNr,PersonNr)

Person2008 <- Person2008 %>%
    arrange(UqNr,PersonNR)

Worker2008WAP <- Worker2008[ which(Worker2008$Age > 14 & Worker2008$Age < 65),]

Person2008WAP <- Person2008[ which(Person2008$Age > 14 & Person2008$Age < 65),]

Person2008Educ <- Person2008WAP[,26]

Person2008Educ <- Person2008WAP[,26] %>%
    as.data.frame(Person2008Educ) %>%
    mutate(UqNr = Person2008WAP$UqNr) %>%
    mutate(PersonNr = Person2008WAP$PersonNR) %>%
    rename("Educ" = ".")

## 59k+ observations

WEduc2008 <- merge(Worker2008, Person2008Educ)

Worker2008WAPE <- WEduc2008[ which(WEduc2008$Status1 == "Employed"),]
## 24k+ observations

Worker2008WAPES <- Worker2008WAPE ##[ which(Worker2008WAPE$Q29Salto != "Unspecified" & Worker2008WAPE$Q29Salto != "Not applicable"),]
## 16k+ observations


# # New salary variable ----
# class(Worker2008WAPES$Q29Salto)
#
# Worker2008WAPES$Q29Salto <- as.numeric(Worker2008WAPES$Q29Salto)
#
# Worker2008WAPES <- Worker2008WAPES %>%
#     mutate(msal = ifelse(
#         Q210Salp == "Per week", Q29Salto *4.2, ifelse(
#             Q210Salp == "Annually", Q29Salto /12, Q29Salto)))
#
# Worker2008WAPES$logsal <- log(Worker2008WAPES$msal)

# Education Variable ----
Worker2008WAPES <- mutate(Worker2008WAPES, educnum = recode(Educ,
                                                            "No schooling" = "0",
                                                            "Grade R / 0" = "0",
                                                            "Grade 1/Sub A/Class 1" = "1",
                                                            "Sub A/Grade 1" = "1",
                                                            "Grade 1 / Sub A" = "1",
                                                            "Grade 2/Sub B/Class 2" = "2",
                                                            "Sub B/Grade 2" = "2",
                                                            "Grade 2 / Sub B" = "2",
                                                            "Grade 3/Standard 1/ABET/AET 1" = "3",
                                                            "Grade 3 / Standard 1" = "3",
                                                            "Grade 4 / Standard 2" = "4",
                                                            "Grade 5/Standard 3/ABET/AET 2" = "5",
                                                            "Grade 5 / Standard 3" = "5",
                                                            "Grade 6 / Standard 4" = "6",
                                                            "Grade 7 / Standard 5" = "7",
                                                            "Grade 7/Standard 5/ABET/AET 3" = "7",
                                                            "Grade 8 / Standard 6 / Form 1" = "8",
                                                            "Grade 9 / Standard 7 / Form 2" = "9",
                                                            "Grade 9/Standard 7/Form 2/ABET/AET 4/NCV Level 1/Occupational Certificate-NQF Level 1" = "9",
                                                            "Grade 10/Standard 8/Form 3/NCV Level 2/Occupational Certificate-NQF Level 2" = "10",
                                                            "Grade 10 / Standard 8 / Form 3" = "10",
                                                            "Grade 11 / Standard 9 / Form 4" = "11",
                                                            "Grade 11/Standard 9/Form 4/NCV Level 3/Occupational Certificate-NQF Level 3" = "11",
                                                            "Grade 12/Standard 10/Form 5/National Senior Certificate/Matric/ NCV Level 4/Occupational Certificate-NQF Level 4" = "12",
                                                            "Grade 12 / Standard 10 / Form 5/Matric" = "12",
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
                                                            "Diploma with less than Grade 12 / Std 10" = "11",
                                                            "Diploma with less than Grade 12/Standard 10" = "11",
                                                            "Certificate with less than Grade 12/Standard 10" = "11",
                                                            "Diploma/certificate with Grade 12/Std 10" = "13",
                                                            "Certificate with Grade 12 / Std 10" = "13",
                                                            "Diploma with Grade 12 / Std 10" = "12",
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
                                                            "Diploma with less than grade 12 / STD 10" = "11",
                                                            "Diploma / certificate with less than Grade 12 / Std 10" = "11",
                                                            "Certificate with grade 12 / STD 10" = "13",
                                                            "Diploma with grade 12 / STD 10" = "13",
                                                            "Grade 7/Standard 5" = "7",
                                                            "Grade 12/Standard 10/Form 5/Matric" = "12",
                                                            "Grade 8/Standard 6/Form 1" = "8",
                                                            "Grade 9/Standard 7/Form 2" = "9",
                                                            "Diploma with grade 12/STD 10" = "13",
                                                            "Grade 6/Standard 4" = "6",
                                                            "Certificate with grade 12/STD 10" = "13",
                                                            "Grade 10/Standard 8/Form 3"  = "10",
                                                            "Grade 3/Standard 1" = "3",
                                                            "Grade 4/Standard 2"  = "4",
                                                            "Grade 11/Standard 9/Form 4" = "11",
                                                            "Grade 5/Standard 3" = "5",
                                                            "Diploma with less than grade 12/STD 10" = "11",
                                                            "Bachelor's Degree" = "15",
                                                            "Bachelor's Degree and Diploma" = "16",
                                                            "Grade R/0" = "0"))

unique(Worker2008WAPES$educnum)
class(Worker2008WAPES$educnum)

Worker2008WAPES$educnum <- as.numeric(Worker2008WAPES$educnum)

# Age-squared ----
Worker2008WAPES$AgeSq <- Worker2008WAPES$Age ^2

# Gender Variable ----
Worker2008WAPES <- Worker2008WAPES %>%
    mutate(Worker2008WAPES, Gender1 = recode(Gender,
                                             "Male" = 0,
                                             "Female" = 1))

Worker2008WAPES$Gender1 <- as.numeric(Worker2008WAPES$Gender1)

#
# # Regression ----
# mod1 <- lm(logsal ~ Race + educnum + Age + AgeSq + Gender1, data = Worker2008WAPES)
# mod1 %>%
#     summary()
#
# mod2 <- lm(logsal ~ Race + bs(educnum, knots = c(7,12), degree = 1) + Age + AgeSq + Gender1, data = Worker2008WAPES)
# mod2 %>%
#     summary()
#
# library(ggplot2)
# Worker2008WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()
#

library(splines)

# Create subset

GHS2008 <- Worker2008WAPES[,c("UqNr", "PersonNr", "Prov", "Gender1", "Age", "Race", "Worker_wgt", "AgeSq", "educnum", "Q29Salto", "Q210Salp", "Q211Salc")]

GHS2008 <- GHS2008 %>%
    rename(Weight = Worker_wgt) %>%
    rename(Gender = Gender1) %>%
    rename(TotSal = Q29Salto) %>%
    rename(SalPeriod = Q210Salp) %>%
    rename(Interval = Q211Salc)

# Last Year Dummy
GHS2008$lastyear <- c(0)

# 2002 Dummy
GHS2008$Y2002 <- c(0)

# 2003 Dummy
GHS2008$Y2003 <- c(0)

# 2004 Dummy
GHS2008$Y2004 <- c(0)

# 2005 Dummy
GHS2008$Y2005 <- c(0)

# 2006 Dummy
GHS2008$Y2006 <- c(0)

# 2007 Dummy
GHS2008$Y2007 <- c(0)

# 2008 Dummy
GHS2008$Y2008 <- c(1)

# 2009 Dummy
GHS2008$Y2009 <- c(0)

# 2010 Dummy
GHS2008$Y2010 <- c(0)

# 2011 Dummy
GHS2008$Y2011 <- c(0)

# 2012 Dummy
GHS2008$Y2012 <- c(0)

# 2013 Dummy
GHS2008$Y2013 <- c(0)

# 2014 Dummy
GHS2008$Y2014 <- c(0)

# 2015 Dummy
GHS2008$Y2015 <- c(0)

# 2016 Dummy
GHS2008$Y2016 <- c(0)

# 2017 Dummy
GHS2008$Y2017 <- c(0)

# 2018 Dummy
GHS2008$Y2018 <- c(0)

# 2019 Dummy
GHS2008$Y2019 <- c(0)

# 2020 Dummy
GHS2008$Y2020 <- c(0)

# 2021 Dummy
GHS2008$Y2021 <- c(0)


class(GHS2008$TotSal)
class(GHS2008$Interval)
class(GHS2008$SalPeriod)


