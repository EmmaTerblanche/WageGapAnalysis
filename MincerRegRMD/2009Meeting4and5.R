# Meeting 4 and 5 Work

# Read in new data ----

Person2009 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS 2009 Data/GHS2009Person.csv")

# Create Data Set ----

Person2009WAP <- Person2009[ which(Person2009$Age > 14 & Person2009$Age < 65),]
## 59k+ observations

Person2009WAPE <- Person2009WAP[ which(Person2009WAP$Q140awge == "Yes"),]
## 19k+ observations

Person2009WAPES <- Person2009WAPE ##[ which(Person2009WAPE$Q142msal != "Unspecified"),]
## 16k+ observations

# Descriptives ----
# Cross tab
options(max.print = 999999)

crosstab5 <- table(Person2009WAPE$Q142msal, Person2009WAPE$PersonNR)
crosstab5

# # Log salary variable ----
# Person2009WAPES$Q142msal <- as.numeric(Person2009WAPES$Q142msal)
#
# Person2009WAPES$logsal <- log(Person2009WAPES$Q142msal)

# Age-squared -----
Person2009WAPES$AgeSq <- Person2009WAPES$Age ^2

# Education variable
Person2009WAPES <- mutate(Person2009WAPES, educnum = recode(Q16hiedu,
                                                            "No schooling" = "0",
                                                            "Grade R/0" = "0",
                                                            "Grade 1/ Sub A/Class 1" = "1",
                                                            "Grade 2 / Sub B/Class 2" = "2",
                                                            "Grade 3/Standard 1/ ABET 1(Kha Ri Gude, Sanli)" = "3",
                                                            "Grade 4/ Standard 2" = "4",
                                                            "Grade 5/ Standard 3/ ABET 2" = "5",
                                                            "Grade 6/Standard 4" = "6",
                                                            "Grade 7/Standard 5/ ABET 3" = "7",
                                                            "Grade 8/Standard 6/Form 1" = "8",
                                                            "Grade 9/Standard 7/Form 2/ ABET 4" = "9",
                                                            "Grade 10/ Standard 8/ Form 3" = "10",
                                                            "Grade 11/ Standard 9/ Form 4" = "11",
                                                            "Grade 12/Standard 10/Form 5/Matric (Exemption *)" = "12",
                                                            "Grade 12/Standard 10/Form 5/Matric (No Exemption" = "12",
                                                            "NTC 1/ N1/NC (V) Level 2" = "10",
                                                            "NTC 2/ N2/ NC (V) Level 3" = "11",
                                                            "NTC 3/ N3/NC (V)/Level 4" = "11",
                                                            "N4/NTC 4" = "13",
                                                            "N5/NTC 5" = "13",
                                                            "N6/NTC 6" = "13",
                                                            "Diploma with less than Grade 12/Std 10" = "11",
                                                            "Certificate with less than Grade 12/Std 10" = "11",
                                                            "Certificate with Grade 12/Std 10" = "13",
                                                            "Diploma with Grade 12/Std 10" = "13",
                                                            "Bachelors Degree" = "15",
                                                            "Bachelors Degree and post-graduate diploma" = "16",
                                                            "Honours Degree" = "16",
                                                            "Higher Diploma (Technikon/University of Technology)" = "13",
                                                            "Post Higher Diploma (Technikon/University of Technology Masters, Doctoral)" = "15",
                                                            "Doctoral Degrees (Doctoral Diploma and PhD)-NQF Level 10" = "18",
                                                            "Higher degree (Masters, Doctorate)" = "17",
                                                            "Other" = "NA",
                                                            "Do not know" = "NA",
                                                            "Unspecified" = "NA"))

Person2009WAPES$educnum <- as.numeric(Person2009WAPES$educnum)

# Gender Variable
Person2009WAPES <- Person2009WAPES %>%
  mutate(Person2009WAPES, Gender1 = recode(Gender,
                                           "Male" = 0,
                                           "Female" = 1))

Person2009WAPES$Gender1 <- as.numeric(Person2009WAPES$Gender1)

# # Run regressions with msal variable -----
#
# reg1 <- lm(logsal ~ Race + educnum + AgeSq + Age + Gender1, data = Person2009WAPES)
# reg1 %>%
#   summary()
#
# reg2 <- lm(logsal ~ Race + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + Age + Gender1, data = Person2009WAPES)
# reg2 %>%
#   summary()


# Create Subset ----
GHS2009 <- Person2009WAPES[,c("UqNr", "PersonNR", "Prov", "Gender1", "Age", "person_wgt", "AgeSq", "Race", "educnum", "Q141asto", "Q141bsp", "Q142salc")]

GHS2009 <- GHS2009 %>%
  rename(PersonNr = PersonNR) %>%
  rename(Weight = person_wgt) %>%
  rename(Gender = Gender1) %>%
    rename(TotSal = Q141asto) %>%
    rename(SalPeriod = Q141bsp) %>%
    rename(Interval = Q142salc)

# Last Year Dummy
GHS2009$lastyear <- c(0)

# 2002 Dummy
GHS2009$Y2002 <- c(0)

# 2003 Dummy
GHS2009$Y2003 <- c(0)

# 2004 Dummy
GHS2009$Y2004 <- c(0)

# 2005 Dummy
GHS2009$Y2005 <- c(0)

# 2006 Dummy
GHS2009$Y2006 <- c(0)

# 2007 Dummy
GHS2009$Y2007 <- c(0)

# 2008 Dummy
GHS2009$Y2008 <- c(0)

# 2009 Dummy
GHS2009$Y2009 <- c(1)

# 2010 Dummy
GHS2009$Y2010 <- c(0)

# 2011 Dummy
GHS2009$Y2011 <- c(0)

# 2012 Dummy
GHS2009$Y2012 <- c(0)

# 2013 Dummy
GHS2009$Y2013 <- c(0)

# 2014 Dummy
GHS2009$Y2014 <- c(0)

# 2015 Dummy
GHS2009$Y2015 <- c(0)

# 2016 Dummy
GHS2009$Y2016 <- c(0)

# 2017 Dummy
GHS2009$Y2017 <- c(0)

# 2018 Dummy
GHS2009$Y2018 <- c(0)

# 2019 Dummy
GHS2009$Y2019 <- c(0)

# 2020 Dummy
GHS2009$Y2020 <- c(0)

# 2021 Dummy
GHS2009$Y2021 <- c(0)


class(GHS2009$TotSal)
class(GHS2009$Interval)
class(GHS2009$SalPeriod)



# Vergadering Nota
## "This derived variable is an estimate of monthly earnings for individuals who were economically active.
## It is a combination of earnings reported in Q1401asto-Q142salc as well as imputed earnings where it was
## possible for cases that were originally classified as unspeciffied, unknown or refused in Q142salc."

## Wanneer mens die splines in sit, dan word die Race variable nie meer categorical nie.
