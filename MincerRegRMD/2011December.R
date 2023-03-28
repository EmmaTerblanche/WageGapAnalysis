# 2011

# Read in new data ----

Person2011 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS2011Data/zaf-statssa-ghs-2011-person-v1.2.csv")

# Create Data Set ----

Person2011WAP <- Person2011[ which(Person2011$Age > 14 & Person2011$Age < 65),]
## 59k+ observations

Person2011WAPE <- Person2011WAP[ which(Person2011WAP$Q21awge == "Yes"),]
## 20k+ observations

Person2011WAPES <- Person2011WAPE ##[ which(Person2011WAPE$Q22asto != "Unspecified" & Person2011WAPE$Q22asto > "0"),]
## 12k+ observations

# # New salary variable
# class(Person2011WAPES$Q22asto)
#
# Person2011WAPES$Q22asto <- as.numeric(Person2011WAPES$Q22asto)
#
# Person2011WAPES <- Person2011WAPES %>%
#     mutate(msal = ifelse(
#         Q22bsp == "Per week", Q22asto *4.2, ifelse(
#             Q22bsp == "Annually", Q22asto /12, Q22asto)))
#
# # Log salary variable ----
# Person2011WAPES$logsal <- log(Person2011WAPES$Q22asto)

# Age-squared -----
Person2011WAPES$AgeSq <- Person2011WAPES$Age ^2

# Education variable
Person2011WAPES <- mutate(Person2011WAPES, educnum = recode(Q16hiedu,
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
                                                            "Other" = "NA"))

unique(Person2010WAPES$educnum)
Person2011WAPES$educnum <- as.numeric(Person2011WAPES$educnum)

# Gender Variable

Person2011WAPES <- Person2011WAPES %>%
    mutate(Person2011WAPES, Gender1 = recode(Gender,
                                             "Male" = 0,
                                             "Female" = 1))

Person2011WAPES$Gender1 <- as.numeric(Person2011WAPES$Gender1)

# Province Variable
Person2011WAPES <- Person2011WAPES %>%
    mutate(Person2011WAPES, Prov1 = recode(Prov,
                                           "1" = "Western Cape",
                                           "2" = "Eastern Cape",
                                           "3" = "Northern Cape",
                                           "4" = "Free State",
                                           "5" = "KwaZulu-Natal",
                                           "6" = "North West",
                                           "7" = "Gauteng",
                                           "8" = "Mpumalanga",
                                           "9" = "Limpopo"))


# # Run regressions with msal variable -----
#
# r1 <- lm(logsal ~ Race + educnum + AgeSq + Age + Gender1, data = Person2011WAPES)
# r1 %>%
#     summary()
#
# r2 <- lm(logsal ~ Race + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + Age + Gender1, data = Person2011WAPES)
# r2 %>%
#     summary()
#
# Person2011WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()
# # lyk anders as ander jare

# Create Subset ----
GHS2011 <- Person2011WAPES[,c("UqNr", "PersonNR", "Prov1", "Gender1", "Age", "person_wgt", "AgeSq", "Race", "educnum", "Q22asto", "Q22bsp", "Q23salc")]

GHS2011 <- GHS2011 %>%
    rename(PersonNr = PersonNR) %>%
    rename(Weight = person_wgt) %>%
    rename(Gender = Gender1) %>%
    rename(Prov = Prov1) %>%
    rename(TotSal = Q22asto) %>%
    rename(SalPeriod = Q22bsp) %>%
    rename(Interval = Q23salc)

# Last Year Dummy
GHS2011$lastyear <- c(0)

# 2002 Dummy
GHS2011$Y2002 <- c(0)

# 2003 Dummy
GHS2011$Y2003 <- c(0)

# 2004 Dummy
GHS2011$Y2004 <- c(0)

# 2005 Dummy
GHS2011$Y2005 <- c(0)

# 2006 Dummy
GHS2011$Y2006 <- c(0)

# 2007 Dummy
GHS2011$Y2007 <- c(0)

# 2008 Dummy
GHS2011$Y2008 <- c(0)

# 2009 Dummy
GHS2011$Y2009 <- c(0)

# 2010 Dummy
GHS2011$Y2010 <- c(0)

# 2011 Dummy
GHS2011$Y2011 <- c(1)

# 2012 Dummy
GHS2011$Y2012 <- c(0)

# 2013 Dummy
GHS2011$Y2013 <- c(0)

# 2014 Dummy
GHS2011$Y2014 <- c(0)

# 2015 Dummy
GHS2011$Y2015 <- c(0)

# 2016 Dummy
GHS2011$Y2016 <- c(0)

# 2017 Dummy
GHS2011$Y2017 <- c(0)

# 2018 Dummy
GHS2011$Y2018 <- c(0)

# 2019 Dummy
GHS2011$Y2019 <- c(0)

# 2020 Dummy
GHS2011$Y2020 <- c(0)

# 2021 Dummy
GHS2011$Y2021 <- c(0)



class(GHS2011$TotSal)
class(GHS2011$Interval)
class(GHS2011$SalPeriod)


