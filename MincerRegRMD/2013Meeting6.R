# Meeting 6
# Read in new data ----

Person2013 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/data/GHS2013Data/zaf-statssa-ghs-2013-person-v1.1.csv")

# Create Data Set ----

Person2013WAP <- Person2013[ which(Person2013$Age > 14 & Person2013$Age < 65),]
## 59k+ observations

Person2013WAPE <- Person2013WAP[ which(Person2013WAP$Q41aWGE == "Yes"),]
## 21k+ observations

Person2013WAPES <- Person2013WAPE  ##[ which(Person2013WAPE$Q42aSTO != "Unspecified" & Person2013WAPE$Q42aSTO > "0"),]
## 12k+ observations

# # New salary variable
# class(Person2013WAPES$Q42aSTO)
#
# Person2013WAPES$Q42aSTO <- as.numeric(Person2013WAPES$Q42aSTO)
#
# Person2013WAPES <- Person2013WAPES %>%
#   mutate(msal = ifelse(
#     Q42bSP == "Per week", Q42aSTO *4.2, ifelse(
#       Q42bSP == "Annually", Q42aSTO /12, Q42aSTO)))
#
# # Log salary variable ----
# Person2013WAPES$logsal <- log(Person2013WAPES$Q42aSTO)

# Age-squared -----
Person2013WAPES$AgeSq <- Person2013WAPES$Age ^2

# Education variable
Person2013WAPES <- mutate(Person2013WAPES, educnum = recode(Q16HIEDU,
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
                                                            "N5/NTC 5" = "13"))

Person2013WAPES$educnum <- as.numeric(Person2013WAPES$educnum)

# Gender Variable

Person2013WAPES <- Person2013WAPES %>%
  mutate(Person2013WAPES, Gender1 = recode(Gender,
                                           "Male" = 0,
                                           "Female" = 1))

Person2013WAPES$Gender1 <- as.numeric(Person2013WAPES$Gender1)

# Province Variable
Person2013WAPES <- Person2013WAPES %>%
  mutate(Person2013WAPES, Prov1 = recode(Prov,
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
# r1 <- lm(logsal ~ Race + educnum + AgeSq + Age + Gender1, data = Person2013WAPES)
# r1 %>%
#   summary()
#
# r2 <- lm(logsal ~ Race + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + Age + Gender1, data = Person2013WAPES)
# r2 %>%
#   summary()
#
# Person2013WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()

# Create Subset ----
GHS2013 <- Person2013WAPES[,c("UqNr", "PersonNR", "Prov1", "Gender1", "Age", "person_wgt", "AgeSq", "Race", "educnum", "Q42aSTO", "Q42bSP", "Q43SALC")]

GHS2013 <- GHS2013 %>%
  rename(PersonNr = PersonNR) %>%
  rename(Weight = person_wgt) %>%
  rename(Gender = Gender1) %>%
  rename(Prov = Prov1) %>%
    rename(TotSal = Q42aSTO) %>%
    rename(SalPeriod = Q42bSP) %>%
    rename(Interval = Q43SALC)

# Last Year Dummy
GHS2013$lastyear <- c(0)

# 2002 Dummy
GHS2013$Y2002 <- c(0)

# 2003 Dummy
GHS2013$Y2003 <- c(0)

# 2004 Dummy
GHS2013$Y2004 <- c(0)

# 2005 Dummy
GHS2013$Y2005 <- c(0)

# 2006 Dummy
GHS2013$Y2006 <- c(0)

# 2007 Dummy
GHS2013$Y2007 <- c(0)

# 2008 Dummy
GHS2013$Y2008 <- c(0)

# 2009 Dummy
GHS2013$Y2009 <- c(0)

# 2010 Dummy
GHS2013$Y2010 <- c(0)

# 2011 Dummy
GHS2013$Y2011 <- c(0)

# 2012 Dummy
GHS2013$Y2012 <- c(0)

# 2013 Dummy
GHS2013$Y2013 <- c(1)

# 2014 Dummy
GHS2013$Y2014 <- c(0)

# 2015 Dummy
GHS2013$Y2015 <- c(0)

# 2016 Dummy
GHS2013$Y2016 <- c(0)

# 2017 Dummy
GHS2013$Y2017 <- c(0)

# 2018 Dummy
GHS2013$Y2018 <- c(0)

# 2019 Dummy
GHS2013$Y2019 <- c(0)

# 2020 Dummy
GHS2013$Y2020 <- c(0)

# 2021 Dummy
GHS2013$Y2021 <- c(0)


class(GHS2013$TotSal)

class(GHS2013$Interval)
GHS2013$Interval <- as.character(GHS2013$Interval)

class(GHS2013$SalPeriod)

