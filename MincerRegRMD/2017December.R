# 2017

# Read in new data ----

Person2017 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS 2017 Data/GHS 2017 Person v1.0 CSV.csv")

# Create Data Set ----

Person2017WAP <- Person2017[ which(Person2017$Age > 14 & Person2017$Age < 65),]
## 45k+ observations

Person2017WAPE <- Person2017WAP[ which(Person2017WAP$employ_Status1 == 1),]
## 20k+ observations

Person2017WAPES <- Person2017WAPE ##[ which(Person2017WAPE$Q42aSTO != "999999999" & Person2017WAPE$Q42aSTO != "888888888" & Person2017WAPE$Q42aSTO > "0"),]
## 11k+ observations

# # Log salary variable ----
# class(Person2017WAPES$Q42aSTO)
#
# Person2017WAPES$Q42aSTO <- as.numeric(Person2017WAPES$Q42aSTO)
#
# Person2017WAPES <- Person2017WAPES %>%
#     mutate(msal = ifelse(
#         Q42bSP == "Per week", Q42aSTO *4.2, ifelse(
#             Q42bSP == "Annually", Q42aSTO /12, Q42aSTO)))
#
# Person2017WAPES$logsal <- log(Person2017WAPES$msal)

# Age-squared -----
Person2017WAPES$AgeSq <- Person2017WAPES$Age ^2

# Adding labels
Person2017WAPES <- mutate(Person2017WAPES, Race1 = recode(Race,
                                                          "1" = "African/Black",
                                                          "2" = "Coloured",
                                                          "3" = "Indian/Asian",
                                                          "4" = "White"))

# Education variable
Person2017WAPES <- mutate(Person2017WAPES, educnum = recode(Q15HIEDU,
                                                            "0" = "0",
                                                            "1" = "1",
                                                            "2" = "2",
                                                            "3" = "3",
                                                            "4" = "4",
                                                            "5" = "5",
                                                            "6" = "6",
                                                            "7" = "7",
                                                            "8" = "8",
                                                            "9" = "9",
                                                            "10" = "10",
                                                            "11" = "11",
                                                            "12" = "12",
                                                            "13" = "10",
                                                            "14" = "11",
                                                            "15" = "11",
                                                            "16" = "13",
                                                            "17" = "13",
                                                            "18" = "13",
                                                            "19" = "11",
                                                            "20" = "11",
                                                            "21" = "13",
                                                            "22" = "13",
                                                            "23" = "15",
                                                            "24" = "17",
                                                            "25" = "15",
                                                            "26" = "16",
                                                            "27" = "18",
                                                            "28" = "NA",
                                                            "29" = "NA",
                                                            "98" = "0",
                                                            "99" = "NA"))

Person2017WAPES$educnum <- as.numeric(Person2017WAPES$educnum)

# Gender Variable

Person2017WAPES <- Person2017WAPES %>%
    mutate(Person2017WAPES, Gender1 = recode(Gender,
                                             "1" = 0,
                                             "2" = 1))

Person2017WAPES$Gender1 <- as.numeric(Person2017WAPES$Gender1)

# Province Variable
Person2017WAPES <- Person2017WAPES %>%
    mutate(Person2017WAPES, Prov1 = recode(Prov,
                                           "1" = "Western Cape",
                                           "2" = "Eastern Cape",
                                           "3" = "Northern Cape",
                                           "4" = "Free State",
                                           "5" = "KwaZulu-Natal",
                                           "6" = "North West",
                                           "7" = "Gauteng",
                                           "8" = "Mpumalanga",
                                           "9" = "Limpopo"))

#
# # Run regressions with msal variable -----
#
# r1 <- lm(logsal ~ Race1 + educnum + AgeSq + Age + Gender1, data = Person2017WAPES)
# r1 %>%
#     summary()
#
# r2 <- lm(logsal ~ Race1 + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + Age + Gender1, data = Person2017WAPES)
# r2 %>%
#     summary()
#
# Person2017WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()


# Create Subset ----
GHS2017 <- Person2017WAPES[,c("UqNr", "PersonNR", "Prov1", "Gender1", "Age", "person_wgt", "AgeSq", "Race1", "educnum", "Q42aSTO", "Q42bSP", "Q43SALC")]

GHS2017 <- GHS2017 %>%
    rename(Race = Race1) %>%
    rename(PersonNr = PersonNR) %>%
    rename(Weight = person_wgt) %>%
    rename(Gender = Gender1) %>%
    rename(Prov = Prov1) %>%
    rename(TotSal = Q42aSTO) %>%
    rename(SalPeriod = Q42bSP) %>%
    rename(Interval = Q43SALC)

# Last Year Dummy
GHS2017$lastyear <- c(0)

# 2002 Dummy
GHS2017$Y2002 <- c(0)

# 2003 Dummy
GHS2017$Y2003 <- c(0)

# 2004 Dummy
GHS2017$Y2004 <- c(0)

# 2005 Dummy
GHS2017$Y2005 <- c(0)

# 2006 Dummy
GHS2017$Y2006 <- c(0)

# 2007 Dummy
GHS2017$Y2007 <- c(0)

# 2008 Dummy
GHS2017$Y2008 <- c(0)

# 2009 Dummy
GHS2017$Y2009 <- c(0)

# 2010 Dummy
GHS2017$Y2010 <- c(0)

# 2011 Dummy
GHS2017$Y2011 <- c(0)

# 2012 Dummy
GHS2017$Y2012 <- c(0)

# 2013 Dummy
GHS2017$Y2013 <- c(0)

# 2014 Dummy
GHS2017$Y2014 <- c(0)

# 2015 Dummy
GHS2017$Y2015 <- c(0)

# 2016 Dummy
GHS2017$Y2016 <- c(0)

# 2017 Dummy
GHS2017$Y2017 <- c(1)

# 2018 Dummy
GHS2017$Y2018 <- c(0)

# 2019 Dummy
GHS2017$Y2019 <- c(0)

# 2020 Dummy
GHS2017$Y2020 <- c(0)

# 2021 Dummy
GHS2017$Y2021 <- c(0)


class(GHS2017$TotSal)
GHS2017$TotSal <- as.character(GHS2017$TotSal)

class(GHS2017$Interval)
GHS2017$Interval <- as.character(GHS2017$Interval)

class(GHS2017$SalPeriod)
GHS2017$SalPeriod <- as.character(GHS2017$SalPeriod)





