# 2015


# Read in new data ----

Person2015 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS 2015 Data/GHS 2015 Person v1.2 CSV.csv")

# Create Data Set ----

Person2015WAP <- Person2015[ which(Person2015$Age > 14 & Person2015$Age < 65),]
## 47k+ observations

Person2015WAPE <- Person2015WAP[ which(Person2015WAP$employ_Status1 == "1"),]
## 21k+ observations

Person2015WAPES <- Person2015WAPE ##[ which(Person2015WAPE$Q42asto != "999999999" & Person2015WAPE$Q42asto != "888888888" & Person2015WAPE$Q42asto > "0"),]
## 12k+ observations

# # New salary variable
# class(Person2015WAPES$Q42asto)
#
# Person2015WAPES$Q42asto <- as.numeric(Person2015WAPES$Q42asto)
#
# Person2015WAPES <- Person2015WAPES %>%
#     mutate(msal = ifelse(
#         Q42bsp == "1", Q42asto *4.2, ifelse(
#             Q42bsp == "3", Q42asto /12, Q42asto)))

# # Log salary variable ----
# Person2015WAPES$logsal <- log(Person2015WAPES$Q42asto)

# Age-squared -----
Person2015WAPES$AgeSq <- Person2015WAPES$Age ^2


# Adding labels
Person2015WAPES <- mutate(Person2015WAPES, Race1 = recode(race,
                                                          "1" = "African/Black",
                                                          "2" = "Coloured",
                                                          "3" = "Indian/Asian",
                                                          "4" = "White"))

# Education variable
Person2015WAPES <- mutate(Person2015WAPES, educnum = recode(Q15HIEDU,
                                                            "98" = "0",
                                                            "00" = "0",
                                                            "01" = "1",
                                                            "02" = "2",
                                                            "03" = "3",
                                                            "04" = "4",
                                                            "05" = "5",
                                                            "06" = "6",
                                                            "07" = "7",
                                                            "08" = "8",
                                                            "09" = "9",
                                                            "10" = "10",
                                                            "11" = "11",
                                                            "12" = "12",
                                                            "13" = "12",
                                                            "14" = "10",
                                                            "15" = "11",
                                                            "16" = "11",
                                                            "17" = "13",
                                                            "18" = "13",
                                                            "19" = "13",
                                                            "20" = "11",
                                                            "21" = "11",
                                                            "22" = "13",
                                                            "23" = "13",
                                                            "24" = "15",
                                                            "25" = "17",
                                                            "26" = "15",
                                                            "27" = "16",
                                                            "28" = "16",
                                                            "29" = "17",
                                                            "30" = "NA",
                                                            "31" = "NA"
))

unique(Person2015WAPES$educnum)
Person2015WAPES$educnum <- as.numeric(Person2015WAPES$educnum)

# Gender Variable

Person2015WAPES <- Person2015WAPES %>%
    mutate(Person2015WAPES, Gender1 = recode(gender,
                                             "1" = 0,
                                             "2" = 1))

Person2015WAPES$Gender1 <- as.numeric(Person2015WAPES$Gender1)

# Province Variable
Person2015WAPES <- Person2015WAPES %>%
    mutate(Person2015WAPES, Prov1 = recode(prov,
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
# r1 <- lm(logsal ~ Race + educnum + AgeSq + Age + Gender1, data = Person2015WAPES)
# r1 %>%
#     summary()
#
# r2 <- lm(logsal ~ Race + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + Age + Gender1, data = Person2015WAPES)
# r2 %>%
#     summary()
#
# Person2015WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()


# Create Subset ----
GHS2015 <- Person2015WAPES[,c("uqnr", "personnr", "Prov1", "Gender1", "Age", "person_wgt", "AgeSq", "Race1", "educnum", "Q42asto", "Q42bsp", "Q43salc")]

GHS2015 <- GHS2015 %>%
    rename(PersonNr = personnr) %>%
    rename(UqNr = uqnr) %>%
    rename(Weight = person_wgt) %>%
    rename(Gender = Gender1) %>%
    rename(Prov = Prov1) %>%
    rename(Race = Race1) %>%
    rename(TotSal = Q42asto) %>%
    rename(SalPeriod = Q42bsp) %>%
    rename(Interval = Q43salc)

# Last Year Dummy
GHS2015$lastyear <- c(0)

# 2002 Dummy
GHS2015$Y2002 <- c(0)

# 2003 Dummy
GHS2015$Y2003 <- c(0)

# 2004 Dummy
GHS2015$Y2004 <- c(0)

# 2005 Dummy
GHS2015$Y2005 <- c(0)

# 2006 Dummy
GHS2015$Y2006 <- c(0)

# 2007 Dummy
GHS2015$Y2007 <- c(0)

# 2008 Dummy
GHS2015$Y2008 <- c(0)

# 2009 Dummy
GHS2015$Y2009 <- c(0)

# 2010 Dummy
GHS2015$Y2010 <- c(0)

# 2011 Dummy
GHS2015$Y2011 <- c(0)

# 2012 Dummy
GHS2015$Y2012 <- c(0)

# 2013 Dummy
GHS2015$Y2013 <- c(0)

# 2014 Dummy
GHS2015$Y2014 <- c(0)

# 2015 Dummy
GHS2015$Y2015 <- c(1)

# 2016 Dummy
GHS2015$Y2016 <- c(0)

# 2017 Dummy
GHS2015$Y2017 <- c(0)

# 2018 Dummy
GHS2015$Y2018 <- c(0)

# 2019 Dummy
GHS2015$Y2019 <- c(0)

# 2020 Dummy
GHS2015$Y2020 <- c(0)

# 2021 Dummy
GHS2015$Y2021 <- c(0)


class(GHS2015$TotSal)
GHS2015$TotSal <- as.character(GHS2015$TotSal)

class(GHS2015$Interval)
GHS2015$Interval <- as.character(GHS2015$Interval)

class(GHS2015$SalPeriod)
GHS2015$SalPeriod <- as.character(GHS2015$SalPeriod)


