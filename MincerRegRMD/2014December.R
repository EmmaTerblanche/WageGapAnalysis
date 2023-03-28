# 2014

# Read in new data ----

Person2014 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/Data/GHS 2014 Data/GHS 2014 Person v1.1 CSV.csv")

# Create Data Set ----

Person2014WAP <- Person2014[ which(Person2014$Age > 14 & Person2014$Age < 65),]
## 58k+ observations

Person2014WAPE <- Person2014WAP[ which(Person2014WAP$employ_Status1 == "1"),]
## 24k+ observations

Person2014WAPES <- Person2014WAPE ##[ which(Person2014WAPE$Q42aSTO != "999999999" & Person2014WAPE$Q42aSTO != "888888888" & Person2014WAPE$Q42aSTO > "0"),]
## 14k+ observations

# # New salary variable
# class(Person2014WAPES$Q42aSTO)
#
# Person2014WAPES$Q42aSTO <- as.numeric(Person2014WAPES$Q42aSTO)
#
# Person2014WAPES <- Person2014WAPES %>%
#     mutate(msal = ifelse(
#         Q42bSP == "1", Q42aSTO *4.2, ifelse(
#             Q42bSP == "3", Q42aSTO /12, Q42aSTO)))
#
# # Log salary variable ----
# Person2014WAPES$logsal <- log(Person2014WAPES$Q42aSTO)

# Age-squared -----
Person2014WAPES$AgeSq <- Person2014WAPES$Age ^2

# Adding labels
Person2014WAPES <- mutate(Person2014WAPES, Race1 = recode(race,
                                                          "1" = "African/Black",
                                                          "2" = "Coloured",
                                                          "3" = "Indian/Asian",
                                                          "4" = "White"))

# Education variable
Person2014WAPES <- mutate(Person2014WAPES, educnum = recode(Q15HIEDU,
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

unique(Person2014WAPES$educnum)
Person2014WAPES$educnum <- as.numeric(Person2014WAPES$educnum)

# Gender Variable

Person2014WAPES <- Person2014WAPES %>%
    mutate(Person2014WAPES, Gender1 = recode(gender,
                                             "1" = 0,
                                             "2" = 1))

Person2014WAPES$Gender1 <- as.numeric(Person2014WAPES$Gender1)

# Province Variable
Person2014WAPES <- Person2014WAPES %>%
    mutate(Person2014WAPES, Prov1 = recode(prov,
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
# r1 <- lm(logsal ~ Race + educnum + AgeSq + Age + Gender1, data = Person2014WAPES)
# r1 %>%
#     summary()
#
# r2 <- lm(logsal ~ Race + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + Age + Gender1, data = Person2014WAPES)
# r2 %>%
#     summary()
#
# Person2014WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()


# Create Subset ----
GHS2014 <- Person2014WAPES[,c("uqnr", "personnr", "Prov1", "Gender1", "Age", "person_wgt", "AgeSq", "Race1", "educnum", "Q42aSTO", "Q42bSP", "Q43SALC")]

GHS2014 <- GHS2014 %>%
    rename(PersonNr = personnr) %>%
    rename(UqNr = uqnr) %>%
    rename(Weight = person_wgt) %>%
    rename(Gender = Gender1) %>%
    rename(Prov = Prov1) %>%
    rename(Race = Race1) %>%
    rename(TotSal = Q42aSTO) %>%
    rename(SalPeriod = Q42bSP) %>%
    rename(Interval = Q43SALC)

# Last Year Dummy
GHS2014$lastyear <- c(0)

# 2002 Dummy
GHS2014$Y2002 <- c(0)

# 2003 Dummy
GHS2014$Y2003 <- c(0)

# 2004 Dummy
GHS2014$Y2004 <- c(0)

# 2005 Dummy
GHS2014$Y2005 <- c(0)

# 2006 Dummy
GHS2014$Y2006 <- c(0)

# 2007 Dummy
GHS2014$Y2007 <- c(0)

# 2008 Dummy
GHS2014$Y2008 <- c(0)

# 2009 Dummy
GHS2014$Y2009 <- c(0)

# 2010 Dummy
GHS2014$Y2010 <- c(0)

# 2011 Dummy
GHS2014$Y2011 <- c(0)

# 2012 Dummy
GHS2014$Y2012 <- c(0)

# 2013 Dummy
GHS2014$Y2013 <- c(0)

# 2014 Dummy
GHS2014$Y2014 <- c(1)

# 2015 Dummy
GHS2014$Y2015 <- c(0)

# 2016 Dummy
GHS2014$Y2016 <- c(0)

# 2017 Dummy
GHS2014$Y2017 <- c(0)

# 2018 Dummy
GHS2014$Y2018 <- c(0)

# 2019 Dummy
GHS2014$Y2019 <- c(0)

# 2020 Dummy
GHS2014$Y2020 <- c(0)

# 2021 Dummy
GHS2014$Y2021 <- c(0)



class(GHS2014$TotSal)
GHS2014$TotSal <- as.character(GHS2014$TotSal)

class(GHS2014$Interval)
GHS2014$Interval <- as.character(GHS2014$Interval)

class(GHS2014$SalPeriod)
GHS2014$SalPeriod <- as.character(GHS2014$SalPeriod)
