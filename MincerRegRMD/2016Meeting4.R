# Meeting 4 work 

# Read in new data ----

Person2016 <- read.csv("/Users/mac/Desktop/Masters/Thesis Datawork/MincerRegRMD/data/GHS2016Data /GHS2016Person.csv")

# Create Data Set ----

Person2016WAP <- Person2016[ which(Person2016$Age > 14 & Person2016$Age < 65),]
## 46k+ observations

Person2016WAPE <- Person2016WAP[ which(Person2016WAP$employ_Status1 == 1),]
## 20k+ observations

Person2016WAPES <- Person2016WAPE[ which(Person2016WAPE$Q42msal < 999999999 & Person2016WAPE$Q42msal > 0),]
## 16k+ observations

# Descriptives ----
# Cross tab 
options(max.print = 999999)

crosstab <- table(Person2016WAP$Q42aSTO, Person2016WAP$employ_Status1)
crosstab

unique(Person2016WAP$employ_Status2)

crosstab2 <- table(Person2016WAP$Q42msal, Person2016WAP$employ_Status1)
crosstab2

## Employment:   1    2    9
## 888888888     0  6312 19328
## 999999999  8650     0     0

## Employment categories: 1, 2, 8, 9

# Income Intervals 
incinttab <- table(Person2016)

# Log salary variable ----
Person2016WAP$logsal <- log(Person2016WAP$Q42msal)

# Age-squared -----
Person2016WAP$AgeSq <- Person2016WAP$Age ^2

# Adding labels 
Person2016WAPES <- mutate(Person2016WAPES, Race1 = recode(Race, 
                                                            "1" = "African/Black",
                                                            "2" = "Coloured",
                                                          "3" = "Indian/Asian",
                                                          "4" = "White"))

# Education variable 
Person2016WAPES <- mutate(Person2016WAPES, educnum = recode(Q15HIEDU, 
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
                                                            "23" = "14",
                                                            "24" = "13",
                                                            "25" = "15",
                                                            "26" = "15",
                                                            "27" = "15",
                                                            "28" = "16", 
                                                            "29" = "17",
                                                            "30" = "NA",
                                                            "31" = "NA", 
                                                            "98" = "0",
                                                            "99" = "NA"))

Person2016WAPES$educnum <- as.numeric(Person2016WAPES$educnum)

# Gender Variable 

Person2016WAPES <- Person2016WAPES %>% 
  mutate(Person2016WAPES, Gender1 = recode(Gender, 
                                           "1" = 0, 
                                           "2" = 1))

Person2016WAPES$Gender1 <- as.numeric(Person2016WAPES$Gender1)

# Province Variable 
Person2016WAPES <- Person2016WAPES %>% 
  mutate(Person2016WAPES, Prov1 = recode(Prov, 
                                           "1" = "Western Cape", 
                                           "2" = "Eastern Cape",
                                         "3" = "Northern Cape",
                                         "4" = "Free State",
                                         "5" = "KwaZulu-Natal",
                                         "6" = "North West",
                                         "7" = "Gauteng",
                                         "8" = "Mpumalanga",
                                         "9" = "Limpopo"))

                          
# Run regressions with msal variable -----

r1 <- lm(logsal ~ Race1 + educnum + AgeSq + Age + Gender1, data = Person2016WAPES)
r1 %>% 
  summary()

r2 <- lm(logsal ~ Race1 + bs(educnum, knots = c(7,12), degree = 1) + AgeSq + Age + Gender1, data = Person2016WAPES)
r2 %>% 
  summary()


# Create Subset ---- 
GHS2016 <- Person2016WAPES[,c("UqNr", "PersonNR", "Prov1", "Gender1", "Age", "person_wgt", "logsal", "AgeSq", "Race1", "educnum")]

GHS2016 <- GHS2016 %>%
  rename(Race = Race1) %>% 
  rename(PersonNr = PersonNR) %>% 
  rename(Weight = person_wgt) %>% 
  rename(Gender = Gender1) %>% 
  rename(Prov = Prov1)

# Last Year Dummy 

GHS2016$lastyear <- c(1)

# 2002 Dummy 
GHS2016$Y2002 <- c(0)

# 2009 Dummy 
GHS2016$Y2009 <- c(0)

# 2016 Dummy 
GHS2016$Y2016 <- c(1)


Person2016WAPES %>% ggplot(aes(x = educnum, y = logsal)) + geom_smooth()


