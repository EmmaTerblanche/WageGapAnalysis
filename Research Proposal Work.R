#Thesis Datawork 

##Working Directory 
setwd("~/Desktop/Masters/Thesis Datawork")

#New:

GHS2018New <- read.csv(file = "~/Desktop/Masters/Thesis Datawork/GHS2018LimitedVariablesEduc.csv", header = TRUE) 

GHS2002New <- read.csv(file = "~/Desktop/Masters/Thesis Datawork/GHS2002LimitedVarsEduc.csv", header = TRUE)


append_GHSNew <- bind_rows(GHS2018New,GHS2002New)


#Regression: 

GHSMincerNew <- lm(logsal ~ education + age + age2 + Sex + Race + lastyear + lastyear*Race, data=append_GHSNew)


#To export regression table 
#Attempt 1 (Doesn't work)
Mincerregr <- tidy(lm(logsal ~ education + age + age2 + Sex + Race + lastyear + lastyear*Race, data=append_GHSNew))

Mincerregr

Mincertable <- Mincerregr %>%
  select(-statistic, -p.value) %>%
  mutate_each(funs(round(., 2)), -term) %>%
  gather(key, value, estimate:std.error) %>%
  spread(model, value) 

write.table(Mincerregr, file = "mincertable.txt", sep = ",", quote = FALSE, row.names = F)


#Attempt 2 (Doesn't Work)
wordreg(Mincerregr,
  file = "~/Desktop/Masters/Thesis Datawork",
  single.row = FALSE,
  stars = c(0.001, 0.01, 0.05),
  custom.model.names = NULL,
  custom.coef.names = NULL,
  custom.coef.map = NULL,
  custom.gof.names = NULL,
  custom.gof.rows = NULL,
  digits = 2,
  leading.zero = TRUE,
  star.symbol = "*",
  symbol = ".",
  override.coef = 0,
  override.se = 0,
  override.pvalues = 0,
  override.ci.low = 0,
  override.ci.up = 0,
  omit.coef = NULL,
  reorder.coef = NULL,
  reorder.gof = NULL,
  ci.force = FALSE,
  ci.force.level = 0.95,
  ci.test = 0,
  groups = NULL,
  custom.columns = NULL,
  custom.col.pos = NULL)

#Attempt 3
HR <- round(exp(coef(Mincerregr)), 2)
CI <- round(exp(confint(Mincerregr)), 2)
P <- round(coef(summary(Mincerregr)), 3)
## Names the columns of CI
colnames(CI) <- c("Lower", "Higher")
## Bind columns together as dataset
Mincertable <- as.data.frame(cbind(HR, CI, P))
Mincertable

## select variables you want to present in table
Mincertable <- Mincertable[c("educationGrade 12/Standard 10/Form 5/Matric","educationDegree","age","age2", "SexMale", "RaceColoured", "RaceIndian/Asian", "RaceWhite", "lastyear", "RaceColoured:lastyear","RaceIndian/Asian:lastyear", "RaceWhite:lastyear"),]
Mincertable

## add brackets and line for later use in table
Mincertable$a <- "("; Mincertable$b <- "-"; Mincertable$c <- ")"

## order the columns
Mincertable <- Mincertable[,c("HR","a","Lower","b","Higher","c", "P")]
Mincertable

## Load the packages
library(ReporteRs)
library(magrittr)

## The script
docx( ) %>% 
  addFlexTable(Mincertable %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold(color = "white"),
                           add.rownames = TRUE ) %>%
                 setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
  writeDoc(file = "Mincertable.docx")

#Attempt 4 (Works with reformatting)
huxreg(Mincerregr)



# Set a custom width to ensure it fits on the page
tab %>% flextable::width(width = 1)
as_flextable(GHSMincerNew) %>% quick_docx()

quick_docx(Mincertable)

#Variable Characteristics 
#Race Variable: (Race)
#1: African/Black
#2: Coloured
#3: Indian/Asian
#4: White 

ello
