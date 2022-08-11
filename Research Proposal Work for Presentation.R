#Research Proposal WOrk with Notes 

#Thesis Datawork 

##Working Directory 
setwd("~/Desktop/Masters/Thesis Datawork")

#New:

GHS2018New <- read.csv(file = "~/Desktop/Masters/Thesis Datawork/GHS2018LimitedVariablesEduc.csv", header = TRUE) 

GHS2002New <- read.csv(file = "~/Desktop/Masters/Thesis Datawork/GHS2002LimitedVarsEduc.csv", header = TRUE)


append_GHSNew <- bind_rows(GHS2018New,GHS2002New)

#From website:
nls_stu <- read_dta(file="https://github.com/ozanj/rclass/raw/master/data/nls72/nls72stu_percontor_vars.dta") %>%
  select(id,schcode,bysex,csex,crace,cbirthm,cbirthd,cbirthyr)

#Often we want to “stack” multiple datasets on top of one another

#typically datasets have the same variables, so stacking means that number of variables remains the same but number of observations increases
#We append data using the bind_rows() function, which is from the dplyr package

append_time <- bind_rows(time1,time2)

append_time

append_time %>% arrange(id,year)


#Regression: 

GHSMincerNew <- lm(logsal ~ education + age + age2 + Sex + Race + lastyear + lastyear*Race, data=append_GHSNew)

#To export regression table 
#website example:
m1 <- tidy(lm(bush00 ~ blkpct, states)) 

m1

ols_table <- all_models %>%
  select(-statistic, -p.value) %>%
  mutate_each(funs(round(., 2)), -term) %>% 
  gather(key, value, estimate:std.error) %>%
  spread(model, value) 

ols_table

write.table(ols_table, file = "olstab.txt", sep = ",", quote = FALSE, row.names = F)

#my data:
Mincerregr <- tidy(lm(logsal ~ education + age + age2 + Sex + Race + lastyear + lastyear*Race, data=append_GHSNew))

Mincerregr

Mincertable <- Mincerregr %>%
  select(-statistic, -p.value) %>%
  mutate_each(funs(round(., 2)), -term) %>%
  gather(key, value, estimate:std.error) %>%
  spread(model, value) 

write.table(Mincerregr, file = "mincertable.txt", sep = ",", quote = FALSE, row.names = F)
#doesnt work 


#another website example: 
wordreg(
  l,
  file = ,
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
  custom.col.pos = NULL,
  ...
)

#mydata: 
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

#website example: 
HR <- round(exp(coef(Mincerregr)), 2)
CI <- round(exp(confint(Mincerregr)), 2)
P <- round(coef(summary(Mincerregr)), 3)
# Names the columns of CI
colnames(CI) <- c("Lower", "Higher")
# Bind columns together as dataset
Mincertable <- as.data.frame(cbind(HR, CI, P))
Mincertable

# select variables you want to present in table
Mincertable <- Mincertable[c("educationGrade 12/Standard 10/Form 5/Matric","educationDegree","age","age2", "SexMale", "RaceColoured", "RaceIndian/Asian", "RaceWhite", "lastyear", "RaceColoured:lastyear","RaceIndian/Asian:lastyear", "RaceWhite:lastyear"),]
Mincertable

# add brackes and line for later use in table
Mincertable$a <- "("; Mincertable$b <- "-"; Mincertable$c <- ")"

# order the columns
Mincertable <- Mincertable[,c("HR","a","Lower","b","Higher","c", "P")]
Mincertable

# Load the packages
library(ReporteRs)
library(magrittr)
# The script
docx( ) %>% 
  addFlexTable(Mincertable %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold(color = "white"),
                           add.rownames = TRUE ) %>%
                 setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
  writeDoc(file = "Mincertable.docx")

#Huxtable
huxreg(Mincerregr)

quick_docx(Mincertable)