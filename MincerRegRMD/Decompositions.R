# Decompositions

#> Original Oaxaca Blinder Decomposition ====

# New data set with only white and black
blackwhite <- appendedGHS[appendedGHS$Race == "African/Black" | appendedGHS$Race == "White",]

# Black dummy (z)
blackwhite <- blackwhite %>%
    mutate(blackdummy = case_when(Race == "African/Black" ~ 1,
                                  Race == "White" ~ 0))

# Oaxaca with log wages
library(oaxaca)
obresults <- oaxaca(formula = logsal ~ exp + expsq + Gender + Year + prispline + secspline + terspline | blackdummy, data = blackwhite, R = 1000)

obresults$n
# sample
#$n.A
#[1] 16513

#$n.B
#[1] 203856

#$n.pooled
#[1] 220369

obresults$y
#$y.A
#[1] 9.047927

#$y.B
#[1] 7.489152

#$y.diff
#[1] 1.558775

obresults$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# -0.028785851        0.003570081        1.602864181        0.010734968       -0.015303767        0.003626321

## Of 1.558775 difference: -0.02878 explained by differences in endowments
## 1.602864181 explained by differences in coefficients
## -0.015303 explained by the interaction between the two


plot.oaxaca(obresults, components = c("endowments", "coefficients"))
## Most variables have a statistically insignifcant effect.
## Experience, experience-squared, 2009, 2016

summary(obresults$reg$reg.pooled.2)$coefficients["exp",]
#    Estimate   Std. Error      t value     Pr(>|t|)
# 3.844068e-02 8.086156e-04 4.753887e+01 0.000000e+00

summary(obresults$reg$reg.pooled.2)$coefficients["expsq",]
#    Estimate   Std. Error      t value     Pr(>|t|)
# -8.411780e-04  1.526363e-05 -5.510995e+01  0.000000e+00

summary(obresults$reg$reg.pooled.2)$coefficients["Year2009",]
#    Estimate   Std. Error      t value     Pr(>|t|)
#   0.91802037  0.01433467 64.04197999  0.00000000

summary(obresults$reg$reg.pooled.2)$coefficients["Year2016",]
#    Estimate   Std. Error      t value     Pr(>|t|)
#    1.30751298  0.01463037 89.36977278  0.00000000

#> Without Year and Gender ====

obnogenyr <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhite, R = 1000)

obnogenyr$n
## sample
#$n.A
#[1] 16513

#$n.B
#[1] 203856

#$n.pooled
#[1] 220369

obnogenyr$y
#$y.A
#[1] 9.047927

#$y.B
#[1] 7.489152

#$y.diff
#[1] 1.558775

obnogenyr$threefold$overall
#coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.741662568        0.006810038        1.224293257        0.061546330       -0.407181262        0.059986535

obnogenyr$twofold$overall
#group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
#[1,]   0.00000000       0.7416626   0.006810038         0.8171120      0.01165011        8.171120e-01      1.165011e-02
#[2,]   1.00000000       0.3344813   0.059880120         1.2242933      0.06154633        0.000000e+00      0.000000e+00
#[3,]   0.50000000       0.5380719   0.030272208         1.0207026      0.03259205        4.085560e-01      5.825054e-03
#[4,]   0.07493341       0.7111511   0.055416481         0.8476235      0.05713880        7.558830e-01      8.729823e-04
#[5,]  -1.00000000       0.7801398   0.006795529         0.7786347      0.01067732        7.202890e-01      9.904825e-03
#[6,]  -2.00000000       0.7068506   0.005976191         0.8519240      0.01148655        1.501849e-12      4.004346e-12
#coef(unexplained B) se(unexplained B)
#[1,]          0.00000000      0.0000000000
#[2,]          1.22429326      0.0615463301
#[3,]          0.61214663      0.0307731650
#[4,]          0.09174047      0.0569344539
#[5,]          0.05834575      0.0008937156
#[6,]          0.85192397      0.0114865470

plot.oaxaca(obnogenyr, components = c("endowments", "coefficients"))

summary(obnogenyr$reg$reg.pooled.2)$coefficients["secspline",]
#    Estimate   Std. Error      t value     Pr(>|t|)
#2.415903e-01 1.616794e-03 1.494255e+02 0.000000e+00

summary(obresults$reg$reg.pooled.2)$coefficients["prispline",]
#    Estimate   Std. Error      t value     Pr(>|t|)
#3.283961e-02 1.610893e-03 2.038597e+01 2.711843e-92

#> Only males ====
blackwhitemale <- blackwhite[blackwhite$Gender == 0,]

obmale <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale, R = 1000)

obmale$n
## sample
#$n.A
#[1] 9078

#$n.B
#[1] 111092

#$n.pooled
#[1] 120170

obmale$y
#$y.A
#[1] 9.184394

#$y.B
#[1] 7.624118

#$y.diff
#[1] 1.560276

obmale$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.729352536        0.009653214        1.114135659        0.057606750       -0.283212027        0.055133003

obmale$twofold$overall
#group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
#[1,]   0.00000000       0.7293525   0.009653214         0.8309236      0.01452355        8.309236e-01      1.452355e-02
#[2,]   1.00000000       0.4461405   0.054451739         1.1141357      0.05760675        0.000000e+00      0.000000e+00
#[3,]   0.50000000       0.5877465   0.027734026         0.9725296      0.03169895        4.154618e-01      7.261777e-03
#[4,]   0.07554298       0.7079579   0.050356424         0.8523183      0.05358640        7.681532e-01      1.097153e-03
#[5,]  -1.00000000       0.7736173   0.009689896         0.7866589      0.01318525        7.272323e-01      1.222219e-02
#[6,]  -2.00000000       0.6901125   0.008392701         0.8701636      0.01421420        1.391610e-13      1.885484e-12
#coef(unexplained B) se(unexplained B)
#[1,]          0.00000000       0.000000000
#[2,]          1.11413566       0.057606750
#[3,]          0.55706783       0.028803375
#[4,]          0.08416513       0.053254964
#[5,]          0.05942656       0.001150152
#[6,]          0.87016363       0.014214205

plot.oaxaca(obmale, components = c("endowments", "coefficients"))

summary(obmale$reg$reg.pooled.2)$coefficients["secspline",]
#     Estimate   Std. Error      t value     Pr(>|t|)
#2.264394e-01 2.114572e-03 1.070852e+02 0.000000e+00

summary(obmale$reg$reg.pooled.2)$coefficients["prispline",]
#      Estimate    Std. Error       t value      Pr(>|t|)
#5.608828e-02  2.212326e-03  2.535263e+01 1.984919e-141

#> 2002 ====
blackwhitemale2002 <- blackwhitemale[blackwhitemale$Year == 2002,]

obmale2002 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2002, R = 1000)

obmale2002$n
## sample
#$n.A
#[1] 672

#$n.B
#[1] 6398

#$n.pooled
#[1] 7070

obmale2002$y
#$y.A
#[1] 8.529952

#$y.B
#[1] 6.981321

#$y.diff
#[1] 1.548631

obmale2002$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.71325641         0.03405645         0.87925109         0.34147103        -0.04387694         0.33842639

obmale2002$twofold$overall
#group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
#[1,]    0.0000000       0.7132564    0.03410814         0.8353741      0.04082547        8.353741e-01      4.082547e-02
#[2,]    1.0000000       0.6693795    0.34161319         0.8792511      0.34147103        0.000000e+00      0.000000e+00
#[3,]    0.5000000       0.6913179    0.17406425         0.8573126      0.17464675        4.176871e-01      2.041273e-02
#[4,]    0.0950495       0.7090859    0.30962299         0.8395446      0.30955834        7.559722e-01      3.880441e-03
#[5,]   -1.0000000       0.8063386    0.03194786         0.7422920      0.03298961        6.717375e-01      3.032553e-02
#[6,]   -2.0000000       0.6540573    0.02719513         0.8945733      0.03740283       -6.811354e-14      8.657063e-14
#coef(unexplained B) se(unexplained B)
#[1,]          0.00000000       0.000000000
#[2,]          0.87925109       0.341471034
#[3,]          0.43962555       0.170735517
#[4,]          0.08357238       0.309014382
#[5,]          0.07055448       0.003809252
#[6,]          0.89457328       0.037292459

plot.oaxaca(obmale2002, components = c("endowments", "coefficients"))

#> 2003 ====
blackwhitemale2003 <- blackwhitemale[blackwhitemale$Year == 2003,]

obmale2003 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2003, R = 1000)

obmale2003$n
## sample
#$n.A
#[1] 637

#$n.B
#[1] 6420

#$n.pooled
#[1] 7057

obmale2003$y
#$y.A
#[1] 8.582358

#$y.B
#[1] 7.119113

#$y.diff
#[1] 1.463245

obmale2003$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.64049467         0.03043677         0.64615100         0.42295712         0.17659944         0.42032989

obmale2003$twofold$overall
#     group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
#[1,]   0.00000000       0.6404947    0.03047393         0.8227504      0.03780121        8.227504e-01      3.780121e-02
#[2,]   1.00000000       0.8170941    0.42155608         0.6461510      0.42295712        0.000000e+00      0.000000e+00
#[3,]   0.50000000       0.7287944    0.21248480         0.7344507      0.21445635        4.113752e-01      1.890060e-02
#[4,]   0.09026499       0.6564354    0.38372407         0.8068097      0.38518271        7.484849e-01      3.412125e-03
#[5,]  -1.00000000       0.7231307    0.03023103         0.7401144      0.03322688        6.733080e-01      3.068232e-02
#[6,]  -2.00000000       0.6017379    0.02585584         0.8615072      0.03587562        2.218035e-13      1.040501e-13
#coef(unexplained B) se(unexplained B)
#[1,]          0.00000000       0.000000000
#[2,]          0.64615100       0.422957124
#[3,]          0.32307550       0.211478562
#[4,]          0.05832481       0.384778905
#[5,]          0.06680642       0.003584968
#[6,]          0.86150722       0.035829637

plot.oaxaca(obmale2003, components = c("endowments", "coefficients"))

#> 2004 ====
blackwhitemale2004 <- blackwhitemale[blackwhitemale$Year == 2004,]

obmale2004 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2004, R = 1000)

obmale2004$n
## sample
#$n.A
#[1] 564

#$n.B
#[1] 6198

#$n.pooled
#[1] 6762

obmale2004$y
#$y.A
#[1] 8.752343

#$y.B
#[1] 7.187611

#$y.diff
#[1] 1.564732

obmale2004$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.63444238         0.03449738         1.33239249         1.07506630        -0.40210305         1.07506112

obmale2004$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
#[1,]   0.00000000       0.6344424    0.03369274         0.9302894      0.03925111        9.302894e-01      3.925111e-02
#[2,]   1.00000000       0.2323393    1.07279477         1.3323925      1.07506630        0.000000e+00      0.000000e+00
#[3,]   0.50000000       0.4333909    0.53579176         1.1313410      0.53825180        4.651447e-01      1.962556e-02
#[4,]   0.08340728       0.6009041    0.98317479         0.9638278      0.98546358        8.526965e-01      3.273828e-03
#[5,]  -1.00000000       0.7158293    0.03303030         0.8489026      0.03283114        7.780979e-01      3.043939e-02
#[6,]  -2.00000000       0.5862881    0.02738681         0.9784437      0.03570310       -3.494677e-15      1.140590e-13
#coef(unexplained B) se(unexplained B)
#[1,]          0.00000000       0.000000000
#[2,]          1.33239249       1.075066303
#[3,]          0.66619624       0.537533152
#[4,]          0.11113123       0.985397951
#[5,]          0.07080465       0.003883204
#[6,]          0.97844372       0.036126081

plot.oaxaca(obmale2004, components = c("endowments", "coefficients"))

#> 2005 ====
blackwhitemale2005 <- blackwhitemale[blackwhitemale$Year == 2005,]

obmale2005 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2005, R = 1000)

obmale2005$n
## sample
#$n.A
#[1] 527

#$n.B
#[1] 6561

#$n.pooled
#[1] 7088

obmale2005$y
#$y.A
#[1] 8.647879

#$y.B
#[1] 7.198578

#$y.diff
#[1] 1.4493

obmale2005$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.5902633          0.0301759          0.6348244          1.2252039          0.2242126          1.2236004

obmale2005$twofold$overall
#     group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
#[1,]   0.00000000       0.5902633    0.02996751         0.8590370      0.03994386        8.590370e-01      3.994386e-02
#[2,]   1.00000000       0.8144759    1.22270501         0.6348244      1.22520390        0.000000e+00      0.000000e+00
#[3,]   0.50000000       0.7023696    0.61127188         0.7469307      0.61405259        4.295185e-01      1.997193e-02
#[4,]   0.07435102       0.6069337    1.13175855         0.8423666      1.13428018        7.951668e-01      2.969867e-03
#[5,]  -1.00000000       0.6697985    0.03226116         0.7795018      0.03611072        7.215451e-01      3.372449e-02
#[6,]  -2.00000000       0.5759991    0.02707048         0.8733012      0.03927360        1.633875e-13      1.134727e-13
#coef(unexplained B) se(unexplained B)
#[1,]          0.00000000       0.000000000
#[2,]          0.63482443       1.225203904
#[3,]          0.31741221       0.612601952
#[4,]          0.04719984       1.134108749
#[5,]          0.05795675       0.003454131
#[6,]          0.87330117       0.039216042

plot.oaxaca(obmale2005, components = c("endowments", "coefficients"))

#> 2006 ====
blackwhitemale2006 <- blackwhitemale[blackwhitemale$Year == 2006,]

obmale2006 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2006, R = 1000)

obmale2006$n
## sample
#$n.A
#[1] 636

#$n.B
#[1] 7149

#$n.pooled
#[1] 7785

obmale2006$y
#$y.A
#[1] 8.70609

#$y.B
#[1] 7.303676

#$y.diff
#[1] 1.402414

obmale2006$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.5652656          0.0270097          0.7359520          0.1362808          0.1011962          0.1329955

obmale2006$twofold$overall
#  group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
#[1,]   0.00000000       0.5652656    0.02700994         0.8371483      0.03619171        8.371483e-01      3.619171e-02
#[2,]   1.00000000       0.6664618    0.13486983         0.7359520      0.13628083        0.000000e+00      0.000000e+00
#[3,]   0.50000000       0.6158637    0.07097714         0.7865501      0.07429133        4.185741e-01      1.809585e-02
#[4,]   0.08169557       0.5735328    0.12424361         0.8288810      0.12583808        7.687569e-01      2.956702e-03
#[5,]  -1.00000000       0.6317287    0.02801273         0.7706852      0.03249505        7.077236e-01      3.018505e-02
#[6,]  -2.00000000       0.5453092    0.02357192         0.8571046      0.03512803        4.001703e-13      1.160284e-13
#coef(unexplained B) se(unexplained B)
#[1,]          0.00000000       0.000000000
#[2,]          0.73595202       0.136280832
#[3,]          0.36797601       0.068140416
#[4,]          0.06012402       0.125147292
#[5,]          0.06296156       0.003402534
#[6,]          0.85710463       0.035092113

plot.oaxaca(obmale2006, components = c("endowments", "coefficients"))

#> 2007 ====
blackwhitemale2007 <- blackwhitemale[blackwhitemale$Year == 2007,]

obmale2007 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2007, R = 1000)

obmale2007$n
## sample
#$n.A
#[1] 609

#$n.B
#[1] 7715

#$n.pooled
#[1] 8324

obmale2007$y
#$y.A
#[1] 8.913732

#$y.B
#[1] 7.413632

#$y.diff
#[1] 1.5001

obmale2007$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.63773171         0.03016946         0.99612766         0.05929164        -0.13375952         0.06140768

obmale2007$twofold$overall
#group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
#[1,]   0.00000000       0.6377317    0.03035357         0.8623681      0.04204320        8.623681e-01      4.204320e-02
#[2,]   1.00000000       0.5039722    0.06145103         0.9961277      0.05929164        0.000000e+00      0.000000e+00
#[3,]   0.50000000       0.5708520    0.03749744         0.9292479      0.04121698        4.311841e-01      2.102160e-02
#[4,]   0.07316194       0.6279456    0.05754700         0.8721542      0.05596355        7.992756e-01      3.075962e-03
#[5,]  -1.00000000       0.7003934    0.02990596         0.7997064      0.03588603        7.411983e-01      3.356587e-02
#[6,]  -2.00000000       0.5947777    0.02445942         0.9053221      0.03831858        5.461436e-14      1.837398e-13
#coef(unexplained B) se(unexplained B)
#[1,]          0.00000000        0.00000000
#[2,]          0.99612766        0.05929164
#[3,]          0.49806383        0.02964582
#[4,]          0.07287863        0.05495375
#[5,]          0.05850807        0.00339178
#[6,]          0.90532213        0.03862136

plot.oaxaca(obmale2007, components = c("endowments", "coefficients"))

#> 2008 ====
blackwhitemale2008 <- blackwhitemale[blackwhitemale$Year == 2008,]

obmale2008 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2008, R = 1000)

obmale2008$n
## sample
# $n.A
# [1] 543
#
# $n.B
# [1] 7210
#
# $n.pooled
# [1] 7753

obmale2008$y
# $y.A
# [1] 9.099855
#
# $y.B
# [1] 7.590997
#
# $y.diff
# [1] 1.508858

obmale2008$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.54674055         0.02946264         1.07929570         0.20046293        -0.11717803         0.19756684

obmale2008$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]    0.0000000       0.5467406    0.02939278         0.9621177      0.03706393        9.621177e-01      3.706393e-02
# [2,]    1.0000000       0.4295625    0.19634090         1.0792957      0.20046293        0.000000e+00      0.000000e+00
# [3,]    0.5000000       0.4881515    0.09974306         1.0207067      0.10498284        4.810588e-01      1.853197e-02
# [4,]    0.0700374       0.5385337    0.18266924         0.9703245      0.18688188        8.947334e-01      2.595862e-03
# [5,]   -1.0000000       0.6164257    0.03165657         0.8924325      0.03350713        8.299288e-01      3.149723e-02
# [6,]   -2.0000000       0.5186343    0.02479472         0.9902240      0.03501053       -1.383853e-13      1.223049e-13
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          1.07929570       0.200462926
# [3,]          0.53964785       0.100231463
# [4,]          0.07559107       0.186423023
# [5,]          0.06250366       0.003352112
# [6,]          0.99022395       0.034997137

plot.oaxaca(obmale2008, components = c("endowments", "coefficients"))

#> 2009 ====
blackwhitemale2009 <- blackwhitemale[blackwhitemale$Year == 2009,]

obmale2009 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2009, R = 1000)

obmale2009$n
## sample
#$n.A
# [1] 1011
#
# $n.B
# [1] 7732
#
# $n.pooled
# [1] 8743

obmale2009$y
# $y.A
# [1] 9.202309
#
# $y.B
# [1] 7.806802
#
# $y.diff
# [1] 1.395507

obmale2009$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.681112663        0.026277438        0.712161187        0.046035106        0.002232819        0.051778808

obmale2009$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]    0.0000000       0.6811127    0.02565830         0.7143940      0.03383021        7.143940e-01      3.383021e-02
# [2,]    1.0000000       0.6833455    0.04794482         0.7121612      0.04603511        0.000000e+00      0.000000e+00
# [3,]    0.5000000       0.6822291    0.02843001         0.7132776      0.03100961        3.571970e-01      1.691511e-02
# [4,]    0.1156354       0.6813709    0.04283513         0.7141358      0.04162141        6.317848e-01      3.911969e-03
# [5,]   -1.0000000       0.7138846    0.02360855         0.6816221      0.02774643        6.028025e-01      2.475739e-02
# [6,]   -2.0000000       0.6074530    0.01998085         0.7880536      0.03029770       -1.718705e-13      2.194967e-13
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          0.71216119       0.046035106
# [3,]          0.35608059       0.023017553
# [4,]          0.08235102       0.040711819
# [5,]          0.07881962       0.003863779
# [6,]          0.78805364       0.030418272

plot.oaxaca(obmale2009, components = c("endowments", "coefficients"))

#> 2010 ====
blackwhitemale2010 <- blackwhitemale[blackwhitemale$Year == 2010,]

obmale2010 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2010, R = 1000)

obmale2010$n
## sample
# $n.A
# [1] 423
#
# $n.B
# [1] 5371
#
# $n.pooled
# [1] 5794

obmale2010$y
# $n.A
# [1] 423
#
# $n.B
# [1] 5371
#
# $n.pooled
# [1] 5794

obmale2010$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.61233787         0.04126963         1.33587184         0.26182962        -0.23553160         0.25941928

obmale2010$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.6123379    0.04045647          1.100340      0.04918318        1.100340e+00      4.918318e-02
# [2,]   1.00000000       0.3768063    0.25883321          1.335872      0.26182962        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.4945721    0.13225387          1.218106      0.13660995        5.501701e-01      2.459159e-02
# [4,]   0.07300656       0.5951425    0.24014277          1.117536      0.24325271        1.020008e+00      3.590695e-03
# [5,]  -1.00000000       0.6704714    0.04528626          1.042207      0.04664000        9.661188e-01      4.357867e-02
# [6,]  -2.00000000       0.5881925    0.03810990          1.124486      0.04772164       -8.207997e-14      9.373607e-14
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          1.33587184       0.261829624
# [3,]          0.66793592       0.130914812
# [4,]          0.09752741       0.242714344
# [5,]          0.07608792       0.004801428
# [6,]          1.12448565       0.047436904

plot.oaxaca(obmale2010, components = c("endowments", "coefficients"))

#> 2011 ====
blackwhitemale2011 <- blackwhitemale[blackwhitemale$Year == 2011,]

obmale2011 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2011, R = 1000)

obmale2011$n
## sample
#$n.A
# [1] 430
#
# $n.B
# [1] 5193
#
# $n.pooled
# [1] 5623

obmale2011$y
# $y.A
# [1] 9.729678
#
# $y.B
# [1] 7.683636
#
# $y.diff
# [1] 2.046042

obmale2011$threefold$overall
#coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.76632648         0.05473859         1.43960583         0.16573063        -0.15989078         0.15785174

obmale2011$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.7663265    0.05475212          1.279715       0.1302699        1.279715e+00      1.302699e-01
# [2,]   1.00000000       0.6064357    0.15463420          1.439606       0.1657306        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.6863811    0.08500277          1.359660       0.1264482        6.398575e-01      6.513493e-02
# [4,]   0.07647163       0.7540994    0.14336202          1.291942       0.1578107        1.181853e+00      9.961949e-03
# [5,]  -1.00000000       0.8508336    0.05751154          1.195208       0.1174913        1.103808e+00      1.088027e-01
# [6,]  -2.00000000       0.7316038    0.04990405          1.314438       0.1275078        7.836684e-14      1.050704e-13
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          1.43960583       0.165730631
# [3,]          0.71980291       0.082865316
# [4,]          0.11008901       0.153056939
# [5,]          0.09139951       0.009772213
# [6,]          1.31443774       0.127443996


plot.oaxaca(obmale2011, components = c("endowments", "coefficients"))

#> 2012 ====
blackwhitemale2012 <- blackwhitemale[blackwhitemale$Year == 2012,]

obmale2012 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2012, R = 1000)

obmale2012$n
## sample
# $n.A
# [1] 376
#
# $n.B
# [1] 5109
#
# $n.pooled
# [1] 5485

obmale2012$y
# #$y.A
# #[1] 8.529952
#
# #$y.B
# #[1] 6.981321
#
# #$y.diff
# #[1] 1.548631

obmale2012$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.68862977         0.05804812         2.08595844         0.33379254        -0.86639678         0.29355103

obmale2012$twofold$overall
#group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.6886298    0.05872014          1.219562       0.1234635        1.219562e+00      1.234635e-01
# [2,]   1.00000000      -0.1777670    0.29011777          2.085958       0.3337925        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.2554314    0.14921507          1.652760       0.2044194        6.097808e-01      6.173176e-02
# [4,]   0.06855059       0.6292378    0.27043055          1.278954       0.3151546        1.135960e+00      8.463498e-03
# [5,]  -1.00000000       0.7140756    0.05530452          1.194116       0.1174157        1.112258e+00      1.095191e-01
# [6,]  -2.00000000       0.6213588    0.04856884          1.286833       0.1247632        3.100445e-13      9.708528e-14
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          2.08595844       0.333792544
# [3,]          1.04297922       0.166896272
# [4,]          0.14299369       0.310910867
# [5,]          0.08185734       0.008923643
# [6,]          1.28683262       0.123429159

plot.oaxaca(obmale2012, components = c("endowments", "coefficients"))

#> 2013 ====
blackwhitemale2013 <- blackwhitemale[blackwhitemale$Year == 2013,]

obmale2013 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2013, R = 1000)

obmale2013$n
## sample
# $n.A
# [1] 431
#
# $n.B
# [1] 5415
#
# $n.pooled
# [1] 5846

obmale2013$y
# $y.A
# [1] 10.05655
#
# $y.B
# [1] 7.785106
#
# $y.diff
# [1] 2.271443

obmale2013$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.84219110         0.05751811         1.76821234         0.23235853        -0.33896064         0.19517575

obmale2013$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.8421911    0.05751811          1.429252       0.1339629        1.429252e+00      1.339629e-01
# [2,]   1.00000000       0.5032305    0.18978134          1.768212       0.2323585        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.6727108    0.10069385          1.598732       0.1626188        7.146258e-01      6.698144e-02
# [4,]   0.07372562       0.8172010    0.17607974          1.454242       0.2207525        1.323879e+00      9.876497e-03
# [5,]  -1.00000000       0.9307729    0.06002229          1.340670       0.1239665        1.241828e+00      1.149954e-01
# [6,]  -2.00000000       0.7778362    0.05061320          1.493607       0.1343608       -1.343746e-13      2.488532e-14
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000        0.00000000
# [2,]          1.76821234        0.23235853
# [3,]          0.88410617        0.11617927
# [4,]          0.13036256        0.21522776
# [5,]          0.09884172        0.01020989
# [6,]          1.49360657        0.13436080

plot.oaxaca(obmale2013, components = c("endowments", "coefficients"))

#> 2014 ====
blackwhitemale2014 <- blackwhitemale[blackwhitemale$Year == 2014,]

obmale2014 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2014, R = 1000)

obmale2014$n
## sample
#$n.A
# [1] 406
#
# $n.B
# [1] 5984
#
# $n.pooled
# [1] 6390

obmale2014$y
# $y.A
# [1] 9.504246
#
# $y.B
# [1] 7.690275
#
# $y.diff
# [1] 1.813971

obmale2014$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.76004473         0.04933202         1.32047585         0.21498797        -0.26654939         0.20881040

obmale2014$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.7600447    0.04941715         1.0539265      0.08005209        1.053926e+00      8.005209e-02
# [2,]   1.00000000       0.4934953    0.20475041         1.3204759      0.21498797        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.6267700    0.10621651         1.1872012      0.12415166        5.269632e-01      4.002604e-02
# [4,]   0.06353678       0.7431090    0.19188527         1.0708622      0.20272156        9.869634e-01      5.086252e-03
# [5,]  -1.00000000       0.8189441    0.05085010         0.9950271      0.07235012        9.318062e-01      6.790550e-02
# [6,]  -2.00000000       0.7216026    0.04401975         1.0923686      0.07735221       -8.949430e-14      1.112577e-13
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          1.32047585       0.214987967
# [3,]          0.66023793       0.107493984
# [4,]          0.08389878       0.201328325
# [5,]          0.06322081       0.005532556
# [6,]          1.09236860       0.077244816

plot.oaxaca(obmale2014, components = c("endowments", "coefficients"))

#> 2015 ====
blackwhitemale2015 <- blackwhitemale[blackwhitemale$Year == 2015,]

obmale2015 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2015, R = 1000)

obmale2015$n
## sample
# $n.A
# [1] 346
#
# $n.B
# [1] 5587
#
# $n.pooled
# [1] 5933

obmale2015$y
# $y.A
# [1] 9.471878
#
# $y.B
# [1] 7.799421
#
# $y.diff
# [1] 1.672457

obmale2015$threefold$overall
#coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.76787875         0.05389511         1.14097310         0.14057371        -0.23639468         0.10942257

obmale2015$twofold$overall
#group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.7678788    0.05382467         0.9045784      0.08375199        9.045784e-01      8.375199e-02
# [2,]   1.00000000       0.5314841    0.10917356         1.1409731      0.14057371        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.6496814    0.06644288         1.0227758      0.10195273        4.522892e-01      4.187600e-02
# [4,]   0.05831788       0.7540927    0.10361081         0.9183645      0.13549937        8.518253e-01      4.884239e-03
# [5,]  -1.00000000       0.8091658    0.05485788         0.8632914      0.07717310        8.129460e-01      7.291736e-02
# [6,]  -2.00000000       0.7292583    0.04711172         0.9431989      0.08246353       -1.412668e-13      1.131024e-13
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000        0.00000000
# [2,]          1.14097310        0.14057371
# [3,]          0.57048655        0.07028686
# [4,]          0.06653914        0.13237575
# [5,]          0.05034532        0.00509914
# [6,]          0.94319889        0.08229797

plot.oaxaca(obmale2015, components = c("endowments", "coefficients"))

#> 2016 ====
blackwhitemale2016 <- blackwhitemale[blackwhitemale$Year == 2016,]

obmale2016 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2016, R = 1000)

obmale2016$n
## sample
#$n.A
# [1] 744
#
# $n.B
# [1] 6964
#
# $n.pooled
# [1] 7708

obmale2016$y
# $y.A
# [1] 9.807896
#
# $y.B
# [1] 8.193648
#
# $y.diff
# [1] 1.614248

obmale2016$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.73575089         0.03103026         1.32192225         0.05095134        -0.44342524         0.04738002

obmale2016$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.7357509    0.03109260         0.8784970      0.03484446        8.784970e-01      3.484446e-02
# [2,]   1.00000000       0.2923257    0.04334348         1.3219222      0.05095134        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.5140383    0.02935110         1.1002096      0.03665885        4.392485e-01      1.742223e-02
# [4,]   0.09652309       0.6929501    0.03993583         0.9212978      0.04761183        7.937018e-01      3.363295e-03
# [5,]  -1.00000000       0.7974067    0.03052112         0.8168412      0.03072413        7.379971e-01      2.798840e-02
# [6,]  -2.00000000       0.6691968    0.02547063         0.9450511      0.03295142       -4.201964e-13      2.033732e-13
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          1.32192225       0.050951338
# [3,]          0.66096112       0.025475669
# [4,]          0.12759602       0.046033357
# [5,]          0.07884404       0.004077979
# [6,]          0.94505105       0.033641382

plot.oaxaca(obmale2016, components = c("endowments", "coefficients"))

#> 2017 ====
blackwhitemale2017 <- blackwhitemale[blackwhitemale$Year == 2017,]

obmale2017 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2017, R = 1000)

obmale2017$n
## sample
#$n.A
#[1] 265

#$n.B
# [1] 5388
#
# $n.pooled
# [1] 5653

obmale2017$y
# $y.A
# [1] 9.639313
#
# $y.B
# [1] 7.920392
#
# $y.diff
# [1] 1.71892

obmale2017$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.78376317         0.05685267         1.25448040         0.20886424        -0.31932315         0.18375642

obmale2017$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.7837632    0.05667030         0.9351572      0.07358534        9.351572e-01      7.358534e-02
# [2,]   1.00000000       0.4644400    0.17907494         1.2544804      0.20886424        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.6241016    0.09590659         1.0948188      0.12679878        4.675786e-01      3.679267e-02
# [4,]   0.04687776       0.7687940    0.17089887         0.9501264      0.20080938        8.913192e-01      3.449516e-03
# [5,]  -1.00000000       0.8232332    0.05760487         0.8956872      0.06955691        8.536994e-01      6.622971e-02
# [6,]  -2.00000000       0.7452107    0.05022551         0.9737097      0.07326376       -9.018291e-14      1.091922e-13
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          1.25448040       0.208864244
# [3,]          0.62724020       0.104432122
# [4,]          0.05880724       0.199073155
# [5,]          0.04198782       0.004238413
# [6,]          0.97370973       0.073317729

plot.oaxaca(obmale2017, components = c("endowments", "coefficients"))

#> 2018 ====
blackwhitemale2018 <- blackwhitemale[blackwhitemale$Year == 2018,]

obmale2018 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2018, R = 1000)

obmale2018$n
## sample
# $n.A
# [1] 246
#
# $n.B
# [1] 5178
#
# $n.pooled
# [1] 5424

obmale2018$y
# $y.A
# [1] 9.618164
#
# $y.B
# [1] 8.248568
#
# $y.diff
# [1] 1.369597

obmale2018$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.54240693         0.04612025         1.00865330         0.26037947        -0.18146352         0.25269467

obmale2018$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.5424069    0.04679450         0.8271898      0.06865694        8.271898e-01      6.865694e-02
# [2,]   1.00000000       0.3609434    0.25422578         1.0086533      0.26037947        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.4516752    0.13208553         0.9179215      0.14244990        4.135949e-01      3.432847e-02
# [4,]   0.04535398       0.5341768    0.24296931         0.8354199      0.24934246        7.896734e-01      3.113866e-03
# [5,]  -1.00000000       0.5649772    0.04688028         0.8046195      0.06584212        7.681268e-01      6.288064e-02
# [6,]  -2.00000000       0.5206834    0.04179115         0.8489133      0.06793895        2.817880e-14      1.097724e-13
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          1.00865330       0.260379471
# [3,]          0.50432665       0.130189736
# [4,]          0.04574644       0.248570225
# [5,]          0.03649270       0.003725259
# [6,]          0.84891329       0.068434518

plot.oaxaca(obmale2018, components = c("endowments", "coefficients"))

#> 2019 ====
blackwhitemale2019 <- blackwhitemale[blackwhitemale$Year == 2019,]

obmale2019 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2019, R = 1000)

obmale2019$n
## sample
# $n.A
# [1] 149
#
# $n.B
# [1] 3318
#
# $n.pooled
# [1] 3467

obmale2019$y
# $y.A
# [1] 9.931947
#
# $y.B
# [1] 8.52251
#
# $y.diff
# [1] 1.409437

obmale2019$threefold$overall
#   coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.59146251         0.05934206         1.03630860         0.18496568        -0.21833396         0.15408033

obmale2019$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000       0.5914625    0.05793548         0.8179746      0.08757748        8.179746e-01      8.757748e-02
# [2,]   1.00000000       0.3731286    0.15289882         1.0363086      0.18496568        0.000000e+00      0.000000e+00
# [3,]   0.50000000       0.4822955    0.08620961         0.9271416      0.12249846        4.089873e-01      4.378874e-02
# [4,]   0.04297664       0.5820793    0.14676901         0.8273579      0.17915119        7.828208e-01      3.763785e-03
# [5,]  -1.00000000       0.6224086    0.05970243         0.7870285      0.08376342        7.532047e-01      8.035594e-02
# [6,]  -2.00000000       0.5610710    0.05219835         0.8483661      0.08724700        1.129110e-13      6.875995e-14
# coef(unexplained B) se(unexplained B)
# [1,]          0.00000000       0.000000000
# [2,]          1.03630860       0.184965679
# [3,]          0.51815430       0.092482840
# [4,]          0.04453706       0.177016476
# [5,]          0.03382384       0.004458989
# [6,]          0.84836613       0.091412276

plot.oaxaca(obmale2019, components = c("endowments", "coefficients"))

#> 2020 ====
blackwhitemale2020 <- blackwhitemale[blackwhitemale$Year == 2020,]

obmale2020 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2020, R = 1000)

obmale2020$n
## sample
# $n.A
# [1] 30
#
# $n.B
# [1] 1078
#
# $n.pooled
# [1] 1108

obmale2020$y
# $y.A
# [1] 9.986863
#
# $y.B
# [1] 8.376622
#
# $y.diff
# [1] 1.610241

obmale2020$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.6007380          0.1549304                 NA                 NA                 NA                 NA

obmale2020$twofold$overall
#group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000              NA            NA                NA              NA                  NA                NA
# [2,]   1.00000000              NA            NA                NA              NA                  NA                NA
# [3,]   0.50000000              NA            NA                NA              NA                  NA                NA
# [4,]   0.02707581              NA            NA                NA              NA                  NA                NA
# [5,]  -1.00000000       0.6177931     0.1578720                NA              NA                  NA                NA
# [6,]  -2.00000000       0.5755664     0.1442716                NA              NA                  NA                NA
# coef(unexplained B) se(unexplained B)
# [1,]                  NA                NA
# [2,]                  NA                NA
# [3,]                  NA                NA
# [4,]                  NA                NA
# [5,]          0.02687134       0.006293425
# [6,]          1.03467470       0.153251500

plot.oaxaca(obmale2020, components = c("endowments", "coefficients"))

#> 2021
blackwhitemale2021 <- blackwhitemale[blackwhitemale$Year == 2021,]

obmale2021 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitemale2021, R = 1000)

obmale2021$n
## sample
# $n.A
# [1] 33
#
# $n.B
# [1] 1124
#
# $n.pooled
# [1] 1157

obmale2021$y
# $y.A
# [1] 9.66162
#
# $y.B
# [1] 8.334391
#
# $y.diff
# [1] 1.327229

obmale2021$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.4974531          0.1243413                 NA                 NA                 NA                 NA

obmale2021$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A)
# [1,]   0.00000000              NA            NA                NA              NA                  NA                NA
# [2,]   1.00000000              NA            NA                NA              NA                  NA                NA
# [3,]   0.50000000              NA            NA                NA              NA                  NA                NA
# [4,]   0.02852204              NA            NA                NA              NA                  NA                NA
# [5,]  -1.00000000       0.5146028     0.1258225                NA              NA                  NA                NA
# [6,]  -2.00000000       0.4893574     0.1212365                NA              NA                  NA                NA
# coef(unexplained B) se(unexplained B)
# [1,]                  NA                NA
# [2,]                  NA                NA
# [3,]                  NA                NA
# [4,]                  NA                NA
# [5,]          0.02317777       0.005805488
# [6,]          0.83787197       0.142826165

plot.oaxaca(obmale2021, components = c("endowments", "coefficients"))

## 2020 en 2021 het klomp NA's

#> Decomposition Graph ====

decompyrs <- c("2002","2002", "2003", "2003", "2004", "2004", "2005", "2005", "2006", "2006", "2007", "2007", "2008", "2008", "2009","2009", "2010", "2010", "2011", "2011", "2012", "2012", "2013", "2013", "2014", "2014", "2015", "2015", "2016", "2016", "2017", "2017", "2018", "2018", "2019", "2019", "2020", "2021" )

category <- c("Explained", "Unexplained", "Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained")

decompamounts <- c("0.7132564", "0.8353741", "0.6404947", "0.8227504", "0.6344424", "0.9302894", "0.5902633", "0.8590370", "0.5652656", "0.8371483","0.6377317","0.8623681", "0.5467406", "0.9621177", "0.6811127", "0.7143940","0.6123379", "1.100340", "0.7663265", "1.279715","0.6886298", "1.219562" ,"0.8421911", "1.429252", "0.7600447", "1.0539265", "0.7678788", "0.9045784", "0.7357509","0.8784970", "0.7837632", "0.9351572","0.5424069", "0.8271898", "0.5914625", "0.8179746", "0", "0", "0", "0")

decompunexpl <- c("0.8353741", "0.8227504", "0.9302894", "0.8590370", "0.8371483", "0.8623681", "0.9621177", "0.7143940", "1.100340", "1.279715", "1.219562", "1.429252", "1.0539265", "0.9045784", "0.8784970", "0.9351572", "0.8271898", "0.8179746")

decompgraphdf <- cbind(decompyrs, category, decompamounts)

decompgraphdf <- as.data.frame(decompgraphdf)

decompgraphdf$decompamounts <- as.numeric(decompgraphdf$decompamounts)

decompgraphdf$totalcoef <- decompgraphdf$decompexpl + decompgraphdf$decompunexpl

library(ggplot2)

ggplot(decompgraphdf, aes(fill= category, y=decompamounts, x=decompyrs)) +
    geom_bar(position='stack', stat='identity') +
    theme_minimal() +
    labs(x='Year', y='Wage Gap Coefficients') +
    theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
    scale_fill_manual('Position', values=c('coral2', 'steelblue', 'pink'))



