# Female Decompositions
#> Only males ====
blackwhitefem <- blackwhite[blackwhite$Gender == 1,]

obfem <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem, R = 1000)

obfem$n
## sample
# $n.A
# [1] 7435
#
# $n.B
# [1] 92764
#
# $n.pooled
# [1] 100199

obfem$y
# $y.A
# [1] 8.881303
#
# $y.B
# [1] 7.327521
#
# $y.diff
# [1] 1.553782

obfem$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.80502914         0.01067869         1.37663831         0.15821165        -0.62788520         0.15575911

obfem$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.8050291    0.01067869         0.7487531      0.01722132        7.487531e-01      1.722132e-02          0.00000000       0.000000000
# [2,]   1.00000000       0.1771439    0.15600605         1.3766383      0.15821165        0.000000e+00      0.000000e+00          1.37663831       0.158211645
# [3,]   0.50000000       0.4910865    0.07849035         1.0626957      0.08123130        3.743766e-01      8.610660e-03          0.68831915       0.079105823
# [4,]   0.07420234       0.7584386    0.14447763         0.7953437      0.14672744        6.931939e-01      1.277862e-03          0.10214978       0.146471972
# [5,]  -1.00000000       0.8341407    0.01034751         0.7196415      0.01587830        6.662424e-01      1.471195e-02          0.05339908       0.001316396
# [6,]  -2.00000000       0.7731676    0.00952655         0.7806147      0.01687737       -1.950410e-12      1.484221e-12          0.78061466       0.016877366

plot.oaxaca(obfem, components = c("endowments", "coefficients"))

summary(obfem$reg$reg.pooled.2)$coefficients["secspline",]
# Estimate   Std. Error      t value     Pr(>|t|)
# 2.816582e-01 2.416956e-03 1.165343e+02 0.000000e+00

summary(obfem$reg$reg.pooled.2)$coefficients["prispline",]
#      Estimate    Std. Error       t value      Pr(>|t|)
# 5.608828e-02  2.212326e-03  2.535263e+01 1.984919e-141

#> 2002 ====
blackwhitefem2002 <- blackwhitefem[blackwhitefem$Year == 2002,]

obfem2002 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2002, R = 1000)

obfem2002$n
# $n.A
# [1] 518
#
# $n.B
# [1] 5281
#
# $n.pooled
# [1] 5799

obfem2002$y
#$y.A
#[1] 8.529952

#$y.B
#[1] 6.981321

#$y.diff
#[1] 1.548631

obfem2002$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.71325641         0.03405645         0.87925109         0.34147103        -0.04387694         0.33842639

obfem2002$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.8555380    0.03768674         0.9121697      0.04292037        9.121697e-01      4.292037e-02          0.00000000       0.000000000
# [2,]   1.00000000       0.3353282    0.80228449         1.4323795      0.80494737        0.000000e+00      0.000000e+00          1.43237955       0.804947374
# [3,]   0.50000000       0.5954331    0.40143383         1.1722746      0.40435132        4.560849e-01      2.146019e-02          0.71618977       0.402473687
# [4,]   0.08932575       0.8090699    0.73060063         0.9586379      0.73328885        8.306895e-01      3.833894e-03          0.12794837       0.733044849
# [5,]  -1.00000000       0.9437409    0.03597749         0.8239669      0.03742606        7.503654e-01      3.473795e-02          0.07360146       0.004178866
# [6,]  -2.00000000       0.8135129    0.03158166         0.9541948      0.04009385        2.562160e-13      9.670649e-14          0.95419484       0.040400072

plot.oaxaca(obfem2002, components = c("endowments", "coefficients"))

#> 2003 ====
blackwhitefem2003 <- blackwhitefem[blackwhitefem$Year == 2003,]

obfem2003 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2003, R = 1000)

obfem2003$n
## sample
#$n.A
#[1] 637

#$n.B
#[1] 6420

#$n.pooled
#[1] 7057

obfem2003$y
#$y.A
#[1] 8.582358

#$y.B
#[1] 7.119113

#$y.diff
#[1] 1.463245

obfem2003$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.64049467         0.03043677         0.64615100         0.42295712         0.17659944         0.42032989

obfem2003$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [2,]   1.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [3,]   0.50000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [4,]   0.08921389              NA            NA                NA              NA                  NA                NA                  NA                NA
# [5,]  -1.00000000       0.8085667    0.03408986                NA              NA                  NA                NA          0.06689365       0.004212851
# [6,]  -2.00000000       0.7143088    0.03173586                NA              NA                  NA                NA          0.84406998       0.040350124

plot.oaxaca(obfem2003, components = c("endowments", "coefficients"))

#> 2004 ====
blackwhitefem2004 <- blackwhitefem[blackwhitefem$Year == 2004,]

obfem2004 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2004, R = 1000)

obfem2004$n
## sample
#$n.A
#[1] 564

#$n.B
#[1] 6198

#$n.pooled
#[1] 6762

obfem2004$y
#$y.A
#[1] 8.752343

#$y.B
#[1] 7.187611

#$y.diff
#[1] 1.564732

obfem2004$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.63444238         0.03449738         1.33239249         1.07506630        -0.40210305         1.07506112

obfem2004$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.7433021    0.04357792         0.7899182      0.04563234        7.899182e-01      4.563234e-02          0.00000000        0.00000000
# [2,]   1.00000000       0.5583077    0.33911833         0.9749127      0.34141070        0.000000e+00      0.000000e+00          0.97491267        0.34141070
# [3,]   0.50000000       0.6508049    0.17155030         0.8824154      0.17407227        3.949591e-01      2.281617e-02          0.48745634        0.17070535
# [4,]   0.08406174       0.7277512    0.31073438         0.8054692      0.31304975        7.235163e-01      3.835934e-03          0.08195286        0.31271112
# [5,]  -1.00000000       0.7929021    0.03874896         0.7403182      0.03914323        6.780857e-01      3.611221e-02          0.06223244        0.00413632
# [6,]  -2.00000000       0.7044316    0.03546931         0.8287887      0.04139059       -2.302325e-13      1.125082e-13          0.82878874        0.04128314

plot.oaxaca(obfem2004, components = c("endowments", "coefficients"))

#> 2005 ====
blackwhitefem2005 <- blackwhitefem[blackwhitefem$Year == 2005,]

obfem2005 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2005, R = 1000)

obfem2005$n
## sample
#$n.A
#[1] 527

#$n.B
#[1] 6561

#$n.pooled
#[1] 7088

obfem2005$y
#$y.A
#[1] 8.647879

#$y.B
#[1] 7.198578

#$y.diff
#[1] 1.4493

obfem2005$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.5902633          0.0301759          0.6348244          1.2252039          0.2242126          1.2236004

obfem2005$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.6868675    0.03558027         0.7410565      0.04843612        7.410565e-01      4.843612e-02          0.00000000       0.000000000
# [2,]   1.00000000       0.2509452    0.40332390         1.1769788      0.40255140        0.000000e+00      0.000000e+00          1.17697882       0.402551397
# [3,]   0.50000000       0.4689063    0.20236972         0.9590177      0.20293419        3.705283e-01      2.421806e-02          0.58848941       0.201275699
# [4,]   0.06446053       0.6587677    0.37732264         0.7691563      0.37664252        6.932876e-01      3.122218e-03          0.07586868       0.376602721
# [5,]  -1.00000000       0.7308765    0.03387631         0.6970475      0.04359788        6.521155e-01      4.105258e-02          0.04493206       0.003403526
# [6,]  -2.00000000       0.6695331    0.03134804         0.7583909      0.04635102       -1.331990e-13      8.196231e-14          0.75839093       0.046377538

plot.oaxaca(obfem2005, components = c("endowments", "coefficients"))

#> 2006 ====
blackwhitefem2006 <- blackwhitefem[blackwhitefem$Year == 2006,]

obfem2006 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2006, R = 1000)

obfem2006$n
## sample
#$n.A
#[1] 636

#$n.B
#[1] 7149

#$n.pooled
#[1] 7785

obfem2006$y
#$y.A
#[1] 8.70609

#$y.B
#[1] 7.303676

#$y.diff
#[1] 1.402414

obfem2006$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.5652656          0.0270097          0.7359520          0.1362808          0.1011962          0.1329955

obfem2006$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.6916165    0.03729124         0.7432126      0.04678736        7.432126e-01      4.678736e-02          0.00000000       0.000000000
# [2,]   1.00000000       1.2156617    0.16991996         0.2191673      0.16837749        0.000000e+00      0.000000e+00          0.21916731       0.168377486
# [3,]   0.50000000       0.9536391    0.08881830         0.4811899      0.08959350        3.716063e-01      2.339368e-02          0.10958365       0.084188743
# [4,]   0.07846052       0.7327333    0.15691308         0.7020957      0.15557470        6.848997e-01      3.670961e-03          0.01719598       0.155166501
# [5,]  -1.00000000       0.7409468    0.03287453         0.6938822      0.04032393        6.394398e-01      3.715256e-02          0.05444236       0.004002626
# [6,]  -2.00000000       0.6694389    0.03063578         0.7653901      0.04336595        1.654371e-13      1.049344e-13          0.76539010       0.043286018

plot.oaxaca(obfem2006, components = c("endowments", "coefficients"))

#> 2007 ====
blackwhitefem2007 <- blackwhitefem[blackwhitefem$Year == 2007,]

obfem2007 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2007, R = 1000)

obfem2007$n
## sample
#$n.A
#[1] 609

#$n.B
#[1] 7715

#$n.pooled
#[1] 8324

obfem2007$y
#$y.A
#[1] 8.913732

#$y.B
#[1] 7.413632

#$y.diff
#[1] 1.5001

obfem2007$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.63773171         0.03016946         0.99612766         0.05929164        -0.13375952         0.06140768

obfem2007$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.6904000    0.03458885         0.7470209      0.03987510        7.470209e-01      3.987510e-02          0.00000000        0.00000000
# [2,]   1.00000000       0.4793217    0.92088818         0.9580992      0.92322676        0.000000e+00      0.000000e+00          0.95809925        0.92322676
# [3,]   0.50000000       0.5848609    0.46133605         0.8525601      0.46387938        3.735105e-01      1.993755e-02          0.47904962        0.46161338
# [4,]   0.07118989       0.6753734    0.85541467         0.7620476      0.85776913        6.938406e-01      2.838704e-03          0.06820698        0.85750235
# [5,]  -1.00000000       0.7323996    0.03233473         0.7050214      0.03545386        6.548310e-01      3.305215e-02          0.05019039        0.00332670
# [6,]  -2.00000000       0.6634517    0.02899390         0.7739693      0.03765116       -3.523920e-14      1.112749e-13          0.77396926        0.03781443

plot.oaxaca(obfem2007, components = c("endowments", "coefficients"))

#> 2008 ====
blackwhitefem2008 <- blackwhitefem[blackwhitefem$Year == 2008,]

obfem2008 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2008, R = 1000)

obfem2008$n
## sample
# $n.A
# [1] 543
#
# $n.B
# [1] 7210
#
# $n.pooled
# [1] 7753

obfem2008$y
# $y.A
# [1] 9.099855
#
# $y.B
# [1] 7.590997
#
# $y.diff
# [1] 1.508858

obfem2008$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.54674055         0.02946264         1.07929570         0.20046293        -0.11717803         0.19756684

obfem2008$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [2,]   1.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [3,]   0.50000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [4,]   0.06247971              NA            NA                NA              NA                  NA                NA                  NA                NA
# [5,]  -1.00000000       0.6578924    0.03332559                NA              NA                  NA                NA          0.05150963       0.003464896
# [6,]  -2.00000000       0.5971434    0.02993042                NA              NA                  NA                NA          0.88517060       0.041719982

plot.oaxaca(obfem2008, components = c("endowments", "coefficients"))

#> 2009 ====
blackwhitefem2009 <- blackwhitefem[blackwhitefem$Year == 2009,]

obfem2009 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2009, R = 1000)

obfem2009$n
## sample
#$n.A
# [1] 1011
#
# $n.B
# [1] 7732
#
# $n.pooled
# [1] 8743

obfem2009$y
# $y.A
# [1] 9.202309
#
# $y.B
# [1] 7.806802
#
# $y.diff
# [1] 1.395507

obfem2009$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.681112663        0.026277438        0.712161187        0.046035106        0.002232819        0.051778808

obfem2009$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]    0.0000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [2,]    1.0000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [3,]    0.5000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [4,]    0.1211344              NA            NA                NA              NA                  NA                NA                  NA                NA
# [5,]   -1.0000000       0.6869043    0.02460362                NA              NA                  NA                NA          0.07945543        0.00422273
# [6,]   -2.0000000       0.6099826    0.02210913                NA              NA                  NA                NA          0.73284958        0.03106737

plot.oaxaca(obfem2009, components = c("endowments", "coefficients"))

#> 2010 ====
blackwhitefem2010 <- blackwhitefem[blackwhitefem$Year == 2010,]

obfem2010 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2010, R = 1000)

obfem2010$n
## sample
# $n.A
# [1] 423
#
# $n.B
# [1] 5371
#
# $n.pooled
# [1] 5794

obfem2010$y
# $n.A
# [1] 423
#
# $n.B
# [1] 5371
#
# $n.pooled
# [1] 5794

obfem2010$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.61233787         0.04126963         1.33587184         0.26182962        -0.23553160         0.25941928

obfem2010$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.7082965    0.04735086         0.8104654      0.05840740        8.104654e-01      5.840740e-02          0.00000000       0.000000000
# [2,]   1.00000000       0.2910049    0.15577185         1.2277570      0.15772939        0.000000e+00      0.000000e+00          1.22775702       0.157729393
# [3,]   0.50000000       0.4996507    0.08367849         1.0191112      0.08884591        4.052327e-01      2.920370e-02          0.61387851       0.078864697
# [4,]   0.07282563       0.6779070    0.14481927         0.8408549      0.14706045        7.514428e-01      4.253556e-03          0.08941218       0.146242650
# [5,]  -1.00000000       0.7344522    0.04473870         0.7843097      0.05266067        7.271919e-01      4.890176e-02          0.05711785       0.004843816
# [6,]  -2.00000000       0.6734492    0.04060177         0.8453127      0.05478686        9.295587e-14      2.572818e-14          0.84531266       0.054784040

plot.oaxaca(obfem2010, components = c("endowments", "coefficients"))

#> 2011 ====
blackwhitefem2011 <- blackwhitefem[blackwhitefem$Year == 2011,]

obfem2011 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2011, R = 1000)

obfem2011$n
## sample
#$n.A
# [1] 430
#
# $n.B
# [1] 5193
#
# $n.pooled
# [1] 5623

obfem2011$y
# $y.A
# [1] 9.729678
#
# $y.B
# [1] 7.683636
#
# $y.diff
# [1] 2.046042

obfem2011$threefold$overall
#coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.76632648         0.05473859         1.43960583         0.16573063        -0.15989078         0.15785174

obfem2011$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.7276698    0.04679401          1.090174       0.1167238        1.090174e+00      1.167238e-01          0.00000000        0.00000000
# [2,]   1.00000000       0.3322792    0.21585000          1.485564       0.2349785        0.000000e+00      0.000000e+00          1.48556441        0.23497851
# [3,]   0.50000000       0.5299745    0.11256472          1.287869       0.1506653        5.450869e-01      5.836191e-02          0.74278220        0.11748925
# [4,]   0.08593253       0.6936929    0.19772067          1.124151       0.2189948        9.964924e-01      1.003037e-02          0.12765830        0.21478621
# [5,]  -1.00000000       0.7697866    0.04938608          1.048057       0.1115094        9.579949e-01      1.022039e-01          0.09006219        0.01041114
# [6,]  -2.00000000       0.6805627    0.04411763          1.137281       0.1191829        1.553061e-13      1.021821e-13          1.13728089        0.11922920


plot.oaxaca(obfem2011, components = c("endowments", "coefficients"))

#> 2012 ====
blackwhitefem2012 <- blackwhitefem[blackwhitefem$Year == 2012,]

obfem2012 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2012, R = 1000)

obfem2012$n
## sample
# $n.A
# [1] 376
#
# $n.B
# [1] 5109
#
# $n.pooled
# [1] 5485

obfem2012$y
# #$y.A
# #[1] 8.529952
#
# #$y.B
# #[1] 6.981321
#
# #$y.diff
# #[1] 1.548631

obfem2012$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.68862977         0.05804812         2.08595844         0.33379254        -0.86639678         0.29355103

obfem2012$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.7247284    0.06040490          1.197344       0.1256110        1.197344e+00      1.256110e-01           0.0000000       0.000000000
# [2,]   1.00000000       0.5954939    0.10693572          1.326578       0.1350508        0.000000e+00      0.000000e+00           1.3265783       0.135050816
# [3,]   0.50000000       0.6601111    0.06812605          1.261961       0.1187753        5.986719e-01      6.280548e-02           0.6632891       0.067525408
# [4,]   0.07095533       0.7155585    0.10058754          1.206514       0.1315265        1.112386e+00      8.912767e-03           0.0941278       0.125468241
# [5,]  -1.00000000       0.7726743    0.06159407          1.149398       0.1144366        1.067842e+00      1.062680e-01           0.0815559       0.009320447
# [6,]  -2.00000000       0.6876657    0.05351411          1.234406       0.1223586       -1.462972e-13      9.592059e-14           1.2344064       0.122222438

plot.oaxaca(obfem2012, components = c("endowments", "coefficients"))

#> 2013 ====
blackwhitefem2013 <- blackwhitefem[blackwhitefem$Year == 2013,]

obfem2013 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2013, R = 1000)

obfem2013$n
## sample
# $n.A
# [1] 431
#
# $n.B
# [1] 5415
#
# $n.pooled
# [1] 5846

obfem2013$y
# $y.A
# [1] 10.05655
#
# $y.B
# [1] 7.785106
#
# $y.diff
# [1] 2.271443

obfem2013$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.84219110         0.05751811         1.76821234         0.23235853        -0.33896064         0.19517575

obfem2013$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.8907083    0.05809478          1.313392       0.1565748        1.313392e+00      1.565748e-01          0.00000000        0.00000000
# [2,]   1.00000000       0.1245516    0.59300619          2.079549       0.6315557        0.000000e+00      0.000000e+00          2.07954859        0.63155574
# [3,]   0.50000000       0.5076299    0.29941539          1.696470       0.3518850        6.566960e-01      7.828742e-02          1.03977429        0.31577787
# [4,]   0.07525814       0.8330487    0.54862136          1.371051       0.5884132        1.214549e+00      1.178353e-02          0.15650296        0.58402603
# [5,]  -1.00000000       0.9377320    0.05434217          1.266368       0.1450229        1.171064e+00      1.344529e-01          0.09530451        0.01160789
# [6,]  -2.00000000       0.8211349    0.04858094          1.382965       0.1563388        5.682383e-14      4.591858e-14          1.38296532        0.15601192

plot.oaxaca(obfem2013, components = c("endowments", "coefficients"))

#> 2014 ====
blackwhitefem2014 <- blackwhitefem[blackwhitefem$Year == 2014,]

obfem2014 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2014, R = 1000)

obfem2014$n
## sample
#$n.A
# [1] 406
#
# $n.B
# [1] 5984
#
# $n.pooled
# [1] 6390

obfem2014$y
# $y.A
# [1] 9.504246
#
# $y.B
# [1] 7.690275
#
# $y.diff
# [1] 1.813971

obfem2014$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.76004473         0.04933202         1.32047585         0.21498797        -0.26654939         0.20881040

obfem2014$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.7661219    0.05020651         0.8263733      0.09981545        8.263733e-01      9.981545e-02          0.00000000       0.000000000
# [2,]   1.00000000      -0.9781450    0.23721985         2.5706402      0.23485468        0.000000e+00      0.000000e+00          2.57064024       0.234854681
# [3,]   0.50000000      -0.1060115    0.11916392         1.6985068      0.13176868        4.131867e-01      4.990773e-02          1.28532012       0.117427340
# [4,]   0.06127367       0.6592443    0.22244815         0.9332509      0.22111328        7.757384e-01      6.116059e-03          0.15751255       0.220464274
# [5,]  -1.00000000       0.7884164    0.04812365         0.8040788      0.09295126        7.548100e-01      8.729016e-02          0.04926886       0.006305632
# [6,]  -2.00000000       0.7284729    0.04445829         0.8640223      0.09829329        2.282671e-13      1.218066e-13          0.86402231       0.099307058

plot.oaxaca(obfem2014, components = c("endowments", "coefficients"))

#> 2015 ====
blackwhitefem2015 <- blackwhitefem[blackwhitefem$Year == 2015,]

obfem2015 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2015, R = 1000)

obfem2015$n
## sample
# $n.A
# [1] 346
#
# $n.B
# [1] 5587
#
# $n.pooled
# [1] 5933

obfem2015$y
# $y.A
# [1] 9.471878
#
# $y.B
# [1] 7.799421
#
# $y.diff
# [1] 1.672457

obfem2015$threefold$overall
#coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.76787875         0.05389511         1.14097310         0.14057371        -0.23639468         0.10942257

obfem2015$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.8831794    0.05838452         0.8336906      0.08110289        8.336906e-01      8.110289e-02          0.00000000       0.000000000
# [2,]   1.00000000       0.2205459    0.10068279         1.4963241      0.10924923        0.000000e+00      0.000000e+00          1.49632406       0.109249231
# [3,]   0.50000000       0.5518626    0.06041333         1.1650073      0.07831615        4.168453e-01      4.055144e-02          0.74816203       0.054624616
# [4,]   0.05403777       0.8473721    0.09557645         0.8694978      0.10491563        7.886398e-01      4.382619e-03          0.08085801       0.103345647
# [5,]  -1.00000000       0.9060098    0.05759693         0.8108601      0.07289546        7.670431e-01      6.889660e-02          0.04381707       0.004846928
# [6,]  -2.00000000       0.8354318    0.05277386         0.8814382      0.07692398        4.735776e-14      7.890502e-14          0.88143815       0.077948903

plot.oaxaca(obfem2015, components = c("endowments", "coefficients"))

#> 2016 ====
blackwhitefem2016 <- blackwhitefem[blackwhitefem$Year == 2016,]

obfem2016 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2016, R = 1000)

obfem2016$n
## sample
#$n.A
# [1] 744
#
# $n.B
# [1] 6964
#
# $n.pooled
# [1] 7708

obfem2016$y
# $y.A
# [1] 9.807896
#
# $y.B
# [1] 8.193648
#
# $y.diff
# [1] 1.614248

obfem2016$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.73575089         0.03103026         1.32192225         0.05095134        -0.44342524         0.04738002

obfem2016$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [2,]   1.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [3,]   0.50000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [4,]   0.09491879              NA            NA                NA              NA                  NA                NA                  NA                NA
# [5,]  -1.00000000       0.8195658    0.03479439                NA              NA                  NA                NA          0.07270856       0.004222566
# [6,]  -2.00000000       0.7313266    0.03081479                NA              NA                  NA                NA          0.85424731       0.037563565

plot.oaxaca(obfem2016, components = c("endowments", "coefficients"))

#> 2017 ====
blackwhitefem2017 <- blackwhitefem[blackwhitefem$Year == 2017,]

obfem2017 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2017, R = 1000)

obfem2017$n
## sample
#$n.A
#[1] 265

#$n.B
# [1] 5388
#
# $n.pooled
# [1] 5653

obfem2017$y
# $y.A
# [1] 9.639313
#
# $y.B
# [1] 7.920392
#
# $y.diff
# [1] 1.71892

obfem2017$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.78376317         0.05685267         1.25448040         0.20886424        -0.31932315         0.18375642

obfem2017$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.7887384    0.06119042         0.9208128      0.08922897        9.208128e-01      8.922897e-02          0.00000000       0.000000000
# [2,]   1.00000000       0.1362382    0.15436381         1.5733130      0.17824589        0.000000e+00      0.000000e+00          1.57331301       0.178245894
# [3,]   0.50000000       0.4624883    0.08694739         1.2470629      0.11679181        4.604064e-01      4.461449e-02          0.78665651       0.089122947
# [4,]   0.05181024       0.7549322    0.14684729         0.9546190      0.17121478        8.731053e-01      4.622974e-03          0.08151372       0.169010932
# [5,]  -1.00000000       0.8095320    0.06038101         0.9000192      0.08492098        8.533890e-01      8.071607e-02          0.04663021       0.005310968
# [6,]  -2.00000000       0.7377511    0.05495629         0.9718001      0.08861983       -8.601188e-14      9.330336e-14          0.97180009       0.090213934

plot.oaxaca(obfem2017, components = c("endowments", "coefficients"))

#> 2018 ====
blackwhitefem2018 <- blackwhitefem[blackwhitefem$Year == 2018,]

obfem2018 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2018, R = 1000)

obfem2018$n
## sample
# $n.A
# [1] 246
#
# $n.B
# [1] 5178
#
# $n.pooled
# [1] 5424

obfem2018$y
# $y.A
# [1] 9.618164
#
# $y.B
# [1] 8.248568
#
# $y.diff
# [1] 1.369597

obfem2018$threefold$overall
# coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.54240693         0.04612025         1.00865330         0.26037947        -0.18146352         0.25269467

obfem2018$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000       0.7380383    0.05706050         0.7535526      0.07303286        7.535526e-01      7.303286e-02          0.00000000       0.000000000
# [2,]   1.00000000       0.6519182    0.16636522         0.8396727      0.19229433        0.000000e+00      0.000000e+00          0.83967272       0.192294328
# [3,]   0.50000000       0.6949782    0.09307419         0.7966127      0.11979822        3.767763e-01      3.651643e-02          0.41983636       0.096147164
# [4,]   0.04427966       0.7342249    0.15951265         0.7573660      0.18553767        7.201855e-01      3.233870e-03          0.03718042       0.183779601
# [5,]  -1.00000000       0.7696629    0.05762708         0.7219280      0.06927279        6.899613e-01      6.644139e-02          0.03196673       0.003564568
# [6,]  -2.00000000       0.7114826    0.05205189         0.7801083      0.07229319        9.498939e-14      1.030481e-13          0.78010828       0.071741253

plot.oaxaca(obfem2018, components = c("endowments", "coefficients"))

#> 2019 ====
blackwhitefem2019 <- blackwhitefem[blackwhitefem$Year == 2019,]

obfem2019 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2019, R = 1000)

obfem2019$n
## sample
# $n.A
# [1] 149
#
# $n.B
# [1] 3318
#
# $n.pooled
# [1] 3467

obfem2019$y
# $y.A
# [1] 9.931947
#
# $y.B
# [1] 8.52251
#
# $y.diff
# [1] 1.409437

obfem2019$threefold$overall
#   coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.59146251         0.05934206         1.03630860         0.18496568        -0.21833396         0.15408033

obfem2019$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [2,]   1.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [3,]   0.50000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [4,]   0.04942717              NA            NA                NA              NA                  NA                NA                  NA                NA
# [5,]  -1.00000000       0.6087963    0.06374446                NA              NA                  NA                NA          0.03079805       0.004772097
# [6,]  -2.00000000       0.5738191    0.05887154                NA              NA                  NA                NA          0.65807681       0.086894643

plot.oaxaca(obfem2019, components = c("endowments", "coefficients"))

#> 2020 ====
blackwhitefem2020 <- blackwhitefem[blackwhitefem$Year == 2020,]

obfem2020 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2020, R = 1000)

obfem2020$n
## sample
# $n.A
# [1] 30
#
# $n.B
# [1] 1078
#
# $n.pooled
# [1] 1108

obfem2020$y
# $y.A
# [1] 9.986863
#
# $y.B
# [1] 8.376622
#
# $y.diff
# [1] 1.610241

obfem2020$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
# 0.6007380          0.1549304                 NA                 NA                 NA                 NA

obfem2020$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]    0.0000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [2,]    1.0000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [3,]    0.5000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [4,]    0.0323993              NA            NA                NA              NA                  NA                NA                  NA                NA
# [5,]   -1.0000000       0.5435533     0.1233717                NA              NA                  NA                NA          0.03104054       0.006406802
# [6,]   -2.0000000       0.5157737     0.1137094                NA              NA                  NA                NA          0.98584171       0.143080716

plot.oaxaca(obfem2020, components = c("endowments", "coefficients"))

#> 2021
blackwhitefem2021 <- blackwhitefem[blackwhitefem$Year == 2021,]

obfem2021 <- oaxaca(formula = logsal ~ exp + expsq + prispline + secspline + terspline | blackdummy, data = blackwhitefem2021, R = 1000)

obfem2021$n
## sample
# $n.A
# [1] 33
#
# $n.B
# [1] 1124
#
# $n.pooled
# [1] 1157

obfem2021$y
# $y.A
# [1] 9.66162
#
# $y.B
# [1] 8.334391
#
# $y.diff
# [1] 1.327229

obfem2021$threefold$overall
#  coef(endowments)     se(endowments) coef(coefficients)   se(coefficients)  coef(interaction)    se(interaction)
#0.4974531          0.1243413                 NA                 NA                 NA                 NA

obfem2021$twofold$overall
# group.weight coef(explained) se(explained) coef(unexplained) se(unexplained) coef(unexplained A) se(unexplained A) coef(unexplained B) se(unexplained B)
# [1,]   0.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [2,]   1.00000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [3,]   0.50000000              NA            NA                NA              NA                  NA                NA                  NA                NA
# [4,]   0.02459712              NA            NA                NA              NA                  NA                NA                  NA                NA
# [5,]  -1.00000000       0.6840076     0.1594494                NA              NA                  NA                NA          0.02362928       0.005519429
# [6,]  -2.00000000       0.6552129     0.1509796                NA              NA                  NA                NA          0.98944730       0.147432053


plot.oaxaca(obfem2021, components = c("endowments", "coefficients"))

## 2003, 2008, 2009, 2016, 2019, 2020 en 2021 het klomp NA's

#> Decomposition Graph ====

decompyrs <- c("2002","2002", "2003", "2003", "2004", "2004", "2005", "2005", "2006", "2006", "2007", "2007", "2008", "2008", "2009","2009", "2010", "2010", "2011", "2011", "2012", "2012", "2013", "2013", "2014", "2014", "2015", "2015", "2016", "2016", "2017", "2017", "2018", "2018", "2019", "2019", "2020", "2021" )

category <- c("Explained", "Unexplained", "Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained","Explained", "Unexplained")

femdecompamounts <- c("0.8555380", "0.9121697", "0", "0", "0.7433021", "0.7899182", "0.6868675", "0.7410565", "0.6916165", "0.7432126","0.6904000","0.7470209", "0", "0", "0", "0","0.7082965", "0.8104654", "0.7276698", "1.090174","0.7247284", "1.197344" ,"0.8907083", "1.313392", "0.7661219", "0.8263733", "0.8831794", "0.8336906", "0","0", "0.7887384", "0.9208128","0.7380383", "0.7535526", "0", "0", "0", "0", "0", "0")

femdecompgraphdf <- cbind(decompyrs, category, femdecompamounts)

femdecompgraphdf <- as.data.frame(femdecompgraphdf)

femdecompgraphdf$femdecompamounts <- as.numeric(femdecompgraphdf$femdecompamounts)

library(ggplot2)

ggplot(femdecompgraphdf, aes(fill= category, y=femdecompamounts, x=decompyrs)) +
    geom_bar(position='stack', stat='identity') +
    theme_minimal() +
    labs(x='Year', y='Wage Gap Coefficients') +
    theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) +
    scale_fill_manual('Position', values=c('coral2', 'steelblue', 'pink'))

