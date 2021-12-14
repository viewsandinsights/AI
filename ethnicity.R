rm(list=ls())
# ============ AT Austria
AT <- 0.01 * c(90.48, 0.03, 0.52, 4.54, 1.76, 0.73, 0.24, 0.84, 0.17, 0.02, 0.30, 0.06, 0.23, 0.01, 0.04, 0.03) # k=16 family origins N=2,430
x <- AT
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.172 for AT (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
HF
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.191 for AT (Herfindal-Index)

# ============ BE Belgium
rm(list = ls())
BE <- 0.01 * c(86.45, 3.03, 1.75, 1.19, 4.13, 0.13, 0.59, 1.53, 0.10, 0.06, 0.28, 0.30, 0.06, 0.12, 0.08, 0.20) # k=16 family origins N=1,740
x <- BE
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH #  0.2401167 for BE (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF #  0.2658911 for BE (Herfindal-Index)

# ============ BG Bulgaria
rm(list = ls())
BG <- 0.01 * c(0.05, 92.71, 0.37, 6.72, 0.15) # k=5 family origins N=2,186
x <- BG
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH #  0.1776353 for BG (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF #  0.169942 for BG (Herfindal-Index)

# ============ CY Cyprus
rm(list = ls())
CY <- 0.01 * c(0.25, 95.88, 2.10, 0.93, 0.21, 0.46, 0.06, 0.11) # k=8 N=776
x <- CY
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.1104132 for CY (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.09159049 for CY (Herfindal-Index)

# ============ CZ Czech Republic
rm(list = ls())
CZ <- 0.01 * c(0.20, 0.66, 99.07, 0.07) # k=4 N=2,388
x <- CZ
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.0432141 for CZ (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.02462061 for CZ (Herfindal-Index)

# ============ DE Germany
rm(list = ls())
DE <- 0.01 * c(87.28, 0.14, 1.44, 2.37, 3.71, 1.24, 1.08, 0.63, 0.37, 0.15, 0.35, 0.46, 0.31, 0.04, 0.15, 0.10, 0.16, 0.03) # k=18 N=2,321
x <- DE
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.2285613 for DE (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.2495622 for DE (Herfindal-Index)

# ============ EE Estonia
rm(list = ls()) 
EE <- 0.01 * c(0.04, 0.47, 0.05, 98.97, 0.27, 0.19) # k=6 N=1,896
x <- EE
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.03920399 for EE (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.02455261 for EE (Herfindal-Index)

# ============ ES Spain
rm(list = ls()) 
ES <- 0.01 * c(0.64, 89.81, 1.31, 0.65, 1.61, 0.06, 0.28, 0.12, 0.40, 3.85, 1.22, 0.06) # k=12 N=,664
x <- ES
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.209072 for ES (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.2086312 for ES (Herfindal-Index)

# ============ FR France
rm(list = ls()) 
FR <- 0.01 * c(84.95, 0.04, 4.49, 0.39, 0.40, 4.36, 0.09, 0.06, 0.43, 1.92, 0.04, 0.37, 0.57, 0.32, 0.29, 0.04, 0.16, 0.10, 0.91, 0.04) # k=20 N=2,000
x <- FR
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.242439 for FR (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.2882752 for FR (Herfindal-Index)

# ============ GB United Kingdom
rm(list = ls())
GB <- 0.01 * c(90.16, 0.57, 0.60, 2.90, 0.50, 4.25, 0.49, 0.54) # k=8 N=2,098
x <- GB
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.2265934 for GB (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.2106555 for GB (Herfindal-Index)

# ============ HR Croatia
rm(list = ls())
HR <- 0.01 * c( 0.32, 0.18, 98.80, 0.70) # k=4 N=1,800
x <- HR
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.05512488 for HR (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.03172469 for HR (Herfindal-Index)

# ============ HU Hungary
rm(list = ls())
HU <- 0.01 * c( 1.82, 98.18) # k=2 N=1,658
x <- HU
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.1312112 for HU (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.07147504 for HU (Herfindal-Index)

# ============ IE Ireland
rm(list = ls())
IE <- 0.01 * c(88.24, 0.09, 1.16, 0.83, 3.87, 0.17, 0.14, 0.98, 0.06, 0.36, 2.51, 0.28, 0.40, 0.35, 0.46, 0.08) # k=16 N=2,201
x <- IE
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.2183487 for IE (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.2334573 for IE (Herfindal-Index)

# ============ IT Italy
rm(list = ls())
IT <- 0.01 * c(93.30, 3.23, 0.38, 1.29, 0.30, 0.66, 0.21, 0.45, 0.20) # k=9 N=2,690
x <- IT
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.160719 for IT (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.1442313 for IT (Herfindal-Index)

# ============ LT Lithuania
rm(list = ls())
LT <- 0.01 * c( 0.30, 99.69, 0.01) # k=3 N= 1,824
x <- LT
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.01951885 for LT (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.00927207 for LT (Herfindal-Index)

# ============ LV Latvia
rm(list = ls())
LV <- 0.01 * c( 0.09, 0.08, 0.14, 99.63, 0.05) # k=5 N=912
x <- LV
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.01783847 for LV (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.009228313 for LV (Herfindal-Index)

# ============ NL Netherlands
rm(list = ls())
NL <- 0.01 * c(92.92, 0.32, 0.05, 0.97, 1.72, 0.21, 0.65, 0.49, 0.13, 0.04, 0.09, 0.08, 0.57, 0.15, 0.05, 0.18, 1.32, 0.06) # k=18 N=1,658
x <- NL
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.1441849 for NL (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.1438943 for NL (Herfindal-Index)

# ============ PL Poland
rm(list = ls())
PL <- 0.01 * c(99.99, 0.01) #k=2 N=1,471 # 100.00 zu 0.00 auf 99.99 zu 0.01 geaendert wg. Entropyberechnung
x <- PL
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.001473034 for PL (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.00039996 for PL (Herfindal-Index)

# ============ PT Portugal
rm(list = ls())
PT <- 0.01 * c( 1.01, 91.59, 0.18, 0.75, 3.48, 0.39, 0.14, 2.47) # k=8 N=1,048
x <- PT
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.1991174 for PT (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.1818599 for PT (Herfindal-Index)

# ============ SE Sweden
rm(list = ls())
SE <- 0.01 * c(99.99, 0.01) #k=2 N=1,306 # 100.00 zu 0.00 auf 99.99 zu 0.01 geaendert wg. Entropyberechnung
x <- SE
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.001473034 for SE (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.00039996 for SE (Herfindal-Index)

# ============ SI Slovenia
rm(list = ls())
SI <- 0.01 * c( 0.23, 0.23, 99.46, 0.08) # k=4 N=1,313
x <- SI
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.02815737 for SI (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.01434616 for SI (Herfindal-Index)

# ============ SK Slovakia
rm(list = ls())
SK <- 0.01 * c( 3.34, 96.66) # k=2 N=1,079
x <- SK
# ------------
lx <- log(x)*x
H <- -sum(lx)
RH <- -sum(lx)/log(length(x))
RH # 0.2111661 for SK (Relative Entropy)
# -----------
HF <- 1 - sum(x^2)
RHF <- HF * (length(x)/(length(x)-1))
RHF # 0.1291378 for SK (Herfindal-Index)
