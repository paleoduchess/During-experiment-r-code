#3 way anova

#
# Three-Factor ANOVA:
# 
# Read the data using scan():
#
#          a1               a2               a3               a4
#     -------------    -------------    -------------    -------------
#     b1   b2   b3     b1   b2   b3     b1   b2   b3     b1   b2   b3
#     ---  ---  ---    ---  ---  ---    ---  ---  ---    ---  ---  ---
#
# c1:
#     4.1  4.6  3.7    4.9  5.2  4.7    5.0  6.1  5.5    3.9  4.4  3.7
#     4.3  4.9  3.9    4.6  5.6  4.7    5.4  6.2  5.9    3.3  4.3  3.9
#     4.5  4.2  4.1    5.3  5.8  5.0    5.7  6.5  5.6    3.4  4.7  4.0
#     3.8  4.5  4.5    5.0  5.4  4.5    5.3  5.7  5.0    3.7  4.1  4.4
#     4.3  4.8  3.9    4.6  5.5  4.7    5.4  6.1  5.9    3.3  4.2  3.9
#
# c2:
#     4.8  5.6  5.0    4.9  5.9  5.0    6.0  6.0  6.1    4.1  4.9  4.3  
#     4.5  5.8  5.2    5.5  5.3  5.4    5.7  6.3  5.3    3.9  4.7  4.1
#     5.0  5.4  4.6    5.5  5.5  4.7    5.5  5.7  5.5    4.3  4.9  3.8
#     4.6  6.1  4.9    5.3  5.7  5.1    5.7  5.9  5.8    4.0  5.3  4.7
#     5.0  5.4  4.7    5.5  5.5  4.9    5.5  5.7  5.6    4.3  4.3  3.8
#
# NOTE: Cut and paste the numbers without the leading # or labels
#

> Y <- scan()
> A <- gl(4,3, 4*3*2*5, labels=c("a1","a2","a3","a4"));
> B <- gl(3,1, 4*3*2*5, labels=c("b1","b2","b3"));
> C <- gl(2,60, 4*3*2*5, labels=c("c1","c2"));
> anova(lm(Y~A*B*C))   # all effects and interactions
Analysis of Variance Table

Response: Y
Df Sum Sq Mean Sq  F value    Pr(>F)
A          3 40.322  13.441 182.4506 < 2.2e-16 ***
  B          2  8.821   4.411  59.8722 < 2.2e-16 ***
  C          1  4.760   4.760  64.6165 2.356e-12 ***
  A:B        6  0.814   0.136   1.8420   0.09895 .
A:C        3  2.351   0.784  10.6376 4.216e-06 ***
  B:C        2  0.126   0.063   0.8563   0.42793
A:B:C      6  0.944   0.157   2.1354   0.05616 .
Residuals 96  7.072   0.074
---
  Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1

> anova(lm(Y~A+B+C))   # only effects, no interactions
Analysis of Variance Table

Response: Y
Df Sum Sq Mean Sq F value    Pr(>F)
A           3 40.322  13.441 134.321 < 2.2e-16 ***
B           2  8.821   4.411  44.078 7.068e-15 ***
C           1  4.760   4.760  47.571 3.227e-10 ***
Residuals 113 11.307   0.100
---
Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1

> anova(lm(Y~A:B+A:C+B:C))   # only two-way interactions
  Analysis of Variance Table

  Response: Y
	     Df Sum Sq Mean Sq F value    Pr(>F)
  A:B        11 49.957   4.542 57.7902 < 2.2e-16 ***
  A:C         4  7.111   1.778 22.6215 2.147e-13 ***
  B:C         2  0.126   0.063  0.8027    0.4509
  Residuals 102  8.016   0.079
  ---
  Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1
