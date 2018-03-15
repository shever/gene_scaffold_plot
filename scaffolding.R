setwd("E:/IWBBIO 2017")
library(ade4)
library(grid)
library(genoPlotR)
s1 <- read.csv("S1.csv",header = T)
s2 <- read.csv("S2.csv",header = T)
s3 <- read.csv("S3.csv",header = T)
s4 <- read.csv("S4.csv",header = T)
s5 <- read.csv("S5.csv",header = T)
s6 <- read.csv("S6.csv",header = T)
s7 <- read.csv("S7.csv",header = T)
s8 <- read.csv("S8.csv",header = T)
s9 <- read.csv("S9.csv",header = T)
s10 <- read.csv("S10.csv",header = T)
s11 <- read.csv("S11.csv",header = T)
s12 <- read.csv("S12.csv",header = T)
s13 <- read.csv("S13.csv",header = T)
s14 <- read.csv("S14.csv",header = T)
s15 <- read.csv("S15.csv",header = T)
s16 <- read.csv("S16.csv",header = T)
s17 <- read.csv("S17.csv",header = T)
s18 <- read.csv("S18.csv",header = T)
s19 <- read.csv("S19.csv",header = T)
s20 <- read.csv("S20.csv",header = T)
s21 <- read.csv("S21.csv",header = T)
s22 <- read.csv("S22.csv",header = T)
s23 <- read.csv("S23.csv",header = T)
s24 <- read.csv("S24.csv",header = T)
s25 <- read.csv("S25.csv",header = T)
col_s1 <- rep("cornflowerblue",nrow(s1))
col_s2 <- rep("cornflowerblue",nrow(s2))
col_s3 <- rep("cornflowerblue",nrow(s3))
col_s4 <- rep("cornflowerblue",nrow(s4))
col_s5 <- rep("cornflowerblue",nrow(s5))
col_s6 <- rep("cornflowerblue",nrow(s6))
col_s7 <- rep("cornflowerblue",nrow(s7))
col_s8 <- rep("cornflowerblue",nrow(s8))
col_s9 <- rep("cornflowerblue",nrow(s9))
col_s10 <- rep("cornflowerblue",nrow(s10))
col_s11 <- rep("cornflowerblue",nrow(s11))
col_s12 <- rep("cornflowerblue",nrow(s12))
col_s13 <- rep("cornflowerblue",nrow(s13))
col_s14 <- rep("cornflowerblue",nrow(s14))
col_s15 <- rep("cornflowerblue",nrow(s15))
col_s16 <- rep("cornflowerblue",nrow(s16))
col_s17 <- rep("cornflowerblue",nrow(s17))
col_s18 <- rep("cornflowerblue",nrow(s18))
col_s19 <- rep("cornflowerblue",nrow(s19))
col_s20 <- rep("cornflowerblue",nrow(s20))
col_s21 <- rep("cornflowerblue",nrow(s21))
col_s22 <- rep("cornflowerblue",nrow(s22))
col_s23 <- rep("cornflowerblue",nrow(s23))
col_s24 <- rep("cornflowerblue",nrow(s24))
col_s25 <- rep("cornflowerblue",nrow(s25))
for(i in 1:length(col_s1)){
  if(s1$s1[i] %in% c(1:50)) col_s1[i] <- "pink"
  else if (s1$s1[i] %in% c(51:100)) col_s1[i] <- "coral4"
  else if (s1$s1[i] %in% c(101:150)) col_s1[i] <- "chocolate2"
  else if (s1$s1[i] %in% c(151:200)) col_s1[i] <- "aquamarine3"
  else if (s1$s1[i] %in% c(201:250)) col_s1[i] <- "deepskyblue4"
  else if (s1$s1[i] %in% c(251:300)) col_s1[i] <- "blueviolet"
  else if (s1$s1[i] %in% c(301:350)) col_s1[i] <- "brown4"
  else if (s1$s1[i] %in% c(351:400)) col_s1[i] <- "darkgoldenrod3"
  else if (s1$s1[i] %in% c(401:450)) col_s1[i] <- "chartreuse3"
  else if (s1$s1[i] %in% c(451:500)) col_s1[i] <- "deeppink4"
  else if (s1$s1[i] %in% c(501:550)) col_s1[i] <- "khaki"
  else if (s1$s1[i] %in% c(551:600)) col_s1[i] <- "midnightblue"
  else if (s1$s1[i] %in% c(601:650)) col_s1[i] <- "rosybrown1"
  else if (s1$s1[i] %in% c(651:700)) col_s1[i] <- "plum"
  else if (s1$s1[i] %in% c(701:750)) col_s1[i] <- "slategray4"
  else if (s1$s1[i] =="TSP") col_s1[i] <- "yellowgreen"
  else if (s1$s1[i] =="IN") col_s1[i] <- "red"
  else if (substr(s1$s1[i],1,1) =="D") col_s1[i] <- "snow"
}
for(i in 1:length(col_s2)){
  if(s2$s2[i] %in% c(1:50)) col_s2[i] <- "pink"
  else if (s2$s2[i] %in% c(51:100)) col_s2[i] <- "coral4"
  else if (s2$s2[i] %in% c(101:150)) col_s2[i] <- "chocolate2"
  else if (s2$s2[i] %in% c(151:200)) col_s2[i] <- "aquamarine3"
  else if (s2$s2[i] %in% c(201:250)) col_s2[i] <- "deepskyblue4"
  else if (s2$s2[i] %in% c(251:300)) col_s2[i] <- "blueviolet"
  else if (s2$s2[i] %in% c(301:350)) col_s2[i] <- "brown4"
  else if (s2$s2[i] %in% c(351:400)) col_s2[i] <- "darkgoldenrod3"
  else if (s2$s2[i] %in% c(401:450)) col_s2[i] <- "chartreuse3"
  else if (s2$s2[i] %in% c(451:500)) col_s2[i] <- "deeppink4"
  else if (s2$s2[i] %in% c(501:550)) col_s2[i] <- "khaki"
  else if (s2$s2[i] %in% c(551:600)) col_s2[i] <- "midnightblue"
  else if (s2$s2[i] %in% c(601:650)) col_s2[i] <- "rosybrown1"
  else if (s2$s2[i] %in% c(651:700)) col_s2[i] <- "plum"
  else if (s2$s2[i] %in% c(701:750)) col_s2[i] <- "slategray4"
  else if (s2$s2[i] =="TSP") col_s2[i] <- "yellowgreen"
  else if (s2$s2[i] =="IN") col_s2[i] <- "red"
  else if (substr(s2$s2[i],1,1) =="D") col_s2[i] <- "snow"
}
for(i in 1:length(col_s3)){
  if(s3$s3[i] %in% c(1:50)) col_s3[i] <- "pink"
  else if (s3$s3[i] %in% c(51:100)) col_s3[i] <- "coral4"
  else if (s3$s3[i] %in% c(101:150)) col_s3[i] <- "chocolate2"
  else if (s3$s3[i] %in% c(151:200)) col_s3[i] <- "aquamarine3"
  else if (s3$s3[i] %in% c(201:250)) col_s3[i] <- "deepskyblue4"
  else if (s3$s3[i] %in% c(251:300)) col_s3[i] <- "blueviolet"
  else if (s3$s3[i] %in% c(301:350)) col_s3[i] <- "brown4"
  else if (s3$s3[i] %in% c(351:400)) col_s3[i] <- "darkgoldenrod3"
  else if (s3$s3[i] %in% c(401:450)) col_s3[i] <- "chartreuse3"
  else if (s3$s3[i] %in% c(451:500)) col_s3[i] <- "deeppink4"
  else if (s3$s3[i] %in% c(501:550)) col_s3[i] <- "khaki"
  else if (s3$s3[i] %in% c(551:600)) col_s3[i] <- "midnightblue"
  else if (s3$s3[i] %in% c(601:650)) col_s3[i] <- "rosybrown1"
  else if (s3$s3[i] %in% c(651:700)) col_s3[i] <- "plum"
  else if (s3$s3[i] %in% c(701:750)) col_s3[i] <- "slategray4"
  else if (s3$s3[i] =="TSP") col_s3[i] <- "yellowgreen"
  else if (s3$s3[i] =="IN") col_s3[i] <- "red"
  else if (substr(s3$s3[i],1,1) =="D") col_s3[i] <- "snow"
}
for(i in 1:length(col_s4)){
  if(s4$s4[i] %in% c(1:50)) col_s4[i] <- "pink"
  else if (s4$s4[i] %in% c(51:100)) col_s4[i] <- "coral4"
  else if (s4$s4[i] %in% c(101:150)) col_s4[i] <- "chocolate2"
  else if (s4$s4[i] %in% c(151:200)) col_s4[i] <- "aquamarine3"
  else if (s4$s4[i] %in% c(201:250)) col_s4[i] <- "deepskyblue4"
  else if (s4$s4[i] %in% c(251:300)) col_s4[i] <- "blueviolet"
  else if (s4$s4[i] %in% c(301:350)) col_s4[i] <- "brown4"
  else if (s4$s4[i] %in% c(351:400)) col_s4[i] <- "darkgoldenrod3"
  else if (s4$s4[i] %in% c(401:450)) col_s4[i] <- "chartreuse3"
  else if (s4$s4[i] %in% c(451:500)) col_s4[i] <- "deeppink4"
  else if (s4$s4[i] %in% c(501:550)) col_s4[i] <- "khaki"
  else if (s4$s4[i] %in% c(551:600)) col_s4[i] <- "midnightblue"
  else if (s4$s4[i] %in% c(601:650)) col_s4[i] <- "rosybrown1"
  else if (s4$s4[i] %in% c(651:700)) col_s4[i] <- "plum"
  else if (s4$s4[i] %in% c(701:750)) col_s4[i] <- "slategray4"
  else if (s4$s4[i] =="TSP") col_s4[i] <- "yellowgreen"
  else if (s4$s4[i] =="IN") col_s4[i] <- "red"
  else if (substr(s4$s4[i],1,1) =="D") col_s4[i] <- "snow"
}
for(i in 1:length(col_s5)){
  if(s5$s5[i] %in% c(1:50)) col_s5[i] <- "pink"
  else if (s5$s5[i] %in% c(51:100)) col_s5[i] <- "coral4"
  else if (s5$s5[i] %in% c(101:150)) col_s5[i] <- "chocolate2"
  else if (s5$s5[i] %in% c(151:200)) col_s5[i] <- "aquamarine3"
  else if (s5$s5[i] %in% c(201:250)) col_s5[i] <- "deepskyblue4"
  else if (s5$s5[i] %in% c(251:300)) col_s5[i] <- "blueviolet"
  else if (s5$s5[i] %in% c(301:350)) col_s5[i] <- "brown4"
  else if (s5$s5[i] %in% c(351:400)) col_s5[i] <- "darkgoldenrod3"
  else if (s5$s5[i] %in% c(401:450)) col_s5[i] <- "chartreuse3"
  else if (s5$s5[i] %in% c(451:500)) col_s5[i] <- "deeppink4"
  else if (s5$s5[i] %in% c(501:550)) col_s5[i] <- "khaki"
  else if (s5$s5[i] %in% c(551:600)) col_s5[i] <- "midnightblue"
  else if (s5$s5[i] %in% c(601:650)) col_s5[i] <- "rosybrown1"
  else if (s5$s5[i] %in% c(651:700)) col_s5[i] <- "plum"
  else if (s5$s5[i] %in% c(701:750)) col_s5[i] <- "slategray4"
  else if (s5$s5[i] =="TSP") col_s5[i] <- "yellowgreen"
  else if (s5$s5[i] =="IN") col_s5[i] <- "red"
  else if (substr(s5$s5[i],1,1) =="D") col_s5[i] <- "snow"
}
for(i in 1:length(col_s6)){
  if(s6$s6[i] %in% c(1:50)) col_s6[i] <- "pink"
  else if (s6$s6[i] %in% c(51:100)) col_s6[i] <- "coral4"
  else if (s6$s6[i] %in% c(101:150)) col_s6[i] <- "chocolate2"
  else if (s6$s6[i] %in% c(151:200)) col_s6[i] <- "aquamarine3"
  else if (s6$s6[i] %in% c(201:250)) col_s6[i] <- "deepskyblue4"
  else if (s6$s6[i] %in% c(251:300)) col_s6[i] <- "blueviolet"
  else if (s6$s6[i] %in% c(301:350)) col_s6[i] <- "brown4"
  else if (s6$s6[i] %in% c(351:400)) col_s6[i] <- "darkgoldenrod3"
  else if (s6$s6[i] %in% c(401:450)) col_s6[i] <- "chartreuse3"
  else if (s6$s6[i] %in% c(451:500)) col_s6[i] <- "deeppink4"
  else if (s6$s6[i] %in% c(501:550)) col_s6[i] <- "khaki"
  else if (s6$s6[i] %in% c(551:600)) col_s6[i] <- "midnightblue"
  else if (s6$s6[i] %in% c(601:650)) col_s6[i] <- "rosybrown1"
  else if (s6$s6[i] %in% c(651:700)) col_s6[i] <- "plum"
  else if (s6$s6[i] %in% c(701:750)) col_s6[i] <- "slategray4"
  else if (s6$s6[i] =="TSP") col_s6[i] <- "yellowgreen"
  else if (s6$s6[i] =="IN") col_s6[i] <- "red"
  else if (substr(s6$s6[i],1,1) =="D") col_s6[i] <- "snow"
}
for(i in 1:length(col_s7)){
  if(s7$s7[i] %in% c(1:50)) col_s7[i] <- "pink"
  else if (s7$s7[i] %in% c(51:100)) col_s7[i] <- "coral4"
  else if (s7$s7[i] %in% c(101:150)) col_s7[i] <- "chocolate2"
  else if (s7$s7[i] %in% c(151:200)) col_s7[i] <- "aquamarine3"
  else if (s7$s7[i] %in% c(201:250)) col_s7[i] <- "deepskyblue4"
  else if (s7$s7[i] %in% c(251:300)) col_s7[i] <- "blueviolet"
  else if (s7$s7[i] %in% c(301:350)) col_s7[i] <- "brown4"
  else if (s7$s7[i] %in% c(351:400)) col_s7[i] <- "darkgoldenrod3"
  else if (s7$s7[i] %in% c(401:450)) col_s7[i] <- "chartreuse3"
  else if (s7$s7[i] %in% c(451:500)) col_s7[i] <- "deeppink4"
  else if (s7$s7[i] %in% c(501:550)) col_s7[i] <- "khaki"
  else if (s7$s7[i] %in% c(551:600)) col_s7[i] <- "midnightblue"
  else if (s7$s7[i] %in% c(601:650)) col_s7[i] <- "rosybrown1"
  else if (s7$s7[i] %in% c(651:700)) col_s7[i] <- "plum"
  else if (s7$s7[i] %in% c(701:750)) col_s7[i] <- "slategray4"
  else if (s7$s7[i] =="TSP") col_s7[i] <- "yellowgreen"
  else if (s7$s7[i] =="IN") col_s7[i] <- "red"
  else if (substr(s7$s7[i],1,1) =="D") col_s7[i] <- "snow"
}
for(i in 1:length(col_s8)){
  if(s8$s8[i] %in% c(1:50)) col_s8[i] <- "pink"
  else if (s8$s8[i] %in% c(51:100)) col_s8[i] <- "coral4"
  else if (s8$s8[i] %in% c(101:150)) col_s8[i] <- "chocolate2"
  else if (s8$s8[i] %in% c(151:200)) col_s8[i] <- "aquamarine3"
  else if (s8$s8[i] %in% c(201:250)) col_s8[i] <- "deepskyblue4"
  else if (s8$s8[i] %in% c(251:300)) col_s8[i] <- "blueviolet"
  else if (s8$s8[i] %in% c(301:350)) col_s8[i] <- "brown4"
  else if (s8$s8[i] %in% c(351:400)) col_s8[i] <- "darkgoldenrod3"
  else if (s8$s8[i] %in% c(401:450)) col_s8[i] <- "chartreuse3"
  else if (s8$s8[i] %in% c(451:500)) col_s8[i] <- "deeppink4"
  else if (s8$s8[i] %in% c(501:550)) col_s8[i] <- "khaki"
  else if (s8$s8[i] %in% c(551:600)) col_s8[i] <- "midnightblue"
  else if (s8$s8[i] %in% c(601:650)) col_s8[i] <- "rosybrown1"
  else if (s8$s8[i] %in% c(651:700)) col_s8[i] <- "plum"
  else if (s8$s8[i] %in% c(701:750)) col_s8[i] <- "slategray4"
  else if (s8$s8[i] =="TSP") col_s8[i] <- "yellowgreen"
  else if (s8$s8[i] =="IN") col_s8[i] <- "red"
  else if (substr(s8$s8[i],1,1) =="D") col_s8[i] <- "snow"
}
for(i in 1:length(col_s9)){
  if(s9$s9[i] %in% c(1:50)) col_s9[i] <- "pink"
  else if (s9$s9[i] %in% c(51:100)) col_s9[i] <- "coral4"
  else if (s9$s9[i] %in% c(101:150)) col_s9[i] <- "chocolate2"
  else if (s9$s9[i] %in% c(151:200)) col_s9[i] <- "aquamarine3"
  else if (s9$s9[i] %in% c(201:250)) col_s9[i] <- "deepskyblue4"
  else if (s9$s9[i] %in% c(251:300)) col_s9[i] <- "blueviolet"
  else if (s9$s9[i] %in% c(301:350)) col_s9[i] <- "brown4"
  else if (s9$s9[i] %in% c(351:400)) col_s9[i] <- "darkgoldenrod3"
  else if (s9$s9[i] %in% c(401:450)) col_s9[i] <- "chartreuse3"
  else if (s9$s9[i] %in% c(451:500)) col_s9[i] <- "deeppink4"
  else if (s9$s9[i] %in% c(501:550)) col_s9[i] <- "khaki"
  else if (s9$s9[i] %in% c(551:600)) col_s9[i] <- "midnightblue"
  else if (s9$s9[i] %in% c(601:650)) col_s9[i] <- "rosybrown1"
  else if (s9$s9[i] %in% c(651:700)) col_s9[i] <- "plum"
  else if (s9$s9[i] %in% c(701:750)) col_s9[i] <- "slategray4"
  else if (s9$s9[i] =="TSP") col_s9[i] <- "yellowgreen"
  else if (s9$s9[i] =="IN") col_s9[i] <- "red"
  else if (substr(s9$s9[i],1,1) =="D") col_s9[i] <- "snow"
}
for(i in 1:length(col_s10)){
  if(s10$s10[i] %in% c(1:50)) col_s10[i] <- "pink"
  else if (s10$s10[i] %in% c(51:100)) col_s10[i] <- "coral4"
  else if (s10$s10[i] %in% c(101:150)) col_s10[i] <- "chocolate2"
  else if (s10$s10[i] %in% c(151:200)) col_s10[i] <- "aquamarine3"
  else if (s10$s10[i] %in% c(201:250)) col_s10[i] <- "deepskyblue4"
  else if (s10$s10[i] %in% c(251:300)) col_s10[i] <- "blueviolet"
  else if (s10$s10[i] %in% c(301:350)) col_s10[i] <- "brown4"
  else if (s10$s10[i] %in% c(351:400)) col_s10[i] <- "darkgoldenrod3"
  else if (s10$s10[i] %in% c(401:450)) col_s10[i] <- "chartreuse3"
  else if (s10$s10[i] %in% c(451:500)) col_s10[i] <- "deeppink4"
  else if (s10$s10[i] %in% c(501:550)) col_s10[i] <- "khaki"
  else if (s10$s10[i] %in% c(551:600)) col_s10[i] <- "midnightblue"
  else if (s10$s10[i] %in% c(601:650)) col_s10[i] <- "rosybrown1"
  else if (s10$s10[i] %in% c(651:700)) col_s10[i] <- "plum"
  else if (s10$s10[i] %in% c(701:750)) col_s10[i] <- "slategray4"
  else if (s10$s10[i] =="TSP") col_s10[i] <- "yellowgreen"
  else if (s10$s10[i] =="IN") col_s10[i] <- "red"
  else if (substr(s10$s10[i],1,1) =="D") col_s10[i] <- "snow"
}
for(i in 1:length(col_s11)){
  if(s11$s11[i] %in% c(1:50)) col_s11[i] <- "pink"
  else if (s11$s11[i] %in% c(51:100)) col_s11[i] <- "coral4"
  else if (s11$s11[i] %in% c(101:150)) col_s11[i] <- "chocolate2"
  else if (s11$s11[i] %in% c(151:200)) col_s11[i] <- "aquamarine3"
  else if (s11$s11[i] %in% c(201:250)) col_s11[i] <- "deepskyblue4"
  else if (s11$s11[i] %in% c(251:300)) col_s11[i] <- "blueviolet"
  else if (s11$s11[i] %in% c(301:350)) col_s11[i] <- "brown4"
  else if (s11$s11[i] %in% c(351:400)) col_s11[i] <- "darkgoldenrod3"
  else if (s11$s11[i] %in% c(401:450)) col_s11[i] <- "chartreuse3"
  else if (s11$s11[i] %in% c(451:500)) col_s11[i] <- "deeppink4"
  else if (s11$s11[i] %in% c(501:550)) col_s11[i] <- "khaki"
  else if (s11$s11[i] %in% c(551:600)) col_s11[i] <- "midnightblue"
  else if (s11$s11[i] %in% c(601:650)) col_s11[i] <- "rosybrown1"
  else if (s11$s11[i] %in% c(651:700)) col_s11[i] <- "plum"
  else if (s11$s11[i] %in% c(701:750)) col_s11[i] <- "slategray4"
  else if (s11$s11[i] =="TSP") col_s11[i] <- "yellowgreen"
  else if (s11$s11[i] =="IN") col_s11[i] <- "red"
  else if (substr(s11$s11[i],1,1) =="D") col_s11[i] <- "snow"
}
for(i in 1:length(col_s12)){
  if(s12$s12[i] %in% c(1:50)) col_s12[i] <- "pink"
  else if (s12$s12[i] %in% c(51:100)) col_s12[i] <- "coral4"
  else if (s12$s12[i] %in% c(101:150)) col_s12[i] <- "chocolate2"
  else if (s12$s12[i] %in% c(151:200)) col_s12[i] <- "aquamarine3"
  else if (s12$s12[i] %in% c(201:250)) col_s12[i] <- "deepskyblue4"
  else if (s12$s12[i] %in% c(251:300)) col_s12[i] <- "blueviolet"
  else if (s12$s12[i] %in% c(301:350)) col_s12[i] <- "brown4"
  else if (s12$s12[i] %in% c(351:400)) col_s12[i] <- "darkgoldenrod3"
  else if (s12$s12[i] %in% c(401:450)) col_s12[i] <- "chartreuse3"
  else if (s12$s12[i] %in% c(451:500)) col_s12[i] <- "deeppink4"
  else if (s12$s12[i] %in% c(501:550)) col_s12[i] <- "khaki"
  else if (s12$s12[i] %in% c(551:600)) col_s12[i] <- "midnightblue"
  else if (s12$s12[i] %in% c(601:650)) col_s12[i] <- "rosybrown1"
  else if (s12$s12[i] %in% c(651:700)) col_s12[i] <- "plum"
  else if (s12$s12[i] %in% c(701:750)) col_s12[i] <- "slategray4"
  else if (s12$s12[i] =="TSP") col_s12[i] <- "yellowgreen"
  else if (s12$s12[i] =="IN") col_s12[i] <- "red"
  else if (substr(s12$s12[i],1,1) =="D") col_s12[i] <- "snow"
}
for(i in 1:length(col_s13)){
  if(s13$s13[i] %in% c(1:50)) col_s13[i] <- "pink"
  else if (s13$s13[i] %in% c(51:100)) col_s13[i] <- "coral4"
  else if (s13$s13[i] %in% c(101:150)) col_s13[i] <- "chocolate2"
  else if (s13$s13[i] %in% c(151:200)) col_s13[i] <- "aquamarine3"
  else if (s13$s13[i] %in% c(201:250)) col_s13[i] <- "deepskyblue4"
  else if (s13$s13[i] %in% c(251:300)) col_s13[i] <- "blueviolet"
  else if (s13$s13[i] %in% c(301:350)) col_s13[i] <- "brown4"
  else if (s13$s13[i] %in% c(351:400)) col_s13[i] <- "darkgoldenrod3"
  else if (s13$s13[i] %in% c(401:450)) col_s13[i] <- "chartreuse3"
  else if (s13$s13[i] %in% c(451:500)) col_s13[i] <- "deeppink4"
  else if (s13$s13[i] %in% c(501:550)) col_s13[i] <- "khaki"
  else if (s13$s13[i] %in% c(551:600)) col_s13[i] <- "midnightblue"
  else if (s13$s13[i] %in% c(601:650)) col_s13[i] <- "rosybrown1"
  else if (s13$s13[i] %in% c(651:700)) col_s13[i] <- "plum"
  else if (s13$s13[i] %in% c(701:750)) col_s13[i] <- "slategray4"
  else if (s13$s13[i] =="TSP") col_s13[i] <- "yellowgreen"
  else if (s13$s13[i] =="IN") col_s13[i] <- "red"
  else if (substr(s13$s13[i],1,1) =="D") col_s13[i] <- "snow"
}
for(i in 1:length(col_s14)){
  if(s14$s14[i] %in% c(1:50)) col_s14[i] <- "pink"
  else if (s14$s14[i] %in% c(51:100)) col_s14[i] <- "coral4"
  else if (s14$s14[i] %in% c(101:150)) col_s14[i] <- "chocolate2"
  else if (s14$s14[i] %in% c(151:200)) col_s14[i] <- "aquamarine3"
  else if (s14$s14[i] %in% c(201:250)) col_s14[i] <- "deepskyblue4"
  else if (s14$s14[i] %in% c(251:300)) col_s14[i] <- "blueviolet"
  else if (s14$s14[i] %in% c(301:350)) col_s14[i] <- "brown4"
  else if (s14$s14[i] %in% c(351:400)) col_s14[i] <- "darkgoldenrod3"
  else if (s14$s14[i] %in% c(401:450)) col_s14[i] <- "chartreuse3"
  else if (s14$s14[i] %in% c(451:500)) col_s14[i] <- "deeppink4"
  else if (s14$s14[i] %in% c(501:550)) col_s14[i] <- "khaki"
  else if (s14$s14[i] %in% c(551:600)) col_s14[i] <- "midnightblue"
  else if (s14$s14[i] %in% c(601:650)) col_s14[i] <- "rosybrown1"
  else if (s14$s14[i] %in% c(651:700)) col_s14[i] <- "plum"
  else if (s14$s14[i] %in% c(701:750)) col_s14[i] <- "slategray4"
  else if (s14$s14[i] =="TSP") col_s14[i] <- "yellowgreen"
  else if (s14$s14[i] =="IN") col_s14[i] <- "red"
  else if (substr(s14$s14[i],1,1) =="D") col_s14[i] <- "snow"
}
for(i in 1:length(col_s15)){
  if(s15$s15[i] %in% c(1:50)) col_s15[i] <- "pink"
  else if (s15$s15[i] %in% c(51:100)) col_s15[i] <- "coral4"
  else if (s15$s15[i] %in% c(101:150)) col_s15[i] <- "chocolate2"
  else if (s15$s15[i] %in% c(151:200)) col_s15[i] <- "aquamarine3"
  else if (s15$s15[i] %in% c(201:250)) col_s15[i] <- "deepskyblue4"
  else if (s15$s15[i] %in% c(251:300)) col_s15[i] <- "blueviolet"
  else if (s15$s15[i] %in% c(301:350)) col_s15[i] <- "brown4"
  else if (s15$s15[i] %in% c(351:400)) col_s15[i] <- "darkgoldenrod3"
  else if (s15$s15[i] %in% c(401:450)) col_s15[i] <- "chartreuse3"
  else if (s15$s15[i] %in% c(451:500)) col_s15[i] <- "deeppink4"
  else if (s15$s15[i] %in% c(501:550)) col_s15[i] <- "khaki"
  else if (s15$s15[i] %in% c(551:600)) col_s15[i] <- "midnightblue"
  else if (s15$s15[i] %in% c(601:650)) col_s15[i] <- "rosybrown1"
  else if (s15$s15[i] %in% c(651:700)) col_s15[i] <- "plum"
  else if (s15$s15[i] %in% c(701:750)) col_s15[i] <- "slategray4"
  else if (s15$s15[i] =="TSP") col_s15[i] <- "yellowgreen"
  else if (s15$s15[i] =="IN") col_s15[i] <- "red"
  else if (substr(s15$s15[i],1,1) =="D") col_s15[i] <- "snow"
}
for(i in 1:length(col_s16)){
  if(s16$s16[i] %in% c(1:50)) col_s16[i] <- "pink"
  else if (s16$s16[i] %in% c(51:100)) col_s16[i] <- "coral4"
  else if (s16$s16[i] %in% c(101:150)) col_s16[i] <- "chocolate2"
  else if (s16$s16[i] %in% c(151:200)) col_s16[i] <- "aquamarine3"
  else if (s16$s16[i] %in% c(201:250)) col_s16[i] <- "deepskyblue4"
  else if (s16$s16[i] %in% c(251:300)) col_s16[i] <- "blueviolet"
  else if (s16$s16[i] %in% c(301:350)) col_s16[i] <- "brown4"
  else if (s16$s16[i] %in% c(351:400)) col_s16[i] <- "darkgoldenrod3"
  else if (s16$s16[i] %in% c(401:450)) col_s16[i] <- "chartreuse3"
  else if (s16$s16[i] %in% c(451:500)) col_s16[i] <- "deeppink4"
  else if (s16$s16[i] %in% c(501:550)) col_s16[i] <- "khaki"
  else if (s16$s16[i] %in% c(551:600)) col_s16[i] <- "midnightblue"
  else if (s16$s16[i] %in% c(601:650)) col_s16[i] <- "rosybrown1"
  else if (s16$s16[i] %in% c(651:700)) col_s16[i] <- "plum"
  else if (s16$s16[i] %in% c(701:750)) col_s16[i] <- "slategray4"
  else if (s16$s16[i] =="TSP") col_s16[i] <- "yellowgreen"
  else if (s16$s16[i] =="IN") col_s16[i] <- "red"
  else if (substr(s16$s16[i],1,1) =="D") col_s16[i] <- "snow"
}
for(i in 1:length(col_s17)){
  if(s17$s17[i] %in% c(1:50)) col_s17[i] <- "pink"
  else if (s17$s17[i] %in% c(51:100)) col_s17[i] <- "coral4"
  else if (s17$s17[i] %in% c(101:150)) col_s17[i] <- "chocolate2"
  else if (s17$s17[i] %in% c(151:200)) col_s17[i] <- "aquamarine3"
  else if (s17$s17[i] %in% c(201:250)) col_s17[i] <- "deepskyblue4"
  else if (s17$s17[i] %in% c(251:300)) col_s17[i] <- "blueviolet"
  else if (s17$s17[i] %in% c(301:350)) col_s17[i] <- "brown4"
  else if (s17$s17[i] %in% c(351:400)) col_s17[i] <- "darkgoldenrod3"
  else if (s17$s17[i] %in% c(401:450)) col_s17[i] <- "chartreuse3"
  else if (s17$s17[i] %in% c(451:500)) col_s17[i] <- "deeppink4"
  else if (s17$s17[i] %in% c(501:550)) col_s17[i] <- "khaki"
  else if (s17$s17[i] %in% c(551:600)) col_s17[i] <- "midnightblue"
  else if (s17$s17[i] %in% c(601:650)) col_s17[i] <- "rosybrown1"
  else if (s17$s17[i] %in% c(651:700)) col_s17[i] <- "plum"
  else if (s17$s17[i] %in% c(701:750)) col_s17[i] <- "slategray4"
  else if (s17$s17[i] =="TSP") col_s17[i] <- "yellowgreen"
  else if (s17$s17[i] =="IN") col_s17[i] <- "red"
  else if (substr(s17$s17[i],1,1) =="D") col_s17[i] <- "snow"
}
for(i in 1:length(col_s18)){
  if(s18$s18[i] %in% c(1:50)) col_s18[i] <- "pink"
  else if (s18$s18[i] %in% c(51:100)) col_s18[i] <- "coral4"
  else if (s18$s18[i] %in% c(101:150)) col_s18[i] <- "chocolate2"
  else if (s18$s18[i] %in% c(151:200)) col_s18[i] <- "aquamarine3"
  else if (s18$s18[i] %in% c(201:250)) col_s18[i] <- "deepskyblue4"
  else if (s18$s18[i] %in% c(251:300)) col_s18[i] <- "blueviolet"
  else if (s18$s18[i] %in% c(301:350)) col_s18[i] <- "brown4"
  else if (s18$s18[i] %in% c(351:400)) col_s18[i] <- "darkgoldenrod3"
  else if (s18$s18[i] %in% c(401:450)) col_s18[i] <- "chartreuse3"
  else if (s18$s18[i] %in% c(451:500)) col_s18[i] <- "deeppink4"
  else if (s18$s18[i] %in% c(501:550)) col_s18[i] <- "khaki"
  else if (s18$s18[i] %in% c(551:600)) col_s18[i] <- "midnightblue"
  else if (s18$s18[i] %in% c(601:650)) col_s18[i] <- "rosybrown1"
  else if (s18$s18[i] %in% c(651:700)) col_s18[i] <- "plum"
  else if (s18$s18[i] %in% c(701:750)) col_s18[i] <- "slategray4"
  else if (s18$s18[i] =="TSP") col_s18[i] <- "yellowgreen"
  else if (s18$s18[i] =="IN") col_s18[i] <- "red"
  else if (substr(s18$s18[i],1,1) =="D") col_s18[i] <- "snow"
}
for(i in 1:length(col_s19)){
  if(s19$s19[i] %in% c(1:50)) col_s19[i] <- "pink"
  else if (s19$s19[i] %in% c(51:100)) col_s19[i] <- "coral4"
  else if (s19$s19[i] %in% c(101:150)) col_s19[i] <- "chocolate2"
  else if (s19$s19[i] %in% c(151:200)) col_s19[i] <- "aquamarine3"
  else if (s19$s19[i] %in% c(201:250)) col_s19[i] <- "deepskyblue4"
  else if (s19$s19[i] %in% c(251:300)) col_s19[i] <- "blueviolet"
  else if (s19$s19[i] %in% c(301:350)) col_s19[i] <- "brown4"
  else if (s19$s19[i] %in% c(351:400)) col_s19[i] <- "darkgoldenrod3"
  else if (s19$s19[i] %in% c(401:450)) col_s19[i] <- "chartreuse3"
  else if (s19$s19[i] %in% c(451:500)) col_s19[i] <- "deeppink4"
  else if (s19$s19[i] %in% c(501:550)) col_s19[i] <- "khaki"
  else if (s19$s19[i] %in% c(551:600)) col_s19[i] <- "midnightblue"
  else if (s19$s19[i] %in% c(601:650)) col_s19[i] <- "rosybrown1"
  else if (s19$s19[i] %in% c(651:700)) col_s19[i] <- "plum"
  else if (s19$s19[i] %in% c(701:750)) col_s19[i] <- "slategray4"
  else if (s19$s19[i] =="TSP") col_s19[i] <- "yellowgreen"
  else if (s19$s19[i] =="IN") col_s19[i] <- "red"
  else if (substr(s19$s19[i],1,1) =="D") col_s19[i] <- "snow"
}
for(i in 1:length(col_s20)){
  if(s20$s20[i] %in% c(1:50)) col_s20[i] <- "pink"
  else if (s20$s20[i] %in% c(51:100)) col_s20[i] <- "coral4"
  else if (s20$s20[i] %in% c(101:150)) col_s20[i] <- "chocolate2"
  else if (s20$s20[i] %in% c(151:200)) col_s20[i] <- "aquamarine3"
  else if (s20$s20[i] %in% c(201:250)) col_s20[i] <- "deepskyblue4"
  else if (s20$s20[i] %in% c(251:300)) col_s20[i] <- "blueviolet"
  else if (s20$s20[i] %in% c(301:350)) col_s20[i] <- "brown4"
  else if (s20$s20[i] %in% c(351:400)) col_s20[i] <- "darkgoldenrod3"
  else if (s20$s20[i] %in% c(401:450)) col_s20[i] <- "chartreuse3"
  else if (s20$s20[i] %in% c(451:500)) col_s20[i] <- "deeppink4"
  else if (s20$s20[i] %in% c(501:550)) col_s20[i] <- "khaki"
  else if (s20$s20[i] %in% c(551:600)) col_s20[i] <- "midnightblue"
  else if (s20$s20[i] %in% c(601:650)) col_s20[i] <- "rosybrown1"
  else if (s20$s20[i] %in% c(651:700)) col_s20[i] <- "plum"
  else if (s20$s20[i] %in% c(701:750)) col_s20[i] <- "slategray4"
  else if (s20$s20[i] =="TSP") col_s20[i] <- "yellowgreen"
  else if (s20$s20[i] =="IN") col_s20[i] <- "red"
  else if (substr(s20$s20[i],1,1) =="D") col_s20[i] <- "snow"
}
for(i in 1:length(col_s21)){
  if(s21$s21[i] %in% c(1:50)) col_s21[i] <- "pink"
  else if (s21$s21[i] %in% c(51:100)) col_s21[i] <- "coral4"
  else if (s21$s21[i] %in% c(101:150)) col_s21[i] <- "chocolate2"
  else if (s21$s21[i] %in% c(151:200)) col_s21[i] <- "aquamarine3"
  else if (s21$s21[i] %in% c(201:250)) col_s21[i] <- "deepskyblue4"
  else if (s21$s21[i] %in% c(251:300)) col_s21[i] <- "blueviolet"
  else if (s21$s21[i] %in% c(301:350)) col_s21[i] <- "brown4"
  else if (s21$s21[i] %in% c(351:400)) col_s21[i] <- "darkgoldenrod3"
  else if (s21$s21[i] %in% c(401:450)) col_s21[i] <- "chartreuse3"
  else if (s21$s21[i] %in% c(451:500)) col_s21[i] <- "deeppink4"
  else if (s21$s21[i] %in% c(501:550)) col_s21[i] <- "khaki"
  else if (s21$s21[i] %in% c(551:600)) col_s21[i] <- "midnightblue"
  else if (s21$s21[i] %in% c(601:650)) col_s21[i] <- "rosybrown1"
  else if (s21$s21[i] %in% c(651:700)) col_s21[i] <- "plum"
  else if (s21$s21[i] %in% c(701:750)) col_s21[i] <- "slategray4"
  else if (s21$s21[i] =="TSP") col_s21[i] <- "yellowgreen"
  else if (s21$s21[i] =="IN") col_s21[i] <- "red"
  else if (substr(s21$s21[i],1,1) =="D") col_s21[i] <- "snow"
}
for(i in 1:length(col_s22)){
  if(s22$s22[i] %in% c(1:50)) col_s22[i] <- "pink"
  else if (s22$s22[i] %in% c(51:100)) col_s22[i] <- "coral4"
  else if (s22$s22[i] %in% c(101:150)) col_s22[i] <- "chocolate2"
  else if (s22$s22[i] %in% c(151:200)) col_s22[i] <- "aquamarine3"
  else if (s22$s22[i] %in% c(201:250)) col_s22[i] <- "deepskyblue4"
  else if (s22$s22[i] %in% c(251:300)) col_s22[i] <- "blueviolet"
  else if (s22$s22[i] %in% c(301:350)) col_s22[i] <- "brown4"
  else if (s22$s22[i] %in% c(351:400)) col_s22[i] <- "darkgoldenrod3"
  else if (s22$s22[i] %in% c(401:450)) col_s22[i] <- "chartreuse3"
  else if (s22$s22[i] %in% c(451:500)) col_s22[i] <- "deeppink4"
  else if (s22$s22[i] %in% c(501:550)) col_s22[i] <- "khaki"
  else if (s22$s22[i] %in% c(551:600)) col_s22[i] <- "midnightblue"
  else if (s22$s22[i] %in% c(601:650)) col_s22[i] <- "rosybrown1"
  else if (s22$s22[i] %in% c(651:700)) col_s22[i] <- "plum"
  else if (s22$s22[i] %in% c(701:750)) col_s22[i] <- "slategray4"
  else if (s22$s22[i] =="TSP") col_s22[i] <- "yellowgreen"
  else if (s22$s22[i] =="IN") col_s22[i] <- "red"
  else if (substr(s22$s22[i],1,1) =="D") col_s22[i] <- "snow"
}
for(i in 1:length(col_s23)){
  if(s23$s23[i] %in% c(1:50)) col_s23[i] <- "pink"
  else if (s23$s23[i] %in% c(51:100)) col_s23[i] <- "coral4"
  else if (s23$s23[i] %in% c(101:150)) col_s23[i] <- "chocolate2"
  else if (s23$s23[i] %in% c(151:200)) col_s23[i] <- "aquamarine3"
  else if (s23$s23[i] %in% c(201:250)) col_s23[i] <- "deepskyblue4"
  else if (s23$s23[i] %in% c(251:300)) col_s23[i] <- "blueviolet"
  else if (s23$s23[i] %in% c(301:350)) col_s23[i] <- "brown4"
  else if (s23$s23[i] %in% c(351:400)) col_s23[i] <- "darkgoldenrod3"
  else if (s23$s23[i] %in% c(401:450)) col_s23[i] <- "chartreuse3"
  else if (s23$s23[i] %in% c(451:500)) col_s23[i] <- "deeppink4"
  else if (s23$s23[i] %in% c(501:550)) col_s23[i] <- "khaki"
  else if (s23$s23[i] %in% c(551:600)) col_s23[i] <- "midnightblue"
  else if (s23$s23[i] %in% c(601:650)) col_s23[i] <- "rosybrown1"
  else if (s23$s23[i] %in% c(651:700)) col_s23[i] <- "plum"
  else if (s23$s23[i] %in% c(701:750)) col_s23[i] <- "slategray4"
  else if (s23$s23[i] =="TSP") col_s23[i] <- "yellowgreen"
  else if (s23$s23[i] =="IN") col_s23[i] <- "red"
  else if (substr(s23$s23[i],1,1) =="D") col_s23[i] <- "snow"
}
for(i in 1:length(col_s24)){
  if(s24$s24[i] %in% c(1:50)) col_s24[i] <- "pink"
  else if (s24$s24[i] %in% c(51:100)) col_s24[i] <- "coral4"
  else if (s24$s24[i] %in% c(101:150)) col_s24[i] <- "chocolate2"
  else if (s24$s24[i] %in% c(151:200)) col_s24[i] <- "aquamarine3"
  else if (s24$s24[i] %in% c(201:250)) col_s24[i] <- "deepskyblue4"
  else if (s24$s24[i] %in% c(251:300)) col_s24[i] <- "blueviolet"
  else if (s24$s24[i] %in% c(301:350)) col_s24[i] <- "brown4"
  else if (s24$s24[i] %in% c(351:400)) col_s24[i] <- "darkgoldenrod3"
  else if (s24$s24[i] %in% c(401:450)) col_s24[i] <- "chartreuse3"
  else if (s24$s24[i] %in% c(451:500)) col_s24[i] <- "deeppink4"
  else if (s24$s24[i] %in% c(501:550)) col_s24[i] <- "khaki"
  else if (s24$s24[i] %in% c(551:600)) col_s24[i] <- "midnightblue"
  else if (s24$s24[i] %in% c(601:650)) col_s24[i] <- "rosybrown1"
  else if (s24$s24[i] %in% c(651:700)) col_s24[i] <- "plum"
  else if (s24$s24[i] %in% c(701:750)) col_s24[i] <- "slategray4"
  else if (s24$s24[i] =="TSP") col_s24[i] <- "yellowgreen"
  else if (s24$s24[i] =="IN") col_s24[i] <- "red"
  else if (substr(s24$s24[i],1,1) =="D") col_s24[i] <- "snow"
}
for(i in 1:length(col_s25)){
  if(s25$s25[i] %in% c(1:50)) col_s25[i] <- "pink"
  else if (s25$s25[i] %in% c(51:100)) col_s25[i] <- "coral4"
  else if (s25$s25[i] %in% c(101:150)) col_s25[i] <- "chocolate2"
  else if (s25$s25[i] %in% c(151:200)) col_s25[i] <- "aquamarine3"
  else if (s25$s25[i] %in% c(201:250)) col_s25[i] <- "deepskyblue4"
  else if (s25$s25[i] %in% c(251:300)) col_s25[i] <- "blueviolet"
  else if (s25$s25[i] %in% c(301:350)) col_s25[i] <- "brown4"
  else if (s25$s25[i] %in% c(351:400)) col_s25[i] <- "darkgoldenrod3"
  else if (s25$s25[i] %in% c(401:450)) col_s25[i] <- "chartreuse3"
  else if (s25$s25[i] %in% c(451:500)) col_s25[i] <- "deeppink4"
  else if (s25$s25[i] %in% c(501:550)) col_s25[i] <- "khaki"
  else if (s25$s25[i] %in% c(551:600)) col_s25[i] <- "midnightblue"
  else if (s25$s25[i] %in% c(601:650)) col_s25[i] <- "rosybrown1"
  else if (s25$s25[i] %in% c(651:700)) col_s25[i] <- "plum"
  else if (s25$s25[i] %in% c(701:750)) col_s25[i] <- "slategray4"
  else if (s25$s25[i] =="TSP") col_s25[i] <- "yellowgreen"
  else if (s25$s25[i] =="IN") col_s25[i] <- "red"
  else if (substr(s25$s25[i],1,1) =="D") col_s25[i] <- "snow"
}
strand_s1 <- rep(1,nrow(s1))
strand_s2 <- rep(1,nrow(s2))
strand_s3 <- rep(1,nrow(s3))
strand_s4 <- rep(1,nrow(s4))
strand_s5 <- rep(1,nrow(s5))
strand_s6 <- rep(1,nrow(s6))
strand_s7 <- rep(1,nrow(s7))
strand_s8 <- rep(1,nrow(s8))
strand_s9 <- rep(1,nrow(s9))
strand_s10 <- rep(1,nrow(s10))
strand_s11 <- rep(1,nrow(s11))
strand_s12 <- rep(1,nrow(s12))
strand_s13 <- rep(1,nrow(s13))
strand_s14 <- rep(1,nrow(s14))
strand_s15 <- rep(1,nrow(s15))
strand_s16 <- rep(1,nrow(s16))
strand_s17 <- rep(1,nrow(s17))
strand_s18 <- rep(1,nrow(s18))
strand_s19 <- rep(1,nrow(s19))
strand_s20 <- rep(1,nrow(s20))
strand_s21 <- rep(1,nrow(s21))
strand_s22 <- rep(1,nrow(s22))
strand_s23 <- rep(1,nrow(s23))
strand_s24 <- rep(1,nrow(s24))
strand_s25 <- rep(1,nrow(s25))
for(i in 1:length(strand_s1)){
  if(col_s1[i] == "cornflowerblue") strand_s1[i] <- -1 # repeat
  else if (col_s1[i] == "yellowgreen") strand_s1[i] <- -1 #TSP
  else if (col_s1[i] == "red") strand_s1[i] <- -1 #IN
}
for(i in 1:length(strand_s2)){
  if(col_s2[i] == "cornflowerblue") strand_s2[i] <- -1 # repeat
  else if (col_s2[i] == "yellowgreen") strand_s2[i] <- -1 #TSP
  else if (col_s2[i] == "red") strand_s2[i] <- -1 #IN
}
for(i in 1:length(strand_s3)){
  if(col_s3[i] == "cornflowerblue") strand_s3[i] <- -1 # repeat
  else if (col_s3[i] == "yellowgreen") strand_s3[i] <- -1 #TSP
  else if (col_s3[i] == "red") strand_s3[i] <- -1 #IN
}
for(i in 1:length(strand_s4)){
  if(col_s4[i] == "cornflowerblue") strand_s4[i] <- -1 # repeat
  else if (col_s4[i] == "yellowgreen") strand_s4[i] <- -1 #TSP
  else if (col_s4[i] == "red") strand_s4[i] <- -1 #IN
}
for(i in 1:length(strand_s5)){
  if(col_s5[i] == "cornflowerblue") strand_s5[i] <- -1 # repeat
  else if (col_s5[i] == "yellowgreen") strand_s5[i] <- -1 #TSP
  else if (col_s5[i] == "red") strand_s5[i] <- -1 #IN
}
for(i in 1:length(strand_s6)){
  if(col_s6[i] == "cornflowerblue") strand_s6[i] <- -1 # repeat
  else if (col_s6[i] == "yellowgreen") strand_s6[i] <- -1 #TSP
  else if (col_s6[i] == "red") strand_s6[i] <- -1 #IN
}
for(i in 1:length(strand_s7)){
  if(col_s7[i] == "cornflowerblue") strand_s7[i] <- -1 # repeat
  else if (col_s7[i] == "yellowgreen") strand_s7[i] <- -1 #TSP
  else if (col_s7[i] == "red") strand_s7[i] <- -1 #IN
}
for(i in 1:length(strand_s8)){
  if(col_s8[i] == "cornflowerblue") strand_s8[i] <- -1 # repeat
  else if (col_s8[i] == "yellowgreen") strand_s8[i] <- -1 #TSP
  else if (col_s8[i] == "red") strand_s8[i] <- -1 #IN
}
for(i in 1:length(strand_s9)){
  if(col_s9[i] == "cornflowerblue") strand_s9[i] <- -1 # repeat
  else if (col_s9[i] == "yellowgreen") strand_s9[i] <- -1 #TSP
  else if (col_s9[i] == "red") strand_s9[i] <- -1 #IN
}
for(i in 1:length(strand_s10)){
  if(col_s10[i] == "cornflowerblue") strand_s10[i] <- -1 # repeat
  else if (col_s10[i] == "yellowgreen") strand_s10[i] <- -1 #TSP
  else if (col_s10[i] == "red") strand_s10[i] <- -1 #IN
}
for(i in 1:length(strand_s11)){
  if(col_s11[i] == "cornflowerblue") strand_s11[i] <- -1 # repeat
  else if (col_s11[i] == "yellowgreen") strand_s11[i] <- -1 #TSP
  else if (col_s11[i] == "red") strand_s11[i] <- -1 #IN
}
for(i in 1:length(strand_s12)){
  if(col_s12[i] == "cornflowerblue") strand_s12[i] <- -1 # repeat
  else if (col_s12[i] == "yellowgreen") strand_s12[i] <- -1 #TSP
  else if (col_s12[i] == "red") strand_s12[i] <- -1 #IN
}
for(i in 1:length(strand_s13)){
  if(col_s13[i] == "cornflowerblue") strand_s13[i] <- -1 # repeat
  else if (col_s13[i] == "yellowgreen") strand_s13[i] <- -1 #TSP
  else if (col_s13[i] == "red") strand_s13[i] <- -1 #IN
}
for(i in 1:length(strand_s14)){
  if(col_s14[i] == "cornflowerblue") strand_s14[i] <- -1 # repeat
  else if (col_s14[i] == "yellowgreen") strand_s14[i] <- -1 #TSP
  else if (col_s14[i] == "red") strand_s14[i] <- -1 #IN
}
for(i in 1:length(strand_s15)){
  if(col_s15[i] == "cornflowerblue") strand_s15[i] <- -1 # repeat
  else if (col_s15[i] == "yellowgreen") strand_s15[i] <- -1 #TSP
  else if (col_s15[i] == "red") strand_s15[i] <- -1 #IN
}
for(i in 1:length(strand_s16)){
  if(col_s16[i] == "cornflowerblue") strand_s16[i] <- -1 # repeat
  else if (col_s16[i] == "yellowgreen") strand_s16[i] <- -1 #TSP
  else if (col_s16[i] == "red") strand_s16[i] <- -1 #IN
}
for(i in 1:length(strand_s17)){
  if(col_s17[i] == "cornflowerblue") strand_s17[i] <- -1 # repeat
  else if (col_s17[i] == "yellowgreen") strand_s17[i] <- -1 #TSP
  else if (col_s17[i] == "red") strand_s17[i] <- -1 #IN
}
for(i in 1:length(strand_s18)){
  if(col_s18[i] == "cornflowerblue") strand_s18[i] <- -1 # repeat
  else if (col_s18[i] == "yellowgreen") strand_s18[i] <- -1 #TSP
  else if (col_s18[i] == "red") strand_s18[i] <- -1 #IN
}
for(i in 1:length(strand_s19)){
  if(col_s19[i] == "cornflowerblue") strand_s19[i] <- -1 # repeat
  else if (col_s19[i] == "yellowgreen") strand_s19[i] <- -1 #TSP
  else if (col_s19[i] == "red") strand_s19[i] <- -1 #IN
}
for(i in 1:length(strand_s20)){
  if(col_s20[i] == "cornflowerblue") strand_s20[i] <- -1 # repeat
  else if (col_s20[i] == "yellowgreen") strand_s20[i] <- -1 #TSP
  else if (col_s20[i] == "red") strand_s20[i] <- -1 #IN
}
for(i in 1:length(strand_s21)){
  if(col_s21[i] == "cornflowerblue") strand_s21[i] <- -1 # repeat
  else if (col_s21[i] == "yellowgreen") strand_s21[i] <- -1 #TSP
  else if (col_s21[i] == "red") strand_s21[i] <- -1 #IN
}
for(i in 1:length(strand_s22)){
  if(col_s22[i] == "cornflowerblue") strand_s22[i] <- -1 # repeat
  else if (col_s22[i] == "yellowgreen") strand_s22[i] <- -1 #TSP
  else if (col_s22[i] == "red") strand_s22[i] <- -1 #IN
}
for(i in 1:length(strand_s23)){
  if(col_s23[i] == "cornflowerblue") strand_s23[i] <- -1 # repeat
  else if (col_s23[i] == "yellowgreen") strand_s23[i] <- -1 #TSP
  else if (col_s23[i] == "red") strand_s23[i] <- -1 #IN
}
for(i in 1:length(strand_s24)){
  if(col_s24[i] == "cornflowerblue") strand_s24[i] <- -1 # repeat
  else if (col_s24[i] == "yellowgreen") strand_s24[i] <- -1 #TSP
  else if (col_s24[i] == "red") strand_s24[i] <- -1 #IN
}
for(i in 1:length(strand_s25)){
  if(col_s25[i] == "cornflowerblue") strand_s25[i] <- -1 # repeat
  else if (col_s25[i] == "yellowgreen") strand_s25[i] <- -1 #TSP
  else if (col_s25[i] == "red") strand_s25[i] <- -1 #IN
}

df_s1 <- data.frame(name=s1$s1,start=s1$X1_Loc,end=s1$X1_end,strand=strand_s1,col=col_s1)
dna_seg1 <- dna_seg(df_s1)
df_s2 <- data.frame(name=s2$s2,start=s2$X2_Loc,end=s2$X2_end,strand=strand_s2,col=col_s2)
dna_seg2 <- dna_seg(df_s2)
df_s3 <- data.frame(name=s3$s3, start=s3$X3_Loc,end=s3$X3_end,strand=strand_s3,col=col_s3)
dna_seg3 <- dna_seg(df_s3)
df_s4 <- data.frame(name=s4$s4,start=s4$X4_Loc,end=s4$X4_end, strand=strand_s4,col=col_s4)
dna_seg4 <- dna_seg(df_s4)
df_s5 <- data.frame(name=s5$s5,start=s5$X5_Loc,end=s5$X5_end, strand=strand_s5,col=col_s5)
dna_seg5 <- dna_seg(df_s5)
df_s6 <- data.frame(name=s6$s6,start=s6$X6_Loc,end=s6$X6_end, strand=strand_s6,col=col_s6)
dna_seg6 <- dna_seg(df_s6)
df_s7 <- data.frame(name=s7$s7,start=s7$X7_Loc,end=s7$X7_end, strand=strand_s7,col=col_s7)
dna_seg7 <- dna_seg(df_s7)
df_s8 <- data.frame(name=s8$s8,start=s8$X8_Loc,end=s8$X8_end, strand=strand_s8,col=col_s8)
dna_seg8 <- dna_seg(df_s8)
df_s9 <- data.frame(name=s9$s9,start=s9$X9_Loc,end=s9$X9_end, strand=strand_s9,col=col_s9)
dna_seg9 <- dna_seg(df_s9)
df_s10 <- data.frame(name=s10$s10,start=s10$X10_Loc,end=s10$X10_end, strand=strand_s10,col=col_s10)
dna_seg10 <- dna_seg(df_s10)
df_s11 <- data.frame(name=s11$s11,start=s11$X11_Loc,end=s11$X11_end, strand=strand_s11,col=col_s11)
dna_seg11 <- dna_seg(df_s11)
df_s12 <- data.frame(name=s12$s12,start=s12$X12_Loc,end=s12$X12_end, strand=strand_s12,col=col_s12)
dna_seg12 <- dna_seg(df_s12)
df_s13 <- data.frame(name=s13$s13,start=s13$X13_Loc,end=s13$X13_end, strand=strand_s13,col=col_s13)
dna_seg13 <- dna_seg(df_s13)
df_s14 <- data.frame(name=s14$s14,start=s14$X14_Loc,end=s14$X14_end, strand=strand_s14,col=col_s14)
dna_seg14 <- dna_seg(df_s14)
df_s15 <- data.frame(name=s15$s15,start=s15$X15_Loc,end=s15$X15_end, strand=strand_s15,col=col_s15)
dna_seg15 <- dna_seg(df_s15)
df_s16 <- data.frame(name=s16$s16,start=s16$X16_Loc,end=s16$X16_end, strand=strand_s16,col=col_s16)
dna_seg16 <- dna_seg(df_s16)
df_s17 <- data.frame(name=s17$s17,start=s17$X17_Loc,end=s17$X17_end, strand=strand_s17,col=col_s17)
dna_seg17 <- dna_seg(df_s17)
df_s18 <- data.frame(name=s18$s18,start=s18$X18_Loc,end=s18$X18_end, strand=strand_s18,col=col_s18)
dna_seg18 <- dna_seg(df_s18)
df_s19 <- data.frame(name=s19$s19,start=s19$X19_Loc,end=s19$X19_end, strand=strand_s19,col=col_s19)
dna_seg19 <- dna_seg(df_s19)
df_s20 <- data.frame(name=s20$s20,start=s20$X20_Loc,end=s20$X20_end, strand=strand_s20,col=col_s20)
dna_seg20 <- dna_seg(df_s20)
df_s21 <- data.frame(name=s21$s21,start=s21$X21_Loc,end=s21$X21_end, strand=strand_s21,col=col_s21)
dna_seg21 <- dna_seg(df_s21)
df_s22 <- data.frame(name=s22$s22,start=s22$X22_Loc,end=s22$X22_end, strand=strand_s22,col=col_s22)
dna_seg22 <- dna_seg(df_s22)
df_s23 <- data.frame(name=s23$s23,start=s23$X23_Loc,end=s23$X23_end, strand=strand_s23,col=col_s23)
dna_seg23 <- dna_seg(df_s23)
df_s24 <- data.frame(name=s24$s24,start=s24$X24_Loc,end=s24$X24_end, strand=strand_s24,col=col_s24)
dna_seg24 <- dna_seg(df_s24)
df_s25 <- data.frame(name=s25$s25,start=s25$X25_Loc,end=s25$X25_end, strand=strand_s25,col=col_s25)
dna_seg25 <- dna_seg(df_s25)

dna_segs <- list(dna_seg1, dna_seg2, dna_seg3,dna_seg4,dna_seg5, dna_seg6, dna_seg7,dna_seg8,
                 dna_seg9, dna_seg10, dna_seg11,dna_seg12,dna_seg13, dna_seg14, dna_seg15,dna_seg16,
                 dna_seg17, dna_seg18, dna_seg19,dna_seg20,dna_seg21, dna_seg22, dna_seg23,dna_seg24,dna_seg25)
names <- c("strain 1", "strain 2", "strain 3","strain 4","strain 5", "strain 6", "strain 7","strain 8",
           "strain 9", "strain 10", "strain 11","strain 12","strain 13", "strain 14", "strain 15","strain 16",
           "strain 17", "strain 18", "strain 19","strain 20","strain 21", "strain 22", "strain 23","strain 24","strain 25")
names(dna_segs) <- names
pdf("test.pdf",width = 10,height = 15)
genepraph<-plot_gene_map(dna_segs,arrow_head_len=0,gene_type = "side_blocks",annotations=NULL,dna_seg_label_cex=0.8,dna_seg_line=FALSE)
dev.off()
postscript("scaffold-vertical.eps", horizontal = FALSE, onefile = FALSE, height = 30, width = 20)
genepraph<-plot_gene_map(dna_segs,arrow_head_len=0,gene_type = "side_blocks",annotations=NULL,dna_seg_label_cex=0.8,dna_seg_line=FALSE)
dev.off()
#scale=FALSE

