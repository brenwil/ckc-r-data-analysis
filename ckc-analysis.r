# R code to go with study (currently unpublished): 
# Revising the Core Knowledge Confusions Scale: a measure of logical error influenced by cognitive and personality traits. 

library(readr)
library(Cronbach)
library("car")
library(corrplot)
library(psych)
library(relaimpo)
library(effectsize)
library(semPlot)
library(lavaan)
library(semTools)


D <- read_csv("./full.csv") # replace with csv file dataset (see publication for details) 

#attention check: if 4 = attention, if not = fail attention. 
D$CKC_attention <- ifelse(D$attention4 == 4, 1, 0)

######################################
# Cognitive Style: higher = more analytical.

unique(D[c("Q4.2")])
# correct answer 2nd.
D$crt1 <- as.numeric(ifelse(D$Q4.2 %in% c('2', '2nd','2ND', '2nd place',
                                          '2nd place unless I pass another person, and no one else has passed me',
                                          'Seccond', 'second', 'Second', 'SECOND', 'second place', 'second Place', 'Second place', 'Second Place', 'Second place.',
                                          'Second7', 'second8', 'Secondly', 'seconds place', 'Selcond place'), '1', '0'))

# correct answer: 8
unique(D[c("Q4.3")])
D$crt2 <- as.numeric(ifelse(D$Q4.3 %in% c('8', 'Eight', 'eight', '8 sheep','eight sheep'), '1', '0'))
hist(D$crt2)

# correct answer: Emily
unique(D[c("Q4.4")])
D$crt3 <- as.numeric(ifelse(D$Q4.4 %in% c('Emily', 'emily', 'Emy', 'emily,','emily,s', 'EMILY', 'Emily.', 'EMILYS', "Emily's", 'rmily'), '1', '0'))
hist(D$crt3)

# correct answer: 0
unique(D[c("Q4.5")])
D$crt4 <- as.numeric(ifelse(D$Q4.5 %in% c('none', 'None', '0', 'None.','no dirt in a hole', 'No dirt', 'NONE', 'none its a hole', 'No its a hole', 'none, its a hole so its empty', 'No dirt in a hole'
                                          , 'none, 27 cubic feet have been removed.', "None-it's an empty space"), '1', '0'))

hist(D$crt4)

############################################


# Calculations for WordSum Q11.2 to Q11.11 - higher = more correct answers, better word knowledge. 
describe(D$Q11.2)
hist(D$Q11.2)
D$Q11.2[is.na(D$Q11.2)] <- 0

which(colnames(D)=="Q11.11")
for (i in 191:200)
  D[,c(i)][is.na(D[,c(i)])]<-0


D$Q11.2 <-ifelse(D$Q11.2 == 1, 1, 0)
D$Q11.3 <-ifelse(D$Q11.3 == 1, 1, 0)
D$Q11.4 <-ifelse(D$Q11.4 == 1, 1, 0)
D$Q11.5 <-ifelse(D$Q11.5 == 1, 1, 0)
D$Q11.6 <-ifelse(D$Q11.6 == 1, 1, 0)
D$Q11.7 <-ifelse(D$Q11.7 == 1, 1, 0)
D$Q11.8 <-ifelse(D$Q11.8 == 1, 1, 0)
D$Q11.9 <-ifelse(D$Q11.9 == 1, 1, 0)
D$Q11.10 <-ifelse(D$Q11.10 == 1, 1, 0)
D$Q11.11<-ifelse(D$Q11.11 == 1, 1, 0)

# Calculate scales: 


################################################
# SCALE 1: ONTOLOGICAL CONFUSIONS: 1 = metaphorical, 5 = literal

D$CKC_short <- rowSums(D[,c("furnhome", "houseknow", "househistory", "forcetries", "forcelives", 
                            "plantsseasons", "flowerslight","earthwater", "planets", "skythunder", "plannature", "mindtouch", "fearpoison", 
                            "rocklives")])

cat("cronbach for CKC short form : ", cronbach(D[,c("furnhome", "houseknow", "househistory", "forcetries", "forcelives", 
                                                    "plantsseasons", "flowerslight","earthwater", "planets", "skythunder", "plannature", "mindtouch", "fearpoison", 
                                                    "rocklives")]))

omega(D[,c("furnhome", "houseknow", "househistory", "forcetries", "forcelives", 
                 "plantsseasons", "flowerslight","earthwater", "planets", "skythunder", "plannature", "mindtouch", "fearpoison", 
                 "rocklives")], fm='ml')


##################################
# Cognitive Style: higher = more analytical.

D$crt = rowSums(D[,c("crt1", "crt2", "crt3", "crt4")])
cat("Cronbach for crt: ", cronbach(D[,c("crt1", "crt2", "crt3", "crt4")]))
#
#################################


#################################
# Anthropomorphism: IDAQ
#

D$IDAQ <- rowSums(D[,c("anthtechint", "anthfishfree", "anthmountainfree", "anthtvemot", "anthrobotcon", "antcowint", "anthcarfree", "anthoceancon", "anthcompmind", 
                       "anthcheetahemot", "anthenvemot", "anthinsectmind","anthtreemind", "anthwindint", "anthreptilecon")])
cat("Cronbach for IDAQ : ", cronbach(D[,c("anthtechint", "anthfishfree", "anthmountainfree", "anthtvemot", "anthrobotcon", "antcowint", "anthcarfree", "anthoceancon", "anthcompmind", 
                                          "anthcheetahemot", "anthenvemot", "anthinsectmind","anthtreemind", "anthwindint", "anthreptilecon")]))

D$IDAQ_NA <- rowSums(D[,c("anthdesertleth", "antcomputeractive", "anthcloudgood", "anthpetsuseful", "anthamphibianleth", "anthrobotgood", "anthcameraleth", "anthriveruseful"
                          ,"anthtreeactive", "anthkitchenuseful", "anthtechdurable", "anthcatactive", "anthforestdurable", "anthtortoisedurable", "anthdoggood")])
cat("Cronbach for IDAQ NA : ", cronbach(D[,c("anthdesertleth", "antcomputeractive", "anthcloudgood", "anthpetsuseful", "anthamphibianleth", "anthrobotgood", "anthcameraleth", "anthriveruseful"
                                             ,"anthtreeactive", "anthkitchenuseful", "anthtechdurable", "anthcatactive", "anthforestdurable", "anthtortoisedurable", "anthdoggood")]))


#
##################################

##################################
# ABSORPTION: higher = more absorption (MTAS)
#

D$ABSsynaesthesia <- rowSums(D[,c("abstextures", "abscolours","absmusicpictures","absodorcolours")])
cat("cronbach for ABS Synaesthesia : ", cronbach(D[,c("abstextures", "abscolours","absmusicpictures","absodorcolours")]))

D$ABSasc <- rowSums(D[, c("absmind", "absmystical", "absselfout","absexperience")])
cat("Cronbach for ABS Ascetics : ", cronbach(D[, c("absmind", "absmystical", "absselfout","absexperience")]))

D$ABSnature <-rowSums(D[, c("absclouds", "absflames", "absconsciousness", "absdelight", "abssunsetmove")])
cat("cronbach for ABS Nature : ", cronbach(D[, c("absclouds", "absflames", "absconsciousness", "absdelight", "abssunsetmove")]))

#3,7,18,19,20,21,22,25,32
D$ABSimagination <- rowSums(D[, c("absTV", "absdaydream", "abswander","abspast","absmeaningless","absbecome","absthoughts"
                                  ,"absnoisetomusic","absthoughtseasy")])
cat("Cronbach for ABS Imagination : ", cronbach(D[, c("absTV", "absdaydream", "abswander","abspast","absmeaningless","absbecome","absthoughts"
                                                      ,"absnoisetomusic","absthoughtseasy")]))

D$ABSesp <- rowSums(D[,c("abspresence", "absknow","abspresencenotthere")])
cat("Cronbach for ABS E.S.P : ", cronbach(D[,c("abspresence", "absknow","abspresencenotthere")]))

D$absorption <- D$ABSesp + D$ABSimagination + D$ABSnature + D$ABSasc + D$ABSsynaesthesia
cat("cronbach for total absorption scale : ", cronbach(D[,c("abstextures", "abscolours","absmusicpictures","absodorcolours","absmind", "absmystical", "absselfout","absexperience"
                                                            ,"absclouds", "absflames", "absconsciousness", "absdelight", "abssunsetmove",
                                                            "absTV", "absdaydream", "abswander","abspast","absmeaningless","absbecome","absthoughts","absnoisetomusic","absthoughtseasy",
                                                            "abspresence", "absknow","abspresencenotthere")]))



########################################
# BS Scale: DV = not scaled. Higher = more BS prone.

D$BS <- rowSums(D[,c("Q7.2","Q7.3","Q7.4","Q7.5","Q7.6","Q7.7","Q7.8")])
D$BSprofound <- rowSums(D[,c("Q7.9","Q7.10","Q7.11","Q7.12","Q7.13","Q7.14","Q7.15")])

# cronbach for BS : 
cat("cronbach for Bull Sh*t scale: ", cronbach(D[,c("Q7.2","Q7.3","Q7.4","Q7.5","Q7.6","Q7.7","Q7.8")]))

#cronbach for PROFOUND
cat("Cronbach for BS Profound : ", cronbach(D[,c("Q7.9","Q7.10","Q7.11","Q7.12","Q7.13","Q7.14","Q7.15")]))

#########################################


#########################################
# Paranormal and Supernatural: DV = not yet scaled. 
# reversed scored: 
D$rpbssixthsense = 5 - D$rpbssixthsense
D$rpbsfortune = 5 - D$rpbsfortune
D$rpbsspirits = 5-D$rpbsspirits

D$paranormal <- rowSums(D[,c("rpbssoul","rpbsmirror","rpbsreincarnate","rpbsmind",
                             "rpbsstar","rpbsfairies","rpbsfortune", "rpbssixthsense","rpbspsychic","rpbsshaman","rpbsdream","rpbsspirits","rpbsevil")])

cat("Cronbach for paranormal scale: ", cronbach( D[,c("rpbssoul","rpbsmirror","rpbsreincarnate","rpbsmind",
                                                      "rpbsstar","rpbsfairies","rpbsfortune", "rpbssixthsense","rpbspsychic","rpbsshaman","rpbsdream","rpbsspirits","rpbsevil")]))



#########################################
# WordSum Q11.2 to Q11.11 - higher = more correct answers, better word knowledge. 

D$wordsum <- rowSums(D[,c("Q11.2","Q11.3","Q11.4","Q11.5","Q11.6","Q11.7","Q11.8","Q11.9","Q11.10","Q11.11")])
cat("Cronbach for WordSum : ", cronbach(D[,c("Q11.2","Q11.3","Q11.4","Q11.5","Q11.6","Q11.7","Q11.8","Q11.9","Q11.10","Q11.11")]))


#########################################



# using S and will scale. 
S<- D[c(  
  "BS", "BSprofound", "CKC_short",
  "absorption", 
  "IDAQ", "wordsum", "crt", "paranormal")]
head(S)


#boxplot(D$CKC_literal)$out
describe(S)
write.csv(describe(S), "Vars-description.csv")

#attention checks: 
table(D$CKC_attention)
prop.table(table(D$CKC_attention))

#historgrams:
multi.hist(S, global=FALSE, bcol="blue")


cor.plot(S, stars=TRUE)
cor(S)
write.csv(cor(S),'item_correlations.csv')


# Scale and centre items: 

for (i in 1:ncol(S))
  S[,i] <- scale(S[,i], center = TRUE)


############ Functions are adapted from BROWNE et al. (2018)  

bc <- function(cn, idx, opt)
{
  nam <- paste(cn,idx, sep="")
  if (opt == 1)
    return(nam)
  lv <- paste(cn, " =~ ", paste(nam, collapse = " + "))
  if (opt == 2)
    return(lv)
  warning("Index must be 1 to 2")
}


# Set up the function for translating the definition into a lavaan model specification

def2lavmod <- function(def)
{
  paste(
    
    paste("CKCr =~ ", paste(def[], collapse = " + "), " \n "),
    "paranormal ~ CKCr")
}




reduce_scale2 <- function(testset)
{
  library(lavaan)
  for (i in 1:ncol(testset)) {
    cand_model <- def2lavmod(testset[,i])
    cand_fit<- sem(model = cand_model, data = D)
    cand_crit <- crit_fn(cand_fit)
    trace <- rbind(trace, data.frame(step = i, crit = cand_crit))
    assign('trace',trace,envir=.GlobalEnv)
    # trace
    cat("\nThis: step = ",i, "crit =", cand_crit, "\n")
  }
}


crit_fn <- function(fit)
{
  dv <- c("paranormal")
  lv <- c("CKCr")
  err <- inspect(fit,"rsquare")
  iv <- setdiff(names(err), c(dv,lv))
  
  cat("\n IV IS: ",iv, "\n")
  
  dv_sse <- sum((1 - err[dv])^2) # smaller is better
  cat("\ndv_sse : ", dv, " : ", dv_sse, "err: ", err[dv], "\n")
  cov_sse <- sum(residuals(fit)$cov^2)
  cat("residuals for base fit cov: ", sum(residuals(fit)$cov), "\n" )
  cat("cov_sse : ", cov_sse, "\n")
  iv_sse <- sum((1 - err[iv])^2) # smaller is better.
  cat("Ivsse is: ", iv_sse, " & err: ", err[iv], "\n")
  return(iv_sse + dv_sse + cov_sse)
}


crit_fn2 <- function(fit)
{
  dv <- c("paranormal")
  lv <- c("artanim", "forceanim", "inananim", "natliving", "lifelanim")
  err <- inspect(fit,"rsquare")
  iv <- setdiff(names(err), c(dv,lv))
  
  cat("\n IV IS: ",iv, "\n")
  
  dv_sse <- sum((1 - err[dv])^2) # smaller is better
  cat("\ndv_sse : ", dv, " : ", dv_sse, "err: ", err[dv], "\n")
  
  cov_sse <- sum(residuals(fit)$cov^2)
  cat("residuals for base fit cov: ", sum(residuals(fit)$cov), "\n" )
  cat("cov_sse : ", cov_sse, "\n")
  iv_sse <- sum((1 - err[iv])^2) # smaller is better.
  cat("Ivsse is: ", iv_sse, " & err: ", err[iv], "\n")
  return(iv_sse + dv_sse + cov_sse)
}


#################################### END FUNCTIONS


##################################
# Item checks:
##################################


ckcitems <- (D[,c("furnhome","houseknow","housemiss","housesense","househistory",
                   "forceact","forcetries","forcelives","forcedirection","forcehuman",
                   "plantsseasons","flowerslight","plantsface","mushrooms","flowersbees",
                   "rocklives","moonalive","stoneslive","starslive","waterlives",
                   "earthwater","planets","skythunder","seaembraces","skydarkens","sunmorning",
                   "plannature","mindtouch","fearpoison","miserycompany","sleepwelcomes","knowledgehands","knowledgelive")])
multi.hist(ckcitems)
describe(ckcitems)
write.csv(describe(ckcitems), 'ckc-item-descriptions.csv')
# correlation checks: (expect some correlation between variables)
write.csv(cor(ckcitems), 'ckc-item-corrleations.csv')
cor.plot(ckcitems, xlas='2', stars=TRUE)


KMO(r=cor(ckcitems)) # want > .60
check_sphericity_bartlett(ckcitems)
det(cor(na.omit(ckcitems)))


# Base model ==> Everything - full CKC item list (well... excluding metaphors and fillers). 
base_model <-
  "artanim =~ furnhome + houseknow + housemiss + housesense + househistory \n
forceanim =~ forceact + forcetries + forcelives + forcedirection + forcehuman \n
inananim =~ plantsseasons + flowerslight + plantsface + mushrooms + flowersbees \n
natliving =~ rocklives + moonalive + stoneslive + starslive + waterlives \n
lifelanim =~ earthwater + planets + skythunder + seaembraces + skydarkens + sunmorning \n
mentalmat =~ plannature + mindtouch + fearpoison + miserycompany + sleepwelcomes + knowledgehands + knowledgelive \n
artanim ~~ 0*forceanim + 0*inananim + 0*natliving + 0*lifelanim + 0*mentalmat  \n
forceanim ~~ 0*inananim + 0*natliving + 0*lifelanim + 0*mentalmat \n
inananim ~~ 0*natliving + 0*lifelanim + 0*mentalmat  \n
natliving ~~ 0*lifelanim + 0*mentalmat  \n
lifelanim ~~ 0*mentalmat  \n
CKCr =~ artanim + forceanim + inananim + natliving + lifelanim + mentalmat \n
paranormal ~ CKCr \n"



base_model
base_fit<- sem(model = base_model, data=D, orthogonal=TRUE)
base_crit <- crit_fn2(base_fit)
summary(base_fit, fit.measures = TRUE)
inspect(base_fit, "rsquare")
base_fit

semPaths(base_fit,"std")
interpret(base_fit)
err<-inspect(base_fit,"rsquare")
err["paranormal"]
inspect(base_fit, "rsquare")
write.csv(residuals(base_fit)$cov, "SEM_residuals.csv")
write.csv(err, "SEM_rsquared.csv")
residuals(base_fit)$cov



############## SEQUENTIAL MODEL RUNS: 


# ITERATION 1 : remove inanimanim ==> Everything - full CKC item list (well... excluding metaphors and fillers). 
it1 <-
  "artanim =~ furnhome + houseknow + housemiss + housesense + househistory \n
forceanim =~ forceact + forcetries + forcelives + forcedirection + forcehuman \n
natliving =~ rocklives + moonalive + stoneslive + starslive + waterlives \n
lifelanim =~ earthwater + planets + skythunder + seaembraces + skydarkens + sunmorning \n
mentalmat =~ plannature + mindtouch + fearpoison + miserycompany + sleepwelcomes + knowledgehands + knowledgelive \n
artanim ~~ 0*forceanim + 0*natliving + 0*lifelanim + 0*mentalmat  \n
forceanim ~~ 0*natliving + 0*lifelanim + 0*mentalmat \n
natliving ~~ 0*lifelanim + 0*mentalmat  \n
lifelanim ~~ 0*mentalmat  \n
CKCr =~ artanim + forceanim + natliving + lifelanim + mentalmat \n
paranormal ~ CKCr \n"


it1
it1_fit<- sem(model = it1, data=D, orthogonal=TRUE)
summary(it1_fit, fit.measures = TRUE)
inspect(it1_fit, "rsquare")
it1_fit
interpret(it1_fit)
err<-inspect(it1_fit,"rsquare")
err["paranormal"]
inspect(it1_fit, "rsquare")
write.csv(residuals(it1_fit)$cov, "SEM_residuals.csv")
write.csv(err, "SEM_rsquared.csv")
residuals(it1_fit)$cov
lavInspect(it1_fit, "cor.lv")





# ITERATION 2 : removed; earthwater (subscale ahs high correlation to other variables - eathwater high residuals and error terms (3rd highest) - SRMR = 0.59 from last iteration ) 
it2 <-
  "artanim =~ furnhome + houseknow + housemiss + housesense + househistory \n
forceanim =~ forceact + forcetries + forcelives + forcedirection + forcehuman \n
natliving =~ rocklives + moonalive + stoneslive + starslive + waterlives \n
lifelanim =~  planets + skythunder + seaembraces + skydarkens + sunmorning \n
mentalmat =~ plannature + mindtouch + fearpoison + miserycompany + sleepwelcomes + knowledgehands + knowledgelive \n
artanim ~~ 0*forceanim + 0*natliving + 0*lifelanim + 0*mentalmat  \n
forceanim ~~ 0*natliving + 0*lifelanim + 0*mentalmat \n
natliving ~~ 0*lifelanim + 0*mentalmat  \n
lifelanim ~~ 0*mentalmat  \n
CKCr =~ artanim + forceanim + natliving + lifelanim + mentalmat \n
paranormal ~ CKCr \n"

it2_fit<- sem(model = it2, data=D, orthogonal=TRUE)
summary(it2_fit, fit.measures = TRUE)
inspect(it2_fit, "rsquare")
it2_fit
interpret(it2_fit)
err<-inspect(it2_fit,"rsquare")
err["paranormal"]
inspect(it2_fit, "rsquare")
write.csv(residuals(it2_fit)$cov, "SEM_residuals.csv")
write.csv(err, "SEM_rsquared.csv")
residuals(it2_fit)$cov
lavInspect(it2_fit, "cor.lv")


## 3
# ITERATION 3 : removed; knowledgelives - high error - SRMR = 0.544, GFI = .898 ) 
it3 <-
  "artanim =~ furnhome + houseknow + housemiss + housesense + househistory \n
forceanim =~ forceact + forcetries + forcelives + forcedirection + forcehuman \n
natliving =~ rocklives + moonalive + stoneslive + starslive + waterlives \n
lifelanim =~  planets + skythunder + seaembraces + skydarkens + sunmorning \n
mentalmat =~ plannature + mindtouch + fearpoison + miserycompany + sleepwelcomes + knowledgehands \n
artanim ~~ 0*forceanim + 0*natliving + 0*lifelanim + 0*mentalmat  \n
forceanim ~~ 0*natliving + 0*lifelanim + 0*mentalmat \n
natliving ~~ 0*lifelanim + 0*mentalmat  \n
lifelanim ~~ 0*mentalmat  \n
CKCr =~ artanim + forceanim + natliving + lifelanim + mentalmat \n
paranormal ~ CKCr \n"

it3_fit<- sem(model = it3, data=D, orthogonal=TRUE)
summary(it3_fit, fit.measures = TRUE)
inspect(it3_fit, "rsquare")
it3_fit
interpret(it3_fit)
err<-inspect(it3_fit,"rsquare")
err["paranormal"]
inspect(it3_fit, "rsquare")
write.csv(residuals(it3_fit)$cov, "SEM_residuals.csv")
write.csv(err, "SEM_rsquared.csv")
residuals(it3_fit)$cov
lavInspect(it3_fit, "cor.lv")


## 4
# ITERATION 4 : removed; waterlives - high error - SRMR = 0.042, GFI: .916  ) 
it4 <-
  "artanim =~ furnhome + houseknow + housemiss + housesense + househistory \n
forceanim =~ forceact + forcetries + forcelives + forcedirection + forcehuman \n
natliving =~ rocklives + moonalive + stoneslive + starslive  \n
lifelanim =~  planets + skythunder + seaembraces + skydarkens + sunmorning \n
mentalmat =~ plannature + mindtouch + fearpoison + miserycompany + sleepwelcomes + knowledgehands \n
artanim ~~ 0*forceanim + 0*natliving + 0*lifelanim + 0*mentalmat  \n
forceanim ~~ 0*natliving + 0*lifelanim + 0*mentalmat \n
natliving ~~ 0*lifelanim + 0*mentalmat  \n
lifelanim ~~ 0*mentalmat  \n
CKCr =~ artanim + forceanim + natliving + lifelanim + mentalmat \n
paranormal ~ CKCr \n"

it4_fit<- sem(model = it4, data=D, orthogonal=TRUE)
summary(it4_fit, fit.measures = TRUE)
inspect(it4_fit, "rsquare")
it4_fit
interpret(it4_fit)
err<-inspect(it4_fit,"rsquare")
err["paranormal"]
inspect(it4_fit, "rsquare")
write.csv(residuals(it4_fit)$cov, "SEM_residuals.csv")
write.csv(err, "SEM_rsquared.csv")
residuals(it4_fit)$cov
lavInspect(it4_fit, "cor.lv")



## 5
# ITERATION 5 : removed; starslive - high error - SRMR = 0.039, GFI: .926  ) 
it5 <-
  "artanim =~ furnhome + houseknow + housemiss + housesense + househistory \n
forceanim =~ forceact + forcetries + forcelives + forcedirection + forcehuman \n
natliving =~ rocklives + moonalive + stoneslive   \n
lifelanim =~  planets + skythunder + seaembraces + skydarkens + sunmorning \n
mentalmat =~ plannature + mindtouch + fearpoison + miserycompany + sleepwelcomes + knowledgehands \n
artanim ~~ 0*forceanim + 0*natliving + 0*lifelanim + 0*mentalmat  \n
forceanim ~~ 0*natliving + 0*lifelanim + 0*mentalmat \n
natliving ~~ 0*lifelanim + 0*mentalmat  \n
lifelanim ~~ 0*mentalmat  \n
CKCr =~ artanim + forceanim + natliving + lifelanim + mentalmat \n
paranormal ~ CKCr \n"

it5_fit<- sem(model = it5, data=D, orthogonal=TRUE)
summary(it5_fit, fit.measures = TRUE)
inspect(it5_fit, "rsquare")
it5_fit
interpret(it5_fit)
err<-inspect(it5_fit,"rsquare")
err["paranormal"]
inspect(it5_fit, "rsquare")
write.csv(residuals(it5_fit)$cov, "SEM_residuals.csv")
write.csv(err, "SEM_rsquared.csv")
residuals(it5_fit)$cov
lavInspect(it5_fit, "cor.lv")


## 6
# ITERATION 6 : removed; knowledgehands - high error - SRMR = 0.036, GFI: .935  ) 
it6 <-
  "artanim =~ furnhome + houseknow + housemiss + housesense + househistory \n
forceanim =~ forceact + forcetries + forcelives + forcedirection + forcehuman \n
natliving =~ rocklives + moonalive + stoneslive   \n
lifelanim =~  planets + skythunder + seaembraces + skydarkens + sunmorning \n
mentalmat =~ plannature + mindtouch + fearpoison + miserycompany + sleepwelcomes  \n
artanim ~~ 0*forceanim + 0*natliving + 0*lifelanim + 0*mentalmat  \n
forceanim ~~ 0*natliving + 0*lifelanim + 0*mentalmat \n
natliving ~~ 0*lifelanim + 0*mentalmat  \n
lifelanim ~~ 0*mentalmat  \n
CKCr =~ artanim + forceanim + natliving + lifelanim + mentalmat \n
paranormal ~ CKCr \n"

it6_fit<- sem(model = it6, data=D, orthogonal=TRUE)
summary(it6_fit, fit.measures = TRUE)
inspect(it6_fit, "rsquare")
it6_fit
interpret(it6_fit)
err<-inspect(it6_fit,"rsquare")
err["paranormal"]
inspect(it6_fit, "rsquare")
write.csv(residuals(it6_fit)$cov, "SEM_residuals.csv")
write.csv(err, "SEM_rsquared.csv")
residuals(it6_fit)$cov
lavInspect(it6_fit, "cor.lv")


## 7 (slight deviation for removal rules:)
# ITERATION 7 : removed; Sleepwelcomes - high error - SRMR = 0.041, GFI: .945  ) 
it7 <-
  "artanim =~ furnhome + houseknow + housemiss + housesense + househistory \n
forceanim =~ forceact + forcetries + forcelives + forcedirection + forcehuman \n
natliving =~ rocklives + moonalive + stoneslive   \n
lifelanim =~  planets + skythunder + seaembraces + skydarkens + sunmorning \n
mentalmat =~ plannature + mindtouch + fearpoison + miserycompany  \n
artanim ~~ 0*forceanim + 0*natliving + 0*lifelanim + 0*mentalmat  \n
forceanim ~~ 0*natliving + 0*lifelanim + 0*mentalmat \n
natliving ~~ 0*lifelanim + 0*mentalmat  \n
lifelanim ~~ 0*mentalmat  \n
CKCr =~ artanim + forceanim + natliving + lifelanim + mentalmat \n
paranormal ~ CKCr \n"

it7_fit<- sem(model = it7, data=D, orthogonal=TRUE)
summary(it7_fit, fit.measures = TRUE)
inspect(it7_fit, "rsquare")
it7_fit
err<-inspect(it7_fit,"rsquare")
err["paranormal"]
inspect(it7_fit, "rsquare")
write.csv(residuals(it7_fit)$cov, "SEM_residuals.csv")
write.csv(err, "SEM_rsquared.csv")
residuals(it7_fit)$cov
lavInspect(it7_fit, "cor.lv")
interpret(it7_fit)


## 8
# ITERATION 8 : removed; sunmorning - high error - SRMR = 0.031, GFI: .952 = SATISFACTORY  ) 
it8 <-
  "artanim =~ furnhome + houseknow + housemiss + housesense + househistory \n
forceanim =~ forceact + forcetries + forcelives + forcedirection + forcehuman \n
natliving =~ rocklives + moonalive + stoneslive   \n
lifelanim =~  planets + skythunder + seaembraces + skydarkens \n
mentalmat =~ plannature + mindtouch + fearpoison + miserycompany  \n
artanim ~~ 0*forceanim + 0*natliving + 0*lifelanim + 0*mentalmat  \n
forceanim ~~ 0*natliving + 0*lifelanim + 0*mentalmat \n
natliving ~~ 0*lifelanim + 0*mentalmat  \n
lifelanim ~~ 0*mentalmat  \n
CKCr =~ artanim + forceanim + natliving + lifelanim + mentalmat \n
paranormal ~ CKCr \n"

it8_fit<- sem(model = it8, data=D, orthogonal=TRUE)
summary(it8_fit, fit.measures = TRUE)
inspect(it8_fit, "rsquare")
it8_fit
err<-inspect(it8_fit,"rsquare")
err["paranormal"]
inspect(it8_fit, "rsquare")
write.csv(residuals(it8_fit)$cov, "SEM_residuals.csv")
write.csv(err, "SEM_rsquared.csv")
residuals(it8_fit)$cov
lavInspect(it8_fit, "cor.lv")
interpret(it8_fit)

## ITERATION 8 = ACCEPTED MODEL.
semPaths(it8_fit,"std")
standardizedsolution(it8_fit)

# Omega: 
reliabilityL2(it8_fit, "CKCr")

# Alpha (Cronbach)
cat("cronbach for CKC-R : ", cronbach(D[,c(
            "furnhome", "houseknow","housemiss","housesense","househistory",
            "forceact","forcetries","forcelives","forcedirection","forcehuman",
            "rocklives","moonalive","stoneslive",
            "planets","skythunder","seaembraces","skydarkens",
            "plannature","mindtouch","fearpoison","miserycompany"
)]))




fitmeasures(it8_fit, c("aic")) #smaller is better
fitmeasures(base_fit, c("aic")) #original model


#### ACCEPTED MODEL it8
# insert CKC scores into D$ 
D$ckcr <- predict(it8_fit)[,c("CKCr")]
describe((D$ckcr))
##############################################

# INSERT ALL INTO S (scaled dataset)


S<- D[c("CKC_short", "ckcr",
        "BS", 
        "absorption", 
        "IDAQ", "wordsum", "crt", "paranormal" 
        )]

for (i in 1:ncol(S))
  S[,i] <- scale(S[,i], center = TRUE)


multi.hist(S, global=FALSE, bcol="blue")
describe(S)

cor.plot(S, stars=TRUE)
cor(S)
write.csv(cor(S),'item_correlations.csv')


#####################################
# CKC Short SEM: 

CKC_SF <- 
"CKCSF =~ furnhome + houseknow+ househistory+forcetries +forcelives +plantsseasons + flowerslight + earthwater + planets + skythunder+ plannature + mindtouch + fearpoison + rocklives
 paranormal ~ CKCSF"

CKCSF_fit<- sem(model = CKC_SF, data=D, orthogonal=TRUE)
summary(CKCSF_fit, fit.measures = TRUE)
inspect(CKCSF_fit, "rsquare")
CKCSF_fit
err<-inspect(CKCSF_fit,"rsquare")
err["paranormal"]
inspect(CKCSF_fit, "rsquare")
interpret(CKCSF_fit)

#####################################
# LINEAR MODELS: (USING CKC-R)

# PARANORMAL BELIEF: 

lmparanormal <- lm(S$paranormal ~ S$ckcr + S$wordsum + S$IDAQ + S$absorption + S$crt)
summary(lmparanormal)
calc.relimp(lmparanormal, type='lmg')
confint(lmparanormal)


# without the CKCr (check unique variance)
lmtest <- lm(S$paranormal ~ S$wordsum + S$IDAQ + S$absorption + S$crt)
summary(lmtest)
lmtest
calc.relimp(lmtest, type='lmg')

anova(lmparanormal, lmtest) # the difference between models is significant


# without the wordsum (check unique variance)
lmtest <- lm(S$paranormal ~ S$ckcr + S$IDAQ + S$absorption + S$crt)
summary(lmtest)
calc.relimp(lmtest, type='lmg')

# without the IDAQ (check unique variance)
lmtest <- lm(S$paranormal ~ S$wordsum + S$ckcr + S$absorption + S$crt)
summary(lmtest)
calc.relimp(lmtest, type='lmg')


# without absorption (check unique variance)
lmtest <- lm(S$paranormal ~ S$wordsum + S$ckcr + S$IDAQ + S$crt)
summary(lmtest)
calc.relimp(lmtest, type='lmg')


# without crt (check unique variance)
lmtest <- lm(S$paranormal ~ S$wordsum + S$ckcr + S$IDAQ + S$absorption)
summary(lmtest)
calc.relimp(lmtest, type='lmg')

# CKC short version: 
lmparanormal2 <- rlm(S$paranormal ~ S$CKC_short + S$wordsum + S$IDAQ + S$absorption + S$crt)
summary(lmparanormal2)
calc.relimp(lmparanormal2, type='lmg')
confint.lm(lmparanormal2)


library(performance)
compare_performance(lmparanormal, lmparanormal2, rank=TRUE)

# Paried samples t-test: ckc vs ckc-r; 
t.test(S$CKC_short, S$ckcr, paired=TRUE)
#########################################
# CKCR predicted by validating scales; 
lmckcr <- lm(S$ckcr ~ S$wordsum + S$IDAQ + S$absorption + S$crt + S$BS)
lmckcr <- lm(S$ckcr ~ S$wordsum + S$absorption + S$crt)
summary(lmckcr)
calc.relimp(lmckcr)
confint.lm(lmckcr)
ols_plot_diagnostics(lmckcr) 


# Test CKC_Short vs CKCr:
cor.test(S$paranormal, S$ckcr)
cor.test(S$paranormal, S$CKC_short)
cor.test(S$ckcr, S$CKC_short)
lmckccompare<- rlm(S$paranormal ~ S$CKC_short + S$ckcr)
summary(lmckccompare)
calc.relimp(lmckccompare)
anova(lmckccompare)





# Demographics

# Gender: 1 = male, 2 = female, 3 = non-binary / third gender,4 = not say, 5 = self-describe.
table(D$Q12.1)
table(D$Q12.1_5_TEXT)

print("Education: 1 = year 11 or below, 2 = year 12., 3 = cert iii/iv, 4 = diploma or advanced diploma, 5 = Bachelor degree, 6 = Graduate Diploma or Graduate Certificate., 7= Post Graduate, 8 = Prefer not to say")
table(D$Q12.2)
cor.test(D$Q12.2, S$wordsum) #r = 0.1416531 p < .001 

print("Religion important? 1 = yes, 2 = no")
table(D$Q12.3)

# Income: 
print("income: 1 = 0- 19999, 2 = 20000 - 39999, 3 = 40000 - 59000, 4 = 60000 - 79999, 5 = 80 000 - 99999, 6 = 100000- 119999, 7 = 120000 - 139999, 8 = 140000 - 159999, 9 = 160000 or more, 10 = prefer not to say")
prop.table(table(D$Q12.4))
