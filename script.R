# Set Directory
setwd("C:\\Users\\wesley\\Downloads")

# Install packages
install.packages("mongolite")
install.packages("RCurl")
install.packages("rjson")
# Load libraries 
library(mongolite)
library(RCurl)
library(rjson)

# set api keys
api1 <- "RGAPI-ad7d03c3-1688-4b85-85c1-6d6c86bd50b9"
api2 <- "RGAPI-0eb42309-17cc-4ee9-b6e1-a29f39b57603"

api3 <- "RGAPI-815310cf-7c1e-48b1-936b-724d6183ee24"
api4 <- "RGAPI-fe0c0ef0-28b3-42b2-a0f0-5a603e99c239"


# list of server regions
regions <- c("br", "eune", "euw", "jp", "kr", "lan", "las", "na", "oce", "tr", "ru")


# Gets a list of players ids that are in the same tier
getLeague <- function(region, tier, key){
  url = paste("https://", region, ".api.riotgames.com/api/lol/", toupper(region), "/v2.5/league/", tier, "?type=RANKED_SOLO_5x5&api_key=", key, sep="")
  raw.data <- getURL(url)
  rd <- fromJSON(raw.data)
  # check conncection
  if(length(rd)!= 0){
    league <- vector()
    # sotre the list of players into a dataframe
    for(i in 1:(length(rd$entries))){
      league <- c(league, rd$entries[[i]]$playerOrTeamId)
    }
    return(league)
  }
}

# Gets an individualplayer's ranked stats from a given player id in any regions with an api key
getPlayerStats <- function(region, playerID, key){
  url = paste("https://", region, ".api.riotgames.com/api/lol/", toupper(region), "/v1.3/stats/by-summoner/", playerID,
              "/ranked?season=SEASON2017&api_key=", key, sep="")
  raw.data <- getURL(url)
  rd <- fromJSON(raw.data)
  # check conncection
  if(length(rd)!= 0){
    # player id 
    summonerId <- rd$summonerId
    # intitialize dataframes
    df <- data.frame(summonerId= numeric(), championid= numeric(), totalSessionsPlayed= numeric(), totalSessionsLost= numeric(), 
                     totalSessionsWon= numeric(), totalChampionKills= numeric(), totalDamageDealt= numeric(), 
                     totalDamageTaken= numeric(), mostChampionKillsPerSession= numeric(), totalMinionKills= numeric(),
                     totalDoubleKills= numeric(), totalTripleKills= numeric(), totalQuadraKills= numeric(), 
                     totalPentaKills= numeric(), totalUnrealKills= numeric(), totalDeathsPerSession= numeric(), 
                     totalGoldEarned= numeric(), mostSpellsCast= numeric(), totalTurretsKilled= numeric(), 
                     totalPhysicalDamageDealt= numeric(), totalMagicDamageDealt= numeric(), totalFirstBlood= numeric(), 
                     totalAssists= numeric(), maxChampionsKilled= numeric(), maxNumDeaths= numeric())
    
    
    # sotre the list of stats into a dataframe
    for(i in 1:(length(rd$champions))){
      if(rd$champions[[i]]$id != 0){
        championid <- rd$champions[[i]]$id
        stats <- as.data.frame(rd$champions[[i]]$stats)
        newdf <- cbind(summonerId, championid, stats)
        df <- rbind(df, newdf)
      }
    }
  }
  return(df)
}

# intitialize dataframe
df <- data.frame(summonerId= numeric(), championid= numeric(), totalSessionsPlayed= numeric(), totalSessionsLost= numeric(), 
                 totalSessionsWon= numeric(), totalChampionKills= numeric(), totalDamageDealt= numeric(), 
                 totalDamageTaken= numeric(), mostChampionKillsPerSession= numeric(), totalMinionKills= numeric(),
                 totalDoubleKills= numeric(), totalTripleKills= numeric(), totalQuadraKills= numeric(), 
                 totalPentaKills= numeric(), totalUnrealKills= numeric(), totalDeathsPerSession= numeric(), 
                 totalGoldEarned= numeric(), mostSpellsCast= numeric(), totalTurretsKilled= numeric(), 
                 totalPhysicalDamageDealt= numeric(), totalMagicDamageDealt= numeric(), totalFirstBlood= numeric(), 
                 totalAssists= numeric(), maxChampionsKilled= numeric(), maxNumDeaths= numeric(), tier= numeric())
master <- df

# get data through api
# add tier to dataframe with binary representation
# 1 represents challenger tier
# 0 represents master tier
# request some challengers 
# IMPORTANT sometimes error might occur: 'Error in if (rd$champions[[i]]$id != 0) { : argument is of length zero'
# this means API key usage rate has exceeded, new API keys might need to be regenrated 
for(i in 1:11){
  challengers <- getLeague(regions[i], "challenger", api1)
  tier <- 1
  for(j in 1:5){
    newdf1 <- getPlayerStats(regions[i], challengers[j], api1)
    newdf2 <- getPlayerStats(regions[i], challengers[j+5], api2)
    d <- rbind(d, cbind(tier, newdf1), cbind(tier, newdf2))
  }
}
challenger <- df

# request some masters
for(i in 1:11){
  masters <- getLeague(regions[i], "master", api3)
  tier <- 0
  for(j in 10:15){
    newdf1 <- getPlayerStats(regions[i], masters[j], api3)
    newdf2 <- getPlayerStats(regions[i], masters[j+5], api4)
    m <- rbind(m, cbind(tier, newdf1), cbind(tier, newdf2))
  }
}

# Store vaules using MongoDB
# establish connection using mongo command
leagueData <- mongo(collection = "df", db = "league") 
# insert values into the database
leagueData$insert(df)

# get all the challengers
chanllengers <- leagueData$find('{"tier": 1}')
# get all the masters
masters <- leagueData$find('{"tier": 0}')

# split the data frame into a training set and a testing set
indexes <- sample(1:nrow(chanllengers), size=0.5*nrow(chanllengers))
training <- chanllengers[indexes,]
testing <- chanllengers[-indexes,]

indexes <- sample(1:nrow(masters), size=0.5*nrow(masters))
training <- rbind(training, masters[indexes,])
testing <- rbind(testing, masters[-indexes,])


# test parameters for logistic regression
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost + totalSessionsWon + totalChampionKills 
             + totalDamageDealt +totalDamageTaken + mostChampionKillsPerSession + totalMinionKills + totalDoubleKills 
             + totalTripleKills + totalQuadraKills + totalPentaKills + totalUnrealKills + totalDeathsPerSession 
             + totalGoldEarned + mostSpellsCast + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + totalFirstBlood + totalAssists + maxChampionsKilled + maxNumDeaths, family = binomial, training)

summary(s.glm)

# some variables seems to be linear combination of other explanatory variables or is constant
# it shows that there might exist multi collinearlity and other models such as decision trees might be needed
# machine learning is beyond the field of this course, thus NA coeffiecients will just be omitted instead
# after omitting NA coeffiecients
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost + totalChampionKills 
             + totalDamageDealt +totalDamageTaken + mostChampionKillsPerSession + totalMinionKills + totalDoubleKills 
             + totalTripleKills + totalQuadraKills + totalPentaKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + totalAssists + maxNumDeaths, family = binomial, training)

summary(s.glm)

# omitting totalTripleKills
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost + totalChampionKills 
             + totalDamageDealt +totalDamageTaken + mostChampionKillsPerSession + totalMinionKills + totalDoubleKills 
             + totalQuadraKills + totalPentaKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + totalAssists + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting totalDamageDealt
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost + totalChampionKills 
             + totalDamageTaken + mostChampionKillsPerSession + totalMinionKills + totalDoubleKills 
             + totalQuadraKills + totalPentaKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + totalAssists + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting totalChampionKills
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost 
             + totalDamageTaken + mostChampionKillsPerSession + totalMinionKills + totalDoubleKills 
             + totalQuadraKills + totalPentaKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + totalAssists + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting totalPentaKills
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost 
             + totalDamageTaken + mostChampionKillsPerSession + totalMinionKills + totalDoubleKills 
             + totalQuadraKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + totalAssists + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting totalDoubleKills
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost 
             + totalDamageTaken + mostChampionKillsPerSession + totalMinionKills 
             + totalQuadraKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + totalAssists + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting totalAssists
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost 
             + totalDamageTaken + mostChampionKillsPerSession + totalMinionKills 
             + totalQuadraKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting totalMinionKills
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost 
             + totalDamageTaken + mostChampionKillsPerSession 
             + totalQuadraKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting totalDamageTaken
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed + totalSessionsLost 
             + mostChampionKillsPerSession + totalQuadraKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting totalSessionsLost
s.glm <- glm(formula = tier ~ championid + totalSessionsPlayed 
             + mostChampionKillsPerSession + totalQuadraKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting championid
s.glm <- glm(formula = tier ~ totalSessionsPlayed 
             + mostChampionKillsPerSession + totalQuadraKills + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + maxNumDeaths, family = binomial, training)
summary(s.glm)

# omitting totalQuadraKills
s.glm <- glm(formula = tier ~ totalSessionsPlayed + mostChampionKillsPerSession + totalDeathsPerSession 
             + totalGoldEarned + totalTurretsKilled + totalPhysicalDamageDealt + totalMagicDamageDealt 
             + maxNumDeaths, family = binomial, training)
# final results
summary(s.glm)
# the model can predict if a player is challenger or platnium if
# the predicted value is rounded up if it is bigger than 0.5
# calculate the average confidence of the prediction from the model  
predicted <- predict(s.glm, newdata = testing, type = "response")
actual <- testing$tier
results <- abs(actual - predicted)
confidence <- mean(results)
