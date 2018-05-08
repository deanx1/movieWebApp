## install and load packages
# install.packages("hrbrmstr/omdbapi")
# install.packages("RMySQL")
#
# install.packages("rvest")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("lubridate")
# install.packages("stringr")
## needed for db connection
# install.packages("DBI")
# install.packages("rJava")
# install.packages("RJDBC")
## needed for sql interface on the dataframe
# install.packages("sqldf")

# load into memory
library(DBI)
library(rJava)
library(RJDBC)
library(sqldf)
library(RMySQL)
library(rvest)
library(readr)
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(stringr)

desktop <- F

if (desktop) {
  pathname <- "Dean"
} else {
  pathname <- "Gebruiker"
}
setwd(paste("C:/Users/", pathname, "/Google Drive/LAPTOP_DESKTOP/Big Data/Data Processing and Storage/Individuele opdracht/ml-20m",  sep = ""))

ml_links <- read_csv("links.csv")

# Remove imdbId column, this wont be used.
ml_links$imdbId <- NULL

# Remove the rows where tmdbId is not present. These are hard to join to other dataframes
ml_links <- ml_links[!is.na(ml_links$tmdbId),]

ml_movies <- read_csv("movies.csv")

# Merge links and film
ml_link_film <- merge(ml_links, ml_movies, by = "movieId", all.x = TRUE)

# Check of de dataset geen na's bevat
sum(is.na(ml_link_film))

# Calculate the average rating per movie and save the value in a new column
ml_ratings <- read_csv("ratings.csv")
avg_ratings_ML <- ml_ratings %>% group_by(movieId) %>% summarise(avgRatingMl = mean(rating))

# Merge the dataframes
ml_link_film <- inner_join(ml_link_film, avg_ratings_ML, by = "movieId")

# Extract the year of release and save the value in a new column
ml_link_film <- ml_link_film %>% mutate(year = str_extract(title, "\\(([0-9]{4})\\)$") %>%
                                          str_extract("[0-9]{4}") %>% strtoi())

# Use the api from TMDB to retrieve TMDB rating and other interesting information

# # SCRAPE START
# # Make a new dataframe for the scraped data
# tmdb_scrape <- data.frame(tmdbId = integer(),
#                           #ratingTmdb = double(),
#                           #voteCount = integer(),
#                           adult = character(),
#                           budget = integer(),
#                           revenue = integer(),
#                           popularity = double(),
#                           runtime = integer(),
#                           stringsAsFactors = F)
#
# starttime_scrape <- Sys.time()
# for (i in 1:nrow(ml_link_film)) {
#   tryCatch({  # Haal film op
#     id <- ml_link_film[i, 2]
#     url <- paste("https://api.themoviedb.org/3/movie/",id ,"?api_key=cd5a887a914e81181124a538a2908d2e", sep = "")
#     moviejson <- read_json(url)
#
#     # Get the tmdbId for an easy join
#     tmdbId <- moviejson$id
#     #rating <- moviejson$vote_average
#     #voteCount <- moviejson$voteCount
#     adult <- moviejson$adult
#     budget <- moviejson$budget
#     revenue <- moviejson$revenue
#     popularity <- moviejson$popularity
#     runtime <- moviejson$runtime
#
#     # Add these values to row and add the row the the dataframe
#     newRow <- data.frame(tmdbId = tmdbId,
#                          #ratingTmdb = rating,
#                          #voteCount = voteCount
#                          adult = adult,
#                          budget = budget,
#                          revenue = revenue,
#                          popularity = popularity,
#                          runtime = runtime
#                          )
#
#     tmdb_scrape[i,] <- rbind(newRow)},
#
#     error = function(error_condition) {
#       print(paste("loop count:", i, ", tmdb not found id:", id))
#     })
#
#
#   # Wait 11 seconds after every 40 films as to not override the api limit
#   if (i%% 40 == 0) {
#     Sys.sleep(11)
#     print(paste("loop count:", i))
#   }
# }
# scrape_duration <- Sys.time() - starttime_scrape
# scrape_duration
#
# Convert to csv to save to disk
# write.csv(tmdb_scrape, file = "scrapedata5.csv")
## SCRAPE STOP

# Read the scraped data
scrapedata <- read.csv("scrapedata2.csv")
scrapedata2 <- read.csv("scrapedata4.csv")

# Join the dataframes
ml_link_film <- inner_join(ml_link_film, scrapedata, by = "tmdbId")
ml_link_film <-left_join(ml_link_film, scrapedata2, by = "tmdbId")

# Remove the rows where rating or votecount are 0, these are of no use for our comparison
ml_link_film <- filter(ml_link_film, voteCount > 0 & ratingTmdb > 0)

# Remove columns that only count
ml_link_film <- select(ml_link_film, -X.x, - X.y)

# Remove rows with NA values
ml_link_film <- na.omit(ml_link_film)

# Remove movie without genre, these won't be usefull for our comparison
ml_link_film <- ml_link_film %>% filter(genres != "(no genres listed)")

# Double the ML rating and round these to get a 0 - 10 rating system
ml_link_film$avgRatingMl <- round(ml_link_film$avgRatingMl* 2, 1)

# Write ml_link_film to the database
drv <- JDBC("com.mysql.jdbc.Driver",
            paste("C:/Users/", pathname,
                  "/Google Drive/Big Data/mysql-connector-java-5.1.45/mysql-connector-java-5.1.45-bin.jar", sep = "")
            )
conn <- dbConnect(drv, "jdbc:mysql://sql11.freemysqlhosting.net/sql11228477", "sql11228477", "l2BWqJ3mtK", useSSL=FALSE)
dbListTables(conn)

dbWriteTable(conn,name="ml_link_film", value=ml_link_film , append=FALSE, row.names=FALSE, overwrite=FALSE)
# Haal data op van de databse.
databasedata <- dbGetQuery(conn, "select * from ml_link_film")

ml_link_film$genres <- str_replace_all(ml_link_film$genres, "\\|", " ")

# Get all unique genres
genres <- ml_link_film$genres %>%  str_split(" ") %>% unlist() %>% unique()

# Make dataframes per genre
genreSubset <- list()
for(i in 1:length(genres)) {
  genreStr <- genres[i]
  genreSubset[[i]] <- ml_link_film %>% filter(str_detect(ml_link_film$genres, genreStr))
  cat(genres[i], nrow(genreSubset[[i]]), "\n")
}

# Average rating per genre for Movielens and TMDB
genreRatings <- list()
for(i in 1:length(genres)) {
  genreRatings[[i]] <- genreSubset[[i]] %>%
    summarise(mlAvgRating = round(mean(avgRatingMl),1), tmdbAvgRating = round(mean(ratingTmdb), 1))
  cat(genres[i], genreRatings[[i]]$mlAvgRating, "-", genreRatings[[i]]$tmdbAvgRating, "\t\tnrow:", nrow(genreSubset[[i]]), "\n")
}
# Get average year for Film-Noir films
genreSubset[[19]] %>% summarise(avgYearNoir = round(mean(year), 1))

# Get average total movie rating
ml_link_film %>% summarise(mlAverageTotalRating = mean(avgRatingMl),
                           tmdbAverageTotalRating = mean(ratingTmdb), 1,
                           diff = abs(mlAverageTotalRating - tmdbAverageTotalRating))


# Average ratings per 10 years
genreRatingsYear <- list()
years <- vector()
for(i in 1:length(genres)) {
  # Filter the rows further in intervals of 10 years
   start <- min(ml_link_film$year)
   end <- max(ml_link_film$year)
   count <- 1
   tempVector <- vector()
   genreStr <- genres[i]
  while (start <= end + 3) {
    tempSet <- genreSubset[[i]] %>% filter(year >= start, year < start + 10)
    tempVector[count] = round(abs(mean(tempSet$avgRatingMl) - mean(tempSet$ratingTmdb)), 2)
    years[count] = start
    start <- start + 10
    count <- count + 1
  }
   print(genreStr)
   print(tempVector)
   genreRatingsYear[[i]] <- tempVector
}

# Total genre year rating
totalGenreRatingYear <- vector()
# Per timeframe
for(i in 1:length(years)) {
  sum <- 0
  count <- 0
  # Per genre
  for(x in 1: length(genres)) {
    tempGenre <- genreRatingsYear[[x]]
    if (!is.na(tempGenre[i])) {
      count <- count + 1
      sum <- sum + tempGenre[i]
    }
  }
  totalGenreRatingYear[i] <- sum / count
}

ggplot(mapping = aes(x = years, y = totalGenreRatingYear)) + 
  geom_line() +
  geom_point() +
  labs(y = "Total Difference Rating")