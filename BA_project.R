install.packages("ggthemes")
install.packages("reshape2")
install.packages("taRifx")
library(ggplot2)
library(ggthemes)
library(reshape2)
library(taRifx)
### Import dataset ###

# Read .csv file
movies <- read.csv("movie_metadata.csv", header = T, stringsAsFactors = F)

### Data cleaning ####

# Remove instances which have at least one NA variable
movies <- movies[complete.cases(movies), ]
# Remove instances which are duplicated (duplicated based on title)
movies <- movies[!duplicated(movies$movie_title),]
movies <- movies[, -c(1,13,18,19,22,27,28)]
# Function to remove Ã, leading and trailing whitespace from movies$movie_title
movie_title_processing <- function(str){
  str <- sub(pattern = "Ã", replacement = "", str)
  str <- sub(pattern = "^//s+|//s+$", replacement ="", str)
}

# Apply previous function
movies$movie_title <- sapply(movies$movie_title, FUN = movie_title_processing)

### Keywords Analysis ***

### Plot Keywords

movies0 <- movies[movies$plot_keywords != "", ]
keywords <- c()
i <- 1
for (ins in movies0$plot_keywords){
  kw <- strsplit(ins, "[|]")
  if (length(kw) != 0){
    for (word in kw[[1]]){
      if (!(word %in% keywords)){
        keywords[i] <- word
        i = i + 1
      }
    }
  }
}
keywords
# Create a dataframe with logical values which 
# indiacte the keywords of each movie
movies0$plot_keywords <- strsplit(movies0$plot_keywords, "[|]")
keywords_idx <- movies0[, c("movie_title", "plot_keywords")]
i = 1
mat <- matrix(rep(0, (dim(movies0)[1] * length(keywords))), nrow = dim(movies0)[1])
for (word in keywords_idx$plot_keywords){
  idx <- which(keywords %in% word)
  mat[i, idx] <- 1
  i = i + 1
}
colnames(mat) <- keywords
movies_and_keywords <- data.frame(mat)

# Find how many movies belong in each keyword
sum <- rep(0, length(keywords))
for (i in 1:length(keywords)){
  sum[i] <- sum(movies_and_keywords[, i])
}
keywords_sum <- data.frame(keywords = factor(keywords), sum = sum)
keywords_sum <- keywords_sum[order(sum, decreasing = FALSE),]
keywords_sum$keywords <- factor(keywords_sum$keywords, levels = keywords_sum$keywords)
keywords_sum <- keywords_sum[keywords_sum$sum > 10, ]
keywords_sum <- keywords_sum[(dim(keywords_sum)[1]-100):dim(keywords_sum)[1] ,]
keywords_sum

(keywords_sum$keywords)
colnames(movies_and_keywords)

keyword_col=c()
j=1
for (i in keywords_sum$keywords){
    keyword_col[j]=i
    j=j+1
  }

names(movies_and_keywords) <- gsub("\\.", " ", names(movies_and_keywords))
keyword_col
mat1=data.frame(movies_and_keywords[,keyword_col])


colnames(mat1)

#Do the same for Genre

movies1 <- movies0[movies0$genres!= "", ]
genres_l <- c()
i <- 1
for (ins in movies1$genres){
  kw <- strsplit(ins, "[|]")
  if (length(kw) != 0){
    for (word in kw[[1]]){
      if (!(word %in% genres_l)){
        genres_l[i] <- word
        i = i + 1
      }
    }
  }
}
length(genres_l)
# Create a dataframe with logical values which 
# indiacte the keywords of each movie
movies1$genres <- strsplit(movies1$genres, "[|]")
genres_idx <- movies1[, c("movie_title", "genres")]
i = 1
mat2 <- matrix(rep(0, (dim(movies1)[1] * length(genres_l))), nrow = dim(movies1)[1])
for (word in genres_idx$genres){
  idx <- which(genres_l %in% word)
  mat2[i, idx] <- 1
  i = i + 1
}
colnames(mat2) <- genres_l
movies_and_genres <- data.frame(mat2)
main_movie<-cbind(movies1,movies_and_genres,mat1)
colnames(main_movie)
main_movie1 <- apply(main_movie,2,as.character)
write.csv(file = "F:/R_Projects/main_movie_data20.csv", x=main_movie1)

