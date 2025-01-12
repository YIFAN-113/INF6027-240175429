library(tidyverse)
library(GGally)
library(ggplot2)
library(dplyr)
library(tidytext)
library(stringr)
library(tm)
library(SnowballC)

# Load dataset
songs<-read.delim(file = "songs.csv", header = TRUE, sep = "\t", quote = "")
artists<-read.delim(file = "artists.csv", header = TRUE, sep = "\t", quote = "")
acoustic_features<-read.delim(file = "acoustic_features.csv", header = TRUE, sep = "\t", quote = "")
lyrics<-read.delim(file = "lyrics.csv", header = TRUE, sep = "\t", quote = "")
tracks<-read.delim(file = "tracks.csv", header = TRUE, sep = "\t", quote = "")

# Describe the dataset
summary(songs)
summary(artists)
summary(acoustic_features)
summary(tracks)

#Filter outliers
acoustic_features<-acoustic_features%>%filter(time_signature>0&loudness<=0&tempo>0)
head(songs$artists)

# Get artist id
songs<-songs%>%mutate(artist_id = gsub("\\{\\'(.*?)\\':.*", "\\1", artists))
head(songs$artist_id)

# Convert followers to integers
artists$followers <- as.integer(artists$followers)
summary(artists)
artists <- artists %>% filter(!is.na(followers))

# Get the year the song was released
tracks$release_year <- substr(tracks$release_date,1,4)
head(lyrics)

# Rename column names to prevent confusion
songs<-songs%>%rename(song_pop = popularity)
artists<-artists%>%rename(artist_pop = popularity)

# Splitting datasets
tracks_sub <- tracks[,c("song_id","release_year")]
song_sub <- songs[,c("song_id","artist_id","song_type","song_pop")]
artist_sub <-artists[,c("artist_id","followers","artist_pop","artist_type","main_genre")]

# Inline merged datasets
song_all <- inner_join(acoustic_features, song_sub, by = "song_id")
song_all <- inner_join(song_all,tracks_sub,by="song_id")
song_all <- inner_join(song_all, artist_sub, by="artist_id")
song_all <- inner_join(song_all,lyrics,by="song_id")
summary(song_all)

# Recode the year of issue into year groups
song_all <- song_all %>%
  mutate(year_group = paste0((release_year %/% 10) * 10, "-", (release_year %/% 10) * 10 + 9))

# correlation analysis
cor.test(song_all$song_pop,song_all$duration_ms)#.15
cor.test(song_all$song_pop,song_all$acousticness)#-.22
cor.test(song_all$song_pop,song_all$danceability)#.09
cor.test(song_all$song_pop,song_all$energy)#.18
cor.test(song_all$song_pop,song_all$instrumentalness)#-.19
cor.test(song_all$song_pop,song_all$liveness)#-.06
cor.test(song_all$song_pop,song_all$loudness)#.32
cor.test(song_all$song_pop,song_all$speechiness)#.14
cor.test(song_all$song_pop,song_all$valence)#-.13
cor.test(song_all$song_pop,song_all$tempo)#.009
cor.test(song_all$song_pop,song_all$followers)#.38
cor.test(song_all$song_pop,song_all$artist_pop)#.67

# Calculate the correlation matrix
cor_matrix <- cor(song_all[, c("song_pop", "duration_ms", "acousticness", "danceability", "energy", 
                               "instrumentalness", "liveness", "loudness", "speechiness", "valence", "tempo","followers","artist_pop")])
round(cor_matrix,3)

# Converting the correlation matrix to long format, for graphing purposes
library(reshape2)
melted <- melt(cor_matrix)

# Correlation heat mapping
library(ggplot2)
ggplot(melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features", fill = "Correlation")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Drawing a set of violins
p1<-ggplot(song_all,aes(year_group,song_pop,group=year_group))+
  geom_violin(fill="yellow")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Song pop grouped by year",
       caption="song_all dataset",
       x="year group",
       y="Song pop")

p2<-ggplot(song_all,aes(key,song_pop,group=key))+
  geom_violin(fill="grey")+
  labs(title="Song pop grouped by key",
       caption="song_all dataset",
       x="Class key",
       y="Song pop")

p3<-ggplot(song_all,aes(mode,song_pop,group=mode))+
  geom_violin(fill="plum")+
  labs(title="Song pop grouped by mode",
       caption="song_all dataset",
       x="Class mode",
       y="Song pop")

p4<-ggplot(song_all,aes(song_type,song_pop,group=song_type))+
  geom_violin(fill="blue")+
  labs(title="Song pop grouped by song type",
       caption="song_all dataset",
       x="Song type",
       y="Song pop")

p5<-ggplot(song_all,aes(artist_type,song_pop,group=artist_type))+
  geom_violin(fill="red")+
  labs(title="Song pop grouped by artist type",
       caption="song_all dataset",
       x="Artist type",
       y="Song pop")

p6<-ggplot(song_all,aes(time_signature,song_pop,group=time_signature))+
  geom_violin(fill="green")+
  labs(title="Song pop grouped by time signature",
       caption="song_all dataset",
       x="Time signature",
       y="Song pop")

# Combined Violin Diagram
library(patchwork)
(p1 | p2 | p3) /
  (p4 | p5 | p6)


# Recode song_pop as a binary variable.
song_all$pop_num <- ifelse(song_all$song_pop >= 60,1,0)

# Converting categorical variables to factor types
song_all$key<-as.factor(song_all$key)
song_all$mode<-as.factor(song_all$mode)
song_all$song_type<-as.factor(song_all$song_type)
song_all$artist_type<-as.factor(song_all$artist_type)
song_all$time_signature<-as.factor(song_all$time_signature)
song_all$pop_num<-as.factor(song_all$pop_num)
song_all$pop_category<-as.factor(song_all$pop_category)

# disrupt dataset
song_all <- song_all[sample(nrow(song_all)), ]

# Filtering Missing Values
song_all<-song_all%>%filter(artist_type!="-")

# Divide the training dataset and test dataset
song_train <- song_all[1:11364,]
song_test <- song_all[11364:16234,]

# binary logistic regression model
Bmodel <- glm(pop_num~duration_ms + key + mode + time_signature + acousticness + danceability + energy + instrumentalness + liveness + loudness + speechiness + valence + tempo + artist_type + year_group + followers + artist_pop + song_type, family=binomial(link='logit'),data = song_train)
summary(Bmodel)

# Predicting whether a song is popular using a test dataset
BP<-predict(
  Bmodel,
  newdata = song_test,
  type='response'
)

# Setting classification thresholds
BPP<-ifelse(BP>0.5,1,0)

# Calculating correct rate
Error <- mean(
  BPP = song_test$pop_num
)
print(paste('Accuracy',1-Error))


# Preprocessing Lyrics Column
song_all <- song_all %>%
  mutate(lyrics = str_replace_all(lyrics, "\\\\n", "\n"),         # convert line breaks
         lyrics = str_replace_all(lyrics, "\\\\'", "'"),          # Fix escaped single quotes
         lyrics = str_replace_all(lyrics, "\\\\\"", "\""),        # Fix escaped double quotes
         lyrics = str_replace_all(lyrics, "\\[.*?\\]", ""),       # Remove structure tags
         lyrics = str_replace_all(lyrics, "\\d+", ""),            # Remove all numbers
         lyrics = str_replace_all(lyrics, "[^\\w\\s']", " "),     # Remove punctuation but retain single quotes
         lyrics = str_replace_all(lyrics, "\\s+", " "),           # Replace characters with spaces
         lyrics = str_replace_all(lyrics, "xa", ""),              # If this step is not performed,
                                                                  # the final word frequency result will have a large number of "xa", 
                                                                  # the exact reason for which is still unknown and may be some kind of special character.
         lyrics = str_trim(lyrics))                               # Remove extra spaces



# Creating a corpus
lyrics_corpus <- Corpus(VectorSource(song_all$lyrics))

# Lowercase
lyrics_corpus <- tm_map(lyrics_corpus, content_transformer(tolower))

# Remove Discontinued Words
lyrics_corpus <- tm_map(lyrics_corpus, removeWords, stopwords("en"))

# stem extraction
lyrics_corpus <- tm_map(lyrics_corpus, stemDocument)

# Creating a word matrix
dtm <- DocumentTermMatrix(lyrics_corpus)
print(dtm)

# Removal of sparse terms
dtm <- removeSparseTerms(dtm, 0.95)
print(dtm)

# Converting matrices to dataframes
dtm_data <- as.data.frame(as.matrix(dtm))

# Add popularity identifier
dtm_data$pop_num <- song_all$pop_num

# Categorise songs according to whether they are popular or not
popular_songs <- dtm_data[dtm_data$pop_num == 1, -ncol(dtm_data)]
unpopular_songs <- dtm_data[dtm_data$pop_num == 0, -ncol(dtm_data)]

# Calculate the frequency of occurrence of a word
popular_word_freq <- colSums(popular_songs)
unpopular_word_freq <- colSums(unpopular_songs)

# Extract the top 10 words in each group
popular_words <- sort(popular_word_freq, decreasing = TRUE)[1:10]
unpopular_words <- sort(unpopular_word_freq, decreasing = TRUE)[1:10]



# Convert to data frame
popular_df <- data.frame(Word = names(popular_words), Frequency = popular_words)
unpopular_df <- data.frame(Word = names(unpopular_words), Frequency = unpopular_words)



# Plotting cross-sectional bars of word frequencies of popular songs
p7<-ggplot(popular_df, aes(x = Frequency, y = reorder(Word, Frequency))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Words in Popular Songs",
       x = "Frequency",
       y = "Words") +
  theme_minimal()

# Plotting cross-sectional bars of word frequencies of unpopular songs
p8<-ggplot(unpopular_df, aes(x = Frequency, y = reorder(Word, Frequency))) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(title = "Top 10 Words in Unpopular Songs",
       x = "Frequency",
       y = "Words") +
  theme_minimal()

# Combine Bar Charts
(p7 | p8 )


# Divide the training dataset and test dataset
train_data <- dtm_data[1:11364,]
test_data <- dtm_data[11364:16234,]

library(glmnet)
# Separate target variable
train_words <- as.matrix(train_data[, -ncol(train_data)])
train_pop <- train_data$pop_num # Target variable

# Binary logistic regression model
logit_model <- glmnet(train_words, train_pop, family = "binomial")


# Preparing Test Data
test_words <- as.matrix(test_data[, -ncol(test_data)])
test_pop <- test_data$pop_num

# Predicting whether a song is popular or not
pred_prob <- predict(logit_model, test_words, s = tail(logit_model$lambda, 1), type = "response")

# Setting classification thresholds
pred_labels <- ifelse(pred_prob > 0.5, 1, 0)

# Calculating model prediction accuracy
accuracy <- mean(pred_labels == y_test)
print(paste("Accuracy:", accuracy))
