  The study used R as the analysis tool and MusicOSet as the dataset to study the factors that may affect the popularity of a song and build two regression prediction models to predict the popularity of a song. The study performed preprocessing operations such as cleaning, recoding, and merging of data, and analysed the variables with correlation analysis, visualization, and logistic regression, and identified variables that have significant correlation with the popularity of a song and predicted the popularity of a song with a high success rate. This study identified the variables that have significant correlation on the popularity of songs and predicted whether the songs are popular or not with a high success rate. The results of this study are useful for establishing recommendation algorithms for music platforms and guiding creators to produce highly popular songs.
  
  Research questions:
  1. Which song features are significantly correlated with song popularity?
  2. Which artist features are significantly correlated with song popularity?
  3. Is it possible to predict whether a song is popular by song features and artist features?
  4. Is it possible to predict whether a song is popular by its lyrics?

  Key findings:
  1. Key=9, danceability, loudness, solo songs, and artist popularity were positively correlated with song popularity.
  2. Acousticness, energy, liveness, valence, artist_type=rapper, and artist_type=singer were negatively correlated with song popularity.
  4. Acoustic characteristics, artist characteristics, and lyrics are somewhat effective in predicting whether a song is popular or not.


  How to ues this R code:
  To use the code for this study, you will need to download the R language and R Studio in your computer. You will also need to download the MusicOSet dataset. If there are packages that won't load, this means you need to download them into Rstudio before loading them. All code is clearly commented, please read it carefully before running.
