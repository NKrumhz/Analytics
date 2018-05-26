# Nathan Krumholz 
    # library(devtools)
    # install_github("tiagomendesdantas/Rspotify")
# run above once 
library(Rspotify)
library(dplyr)
load("OUTH") # load your authorization key from memory 
# yes its stored as plain text but i didnt feel like encrypting it 

# define the login user via their account information 
user <- getUser("Krumh010", token = key)

# get list of playlists from spotify 
sptf.list <- getPlaylist("Spotify", offset = 160, token = key)
    sptf.list$name

###
# what to run 
###
# get the names of songs in a playlist 
# need: playlist author, playlist uri, and your authorization token 
list <- getPlaylistSongs("Spotify", "37i9dQZF1DX0BcQWzuB7ZO", token = key)

# define data frame 
dat <- data.frame(matrix(nrow = nrow(list), ncol = 16))

# pull data and fill out data frame 
for (i in 1:nrow(list)) {
x <- getFeatures(list$id[i], token = key)   # get feautres and put it in a vector
    dat[i,] <- x                            # put vector in data frame
}

dat@names = colnames(x)                 # add column names to the dataframe 
dat$label <- rep("party", nrow(dat))    # define the label for the dataframe 

# save dataframe as a csv file for later usage 
write.csv(dat, file = "Dance.csv")
