library(tidyverse)

# plot template 
pt <- ggplot(A.dat) +
    geom_point(aes(acousticness, danceability, col = label), alpha = 0.25)
pt

## pairs plot 

#pairs(~acousticness + danceability + duration_ms + instrumentalness + liveness + loudness + speechiness + valence, data = A.dat)
gatherpairs <- function(data, ..., xkey = '.xkey', xvalue = '.xvalue', ykey = '.ykey', yvalue = '.yvalue', na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
    vars <- quos(...)
    xkey <- enquo(xkey)
    xvalue <- enquo(xvalue)
    ykey <- enquo(ykey)
    yvalue <- enquo(yvalue)

    data %>% {
        cbind(gather(., key = !!xkey, value = !!xvalue, !!!vars, na.rm = na.rm, convert = convert, factor_key = factor_key), dplyr::select(., !!!vars))
    } %>% gather(., key = !!ykey, value = !!yvalue, !!!vars, na.rm = na.rm, convert = convert, factor_key = factor_key)
}

A.dat %>% gatherpairs(acousticness, energy, instrumentalness, loudness) %>%
    ggplot(aes(x = .xvalue, y = .yvalue, color = label)) +
    geom_point(alpha = 0.25) +
    facet_wrap(.xkey ~ .ykey, scales = 'free')


## plotting marginal effects of features on the model 
plot(fbm, i.var =9 , lwd = 2)
grid()
legend(0.7, -1, legend = lbs, lty = c(1, 1, 1, 1), col = c("black", "red", "green", "blue"))
