library(ggplot2)

blue <- data.frame(
    x = c(
        0,
        30,
        60,
        90,
        120,
        150,
        180,
        210,
        240,
        255
    ),
    y = c(
        0,
        7,
        20,
        50,
        110,
        160,
        200,
        240,
        250,
        255
    ))
green <- data.frame(
    x = c(
        0,
        30,
        60,
        90,
        120,
        150,
        180,
        210,
        240,
        255
    ),
    y = c(
        0,
        7,
        20,
        50,
        110,
        160,
        200,
        240,
        250,
        255
    ))
red <- data.frame(
    x = c(
        0,
        30,
        60,
        90,
        120,
        150,
        180,
        210,
        240,
        255
    ),
    y = c(
        0,
        7,
        20,
        50,
        110,
        160,
        200,
        240,
        250,
        255
    ))

baseline <- data.frame(
    x = c(
        0,
        255
    ),
    y = c(
        0,
        255
    ))

blue$channel <- c('Blue')
green$channel <- c('Green')
red$channel <- c('Red')
baseline$channel <- c('Channel')

channel_curves <- rbind(red, blue, green, baseline)

ggplot(data = channel_curves, aes(x = x, y = y, color = factor(channel))) +
    geom_line() +
    scale_color_manual(values = c('blue', 'black', 'green', 'red'))
