library(ggplot2)

blue <- data.frame(
    x = c(
        0,
        24,
        93,
        148,
        198,
        255
    ),
    y = c(
        0,
        10,
        82,
        177,
        226,
        255
    ))
green <- data.frame(
    x = c(
        0,
        28,
        61,
        92,
        149,
        189,
        255
    ),
    y = c(
        0,
        7,
        35,
        82,
        181,
        221,
        255
    ))
red <- data.frame(
    x = c(
        0,
        44,
        70,
        96,
        149,
        191,
        255
    ),
    y = c(
        0,
        11,
        38,
        88,
        179,
        223,
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
