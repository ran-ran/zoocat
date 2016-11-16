
context('Test gmon')

test_that('Test gmon object should not lose the class attribute', {
    gm <- gmon(-10 : 10)
    expect_is(gm[1:3], 'gmon')
    expect_is(gm + 10, 'gmon')
})


test_that('plot gmon', {
    library(ggplot2)
    df <- data.frame(month = gmon(-23 : 24), y = 1 : 48)
    ggplot(df, aes(month, y)) + 
        scale_x_gmon(breaks = seq(from = -23, to = 24, by = 8)) + 
        geom_point() + theme_bw()
})
