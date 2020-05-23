preds <- read.csv("images.csv")
images <- c('<img src="./www/test_0.jpg" height="52"></img>','<img src="./www/test_1.jpg" height="52"></img>',
            '<img src="./www/test_2.jpg" height="52"></img>','<img src="./www/test_3.jpg" height="52"></img>',
            '<img src="./www/test_4.jpg" height="52"></img>','<img src="./www/test_5.jpg" height="52"></img>',
            '<img src="./www/test_6.jpg" height="52"></img>','<img src="./www/test_7.jpg" height="52"></img>',
            '<img src="./www/test_8.jpg" height="52"></img>','<img src="./www/test_9.jpg" height="52"></img>',
            '<img src="./www/test_10.jpg" height="52"></img>')
tt <- cbind(preds,images)

ind <- read.csv("Combined_1991_2021_Adj.csv", stringsAsFactors = FALSE)
