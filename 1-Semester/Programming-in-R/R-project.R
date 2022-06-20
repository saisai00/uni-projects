library(ggplot2)
library(tidyverse)
library("choroplethr")
library("choroplethrAdmin1")


#setwd("~/Desktop/Prigramming in R/Project")
df <- read.csv("SSDSE-B-2021.csv", header=TRUE, row.names = NULL, stringsAsFactors=FALSE, fileEncoding = "shift-jis")
print(df)

#extraction based on columnList
columnList <- c("SSDSE.B.2021", "Code", "Prefecture",
                "E4601", "E4602", "L3221","L322101",
                "L322102", "L322103", "L322104", "L322105",
                "L322106", "L322107", "L322108", "L322109" ) #make columnList
data.selected <- df[, columnList] #access data
data.selected <- data.selected[-1,]
colnames(data.selected) <- c("year", "code of prefecture", "prefecture", 
                             "Number of those who graduated from high school",
                             "Number of those who continue to further school after high school",
                             "Consumption Expenditures", "food expenses",
                             "housing costs", "Utility and water expenses",
                             "Furniture and household expenses",
                             "Clothing and footwear",
                             "health and medical expenses",
                             "transportation and communication expenses",
                             "education",
                             "educational recreation expenses"
                             )

print(data.selected)

data.selected$prefecture
data.selected$prefecture <- data.selected$prefecture %>% 
  str_replace_all(c('北海道' = 'hokkaido', '青森県' = 'aomori', '岩手県' = 'iwate', '宮城県' = 'miyagi', '秋田県' = 'akita',
                    '山形県' = 'yamagata','福島県' = 'fukushima','茨城県' = 'ibaraki','栃木県' = 'tochigi','群馬県' = 'gunma',
                    '埼玉県' = 'saitama','千葉県' = 'chiba','東京都' = 'tokyo','神奈川県' = 'kanagawa','新潟県' = 'niigata',
                    '富山県' = 'toyama','石川県' = 'ishikawa','福井県' = 'fukui','山梨県' = 'yamanashi','長野県' = 'nagano',
                    '岐阜県' = 'gifu','静岡県' = 'shizuoka','愛知県' = 'aichi','三重県' = 'mie','滋賀県' = 'shiga',
                    '京都府' = 'kyoto','大阪府' = 'osaka','兵庫県' = 'hyogo','奈良県' = 'nara','和歌山県' = 'wakayama',
                    '鳥取県' = 'tottori','島根県' = 'shimane','岡山県' = 'okayama','広島県' = 'hiroshima','山口県' = 'yamaguchi',
                    '徳島県' = 'tokushima','香川県' = 'kagawa','愛媛県' = 'ehime','高知県' = 'kochi','福岡県' = 'fukuoka',
                    '佐賀県' = 'saga','長崎県' = 'nagasaki','熊本県' = 'kumamoto','大分県' = 'oita','宮崎県' = 'miyazaki',
                    '鹿児島県' = 'kagoshima','沖縄県' = 'okinawa'
  ))



#make a plot of number of people who graduate from the bachelor by year
years <- unique(data.selected$year)
total_table = data.frame(matrix(ncol = 2, nrow = length(years)))
rownames(total_table) <- years

count <- 0
for (i in years) {
  data_year <- data.selected[data.selected$year == i, ]
  print(data_year)
  total_table[count + 1, 1] <- sum(as.numeric(data_year[, "Number of those who graduated from high school"]))
  total_table[count + 1, 2] <- sum(as.numeric(data_year[, "Number of those who continue to further school after high school"]))
  count <- count + 1
  print(count)
}    
ratio = as.numeric(total_table[, "X2"])/as.numeric(total_table[, "X1"])
barplot(ratio, main = "ratio of people to continue to study after high school")



#make a plot of number of people who graduate from the bachelor against expenses of houshold
#ratio_proc <- as.numeric(as.numeric(data_2018[, "Number of those who continue to further school after high school"]))  / as.numeric(data_2018[, "Number of those who graduated from high school"])
#plot(ratio_proc,data_2018$`Consumption Expenditures`)





#the choroplethr plot of rates of graduation from bachelor in 2018
data_2018 <- data.selected[data.selected$year == 2018, ]
data_2018 <- data_2018[order(data_2018$prefecture), ]
ratio_2018 <- as.numeric(data_2018[, "Number of those who continue to further school after high school"])/as.numeric(data_2018[, "Number of those who graduated from high school"])

admin1_map("japan")　#白地図（黒地図？）を作成

# df_japan_censusデータ
data(df_japan_census)
head(df_japan_census)

df_japan_census[, 1] == data_2018$prefecture


# df_japan_censusデータ
PlotData <- data.frame(region = df_japan_census[, 1], value = ratio_2018)
#プロット
admin1_choropleth(country.name = "japan",
                  df           = PlotData,
                  title        = "2010 Japan Population Estimates",
                  legend       = "Population",
                  num_colors   = 3)






#do the regression analysis to predict the ration of going to university with expenditure in households
ratio_univ <- as.numeric(as.numeric(data.selected[, "Number of those who continue to further school after high school"]))  / as.numeric(data.selected[, "Number of those who graduated from high school"])
add_ratio <- data.frame(ratio_univ)
names(add_ratio) <- c("Ratio of going to the university after graduate from high school")
#data_reg <- cbind(data.selected, add_ratio)
data_reg <- data.selected

data_reg <- data_reg[, -which (colnames(data_reg) %in% c("year", "code of prefecture", "prefecture"
                                                    , "Number of those who graduated from high school",
                                                    "Number of those who continue to further school after high school",
                                                    "Consumption Expenditures"))]

n<-ncol(data_reg)
ix<-1:n
data_reg<-lapply(data_reg[ix],as.numeric)
#確認
data_reg <- data.frame(data_reg)


regression <- lm(ratio_univ~.,data=data_reg)


