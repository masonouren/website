install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")

library(tidyverse)
library(dplyr)
library(tidyr)

#DO WE NEED TO PRINT EVERYTHING AFTER OUR CODE?#################################

# renaming the dataset to easier names
countries <- countries_of_the_world
happiness <- X2017

# Datasets are tidy, so using pivot_longer() to make them untidy then pivot_wider() to make the tidy again. The number of rows doubles when I use pivot_longer() and new variables are created: one with the previous column names, and the other containing the corresponding values.

happiness_untidy <- happiness %>% pivot_longer(cols = c(Whisker.high, Whisker.low), names_to = "untidy")
head(happiness_untidy)
happiness_tidy <- happiness_untidy %>% pivot_wider(names_from = "untidy", values_from = "value")
head(happiness_tidy)

countries_untidy <- countries %>% 
  pivot_longer(cols = c(Birthrate, Deathrate), names_to = "untidy")
head(countries_untidy)
countries_tidy <- countries_untidy %>% 
  pivot_wider(names_from = "untidy", values_from = "value")

# Creating an updated dataset where some of the country names are corrected to match the "happiness" dataset and cause fewer rows to have many NAs.
countries_tidy1 <- countries_tidy %>% 
  rename("Country1" = "Country") %>% 
  mutate(Country = recode(Country1, "Korea, South" = "South Korea", "Trinidad & Tobago" = "Trinidad and Tobago", "Taiwan" = "Taiwan Province of China", "Hong Kong" = "Hong Kong S.A.R., China", "Congo, Repub. of the" = "Congo (Brazzaville)", "Congo, Dem. Rep." = "Congo (Kinshasa)", "Central African Rep." = "Central African Republic")) %>%
  select(-c(Country1))
head(countries_tidy1)

# Creating an updated dataset where some of the country names are corrected to match the "happiness" dataset and cause fewer rows to have many NAs.

countries_tidy1 <- countries_tidy %>% 
  rename("Country1" = "Country") %>% 
  mutate(Country = recode(Country1, "Korea, South" = "South Korea", "Trinidad & Tobago" = "Trinidad and Tobago", "Taiwan" = "Taiwan Province of China", "Hong Kong" = "Hong Kong S.A.R., China", "Congo, Repub. of the" = "Congo (Brazzaville)", "Congo, Dem. Rep." = "Congo (Kinshasa)", "Central African Rep." = "Central African Republic")) %>%
  select(-c(Country1))

# joining the datasets with left_join(); this results in a dataset that has 155 rows and 31 variables
# I chose to left_join() "countries" to "happiness" because most of the "happiness" countries had data in the "countries" dataset, but not all of the "countries" countries had data in the "happiness dataset. Joining in this way created a dataset with fewer rows containing NAs; however, an na.omit() step is still needed.

join <- left_join(happiness_tidy, countries_tidy1, by = "Country")

# Since the "countries" dataset used commas instead of periods for decimals, I needed to replace them with periods to make those columns numeric:

join1 <- apply(apply(join, 2, gsub, patt=",", replace="."), 2, as.numeric)

# Since this made the country names and regions turn into NAs, I'm deleting those columns from join1 using select(-c()), saving as "join2", selecting those two columns from the "join" dataset, saving as "columns", using cbind() to stick them onto the join2 dataset, making the join3 dataset. I also had to turn "join1" into a dataframe because it turned into a matrix.

join1 <- as.data.frame(join1)
join2 <- join1 %>% select(-c(Country), -c(Region))
columns <- join %>% select(Country, Region)
join3 <- cbind(join2, columns)

install.packages("xlsx")
library(xlsx)
write.xlsx(join3, "c:/join3.xlsx")

write.csv(join3, "C:/Downloads/join3.csv")

write.csv(join3, "join3.csv")

write.csv(join3, file = "join3.csv")

write.csv(join, "join.csv")
# I now have a data frame where all of the commas are periods and all variables are correctly characterized (numeric or character).

# for number of rows lost in join, I am using anti_join() and nrow() to determine the number of rows in "countries_tidy1" that are not in "happiness"; I am also doing an anti_join() in the reverse order to see how many rows are in the "happiness" dataset that are not in the "countries_tidy1" dataset.

countries_rows_lost <- anti_join(countries_tidy1, happiness)
print(nrow(countries_rows_lost))
happiness_rows_lost <- anti_join(happiness, countries_tidy1)
print(nrow(happiness_rows_lost))
# 87 rows from the "countries" dataset were not in the "happiness" dataset, and were lost in the join. 8 rows in the "happiness" dataset are not in the "countries" dataset and were not lost in the join, but will be lost if na.omit() is performed. Some other rows will also be lost if na.omit is performed due to incompleteness.

# Filtering for countries with a happiness score that is greater than the mean, and storing in new dataset called "very_happy".
# The resulting dataset has 73 rows, with happiness scores ranging from 5.395 to 7.537.
very_happy <- join3 %>% filter(Happiness.Score, Happiness.Score > mean(Happiness.Score))
print(max(very_happy$Happiness.Score))
print(min(very_happy$Happiness.Score))

# Selecting only the columns for country, happiness rank, happiness score, birth rate, and death rate; results in a 5-column dataset saved as "happy_life_death"
happy_life_death <- join3 %>% select(Country, Happiness.Rank, Happiness.Score, Birthrate, Deathrate)

# Arranging by descending amount of arable land. The observation containing the highest percentage of arable land is at the top, and the lowest is at the bottom.
lots_of_farming <- join3 %>% arrange(desc(`Arable (%)`))

# Grouping by region. This will let me calculate summarizing statistics later according to region.
grouped <- join3 %>% group_by(Region)
head(grouped)

# Adding 2 new columns using mutate(). One contains a function that calculates the Z-score of "Generosity", and the other the percent rank of "Generosity" compared to other countries.
GenerosityZ <- function(x){(x - mean(x))/sd(x)}
join4 <- join3 %>% mutate("GenerosityZ" = GenerosityZ(Generosity), "Generosity.percent.rank" = percent_rank(Generosity))

# Summarizing every variable in the dataset using summarize_all(). I used na.omit() to remove rows that would keep a mean from being generated for a given variable.
summarized <- join4 %>% na.omit %>% summarize_all(mean)

cor <- join3 %>% cor()
cor

# Removing any NA-containing rows from dataset and saving as join5
join5 <- join4 %>% na.omit()

#selecting 5 variables (3 numeric) to generate summary statistics for, and then generating those summary statistics. Then I am reshaping the data to make a better-looking table.
join6 <- join5 %>% select(Country, Region, Generosity, `Phones (per 1000)`, Freedom)

overall_summary <- join6 %>% select(-c(Country, Region)) %>% summarize(mean.generosity = mean(Generosity), sd.generosity = sd(Generosity), var.generosity = var(Generosity), min.generosity = min(Generosity), max.generosity = max(Generosity), ndistinct.generosity = n_distinct(Generosity),
                                                                       mean.phones = mean(`Phones (per 1000)`), sd.phones = sd(`Phones (per 1000)`), var.phones = var(`Phones (per 1000)`), min.phones = min(`Phones (per 1000)`), max.phones = max(`Phones (per 1000)`), ndistinct.phones = n_distinct(`Phones (per 1000)`),
                                                                       mean.freedom = mean(Freedom), sd.freedom = sd(Freedom), var.freedom = var(Freedom), min.freedom = min(Freedom), max.freedom = max(Freedom), ndistinct.freedom = n_distinct(Freedom)) %>%
  pivot_longer(cols = (1:18)) %>%
  separate(name, into = c("stat", "variable")) %>%
  pivot_wider(names_from = "variable", values_from = "value")

# Now grouping those 3 numeric variables by Region, calculating the same statistics, and then reshaping to make it nice to read.
options(scipen=999) #disabling scientific notation

grouped_summary <- join6 %>% select(-c(Country)) %>% group_by(Region) %>% 
  summarize(mean.generosity = mean(Generosity), sd.generosity = sd(Generosity), var.generosity = var(Generosity), min.generosity = min(Generosity), max.generosity = max(Generosity), ndistinct.generosity = n_distinct(Generosity),
   mean.phones = mean(`Phones (per 1000)`), sd.phones = sd(`Phones (per 1000)`), var.phones = var(`Phones (per 1000)`), min.phones = min(`Phones (per 1000)`), max.phones = max(`Phones (per 1000)`), ndistinct.phones = n_distinct(`Phones (per 1000)`),
   mean.freedom = mean(Freedom), sd.freedom = sd(Freedom), var.freedom = var(Freedom), min.freedom = min(Freedom), max.freedom = max(Freedom), ndistinct.freedom = n_distinct(Freedom)) %>%
  pivot_longer(cols = (2:19)) %>%
  separate(name, into = c("stat", "variable")) %>%
  pivot_wider(names_from = "variable", values_from = "value")

#VISUALIZATIONS

free.lit.gdp <- join5 %>% ggplot(aes(x = Freedom, y = `GDP ($ per capita)`, color = `Literacy (%)`)) +
  geom_point(stat="summary")+
  ggtitle("GDP vs. Freedom and Literacy")+
  scale_color_gradient2(midpoint=50, low="black", mid="blue",
                            high="green", space ="Lab" ) +
  theme_classic()
free.lit.gdp

happy.death.mig <- join5 %>% ggplot(aes(y = Happiness.Score, x = Deathrate, color = `Literacy (%)`)) +
  geom_point() +
  ggtitle("Happiness Score vs. Deathrate and Literacy")+
  scale_color_gradient2(midpoint=50, low="blue", mid="orange",
                        high="green", space ="Lab" ) +
  theme_dark() + 
  scale_x_continuous(breaks = seq(0, 35, 5))
happy.death.mig

#####################################################################################
#CORRELATION HEATMAP
correlation <- join5 %>% select(Happiness.Score, Freedom, Deathrate, `GDP ($ per capita)`, `Literacy (%)`, `Phones (per 1000)`) %>% cor
correlation
library(reshape2)

melted_correlation <- melt(correlation)
head(melted_correlation)

heatmap <- ggplot(data = melted_correlation, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))+
  ggtitle("Correlation Heatmap")
  coord_fixed()
heatmap
#################################################################################
#PCA

world_nums <- join5 %>% select_if(is.numeric) %>% scale
rownames(world_nums) <- join5$Name
world_pca <- princomp(world_nums)
names(world_pca)

summary(world_pca, loadings = T)

eigval <- world_pca$sdev^2
varprop <- round(eigval/sum(eigval), 2)

scree <- ggplot() +
  geom_bar(aes(y=varprop, x=1:31), stat = "identity") +
  xlab("") +
  geom_path(aes(y=varprop, x=1:31)) +
  geom_text(aes(x=1:31, y=varprop, label = round(varprop, 2)), vjust=1, col="white", size=5) +
  scale_y_continuous(breaks = seq(0, .4, .1), labels = scales::percent) +
  scale_x_continuous(breaks = 1:31) +
  ggtitle("Scree Plot of Principle Components")
scree

eigval

PC12 <- ggplot() +
  geom_point(aes(world_pca$scores[,1], world_pca$scores[,2], color = join5$Region)) +
  xlab("PC1") +
  ylab("PC2")
PC12

PC34 <- ggplot() +
  geom_point(aes(world_pca$scores[,3], world_pca$scores[,4], color = join5$Region)) +
  xlab("PC3") +
  ylab("PC4")
PC34

world_pca$loadings[1:31, 1:2]%>%as.data.frame%>%rownames_to_column%>%
  ggplot()+geom_hline(aes(yintercept=0),lty=2)+
  geom_vline(aes(xintercept=0),lty=2)+ylab("PC2")+xlab("PC1")+
  geom_segment(aes(x=0,y=0,xend=Comp.1,yend=Comp.2),arrow=arrow(),col="green")+
  geom_label(aes(x=Comp.1*1.1,y=Comp.2*1.1,label=rowname))

install.packages("factoextra")
library(factoextra)
biplot <- fviz_pca_biplot(world_pca,col.ind = join5$Happiness.Rank,)+coord_fixed()
biplot
#generally, happiest countries score lowest on PC1.














