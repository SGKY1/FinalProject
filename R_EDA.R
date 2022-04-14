# Install libraries and load the data

install.packages("ggnewscale")
install.packages("isotree")
install.packages("data.table")
install.packages("tidyverse")
install.packages("scales")
install.packages("factoextra")
install.packages("data.tree")
install.packages("ggrepel")
install.packages("cowplot")
install.packages("tidytext")
install.packages("RColorBrewer")
install.packages('Rcpp')

library(stringr)
library(tidytext)
library(cowplot)
library(ggrepel)
library(data.tree)
library(factoextra)
library(scales)
library(tidyverse)
library(data.table)
library(isotree)
library(ggnewscale)
library(RColorBrewer)
library(Rcpp)

rm(list=ls())
theme_set(theme_light())
plot_colors = c(brewer.pal(n = 9, name = "Pastel1"), brewer.pal(n = 9, name = "Pastel1")[9])

data = fread("data.csv")
input = fread("data.csv")

# Edit year of birth entries in age column

View(head(input[order(-input$age),], 10))
input[age > 1000, age := 2019 - age]

# Edit family size to be at least 1

input[familysize == 0, familysize := 1]

# Convert categorical columns to factors

input[, gender := factor(gender, labels = c("Unknown", "Male", "Female", "Other"))]
input[, education := factor(education, labels = c("Unknown", "Less than high school", "High school", "University degree", "Graduate degree"))]
input[, urban := factor(urban, labels = c("Unknown", "Rural (country side)", "Suburban", "Urban (town, city)"))]
input[, gender := factor(gender, labels = c("Unknown", "Male", "Female", "Other"))]
input[, engnat := factor(engnat, labels = c("Unknown", "Yes", "No"))]
input[, hand := factor(hand, labels = c("Unknown", "Right", "Left", "Both"))]
input[, orientation := factor(orientation, labels = c("Unknown", "Heterosexual", "Bisexual", "Homosexual", "Asexual", "Other"))]
input[, race := factor(race, labels = c("Asian", "Arab", "Black", "Indigenous Australian", "Native American", "White", "Other"))]
input[, religion := factor(religion, labels = c("Unknown", "Agnostic", "Atheist", "Buddhist", "Catholic", "Mormon", "Protestant", "Christian (Other)", "Hindu", "Jewish", "Muslim", "Sikh", "Other"))]
input[, married := factor(married, labels = c("Unknown", "Never married", "Currently married", "Previously married"))]

# Prepare data for analysis

question_cols = names(input)[grepl("^Q.*A$", names(input))]
vcl_cols = names(input)[grepl("^VCL", names(input))]
tipi_cols = names(input)[grepl("^TIPI", names(input))]

input[, `TIPI score` := rowSums(.SD), .SDcols=tipi_cols]
input[, `DAS score` := rowSums(.SD), .SDcols=question_cols]
input[, `Vocab. score` := rowSums(.SD), .SDcols=vcl_cols]

iso_data = input[, .(`TIPI score`, `DAS score`, `Vocab. score`, age, education, urban, gender, hand, orientation, familysize, engnat, race, religion, married)]

cat_cols = c("education", "urban", "gender", "hand", "orientation", "engnat", "race", "religion", "married")

# Brief look at how numerical variables are distributed

num_plot_bef_dt = iso_data[, -..cat_cols]
num_cols = names(num_plot_bef_dt)
num_plot_bef_dt[, names(num_plot_bef_dt) := lapply(.SD, as.integer), .SDcols = names(num_plot_bef_dt)]
num_plot_bef_dt = melt(num_plot_bef_dt, id.vars=integer())

ggplot(num_plot_bef_dt, aes(x = value, fill = variable)) +
  geom_bar(width = 1) +
  facet_wrap(variable ~ ., scales = "free", nrow = 2) +
  scale_fill_manual(guide="none", values = plot_colors) +
  scale_y_continuous(label = comma) + 
  labs(title = "Distribution of numerical variables") +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(color = "black"),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

## There are clearly some outliers in age and family size otherwise the
## densities would not exceed 200 and 100 respectively.
## The TIPI score is centered around 44.
## Given the design of the TIPI questionnaire this makes sense because each
## statement has an opposite e.g. "extroverted, enthusiastic" versus "reserved,
## quiet", and each item is scored on a 1 to 7 scale. Provided we have 10 items
## and half are reverse-scored, the expected total score for each pair of 
## opposing statements is 4, equating to an expected overall score of 40. So 44
## is not too far off our expectations; expectations do not hold fully given
## human factor involved.

# Brief look at how categorical variables are distributed

cat_plot_bef_dt = iso_data[, ..cat_cols]
cat_plot_bef_dt = melt(cat_plot_bef_dt, id.vars = integer())
cat_plot_bef_dt = cat_plot_bef_dt[, .(Participants = .N), by= .(variable, value)]
cat_plot_bef_dt[, value := str_wrap(value, 15)]
cat_plot_bef_dt[, value := reorder_within(value, Participants, variable)]

ggplot(cat_plot_bef_dt, aes(x = value, fill = variable, y = Participants)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(add=15)) +
  scale_x_reordered() +
  scale_fill_manual(guide="none", values = plot_colors) +
  facet_wrap(variable ~ ., scales = "free", nrow = 3) +
  labs(title = "Distribution of categorical variables") +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(color = "black"),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(10, 50, 10, 10))

## The participants are mostly female and unmarried.
## Most participants identify as heterosexual and are right-handed.
## The participants mostly belong to Muslim, Asian backgrounds.

# Convert categorical variables to ordinal integer values starting from 0
# Run isolation forest

isfo = isolation.forest(iso_data, seed = 1)
pred = predict(isfo, iso_data)

h_end = round(max(pred)/0.05)*0.05
h_start = round(min(pred)/0.05)*0.05 - 0.05
if (h_start %/% 2 != 0) {
  h_start = h_start - 0.01
}

ggplot(data.frame(pred), aes(x = pred, fill = ifelse(pred <= 0.5, "safe", "outlier"))) +
  geom_histogram(breaks=seq(h_start, h_end, 0.02)) +
  geom_vline(aes(xintercept = 0.5), size = 1, colour = "red") +
  scale_y_continuous(label = comma, expand = expansion(add = 2)) +
  scale_fill_manual(values = c("safe" = "blue", "outlier"="cyan"), guide="none") +
  labs(title="Histogram of outlier scores", x = "Outlier score", y = "Participants") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

length(which(pred >= 0.5))

## There are 251 observations with an outlier score of 0.5 or more
## We set the cutoff score to be 0.5.

# Difference in distribution between anomalous and normal numerical samples

iso_data[, score:=pred]
iso_data[, anomaly := ifelse(score >= 0.5, "Anomalous", "Normal")]

plot_cols = c(num_cols)
num_plot_comp_dt = iso_data[, ..plot_cols]

num_plot_comp_dt[, setdiff(names(num_plot_comp_dt), "anomaly") := lapply(.SD, as.integer), .SDcols = setdiff(names(num_plot_comp_dt), "anomaly")]

num_plot_comp_dt = melt(num_plot_comp_dt, id.vars="anomaly")
num_plot_comp_dt[, total := sum(value), by = .(anomaly, variable)]
num_plot_comp_dt[, percent := value / total]

num_plot_comp_dt[, y_min := 0]
num_plot_comp_dt[, y_max := 1.1*max(percent), by = .(variable)]


plot_list = list()

list_counter = 1


for (plot_var in c("TIPI score", "DAS score", "Vocab. score", "age", "familysize")){
  
  plot_list[[list_counter]] = ggplot(num_plot_comp_dt[variable == plot_var], 
                                     aes(x = value, fill = variable, y = percent)) +
    geom_bar(stat= "identity", width=1) +
    scale_fill_manual(guide="none", values = plot_colors[list_counter]) +
    facet_wrap(. ~  anomaly,  nrow=1) +
    labs(subtitle = plot_var) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle=element_text(size=10), 
          strip.text = element_text(color = "black"),
          strip.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line  = element_line(color = 'gray'),
          axis.title.y = element_blank(),
          axis.title.x = element_blank())
  list_counter = list_counter + 1
}

plot_list[[1]] = plot_list[[1]] + ggtitle("Numerical features by sample category")

plot_grid(plotlist = plot_list, ncol = 1)

## Normal distributions are noticeably smoother than anomalous ones.
## Extreme values in anomalous distributions tend to be skewed to the right.
## Anomalous distributions are more spread out for age and family size while 
## the normal distribution for age has a range 15 to 75.

# Difference in distribution between anomalous and normal categorical samples

plot_cols = c(cat_cols, "anomaly")

cat_plot_dt = iso_data[, ..plot_cols]
cat_plot_dt = melt(cat_plot_dt, id.vars = "anomaly")
cat_plot_dt = cat_plot_dt[, .(Participants = .N), by = .(anomaly, variable, value)]
cat_plot_dt[, total_var_participants := sum(Participants), by = .(anomaly, variable)]
cat_plot_dt[, percent := Participants/total_var_participants]
cat_plot_dt[, value := str_wrap(value, 15)]
cat_plot_dt[, value := reorder_within(value, percent, list(variable))]

ggplot(cat_plot_dt, aes(x = value, fill = variable, y= percent)) +
  geom_col() +
  coord_flip() +
  facet_grid(variable ~ anomaly, scales = "free") +
  scale_y_continuous(labels = percent, expand = expansion(add=0.01)) +
  scale_x_reordered() +
  scale_fill_manual(guide="none", values = plot_colors) +
  labs(title = "Categorical features by sample category") +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(color = "black"),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

## Unknown entries appear to constitute a larger portion of anomalous samples.
## Otherwise there are no significant differences in the distributions of
## anomalous and normal categorical variables.

# Survey participation summary plot

# Filtering out outliers

input = input[pred <= 0.5]

country_gen_data = input[, .(Participants = .N), by = .(country, gender)][, country_part := sum(Participants), by=country]
top_countries =  country_gen_data[, max(country_part), by = country][order(-V1), country][1:10]

country_gen_chart = country_gen_data[, .(Participants=sum(Participants)), by=.(country= ifelse(country %in% top_countries, country, "Other"), gender)]
country_gen_chart[, country_part := sum(Participants), by=country]

ggplot(country_gen_chart, aes(x = reorder(country, country_part), y = Participants, fill=gender)) +
  geom_col(width=0.75) +
  coord_flip() +
  scale_y_continuous(label = comma, expand = expansion(add = 100)) +
  scale_fill_manual(values= c("Male"="#647C32", "Female"="#ccccff", "Other"="wheat", "Unknown"="grey")) +
  #scale_x_discrete(expand=c(-1, 0)) +
  labs(title = "Survey participants by country and gender", x = NULL) +
  theme(strip.text = element_text(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom  = element_line(color = 'gray'),
        axis.line.y.left  = element_line(color = 'gray'),
        legend.position="bottom",
        text=element_text(family="sans"))

## Again it is clear that the data collected is centered around Malaysian women.

input[, ID:= 1:.N]

survey_raw = input[, .SD, .SDcols = c("ID", names(input)[grepl("^Q", names(input))])]
melted_survey = melt(survey_raw, id.vars = "ID", variable.name="Item", value.name = "Response")
melted_survey[, c("Question", "Response type") := list(str_sub(Item,2,-2), str_sub(Item, start=-1))]
melted_survey[, Item := NULL]
survey_dat = dcast(melted_survey, ID + Question ~ `Response type`, value.var = "Response")
names(survey_dat) = c("ID", "Question", "Answer", "Elapsed_time", "Order")
survey_dat[, Answer := Answer - 1]

# Data checks

print(paste("All answers between 0 and 3:", survey_dat[, all(Answer %in% 0:3)]))
print(paste("All times are numeric:", survey_dat[, all(!is.na(Elapsed_time))]))
print(paste("All order values between 1 and 42:", survey_dat[, all(Order %in% 1:42)]))

# How replies varied by question

avg_question_score = survey_dat[, .(`Average reply` = mean(Answer)), by = Question]
avg_question_score[, Rank := rank(`Average reply`)]
avg_question_score[, Color := ifelse(Rank %in% 1:6, "High", 
                                     ifelse(Rank %in% (nrow(avg_question_score)-1):nrow(avg_question_score), "Low", "Mid"))]
avg_reply = avg_question_score[, mean(`Average reply`)]

ggplot(avg_question_score, aes(x = reorder(Question, `Average reply`), y = `Average reply`, fill = Color)) +
  geom_col(width = 0.75) + 
  geom_hline(yintercept = avg_reply, linetype="dashed", color = "black") +
  annotate("text", x = 6, y = avg_reply + 0.05, label = "Mean reply score", size=6, colour="slategrey") + 
  scale_fill_manual(values = c("High" = muted("green"), "Mid"="gray", "Low"=muted("red")), guide=NULL) +
  scale_y_continuous(expand=c(0,0)) +
  labs(title = "Average reply score by question number") +
  theme(strip.text = element_text(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom  = element_line(color = 'gray'),
        axis.line.y.left  = element_line(color = 'gray'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

## The average response for questions 23, 15, 7, 19, 4, 41 was "Did not apply 
## to me at all", suggesting for most respondents symptoms of DAS did not
## manifest physically.
## Questions 13 and 11 in particular had average responses closer to "Apply to 
## me very much/ most of the time", so most respondents often felt sad and upset.

# Calculate total score from relevant questions for each DAS state 

survey_dat[, Category := ifelse(Question %in% c(3,5,10,13,16,17,21,24,26,31,34,37,38,42), "Depression",
                                ifelse(Question %in% c(2,4,7,9,15,19,20,23,25,28,30,36,40,41), "Anxiety", "Stress"))]
scoring_dat = survey_dat[, .(Score=sum(Answer)), by=.(ID, Category)]

assessment_labels = c("Normal", "Mild", "Moderate", "Severe", "Extremely Severe")
scoring_dat[Category == "Depression", Assessment := cut(Score, breaks=c(-1,9,13,20,27,42), labels=assessment_labels, ordered_result=T)]

scoring_dat[Category == "Anxiety", Assessment := cut(Score, breaks=c(-1,7,9,14,19,42), labels=assessment_labels, ordered_result=T)]

scoring_dat[Category == "Stress", Assessment := cut(Score, breaks=c(-1,14,18,25,33,42), labels=assessment_labels, ordered_result=T)]

scoring_dat = scoring_dat[input[, .(gender,age,married,ID)], on="ID"]

# Plot number of responses for each severity range on the DAS scale

scoring_category_chart = scoring_dat[, .(Participants = .N), by=.(Category, Assessment)]
pal = colorRampPalette(c("palegreen3", muted("red")))

ggplot(scoring_category_chart, aes(x=Assessment, y=Participants, fill=Assessment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(Category ~ ., nrow=3, ncol=1, scales="free") +
  scale_fill_discrete(guide=NULL, palette=pal) +
  scale_y_continuous(label=comma, expand = expansion(add=100), limits=c(0, 14500)) + 
  labs(title = "Participants by severity of emotional state") + 
  theme(strip.text = element_text(size=13, color = "black"),
        strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom  = element_line(color = 'gray'),
        plot.title = element_text(hjust=-0.6))

## Based on the results of the survey, the breakdown for stress seems to be more
## than that of depression and anxiety and most participants have normal levels
## of stress but extremely severe anxiety and depression.

# Calculate correlation between question scores for questions in the same
# emotional state e.g. to find whether a high score in one question relating to
# depression would correlate to a high score in another question in the same 
# category

cor_dat = dcast(survey_dat, ID ~ Question, value.var = "Answer")
correlations = cor(cor_dat[, -"ID"])

cor_plot_data = reshape2::melt(correlations)
setDT(cor_plot_data)

names(cor_plot_data) = c("Question_1", "Question_2", "Correlation")

cor_plot_data[unique(survey_dat[, .(Question=as.numeric(Question), Category)]), on=c(Question_1="Question"), Category_1:=i.Category]

cor_plot_data[unique(survey_dat[, .(Question=as.numeric(Question), Category)]), on=c(Question_2="Question"), Category_2:=i.Category]

cor_plot_data = cor_plot_data[Question_1 != Question_2, mean(Correlation), by = .(Category_1, Category_2)]
cor_plot_data[, dupl := 1:.N, by=round(V1,4)]
cor_plot_data[dupl==2, V1:=NA][, dupl:=NULL]

# Plot correlation matrix

ggplot(cor_plot_data, aes(x=Category_2, y=Category_1, fill=V1)) +
  geom_raster() +
  geom_text(aes(label=round(V1,2)), colour = "white", size=5) +
  scale_fill_continuous(low="skyblue", high="darkblue", na.value="white",guide="none") +
  scale_x_discrete(expand = expansion(add=0.1)) +
  scale_y_discrete(expand = expansion(add=0.1)) +
  labs(title=str_wrap("Average correlation between questions by emotional state", 40)) +
  theme(plot.title = element_text(size = 12),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank())

## Overall there is some positive correlation between questions, the strongest 
## being for questions in the depression category.

tipi_an_cols = c("ID", "age", "gender", tipi_cols)

tipi_dt = input[, ..tipi_an_cols]
tipi_dt[, (tipi_cols) := lapply(.SD, as.integer), .SDcols = tipi_cols]
tipi_dt = melt(tipi_dt, id.vars = c("ID", "age", "gender"), variable.name = "Item", value.name = "Score")
tipi_dt[, Item := as.integer(gsub("TIPI", "", Item))]

# Preparing age groups
tipi_dt[, `Age group` := cut(age, breaks = c(1, 18, 25, 35, 100), labels = c("Below 18", "18-24", "25-34", "35 and above"), right = F)]

# Item category
tipi_dt[Item %in% c(1, 6), Category := "Extraversion"]
tipi_dt[Item %in% c(2, 7), Category := "Agreeableness"]
tipi_dt[Item %in% c(3, 8), Category := "Conscientiousness"]
tipi_dt[Item %in% c(4, 9), Category := "Emotional Stability"]
tipi_dt[Item %in% c(5, 10), Category := "Openness"]

# Reversing opposite scores
tipi_dt[, adjusted_score := Score]
tipi_dt[Item %in% c(6, 2, 8, 4, 10), adjusted_score := 7-adjusted_score]

# Plot TIPI score against gender

tipi_gender = tipi_dt[, .(Score = mean(adjusted_score)), by = .(Category, gender)]
tipi_gender[, Score := round(Score, 2)]


ggplot(tipi_gender, aes(x = Category, y = Score, fill = gender)) +
  geom_col(position="dodge") +
  coord_flip() +
  scale_y_continuous(expand = expansion(add=0), limits = c(0,7), breaks = c(1, 3,5, 7)) +
  scale_fill_manual(values= c("Male"="#647C32", "Female"="#ccccff", "Other"="wheat", "Unknown"="grey")) +
  labs(title = "TIPI score by gender") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        #plot.title = element_text(face = "italic", color = "black"),
        legend.title = element_blank(),
        legend.position = "bottom")

## People who identify as non-binary score are, based on the results of this
## survey, less extroverted and less emotionally stable.
## Men score comparatively higher than women on emotional stability.

# Plot TIPI score against age

tipi_age = tipi_dt[, .(Score = mean(adjusted_score)), by = .(Category, `Age group`)]
tipi_age[, Score := round(Score, 2)]


ggplot(tipi_age, aes(x = Category, y = Score, fill = `Age group`)) +
  geom_col(position="dodge") +
  coord_flip() +
  scale_y_continuous(expand = expansion(add=0), limits = c(0,7), breaks = c(1, 3,5, 7)) +
  scale_fill_manual(values= c("#C7E9C0", "#74C476", "#238B45", "#00441B")) +
  labs(title = "TIPI score by age group") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        #plot.title = element_text(face = "italic", color = "black"),
        legend.title = element_blank(),
        legend.position = "bottom")

## Older age groups scored higher for each personality trait, they appear to
## view themselves more positively.
## The score for emotional stability is significantly low for under 18s.

# Update the dataset with anomalous rows removed and save

data_new <- data[!rownames(data) %in% c(which(pred >= 0.5)), ]
fwrite(data_new, "data_new.csv")
