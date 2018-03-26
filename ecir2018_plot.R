library(dplyr)
library(ggplot2)

### PLOT CLEF RESULTS

# read CLEF runs
# downloaded from 
# https://github.com/CLEF-TAR/tar/blob/master/README.md
results_directory <- "./runs/participant-results-abstract/"
runs_names <- list.files(path = results_directory)

# set data frame column names
column_names <- c("topic", "measure", "value", "name")

# select runs with threshold only (see overview paper Table 4)
runs_threshold <- runs_names[c(13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 35, 36, 43, 44, 45, 46, 50, 52)]

# build dataframe
runs <- data.frame(topic = character(),
                   measure = character(),
                   value = numeric(),
                   name = character(),
                   stringsAsFactors = FALSE)

# read runs
for(run_name in runs_threshold) {
  
  # read run
  run_result <- read.table(file = paste(results_directory, run_name, sep = "/"), 
                           header = FALSE, 
                           sep = "\t", 
                           stringsAsFactors = FALSE)
  
  # add name of run to the dataframe
  run_result <- mutate(run_result, run = run_name)
  names(run_result) <- column_names
  # add run to 
  runs <- rbind(runs, run_result)
}

# subset rows relative to average results (ALL)
runs_clef_all_topics <- runs %>% filter(topic == "ALL")

# subset rows relative to NCG
runs_clef_all_topics_ncg <- runs_clef_all_topics %>% filter(grepl(pattern = "NCG*", 
                                                                  x = runs_clef_all_topics$measure))

# set ncg levels
levels_ncg <- c("NCG@10", "NCG@20", "NCG@30", "NCG@40", "NCG@50",
                "NCG@60", "NCG@70", "NCG@80", "NCG@90", "NCG@100")

# transform column type
runs_clef_all_topics_ncg$value <- as.numeric(runs_clef_all_topics_ncg$value)
runs_clef_all_topics_ncg$measure <- factor(runs_clef_all_topics_ncg$measure, levels = levels_ncg)

# plot clef results NCG@threshold
gp <- ggplot(data = runs_clef_all_topics_ncg, 
             aes(x = measure, y = value, colour = name, group = name)) + 
  geom_line() + geom_point() +
  ylim(0.3, 1.0)
print(gp)

# subset CLEF rows relative to NCG@100 and NOT ALL
runs_ncg100 <- runs %>% filter(topic != "ALL" & measure == "NCG@100")

runs_ncg100$value <- as.numeric(runs_ncg100$value)

ggplot(runs_ncg100, aes(x = topic, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("ncg100")

# get number of documents shown per run
runs_clef_all_topics_num_shown <- runs_clef_all_topics %>% filter(measure == "num_shown")
runs_clef_all_topics_num_shown$value <- as.numeric(runs_clef_all_topics_num_shown$value)

# get NCG100 at documents shown
runs_clef_all_topics_ncg100 <- inner_join(filter(runs_clef_all_topics_ncg, measure == "NCG@100"), 
                                          runs_clef_all_topics_num_shown,
                                          by = c("name"))
runs_clef_all_topics_ncg100 <- runs_clef_all_topics_ncg100 %>% select(value.y, value.x, name) %>% rename(ncg100 = value.x, docs_shown = value.y)

# get best ncg100 runs 
runs_clef_all_topics_ncg100_best <- runs_clef_all_topics_ncg100[c(10, 5, 11, 4, 15, 16, 18), ]


### PLOT IMS RESULTS

# read IMS runs
results_ims_directory <- "./runs/ims/"

runs_ims_names <- list.files(path = results_ims_directory)

column_names <- c("topic", "measure", "value", "name")

# build dataframe
runs_ims <- data.frame(topic = character(),
                       measure = character(),
                       value = numeric(),
                       name = character(),
                       stringsAsFactors = FALSE)

# read all files
for(run_ims_name in runs_ims_names) { #runs_ims_threshold) {
  # read run
  #run_ims_name <- runs_ims_ims_names[32]
  run_ims_result <- read.table(file = paste(results_ims_directory, run_ims_name, sep = "/"), 
                               header = FALSE, 
                               sep = " ", 
                               stringsAsFactors = FALSE, fill = TRUE)
  
  # add name of run to the dataframe
  run_ims_result <- mutate(run_ims_result, run = run_ims_name)
  names(run_ims_result) <- column_names
  # add run to 
  runs_ims <- rbind(runs_ims, run_ims_result)
}

# subset rows relative to average results (ALL)
runs_ims_all_topics <- runs_ims %>% filter(topic == "ALL")

# subset rows relative to NCG
runs_ims_all_topics_ncg <- runs_ims_all_topics %>% filter(grepl(pattern = "NCG*", x = runs_ims_all_topics$measure))

runs_ims_all_topics_ncg$value <- as.numeric(runs_ims_all_topics_ncg$value)
runs_ims_all_topics_ncg$measure <- factor(runs_ims_all_topics_ncg$measure, levels = levels_ncg)

# subset rows relative to number of documents shown
runs_ims_all_topics_num_shown <- runs_ims_all_topics %>% filter(measure == "num_shown")
runs_ims_all_topics_num_shown$value <- as.numeric(runs_ims_all_topics_num_shown$value)

# subset rows relative to NCG@100
runs_ims_all_topics_ncg100 <- inner_join(filter(runs_ims_all_topics_ncg, measure == "NCG@100"), 
                                         runs_ims_all_topics_num_shown,
                                         by = c("name"))

# get ncg@100 at documents shown
runs_ims_all_topics_ncg100 <- runs_ims_all_topics_ncg100 %>% 
  select(value.y, value.x, name) %>% 
  rename(ncg100 = value.x, docs_shown = value.y)

# plot Pareto and IMS runs
runs_clef_all_topics_ncg100 %>%
  ggplot(aes(x = docs_shown, y = ncg100)) +
  geom_point(alpha = 0.4) + 
  geom_line(data = runs_clef_all_topics_ncg100_best,
            linetype = "dotted") + 
  geom_point(data = runs_clef_all_topics_ncg100_best, 
             aes(colour = name)) +
  geom_point(data = runs_ims_all_topics_ncg100)

# subset rows relative to NCG@100 and NOT ALL
runs_ims_ncg100 <- runs_ims %>% filter(topic != "ALL" & measure == "NCG@100")
runs_ims_ncg100$value <- as.numeric(runs_ims_ncg100$value)

# subset runs with threshold t = 500
runs_ims_ncg100_t500 <- runs_ims_ncg100 %>% filter(grepl(runs_ims_ncg100$name, pattern = "t500"))

ggplot(runs_ims_ncg100, aes(x = topic, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("ncg100 at threshold") + 
  geom_line(data = runs_ims_ncg100_t500, aes(group = name), colour = "#3366FF", alpha = 0.5)


######### CLEF VS IMS threshold 500

# Documents Shown
# subset clef rows relative to docs_shown and NOT ALL
runs_clef_num_shown <- runs %>% filter(topic != "ALL" & measure == "num_shown")
runs_clef_num_shown$value <- as.numeric(runs_clef_num_shown$value)

# subset rows relative to docs_shown and NOT ALL
runs_ims_num_shown <- runs_ims %>% filter(topic != "ALL" & measure == "num_shown")
runs_ims_num_shown$value <- as.numeric(runs_ims_num_shown$value)
# subset ims runs with t = 500
runs_ims_num_shown_t500 <- runs_ims_num_shown %>% filter(grepl(runs_ims_num_shown$name, pattern = "t500"))

# plot boxplot results. CLEF runs vs t500 (in blue)
ggplot(runs_clef_num_shown, aes(x = topic, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_line(data = runs_ims_num_shown_t500, aes(group = name), colour = "#3366FF", alpha = 0.5) +
  ylab("documents shown") 


# Average Precision
# subset clef rows relative to average precision and NOT ALL
runs_clef_ap <- runs %>% filter(topic != "ALL" & measure == "ap")
runs_clef_ap$value <- as.numeric(runs_clef_ap$value)

# subset rows relative to docs_shown and NOT ALL
runs_ims_ap <- runs_ims %>% filter(topic != "ALL" & measure == "ap")
runs_ims_ap$value <- as.numeric(runs_ims_ap$value)
# subset ims runs with t = 500
runs_ims_ap_t500 <- runs_ims_ap %>% filter(grepl(runs_ims_ap$name, pattern = "t500"))

# plot boxplot results. CLEF runs vs t500 (in blue)
ggplot(runs_clef_ap, aes(x = topic, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_line(data = runs_ims_ap_t500, aes(group = name), colour = "#3366FF", alpha = 0.5) +
  ylab("average precision") 


# Recall
# subset clef rows relative to average precision and NOT ALL
runs_clef_r <- runs %>% filter(topic != "ALL" & measure == "r")
runs_clef_r$value <- as.numeric(runs_clef_r$value)

# subset rows relative to docs_shown and NOT ALL
runs_ims_r <- runs_ims %>% filter(topic != "ALL" & measure == "r")
runs_ims_r$value <- as.numeric(runs_ims_r$value)
# subset ims runs with t = 500
runs_ims_r_t500 <- runs_ims_r %>% filter(grepl(runs_ims_r$name, pattern = "t500"))

# plot boxplot results. CLEF runs vs t500 (in blue)
ggplot(runs_clef_r, aes(x = topic, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_line(data = runs_ims_r_t500, aes(group = name), colour = "#3366FF", alpha = 0.5) +
  ylab("recall") 


##############

# subset clef rows relative to recall
runs_clef_all_topics_r <- runs_clef_all_topics %>% 
  filter(measure == "r") %>%
  inner_join(runs_clef_all_topics_num_shown, by = c("name")) %>%
  select(value.y, value.x, name) %>% 
  rename(recall = value.x, docs_shown = value.y) %>%
  mutate_at(c("recall"), as.numeric)

runs_clef_all_topics_r_best <- runs_clef_all_topics_r[c(10, 6, 5, 11, 4, 3, 19, 18), ]

# subset ims rows relative to recall
runs_ims_all_topics_r <- runs_ims_all_topics %>% 
  filter(measure == "r") %>%
  inner_join(runs_ims_all_topics_num_shown, by = c("name")) %>%
  select(value.y, value.x, name) %>% 
  rename(recall = value.x, docs_shown = value.y) %>%
  mutate_at(c("recall"), as.numeric)

# plot Pareto and IMS runs
runs_clef_all_topics_r %>%
  ggplot(aes(x = docs_shown, y = recall)) +
  geom_point(alpha = 0.4) + 
  geom_line(data = runs_clef_all_topics_r_best,
            linetype = "dotted") + 
  geom_point(data = runs_clef_all_topics_r_best, 
             aes(colour = name)) +
  geom_point(data = runs_ims_all_topics_r)
