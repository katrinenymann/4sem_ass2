Making functions

``` r
# setup
pacman::p_load(pacman, 
               tidyverse, 
               rethinking,
               patchwork) # for ordering plots together

# We make a df for our observations
results_df <- data.frame(teacher = c("RF", "KT", "JS", "MW"),
                         correct = c(3, 2, 160, 66), 
                         n_quest = c(6, 2, 198, 132))

#We define p_grid
p_grid <-   p_grid <- seq(0,1, length.out = 10000)
#We define the bin_size
bin_size <- abs(p_grid[1] - p_grid[2])
#We define an informed prior
inf_prior <- dbeta(p_grid, 8, 4)

#We make a function
calc_teacher <- function(n_correct, n_question, prior, length_out = 10000){
  # this function calculate the posterior
  p_grid <- seq(0,1, length.out = 10000)
  likelihood <- dbinom(n_correct, 
                       size = n_question, 
                       prob = p_grid)
  unstd_posterior <- prior * likelihood
  bin_size <- abs(p_grid[1] - p_grid[2])
  posterior <- unstd_posterior/sum(unstd_posterior * bin_size)
  return(list(teacher_posterior = posterior, 
              likelihood = likelihood,
              grid = p_grid))
}

# We use the function
kt_results <- calc_teacher(2, 2, prior = inf_prior)
# We plot it
plot(kt_results$grid, kt_results$teacher_posterior)
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# We make a function to make a pretty plot
pretty_plot <- function(p_grid, prior, likelihood, posterior, title = " "){
  # define data
  d <- tibble(p_grid = p_grid, 
              prior = prior, 
              likelihood = likelihood,
              posterior = posterior)
  
  # make to long format
  d <- d %>% 
    pivot_longer(cols = c("prior", "likelihood", "posterior"), names_to = "name", values_to = "value")
  
  # make a 
  p <- ggplot(d, aes(x = p_grid, y = value, color = name)) + 
    geom_line() + 
    labs(x = "x", y = "Density", title = title) + 
    theme_bw() + 
    ggplot2::theme(panel.background = element_rect(fill = "white"),
                   panel.border = element_blank()) +
    scale_colour_brewer(palette = "Dark2", direction = 1)
  return(p)
}
```

In this assignment we learn how to assess rates from a binomial distribution, using the case of assessing your teachers’ knowledge of CogSci
--------------------------------------------------------------------------------------------------------------------------------------------

N.B. there is a second part at the bottom for next week.

### First part

You want to assess your teachers’ knowledge of cognitive science. “These
guys are a bunch of drama(turgist) queens, mindless philosophers,
chattering communication people and Russian spies. Do they really know
CogSci?”, you think.

To keep things simple (your teachers should not be faced with too
complicated things): - You created a pool of equally challenging
questions on CogSci - Each question can be answered correctly or not (we
don’t allow partially correct answers, to make our life simpler). -
Knowledge of CogSci can be measured on a scale from 0 (negative
knowledge, all answers wrong) through 0.5 (random chance) to 1 (awesome
CogSci superpowers)

This is the data: - Riccardo: 3 correct answers out of 6 questions -
Kristian: 2 correct answers out of 2 questions (then he gets bored) -
Josh: 160 correct answers out of 198 questions (Josh never gets bored) -
Mikkel: 66 correct answers out of 132 questions

Questions:

1.  What’s Riccardo’s estimated knowledge of CogSci? What is the
    probability he knows more than chance (0.5) \[try figuring this out.
    if you can’t peek into chapters 3.1 and 3.2 and/or the slides\]?

-   First implement a grid approximation (hint check paragraph 2.4.1!)
    with a uniform prior, calculate the posterior and plot the results
-   Then implement a quadratic approximation (hint check paragraph
    2.4.2!).
-   N.B. for the rest of the exercise just keep using the grid
    approximation (we’ll move to quadratic approximations in two
    classes)

``` r
# Creating uniform prior
prior_uniform <- 1

#Calculating posterior 
rf_uni <- calc_teacher(3, 6, prior_uniform, length_out = 1e4)

# Visualizing posterior 

pretty_plot(rf_uni$grid, prior_uniform, rf_uni$likelihood, rf_uni$teacher_posterior, title = "Proabbility of Riccardo Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
set.seed(3)

# Creating a tibble from the posterior data 
rf_uni_df <- tibble(p_grid=rf_uni$grid, prior=prior_uniform, likelihood = rf_uni$likelihood, posterior = rf_uni$teacher_posterior)

# Sampling from the posterior 
samples_rf_uni <-
  rf_uni_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_rf_uni %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Riccardo being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
#Summarizing the posterior 
rf_uni_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##       sum
    ## 1 0.49995

``` r
# This means the probability of Riccardo performing better than chance is 50 % 

# Quadratic approximation 
globe_qa <-
  rethinking::map(
    alist(
      w ~ dbinom(6, p),  # binomial likelihood
      p ~ dunif(0, 1)    # uniform prior
    ), 
    data = list(w = 3))

# display summary of quadratic approximation
precis(globe_qa)
```

    ##   Mean StdDev 5.5% 94.5%
    ## p  0.5    0.2 0.17  0.83

1.  Estimate all the teachers’ knowledge of CogSci. Who’s best? Use grid
    approximation. Comment on the posteriors of Riccardo and Mikkel. 2a.
    Produce plots of the prior, and posterior for each teacher.

``` r
#################### Kristian #########################
#Calculating posterior 
kt_uni <- calc_teacher(2, 2, prior_uniform, length_out = 1e4)

# Visualizing posterior 
pretty_plot(kt_uni$grid, prior_uniform, kt_uni$likelihood, kt_uni$teacher_posterior, title = "Proabbility of Kristian Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
kt_uni_df <- tibble(p_grid=kt_uni$grid, prior=prior_uniform, likelihood = kt_uni$likelihood, posterior = kt_uni$teacher_posterior)

#Sampling from the posterior 
samples_kt_uni <-
  kt_uni_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_kt_uni %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Kristian being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
#Summarizing the posterior 
kt_uni_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##         sum
    ## 1 0.8749312

``` r
#################################### Joshua #################################
#Calculating posterior 
js_uni <- calc_teacher(160, 198, prior_uniform, length_out = 1e4)

# Visualizing posterior 
pretty_plot(js_uni$grid, prior_uniform, js_uni$likelihood, js_uni$teacher_posterior, title = "Proabbility of Joshua Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
js_uni_df <- tibble(p_grid=js_uni$grid, prior=prior_uniform, likelihood = js_uni$likelihood, posterior = js_uni$teacher_posterior)

#Sampling from the posterior 
samples_js_uni <-
  js_uni_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_js_uni %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Joshua being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-3-4.png)

``` r
#Summarizing the posterior 
js_uni_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##      sum
    ## 1 0.9999

``` r
################################ Mikkel ####################################
#Calculating posterior 
mw_uni <- calc_teacher(66, 132, prior_uniform, length_out = 1e4)

# Visualizing posterior 
pretty_plot(mw_uni$grid, prior_uniform, mw_uni$likelihood, mw_uni$teacher_posterior, title = "Proabbility of Mikkel Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-3-5.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
mw_uni_df <- tibble(p_grid=mw_uni$grid, prior=prior_uniform, likelihood = mw_uni$likelihood, posterior = mw_uni$teacher_posterior)

#Sampling from the posterior 
samples_mw_uni <-
  mw_uni_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_mw_uni %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Mikkel being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-3-6.png)

``` r
#Summarizing the posterior 
mw_uni_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##       sum
    ## 1 0.49995

1.  Change the prior. Given your teachers have all CogSci jobs, you
    should start with a higher appreciation of their knowledge: the
    prior is a normal distribution with a mean of 0.8 and a standard
    deviation of 0.2. Do the results change (and if so how)? 3a. Produce
    plots of the prior and posterior for each teacher.

``` r
################################### Riccardo #####################################
# Creating normally distributed prior
p_grid <- seq(from = 0, to = 1, length.out = 1e4)
prior_norm <- dnorm(p_grid, mean = 0.8, sd = 0.20)

#Calculating posterior 
rf_norm <- calc_teacher(3, 6, prior_norm, length_out = 1e4)

# Visualizing posterior 

pretty_plot(rf_norm$grid, prior_norm, rf_norm$likelihood, rf_norm$teacher_posterior, title = "Proabbility of Riccardo Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
set.seed(3)

# Creating a tibble from the posterior data 
rf_norm_df <- tibble(p_grid=rf_norm$grid, prior=prior_norm, likelihood = rf_norm$likelihood, posterior = rf_norm$teacher_posterior)

# Sampling from the posterior 
samples_rf_norm <-
  rf_norm_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_rf_norm %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Riccardo being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
#Summarizing the posterior 
rf_norm_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##         sum
    ## 1 0.8416926

``` r
################################### Kristian ######################################
#Calculating posterior 
kt_norm <- calc_teacher(2, 2, prior_norm, length_out = 1e4)

# Visualizing posterior 
pretty_plot(kt_norm$grid, prior_norm, kt_norm$likelihood, kt_norm$teacher_posterior, title = "Proabbility of Kristian Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
kt_norm_df <- tibble(p_grid=kt_norm$grid, prior=prior_norm, likelihood = kt_norm$likelihood, posterior = kt_norm$teacher_posterior)

#Sampling from the posterior 
samples_kt_norm <-
  kt_norm_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_kt_norm %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Kristian being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-4-4.png)

``` r
#Summarizing the posterior 
kt_norm_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##         sum
    ## 1 0.9756702

``` r
#################################### Joshua #################################
#Calculating posterior 
js_norm <- calc_teacher(160, 198, prior_norm, length_out = 1e4)

# Visualizing posterior 
pretty_plot(js_norm$grid, prior_norm, js_norm$likelihood, js_norm$teacher_posterior, title = "Proabbility of Joshua Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-4-5.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
js_norm_df <- tibble(p_grid=js_norm$grid, prior=prior_norm, likelihood = js_norm$likelihood, posterior = js_norm$teacher_posterior)

#Sampling from the posterior 
samples_js_norm <-
  js_norm_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_js_norm %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Joshua being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-4-6.png)

``` r
#Summarizing the posterior 
js_norm_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##      sum
    ## 1 0.9999

``` r
################################ Mikkel ####################################
#Calculating posterior 
mw_norm <- calc_teacher(66, 132, prior_norm, length_out = 1e4)

# Visualizing posterior 
pretty_plot(mw_norm$grid, prior_norm, mw_norm$likelihood, mw_norm$teacher_posterior, title = "Proabbility of Mikkel Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-4-7.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
mw_norm_df <- tibble(p_grid=mw_norm$grid, prior=prior_norm, likelihood = mw_norm$likelihood, posterior = mw_norm$teacher_posterior)

#Sampling from the posterior 
samples_mw_norm <-
  mw_norm_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_mw_norm %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Mikkel being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-4-8.png)

``` r
#Summarizing the posterior 
mw_norm_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##         sum
    ## 1 0.6239958

1.  You go back to your teachers and collect more data (multiply the
    previous numbers by 100). Calculate their knowledge with both a
    uniform prior and a normal prior with a mean of 0.8 and a standard
    deviation of 0.2. Do you still see a difference between the results?
    Why?

#### Riccardo

``` r
########## Uniform Prior ################
# Creating uniform prior
prior_uniform <- 1

#Calculating posterior 
rf_uni <- calc_teacher(300, 600, prior_uniform, length_out = 1e4)

# Visualizing posterior 

pretty_plot(rf_uni$grid, prior_uniform, rf_uni$likelihood, rf_uni$teacher_posterior, title = "Proabbility of Riccardo Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
set.seed(3)

# Creating a tibble from the posterior data 
rf_uni_df <- tibble(p_grid=rf_uni$grid, prior=prior_uniform, likelihood = rf_uni$likelihood, posterior = rf_uni$teacher_posterior)

# Sampling from the posterior 
samples_rf_uni <-
  rf_uni_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_rf_uni %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Riccardo being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
#Summarizing the posterior 
rf_uni_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##       sum
    ## 1 0.49995

``` r
##################### Normally distributed prior ################################
# Creating normally distributed prior
p_grid <- seq(from = 0, to = 1, length.out = 1e4)
prior_norm <- dnorm(p_grid, mean = 0.8, sd = 0.20)

#Calculating posterior 
rf_norm <- calc_teacher(300, 600, prior_norm, length_out = 1e4)

# Visualizing posterior 

pretty_plot(rf_norm$grid, prior_norm, rf_norm$likelihood, rf_norm$teacher_posterior, title = "Proabbility of Riccardo Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
set.seed(3)

# Creating a tibble from the posterior data 
rf_norm_df <- tibble(p_grid=rf_norm$grid, prior=prior_norm, likelihood = rf_norm$likelihood, posterior = rf_norm$teacher_posterior)

# Sampling from the posterior 
samples_rf_norm <-
  rf_norm_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_rf_norm %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Riccardo being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-5-4.png)

``` r
#Summarizing the posterior 
rf_norm_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##         sum
    ## 1 0.5603469

#### Kristian

``` r
############# Uniform ##################
#Calculating posterior 
kt_uni <- calc_teacher(200, 200, prior_uniform, length_out = 1e4)

# Visualizing posterior 
pretty_plot(kt_uni$grid, prior_uniform, kt_uni$likelihood, kt_uni$teacher_posterior, title = "Proabbility of Kristian Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
kt_uni_df <- tibble(p_grid=kt_uni$grid, prior=prior_uniform, likelihood = kt_uni$likelihood, posterior = kt_uni$teacher_posterior)

#Sampling from the posterior 
samples_kt_uni <-
  kt_uni_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_kt_uni %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Kristian being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
#Summarizing the posterior 
kt_uni_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##      sum
    ## 1 0.9999

``` r
################### Normally distributed prior ######################
#Calculating posterior 
kt_norm <- calc_teacher(200, 200, prior_norm, length_out = 1e4)

# Visualizing posterior 
pretty_plot(kt_norm$grid, prior_norm, kt_norm$likelihood, kt_norm$teacher_posterior, title = "Proabbility of Kristian Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-6-3.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
kt_norm_df <- tibble(p_grid=kt_norm$grid, prior=prior_norm, likelihood = kt_norm$likelihood, posterior = kt_norm$teacher_posterior)

#Sampling from the posterior 
samples_kt_norm <-
  kt_norm_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_kt_norm %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Kristian being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-6-4.png)

``` r
#Summarizing the posterior 
kt_norm_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##      sum
    ## 1 0.9999

#### Joshua

``` r
############################ Uniform Prior #######################################
#Calculating posterior 
js_uni <- calc_teacher(16000, 19800, prior_uniform, length_out = 1e4)

# Visualizing posterior 
pretty_plot(js_uni$grid, prior_uniform, js_uni$likelihood, js_uni$teacher_posterior, title = "Proabbility of Joshua Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
js_uni_df <- tibble(p_grid=js_uni$grid, prior=prior_uniform, likelihood = js_uni$likelihood, posterior = js_uni$teacher_posterior)

#Sampling from the posterior 
samples_js_uni <-
  js_uni_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_js_uni %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Joshua being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
#Summarizing the posterior 
js_uni_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##      sum
    ## 1 0.9999

``` r
############################ Normally Distriuted #################################
#Calculating posterior 
js_norm <- calc_teacher(16000, 19800, prior_norm, length_out = 1e4)

# Visualizing posterior 
pretty_plot(js_norm$grid, prior_norm, js_norm$likelihood, js_norm$teacher_posterior, title = "Proabbility of Joshua Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-7-3.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
js_norm_df <- tibble(p_grid=js_norm$grid, prior=prior_norm, likelihood = js_norm$likelihood, posterior = js_norm$teacher_posterior)

#Sampling from the posterior 
samples_js_norm <-
  js_norm_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_js_norm %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Joshua being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-7-4.png)

``` r
#Summarizing the posterior 
js_norm_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##      sum
    ## 1 0.9999

#### Mikkel

``` r
############################### Uniform Prior ####################################
#Calculating posterior 
mw_uni <- calc_teacher(6600, 13200, prior_uniform, length_out = 1e4)

# Visualizing posterior 
pretty_plot(mw_uni$grid, prior_uniform, mw_uni$likelihood, mw_uni$teacher_posterior, title = "Proabbility of Mikkel Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
mw_uni_df <- tibble(p_grid=mw_uni$grid, prior=prior_uniform, likelihood = mw_uni$likelihood, posterior = mw_uni$teacher_posterior)

#Sampling from the posterior 
samples_mw_uni <-
  mw_uni_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_mw_uni %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Mikkel being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-8-2.png)

``` r
#Summarizing the posterior 
mw_uni_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##       sum
    ## 1 0.49995

``` r
############################### Normally Distributed ############################
#Calculating posterior 
mw_norm <- calc_teacher(6600, 13200, prior_norm, length_out = 1e4)

# Visualizing posterior 
pretty_plot(mw_norm$grid, prior_norm, mw_norm$likelihood, mw_norm$teacher_posterior, title = "Proabbility of Mikkel Being Right")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-8-3.png)

``` r
set.seed(3)

#Creating a tibble from the posterior etc. 
mw_norm_df <- tibble(p_grid=mw_norm$grid, prior=prior_norm, likelihood = mw_norm$likelihood, posterior = mw_norm$teacher_posterior)

#Sampling from the posterior 
samples_mw_norm <-
  mw_norm_df %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

samples_mw_norm %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("Chance of Mikkel being Right (p)")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-8-4.png)

``` r
#Summarizing the posterior 
mw_norm_df %>% 
  filter(p_grid >.5) %>% 
  summarise(sum = sum(posterior))/1e4
```

    ##         sum
    ## 1 0.5129637

1.  Imagine you’re a skeptic and think your teachers do not know
    anything about CogSci, given the content of their classes. How would
    you operationalize that belief?

2.  Optional question: Can you estimate the difference between
    Riccardo’s estimated knowledge and that of each of the other
    teachers? Would you deem it credible (that is, would you believe
    that it is actually different)?

### Second part: Focusing on predictions

Last year you assessed the teachers (darned time runs quick!). Now you
want to re-test them and assess whether your models are producing
reliable predictions. In Methods 3 we learned how to do machine-learning
style assessment of predictions (e.g. rmse on testing datasets).
Bayesian stats makes things a bit more complicated. So we’ll try out how
that works. N.B. You can choose which prior to use for the analysis of
last year’s data.

Questions to be answered (but see guidance below): 1- Write a paragraph
discussing how assessment of prediction performance is different in
Bayesian vs. frequentist models 2- Provide at least one plot and one
written line discussing prediction errors for each of the teachers.

This is the old data: - Riccardo: 3 correct answers out of 6 questions -
Kristian: 2 correct answers out of 2 questions (then he gets bored) -
Josh: 160 correct answers out of 198 questions (Josh never gets bored) -
Mikkel: 66 correct answers out of 132 questions

This is the new data: - Riccardo: 9 correct answers out of 10 questions
(then he freaks out about teaching preparation and leaves) - Kristian: 8
correct answers out of 12 questions - Josh: 148 correct answers out of
172 questions (again, Josh never gets bored) - Mikkel: 34 correct
answers out of 65 questions

Guidance Tips

1.  There are at least two ways of assessing predictions.
2.  Last year’s results are this year’s expectations.
3.  Are the parameter estimates changing? (way 1)
4.  How does the new data look in last year’s predictive posterior?
    (way 2)

``` r
#creating a dataframe of the results
results_df = data.frame(teacher = c("RF", "KT", "JS", "MW"), 
                        correct = c(3, 2, 160, 66) , 
                        n_quest = c(6, 2, 198, 132))

results_df_2 <- data.frame(teacher = c("RF", "KT", "JS", "MW"),
                           correct = c(9, 8, 148, 34),
                           n_quest = c(10, 12, 172, 65))

#Grid approximation 
length = 1e4 #10.000
p_grid <- seq(0, 1, length.out = length)
inf_prior <- dnorm(p_grid, mean = 0.8, sd = 0.20)

rf_results <- calc_teacher(2, 2, prior = inf_prior)

### Kristian ###
#Old posterior 
kt_results <- calc_teacher(2, 2, prior = inf_prior)

#New posterior
kt_results_2 <- calc_teacher(8, 12, prior = kt_results$teacher_posterior)

pretty_plot(p_grid = kt_results_2$grid, 
            prior = kt_results$teacher_posterior, 
            likelihood = kt_results_2$likelihood, 
            posterior = kt_results_2$teacher_posterior, title = "New Posterior Distribution Kristian")
```

![](Assignment2_tidy_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
js_results <- calc_teacher(2, 2, prior = inf_prior)


mw_results <- calc_teacher(2, 2, prior = inf_prior)
```