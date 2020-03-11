Libraries
---------

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
# We do grid approximation

(r <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = 1) %>%                                   # define prior
   mutate(likelihood      = dbinom(3, size = 6, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid prior likelihood unstd_posterior posterior
    ##       <dbl> <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0            1   0.       0                0.      
    ##  2 0.000100     1   2.00e-11 0.0000000000200  1.40e-14
    ##  3 0.000200     1   1.60e-10 0.000000000160   1.12e-13
    ##  4 0.000300     1   5.40e-10 0.000000000540   3.78e-13
    ##  5 0.000400     1   1.28e- 9 0.00000000128    8.95e-13
    ##  6 0.000500     1   2.50e- 9 0.00000000250    1.75e-12
    ##  7 0.000600     1   4.31e- 9 0.00000000431    3.02e-12
    ##  8 0.000700     1   6.85e- 9 0.00000000685    4.79e-12
    ##  9 0.000800     1   1.02e- 8 0.0000000102     7.15e-12
    ## 10 0.000900     1   1.45e- 8 0.0000000145     1.02e-11
    ## # ... with 9,990 more rows

``` r
## We plot it
r_1 <- r %>% 
  ggplot(aes(x = p_grid, y = posterior, color = "red")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "10000 points",
       x = "probability of Riccardo being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_hline(yintercept=1/1e4, color = "blue") +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
## Prior divided by the 20 grids for scaling
r_1
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
## We do quadritic approximation
Riccardo_qa <-
  rethinking::map(
    alist(
      w ~ dbinom(6, p),  # binomial likelihood
      p ~ dunif(0, 1)    # uniform prior
    ), 
    data = list(w = 3))

# display summary of quadratic approximation
precis(Riccardo_qa)
```

    ##   Mean StdDev 5.5% 94.5%
    ## p  0.5    0.2 0.17  0.83

``` r
# We sample from the Grid approximation
set.seed(3)
samples <-
  r %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

# We make a density plot of the sample
samples %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("proportion of water (p)")
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
# We estimate the Highest posterior density  interval
samples <- as.data.frame(samples)
HPDI(samples, prob=0.5 )
```

    ##      |0.5      0.5| 
    ## 0.3785379 1.0000000

``` r
#We want to know how likely Riccardo is to perform above chance
r %>% 
  filter(p_grid > .5) %>% 
  summarise(sum = sum(posterior))
```

    ## # A tibble: 1 x 1
    ##     sum
    ##   <dbl>
    ## 1 0.500

This means that there is 50% chance that Riccardo will perform better
than chance.

1.  Estimate all the teachers’ knowledge of CogSci. Who’s best? Use grid
    approximation. Comment on the posteriors of Riccardo and Mikkel. 2a.
    Produce plots of the prior, and posterior for each teacher.

``` r
################# We do grid approximation for Kristian ################
(k <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = 1) %>%                                   # define prior
   mutate(likelihood      = dbinom(2, size = 2, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid prior   likelihood unstd_posterior posterior
    ##       <dbl> <dbl>        <dbl>           <dbl>     <dbl>
    ##  1 0            1 0               0             0.      
    ##  2 0.000100     1 0.0000000100    0.0000000100  3.00e-12
    ##  3 0.000200     1 0.0000000400    0.0000000400  1.20e-11
    ##  4 0.000300     1 0.0000000900    0.0000000900  2.70e-11
    ##  5 0.000400     1 0.000000160     0.000000160   4.80e-11
    ##  6 0.000500     1 0.000000250     0.000000250   7.50e-11
    ##  7 0.000600     1 0.000000360     0.000000360   1.08e-10
    ##  8 0.000700     1 0.000000490     0.000000490   1.47e-10
    ##  9 0.000800     1 0.000000640     0.000000640   1.92e-10
    ## 10 0.000900     1 0.000000810     0.000000810   2.43e-10
    ## # ... with 9,990 more rows

``` r
## We plot
k %>% 
  ggplot(aes(x = p_grid, y = posterior, color = "red")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "10000 points",
       x = "probability of Kristian being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_hline(yintercept=1/1e4) + 
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
  ## Prior divided by the 10000 grids for scaling 


# We sample from the Grid approximation
set.seed(3)
samples <-
  k %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

# We make a density plot of the sample
samples %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("proportion of correct answers")
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# We estimate the Highest posterior density  interval
samples <- as.data.frame(samples)
HPDI(samples, prob=0.5 )
```

    ##      |0.5      0.5| 
    ## 0.7923792 1.0000000

``` r
#We want to know how likely Riccardo is to perform above chance
k %>% 
  filter(p_grid > .5) %>% 
  summarise(sum = sum(posterior))
```

    ## # A tibble: 1 x 1
    ##     sum
    ##   <dbl>
    ## 1 0.875

``` r
## We do a prior predictive check
ppc <- rbinom(1e4, 6, runif(1e4, min = 0.5, max = 1))
dens(ppc)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
################# We do a grid approximation for Josh ################
# We do grid approximation

(j <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = 1) %>%                                   # define prior
   mutate(likelihood      = dbinom(160, size = 198, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid prior likelihood unstd_posterior posterior
    ##       <dbl> <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0            1          0               0         0
    ##  2 0.000100     1          0               0         0
    ##  3 0.000200     1          0               0         0
    ##  4 0.000300     1          0               0         0
    ##  5 0.000400     1          0               0         0
    ##  6 0.000500     1          0               0         0
    ##  7 0.000600     1          0               0         0
    ##  8 0.000700     1          0               0         0
    ##  9 0.000800     1          0               0         0
    ## 10 0.000900     1          0               0         0
    ## # ... with 9,990 more rows

``` r
## We plot it
j %>% 
  ggplot(aes(x = p_grid, y = posterior, color = "red")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "10000 points",
       x = "probability of Josh being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_hline(yintercept=1/1e4) +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-4-4.png)

``` r
# We sample from the Grid approximation
set.seed(3)
samples <-
  j %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)

# We make a density plot of the sample
samples %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("proportion of correct answers")
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-4-5.png)

``` r
# We estimate the Highest posterior density  interval
samples <- as.data.frame(samples)
HPDI(samples, prob=0.5 )
```

    ##      |0.5      0.5| 
    ## 0.7878788 1.0000000

``` r
#We want to know how likely Riccardo is to perform above chance
j %>% 
  filter(p_grid > .5) %>% 
  summarise(sum = sum(posterior))
```

    ## # A tibble: 1 x 1
    ##     sum
    ##   <dbl>
    ## 1     1

``` r
################ We do grid approximation for Mikkel ################
# We do grid approximation

(m <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = 1) %>%                                   # define prior
   mutate(likelihood      = dbinom(66, size = 132, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid prior likelihood unstd_posterior posterior
    ##       <dbl> <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0            1  0.              0.        0.       
    ##  2 0.000100     1  3.77e-226       3.77e-226 5.02e-228
    ##  3 0.000200     1  2.77e-206       2.77e-206 3.68e-208
    ##  4 0.000300     1  1.15e-194       1.15e-194 1.53e-196
    ##  5 0.000400     1  2.01e-186       2.01e-186 2.68e-188
    ##  6 0.000500     1  4.98e-180       4.98e-180 6.63e-182
    ##  7 0.000600     1  8.33e-175       8.33e-175 1.11e-176
    ##  8 0.000700     1  2.17e-170       2.17e-170 2.88e-172
    ##  9 0.000800     1  1.45e-166       1.45e-166 1.93e-168
    ## 10 0.000900     1  3.42e-163       3.42e-163 4.55e-165
    ## # ... with 9,990 more rows

``` r
## We plot it
m %>% 
  ggplot(aes(x = p_grid, y = posterior, color = "red")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "10000 points",
       x = "probability of Mikkel being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank())  + geom_hline(yintercept=1/1e4) +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-4-6.png)

``` r
# We sample from the Grid approximation
set.seed(3)
samples <-
  m %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)
# We estimate the Highest posterior density  interval
samples <- as.data.frame(samples)
HPDI(samples, prob=0.5 )
```

    ##      |0.5      0.5| 
    ## 0.4719472 1.0000000

``` r
# We make a density plot of the sample
samples %>% 
  ggplot(aes(x = p_grid)) +
  geom_density(fill = "black") +
  coord_cartesian(xlim = 0:1) +
  xlab("proportion of correct answers")
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-4-7.png)

``` r
samples <- as.data.frame(samples)
# We estimate the Highest posterior density  interval
HPDI(samples, prob=0.5 )
```

    ##      |0.5      0.5| 
    ## 0.4719472 1.0000000

``` r
#We want to know how likely Mikkel is to perform above chance
m %>% 
  filter(p_grid > .5) %>% 
  summarise(sum = sum(posterior))
```

    ## # A tibble: 1 x 1
    ##     sum
    ##   <dbl>
    ## 1 0.500

Josh is the best. Riccard and Mikkel both have a 50 % chance of
performing above chance.

1.  Change the prior. Given your teachers have all CogSci jobs, you
    should start with a higher appreciation of their knowledge: the
    prior is a normal distribution with a mean of 0.8 and a standard
    deviation of 0.2. 3a. Produce plots of the prior and posterior for
    each teacher.

``` r
### Riccardo
# We make the new posterior from a new prior
(r <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.8, sd = 0.2)) %>%                                   # define prior
   mutate(likelihood      = dbinom(3, size = 6, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        0.000669   0.              0.        0.      
    ##  2 0.000100 0.000670   2.00e-11        1.34e-14  1.17e-17
    ##  3 0.000200 0.000672   1.60e-10        1.07e-13  9.39e-17
    ##  4 0.000300 0.000673   5.40e-10        3.63e-13  3.17e-16
    ##  5 0.000400 0.000675   1.28e- 9        8.63e-13  7.54e-16
    ##  6 0.000500 0.000676   2.50e- 9        1.69e-12  1.47e-15
    ##  7 0.000600 0.000677   4.31e- 9        2.92e-12  2.55e-15
    ##  8 0.000700 0.000679   6.85e- 9        4.65e-12  4.06e-15
    ##  9 0.000800 0.000680   1.02e- 8        6.95e-12  6.07e-15
    ## 10 0.000900 0.000681   1.45e- 8        9.91e-12  8.66e-15
    ## # ... with 9,990 more rows

``` r
## We plot
r %>% 
  ggplot(aes(x = p_grid, y = posterior, colour="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "10000 points",
       x = "probability of Riccardo being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_line(aes(p_grid, (dnorm(p_grid, mean = 0.8, sd = 0.2)/1e4), colour="red")) +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
## Prior divided by the 1e4 grids for scaling
# We sample from the Grid approximation
set.seed(3)
samples <-
  r %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)
samples <- as.data.frame(samples)
# We estimate the Highest posterior density  interval
HPDI(samples, prob=0.5 )
```

    ##      |0.5      0.5| 
    ## 0.5652565 1.3991463

``` r
### Kristian
# We make the new posterior from a new prior
(k <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.8, sd = 0.2)) %>%                                   # define prior
   mutate(likelihood      = dbinom(2, size = 2, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior   likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>        <dbl>           <dbl>     <dbl>
    ##  1 0        0.000669 0                   0.        0.      
    ##  2 0.000100 0.000670 0.0000000100        6.71e-12  1.38e-15
    ##  3 0.000200 0.000672 0.0000000400        2.69e-11  5.54e-15
    ##  4 0.000300 0.000673 0.0000000900        6.06e-11  1.25e-14
    ##  5 0.000400 0.000675 0.000000160         1.08e-10  2.23e-14
    ##  6 0.000500 0.000676 0.000000250         1.69e-10  3.48e-14
    ##  7 0.000600 0.000677 0.000000360         2.44e-10  5.03e-14
    ##  8 0.000700 0.000679 0.000000490         3.33e-10  6.86e-14
    ##  9 0.000800 0.000680 0.000000640         4.35e-10  8.97e-14
    ## 10 0.000900 0.000681 0.000000810         5.52e-10  1.14e-13
    ## # ... with 9,990 more rows

``` r
## We plot
k %>% 
  ggplot(aes(x = p_grid, y = posterior, colour="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "10000 points",
       x = "probability of Kristian being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_line(aes(p_grid, (dnorm(p_grid, mean = 0.8, sd = 0.2)/1e4), colour="red")) +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
## Prior divided by the 20 grids for scaling

# We sample from the Grid approximation
set.seed(3)
samples <-
  k %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)
samples <- as.data.frame(samples)
# We estimate the Highest posterior density  interval
HPDI(samples, prob=0.5 )
```

    ##      |0.5      0.5| 
    ## 0.8087809 1.7721318

``` r
#### Josh
# We make the new posterior from a new prior
(j <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.8, sd = 0.2)) %>%                                   # define prior
   mutate(likelihood      = dbinom(160, size = 198, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        0.000669          0               0         0
    ##  2 0.000100 0.000670          0               0         0
    ##  3 0.000200 0.000672          0               0         0
    ##  4 0.000300 0.000673          0               0         0
    ##  5 0.000400 0.000675          0               0         0
    ##  6 0.000500 0.000676          0               0         0
    ##  7 0.000600 0.000677          0               0         0
    ##  8 0.000700 0.000679          0               0         0
    ##  9 0.000800 0.000680          0               0         0
    ## 10 0.000900 0.000681          0               0         0
    ## # ... with 9,990 more rows

``` r
j %>% 
  ggplot(aes(x = p_grid, y = posterior,color="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Josh, 10000 points",
       x = "probability of Josh being Right",
       y = "posterior probability") +
  theme(panel.grid = element_blank())+geom_line(aes(x=p_grid,y=dnorm(p_grid, mean = 0.8, sd = 0.2)/1e4,color="red"))+ scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
# We sample from the Grid approximation
set.seed(3)
samples <-
  j %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)
samples <- as.data.frame(samples)
# We estimate the Highest posterior density  interval
HPDI(samples, prob=0.5 )
```

    ##      |0.5      0.5| 
    ## 0.7886789 1.9852315

``` r
#Mikkel
# We make the new posterior from a new prior
(m <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 10000),  # define grid
        prior             = dnorm(p_grid, mean = 0.8, sd = 0.2)) %>%                                   # define prior
   mutate(likelihood      = dbinom(66, size = 132, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        0.000669  0.              0.        0.       
    ##  2 0.000100 0.000670  3.77e-226       2.53e-229 5.06e-231
    ##  3 0.000200 0.000672  2.77e-206       1.86e-209 3.72e-211
    ##  4 0.000300 0.000673  1.15e-194       7.75e-198 1.55e-199
    ##  5 0.000400 0.000675  2.01e-186       1.36e-189 2.72e-191
    ##  6 0.000500 0.000676  4.98e-180       3.37e-183 6.73e-185
    ##  7 0.000600 0.000677  8.33e-175       5.64e-178 1.13e-179
    ##  8 0.000700 0.000679  2.17e-170       1.47e-173 2.94e-175
    ##  9 0.000800 0.000680  1.45e-166       9.84e-170 1.97e-171
    ## 10 0.000900 0.000681  3.42e-163       2.33e-166 4.66e-168
    ## # ... with 9,990 more rows

``` r
m %>% 
  ggplot(aes(x = p_grid, y = posterior,color="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Mikkel,10000 points",
       x = "probability of Mikkel being Right",
       y = "posterior probability") +
  theme(panel.grid = element_blank())+geom_line(aes(x=p_grid,y=dnorm(p_grid, mean = 0.8, sd = 0.2)/1e4,color="red"))+ scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-5-4.png)

``` r
# We sample from the Grid approximation
set.seed(3)
samples <-
  m %>% 
  sample_n(size = 1e4, weight = posterior, replace = T)
samples <- as.data.frame(samples)
# We estimate the Highest posterior density  interval
HPDI(samples, prob=0.5 )
```

    ##      |0.5      0.5| 
    ## 0.4858486 0.5424749

Do the results change (and if so how)?

For Riccardo, his posterior probability used to be highest around 50 %
but now it is highest around 60 %. He is now more likely to answer
right. For Kristian, his posterior probability used to be highest at
100%, but now it went down to around 85% For Josh, it did not really
change. For Mikkel, it did not really change much. When increasing the
amount of points in the grid, we can see they look very similar.

1.  You go back to your teachers and collect more data (multiply the
    previous numbers by 100). Calculate their knowledge with both a
    uniform prior and a normal prior with a mean of 0.8 and a standard
    deviation of 0.2. Do you still see a difference between the results?
    Why?

We do with a uniform prior

``` r
### Riccardo
# We make the new posterior from a new prior
(r <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = 1) %>%                                   # define prior
   mutate(likelihood      = dbinom(3*100, size = 6*100, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid prior likelihood unstd_posterior posterior
    ##       <dbl> <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0            1          0               0         0
    ##  2 0.000100     1          0               0         0
    ##  3 0.000200     1          0               0         0
    ##  4 0.000300     1          0               0         0
    ##  5 0.000400     1          0               0         0
    ##  6 0.000500     1          0               0         0
    ##  7 0.000600     1          0               0         0
    ##  8 0.000700     1          0               0         0
    ##  9 0.000800     1          0               0         0
    ## 10 0.000900     1          0               0         0
    ## # ... with 9,990 more rows

``` r
## We plot
r %>% 
  ggplot(aes(x = p_grid, y = posterior, colour="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "10000 points",
       x = "probability of Riccardo being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_hline(yintercept=1/1e4, colour="blue") +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
## Prior divided by the 20 grids for scaling

### Kristian
# We make the new posterior from a new prior
(k <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = 1) %>%                                   # define prior
   mutate(likelihood      = dbinom(2*100, size = 2*100, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid prior likelihood unstd_posterior posterior
    ##       <dbl> <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0            1          0               0         0
    ##  2 0.000100     1          0               0         0
    ##  3 0.000200     1          0               0         0
    ##  4 0.000300     1          0               0         0
    ##  5 0.000400     1          0               0         0
    ##  6 0.000500     1          0               0         0
    ##  7 0.000600     1          0               0         0
    ##  8 0.000700     1          0               0         0
    ##  9 0.000800     1          0               0         0
    ## 10 0.000900     1          0               0         0
    ## # ... with 9,990 more rows

``` r
## We plot
k %>% 
  ggplot(aes(x = p_grid, y = posterior, colour="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Kristian, 10000 points",
       x = "probability of Kristian being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_hline(yintercept=1/1e4, colour="blue") +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
## Prior divided by the 20 grids for scaling

#### Josh
# We make the new posterior from a new prior
(j <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = 1) %>%                                   # define prior
   mutate(likelihood      = dbinom(160*100, size = 198*100, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid prior likelihood unstd_posterior posterior
    ##       <dbl> <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0            1          0               0         0
    ##  2 0.000100     1          0               0         0
    ##  3 0.000200     1          0               0         0
    ##  4 0.000300     1          0               0         0
    ##  5 0.000400     1          0               0         0
    ##  6 0.000500     1          0               0         0
    ##  7 0.000600     1          0               0         0
    ##  8 0.000700     1          0               0         0
    ##  9 0.000800     1          0               0         0
    ## 10 0.000900     1          0               0         0
    ## # ... with 9,990 more rows

``` r
j %>% 
  ggplot(aes(x = p_grid, y = posterior,color="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Josh, 10000 points",
       x = "probability of Josh being Right",
       y = "posterior probability") +
  theme(panel.grid = element_blank())+geom_hline(yintercept=1/1e4, color="blue")+ scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-6-3.png)

``` r
#Mikkel
# We make the new posterior from a new prior
(m <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = 1) %>%                                   # define prior
   mutate(likelihood      = dbinom(66*100, size = 132*100, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid prior likelihood unstd_posterior posterior
    ##       <dbl> <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0            1          0               0         0
    ##  2 0.000100     1          0               0         0
    ##  3 0.000200     1          0               0         0
    ##  4 0.000300     1          0               0         0
    ##  5 0.000400     1          0               0         0
    ##  6 0.000500     1          0               0         0
    ##  7 0.000600     1          0               0         0
    ##  8 0.000700     1          0               0         0
    ##  9 0.000800     1          0               0         0
    ## 10 0.000900     1          0               0         0
    ## # ... with 9,990 more rows

``` r
m %>% 
  ggplot(aes(x = p_grid, y = posterior,color="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Mikkel,10000 points",
       x = "probability of Mikkel being Right",
       y = "posterior probability") +
  theme(panel.grid = element_blank())+geom_hline(yintercept=1/1e4, color="blue") +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-6-4.png)

We do with the normal prior

``` r
### Riccardo
# We make the new posterior from a new prior
(r <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.8, sd = 0.2)) %>%                                   # define prior
   mutate(likelihood      = dbinom(3*100, size = 6*100, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        0.000669          0               0         0
    ##  2 0.000100 0.000670          0               0         0
    ##  3 0.000200 0.000672          0               0         0
    ##  4 0.000300 0.000673          0               0         0
    ##  5 0.000400 0.000675          0               0         0
    ##  6 0.000500 0.000676          0               0         0
    ##  7 0.000600 0.000677          0               0         0
    ##  8 0.000700 0.000679          0               0         0
    ##  9 0.000800 0.000680          0               0         0
    ## 10 0.000900 0.000681          0               0         0
    ## # ... with 9,990 more rows

``` r
## We plot
r %>% 
  ggplot(aes(x = p_grid, y = posterior, colour="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Riccardo, 10000 points",
       x = "probability of Riccardo being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_line(aes(p_grid, (dnorm(p_grid, mean = 0.8, sd = 0.2)/1e4), colour="red")) +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
## Prior divided by the 20 grids for scaling
#The previous plot was
r_1
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
### Kristian
# We make the new posterior from a new prior
(k <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.8, sd = 0.2)) %>%                                   # define prior
   mutate(likelihood      = dbinom(2*100, size = 2*100, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        0.000669          0               0         0
    ##  2 0.000100 0.000670          0               0         0
    ##  3 0.000200 0.000672          0               0         0
    ##  4 0.000300 0.000673          0               0         0
    ##  5 0.000400 0.000675          0               0         0
    ##  6 0.000500 0.000676          0               0         0
    ##  7 0.000600 0.000677          0               0         0
    ##  8 0.000700 0.000679          0               0         0
    ##  9 0.000800 0.000680          0               0         0
    ## 10 0.000900 0.000681          0               0         0
    ## # ... with 9,990 more rows

``` r
## We plot
k %>% 
  ggplot(aes(x = p_grid, y = posterior, colour="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Kristian, 10000 points",
       x = "probability of Kristian being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_line(aes(p_grid, (dnorm(p_grid, mean = 0.8, sd = 0.2)/1e4), colour="red")) +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-7-3.png)

``` r
## Prior divided by the 20 grids for scaling

#### Josh
# We make the new posterior from a new prior
(j <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.8, sd = 0.2)) %>%                                   # define prior
   mutate(likelihood      = dbinom(160*100, size = 198*100, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        0.000669          0               0         0
    ##  2 0.000100 0.000670          0               0         0
    ##  3 0.000200 0.000672          0               0         0
    ##  4 0.000300 0.000673          0               0         0
    ##  5 0.000400 0.000675          0               0         0
    ##  6 0.000500 0.000676          0               0         0
    ##  7 0.000600 0.000677          0               0         0
    ##  8 0.000700 0.000679          0               0         0
    ##  9 0.000800 0.000680          0               0         0
    ## 10 0.000900 0.000681          0               0         0
    ## # ... with 9,990 more rows

``` r
j %>% 
  ggplot(aes(x = p_grid, y = posterior,color="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Josh, 10000 points",
       x = "probability of Josh being Right",
       y = "posterior probability") +
  theme(panel.grid = element_blank())+geom_line(aes(x=p_grid,y=dnorm(p_grid, mean = 0.8, sd = 0.2)/1e4,color="red"))+ scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-7-4.png)

``` r
#Mikkel
# We make the new posterior from a new prior
(m <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.8, sd = 0.2)) %>%                                   # define prior
   mutate(likelihood      = dbinom(66*100, size = 132*100, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        0.000669          0               0         0
    ##  2 0.000100 0.000670          0               0         0
    ##  3 0.000200 0.000672          0               0         0
    ##  4 0.000300 0.000673          0               0         0
    ##  5 0.000400 0.000675          0               0         0
    ##  6 0.000500 0.000676          0               0         0
    ##  7 0.000600 0.000677          0               0         0
    ##  8 0.000700 0.000679          0               0         0
    ##  9 0.000800 0.000680          0               0         0
    ## 10 0.000900 0.000681          0               0         0
    ## # ... with 9,990 more rows

``` r
m %>% 
  ggplot(aes(x = p_grid, y = posterior,color="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Mikkel, 10000 points",
       x = "probability of Mikkel being Right",
       y = "posterior probability") +
  theme(panel.grid = element_blank())+geom_line(aes(x=p_grid,y=dnorm(p_grid, mean = 0.8, sd = 0.2)/1e4,color="red"))+ scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-7-5.png) We
see that Kristian now is less affected by the normal prior and once
again has the highest probability at the 100%.

We see that Riccardo also is not very different from the uniform prior
to the normal prior.

Adding x100 times of observations shows that the more observations make
the normal prior change less for the teachers who used to be changed a
lot by the normal prior.

1.  Imagine you’re a skeptic and think your teachers do not know
    anything about CogSci, given the content of their classes. How would
    you operationalize that belief?

``` r
## We will add a normal prior with a mean of 50 % in the belief that they will perform around chance level. 
# We will have  a sd of 0.05 because we are very sure of them not knowing anything!

### Riccardo
# We make the new posterior from a new prior
(r <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.5, sd = 0.05)) %>%                                   # define prior
   mutate(likelihood      = dbinom(3, size = 6, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        1.54e-21   0.              0.        0.      
    ##  2 0.000100 1.57e-21   2.00e-11        3.14e-32  1.04e-35
    ##  3 0.000200 1.60e-21   1.60e-10        2.56e-31  8.45e-35
    ##  4 0.000300 1.63e-21   5.40e-10        8.82e-31  2.91e-34
    ##  5 0.000400 1.67e-21   1.28e- 9        2.13e-30  7.03e-34
    ##  6 0.000500 1.70e-21   2.50e- 9        4.25e-30  1.40e-33
    ##  7 0.000600 1.74e-21   4.31e- 9        7.48e-30  2.47e-33
    ##  8 0.000700 1.77e-21   6.85e- 9        1.21e-29  4.00e-33
    ##  9 0.000800 1.81e-21   1.02e- 8        1.85e-29  6.08e-33
    ## 10 0.000900 1.84e-21   1.45e- 8        2.68e-29  8.83e-33
    ## # ... with 9,990 more rows

``` r
## We plot
r %>% 
  ggplot(aes(x = p_grid, y = posterior, colour="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Riccardo, 10000 points",
       x = "probability of Riccardo being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_line(aes(p_grid, (dnorm(p_grid, mean = 0.5, sd = 0.05)/1e4), colour="red")) +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
## Prior divided by the 20 grids for scaling

### Kristian
# We make the new posterior from a new prior
(k <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.5, sd = 0.05)) %>%                                   # define prior
   mutate(likelihood      = dbinom(2, size = 2, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior   likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>        <dbl>           <dbl>     <dbl>
    ##  1 0        1.54e-21 0                   0.        0.      
    ##  2 0.000100 1.57e-21 0.0000000100        1.57e-29  6.22e-33
    ##  3 0.000200 1.60e-21 0.0000000400        6.41e-29  2.54e-32
    ##  4 0.000300 1.63e-21 0.0000000900        1.47e-28  5.83e-32
    ##  5 0.000400 1.67e-21 0.000000160         2.67e-28  1.06e-31
    ##  6 0.000500 1.70e-21 0.000000250         4.25e-28  1.68e-31
    ##  7 0.000600 1.74e-21 0.000000360         6.25e-28  2.47e-31
    ##  8 0.000700 1.77e-21 0.000000490         8.67e-28  3.44e-31
    ##  9 0.000800 1.81e-21 0.000000640         1.16e-27  4.58e-31
    ## 10 0.000900 1.84e-21 0.000000810         1.49e-27  5.91e-31
    ## # ... with 9,990 more rows

``` r
## We plot
k %>% 
  ggplot(aes(x = p_grid, y = posterior, colour="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Kristian, 10000 points",
       x = "probability of Kristian being right",
       y = "posterior probability") +
  theme(panel.grid = element_blank()) + geom_line(aes(p_grid, (dnorm(p_grid, mean = 0.5, sd = 0.05)/20), colour="red")) +
  scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-8-2.png)

``` r
## Prior divided by the 20 grids for scaling

#### Josh
# We make the new posterior from a new prior
(j <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.5, sd = 0.05)) %>%                                   # define prior
   mutate(likelihood      = dbinom(160, size = 198, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        1.54e-21          0               0         0
    ##  2 0.000100 1.57e-21          0               0         0
    ##  3 0.000200 1.60e-21          0               0         0
    ##  4 0.000300 1.63e-21          0               0         0
    ##  5 0.000400 1.67e-21          0               0         0
    ##  6 0.000500 1.70e-21          0               0         0
    ##  7 0.000600 1.74e-21          0               0         0
    ##  8 0.000700 1.77e-21          0               0         0
    ##  9 0.000800 1.81e-21          0               0         0
    ## 10 0.000900 1.84e-21          0               0         0
    ## # ... with 9,990 more rows

``` r
j %>% 
  ggplot(aes(x = p_grid, y = posterior,color="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Josh, 10000 points",
       x = "probability of Josh being Right",
       y = "posterior probability") +
  theme(panel.grid = element_blank())+geom_line(aes(x=p_grid,y=dnorm(p_grid, mean = 0.5, sd = 0.05)/1e4,color="red"))+ scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-8-3.png)

``` r
#Mikkel
# We make the new posterior from a new prior
(m <-
 tibble(p_grid            = seq(from = 0, to = 1, length.out = 1e4),  # define grid
        prior             = dnorm(p_grid, mean = 0.5, sd = 0.05)) %>%                                   # define prior
   mutate(likelihood      = dbinom(66, size = 132, prob = p_grid)) %>%  # compute likelihood at each value in grid
   mutate(unstd_posterior = likelihood * prior) %>%                  # compute product of likelihood and prior
   mutate(posterior       = unstd_posterior / sum(unstd_posterior))  # standardize the posterior, so it sums to 1
)
```

    ## # A tibble: 10,000 x 5
    ##      p_grid    prior likelihood unstd_posterior posterior
    ##       <dbl>    <dbl>      <dbl>           <dbl>     <dbl>
    ##  1 0        1.54e-21  0.              0.        0.       
    ##  2 0.000100 1.57e-21  3.77e-226       5.93e-247 1.30e-249
    ##  3 0.000200 1.60e-21  2.77e-206       4.43e-227 9.76e-230
    ##  4 0.000300 1.63e-21  1.15e-194       1.88e-215 4.14e-218
    ##  5 0.000400 1.67e-21  2.01e-186       3.36e-207 7.39e-210
    ##  6 0.000500 1.70e-21  4.98e-180       8.47e-201 1.87e-203
    ##  7 0.000600 1.74e-21  8.33e-175       1.44e-195 3.18e-198
    ##  8 0.000700 1.77e-21  2.17e-170       3.84e-191 8.45e-194
    ##  9 0.000800 1.81e-21  1.45e-166       2.61e-187 5.76e-190
    ## 10 0.000900 1.84e-21  3.42e-163       6.30e-184 1.39e-186
    ## # ... with 9,990 more rows

``` r
m %>% 
  ggplot(aes(x = p_grid, y = posterior,color="blue")) +
  geom_point() +
  geom_line() +
  labs(subtitle = "Mikkel, 10000 points",
       x = "probability of Mikkel being Right",
       y = "posterior probability") +
  theme(panel.grid = element_blank())+geom_line(aes(x=p_grid,y=dnorm(p_grid, mean = 0.5, sd = 0.05)/1e4,color="red"))+ scale_color_discrete(name = "Y series", labels = c("Posterior distribution", "Prior distribution"))
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-8-4.png)

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

![](Assignment2_files/figure-markdown_github/unnamed-chunk-9-1.png)

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

#We can retrive a function like this
#source("C:/Users/katri/OneDrive/Dokumenter/Cognitive Science/4th semester/Computational modeling/R code/my_useful_functions.R")
  
# We make a plot with the function
pretty_plot(p_grid = kt_results$grid, 
            prior = inf_prior, 
            likelihood = kt_results$likelihood, 
            posterior = kt_results$teacher_posterior, title = " ")
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-9-2.png)

``` r
#This is something else...
sum(abs(kt_results$likelihood - kt_results$teacher_posterior) ) / length(kt_results$likelihood)
```

    ## [1] 0.8098462

``` r
sum(abs(inf_prior - kt_results$teacher_posterior)*bin_size)
```

    ## [1] 0.3016233

``` r
plot(p_grid,kt_results$teacher_posterior-0.6)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-9-3.png)

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
Bayesian vs. frequentist models

2- Provide at least one plot and one written line discussing prediction
errors for each of the teachers.

``` r
# We make a new df for this years observations
results_df_2 <- data.frame(teacher = c("RF", "KT", "JS", "MW"),
                         correct = c(9, 8, 148, 34), 
                         n_quest = c(10, 12, 172, 65))
#We define p_grid
p_grid <-   p_grid <- seq(0,1, length.out = 10000)
#We define the bin_size
bin_size <- abs(p_grid[1] - p_grid[2])
#We define an informed prior
inf_prior <- dnorm(p_grid, 0.8, 0.2)

# We use the function to calculate the old kt results
kt_results <- calc_teacher(2, 2, prior = inf_prior)
# We plot it
plot(kt_results$grid, kt_results$teacher_posterior)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
#Now we want to calculate the new results with the old results as a prior
# We use the function to calculate the old kt results
kt_results_2 <- calc_teacher(8, 12, prior = kt_results$teacher_posterior)
# We plot it
plot(kt_results_2$grid, kt_results_2$teacher_posterior)
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-2.png)

``` r
# We plot the new
pretty_plot(p_grid, 
            prior = kt_results$teacher_posterior, 
            likelihood = kt_results_2$likelihood, 
            posterior = kt_results_2$teacher_posterior, title = "Kristian's new posterior distribution")
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-3.png)

``` r
###Mikkel###

mw_old<-calc_teacher(66,132,inf_prior,length_out = 10000)
#calculate new results
mw_new<-calc_teacher(34,65,mw_old$teacher_posterior,length_out = 10000)

#function(p_grid, prior, likelihood, posterior, title = " ")
mw_old_plot<-pretty_plot(p_grid,inf_prior,mw_old$likelihood,mw_old$teacher_posterior)
mw_old_plot
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-4.png)

``` r
mw_new_plot<-pretty_plot(p_grid,mw_old$teacher_posterior,mw_new$likelihood,mw_new$teacher_posterior,"Mikkel´s new posterior distribution")
mw_new_plot
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-5.png)

``` r
###Josh###

j_old<-calc_teacher(160,198,inf_prior,length_out = 10000)
#calculate new results
j_new<-calc_teacher(148,172,j_old$teacher_posterior,length_out = 10000)

#function(p_grid, prior, likelihood, posterior, title = " ")
j_old_plot<-pretty_plot(p_grid,inf_prior,j_old$likelihood,j_old$teacher_posterior)
j_old_plot
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-6.png)

``` r
j_new_plot<-pretty_plot(p_grid,j_old$teacher_posterior,j_new$likelihood,j_new$teacher_posterior,"Josh´s new posterior distribution")
j_new_plot
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-7.png)

``` r
###riccardo###

r_old<-calc_teacher(3,6,inf_prior,length_out = 10000)
#calculate new results
r_new<-calc_teacher(9,10,r_old$teacher_posterior,length_out = 10000)

#function(p_grid, prior, likelihood, posterior, title = " ")
r_old_plot<-pretty_plot(p_grid,inf_prior,r_old$likelihood,r_old$teacher_posterior)
r_old_plot
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-8.png)

``` r
r_new_plot<-pretty_plot(p_grid,r_old$teacher_posterior,r_new$likelihood,r_new$teacher_posterior,"Riccardo´s new posterior distribution")
r_new_plot
```

![](Assignment2_files/figure-markdown_github/unnamed-chunk-10-9.png)

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

Notes for part 2

Subtracting map post 1 from map post 2. Then the errors are left. They
will be distributed e.g. normally distributed around 0 –&gt; then it is
a relatively good fit We can also look at HDPI

My notes for Kenneth’s class

standardize posterior. Divide by the sum of the unstandardized
posterior. The posterior is the upper part of bayes theorem. The lower
part of bayes theorem is a scaling of the y axes. It’s a constant. It
makes sure we don’t sum to more than 1. We can standardize the posterior
by saying un\_st\_post/sum(un\_st\_post\*bin\_size) We draw rectangles
under the distribution basically.  
The bin\_size is the distance between two points in the grid. Ypu can
only call it density when it sums to one meaning standardized. We take
the interval over the posterior.
