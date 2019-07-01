




# A ride


## Introduction {#introduction}

Our ambition is to present a gentle  introduction to the inference of a causal
quantity whose statistical analysis is typical  and thus paves the way to more
involved analyses.   The introduction  weaves together  two main  threads, one
theoretical and the other computational.

### A causal story  {#causal-story}

We focus on a causal story where a  random reward (a real number between 0 and
1) depends  on the action  undertaken (one among  two) and the  random context
where the action is  performed (summarized by a real number  between 0 and 1).
The  causal  quantity  of  interest  is the  average  difference  of  the  two
counterfactual rewards.

We  will  build  several  estimators  and  discuss  their  respective  merits,
theoretically  and  computationally. The  construction  of  the most  involved
estimator will  unfold in  *targeted learning territory*,  at the  frontier of
machine  learning and  semiparametrics,  the statistical  theory of  inference
based on semiparametric models.

### The `tlrider` package {#tlrider-package}

The  computational illustrations  will  be developed  based  on the  companion
package `tlrider`. Make  sure you have installed it,  for instance by
running the following chunk of code:


```r
devtools::install_github("achambaz/tlride/tlrider")
```

Note that additional  packages are required, among  which `tidyverse` [@r4ds],
`caret` and  `ggdag`. Assuming that  these are installed  too, we can  run the
next chunk of code:


```r
set.seed(54321) ## because reproducibility matters...
library(tidyverse)
library(caret)
library(ggdag)
library(tlrider)
```

### What we will discuss {#discuss}

...
 
## A simulation study {#simulation-study}



### Reproducible experiment as a law {#reproducible-experiment}

We are interested in a reproducible  experiment. Every time this experiment is
run, it  generates an observation that  we call $O$.  We view $O$ as  a random
variable drawn from *the law of the experiment* that we denote by $P_{0}$. 


We  view $P_{0}$  as  an element  of  *the  model* $\calM$.   The  model is  a
collection of laws.  In particular, the  model contains all laws that we think
may plausibly describe  the law of the experiment.  Thus,  the choice of model
is based on our scientific knowledge of the experiment. The more we know about
the experiment,  the smaller is  $\calM$.  In all  our examples, we  use large
models that reflect a lack of knowledge about many aspects of the experiment.

### A synthetic reproducible experiment {#synthetic-experiment}

Instead  of considering  a  real-life reproducible  experiment,  we focus  for
pedagogical purposes  on a  *synthetic* reproducible  experiment. Thus  we can
from  now  on  take on  two  different  roles:  that  of an  *oracle*  knowing
completely the nature of the experiment, and that of a *statistician* eager to
know more about the experiment by observing some of its outputs.

Let us run the example built into the `tlrider` package:


```r
example(tlrider)
```

A few objects have been defined:


```r
ls()
#> [1] "another_experiment" "experiment"         "expit"             
#> [4] "logit"              "sigma0"
```

Function `expit` implements the link function  $\expit : \bbR \to ]0,1[$ given
by  $\expit(x) \equiv  (1 +  e^{-x})^{-1}$.  Function  `logit` implements  its
inverse function  $\logit : ]0,1[  \to \bbR$  given by $\logit(p)  \equiv \log
[p/(1-p)]$. 

Let us take a look at `experiment`:


```r
experiment
#> A law for (W,A,Y) in [0,1] x {0,1} x [0,1].
#> 
#> If the law is fully characterized, you can use method
#> 'sample_from' to sample from it.
#> 
#> If you built the law, or if you are an _oracle_, you can
#> also use methods 'reveal' to reveal its relevant features
#> (QW, Gbar, Qbar, qY -- see '?reveal'), and 'alter' to change
#> some of them.
#> 
#> If all its relevant features are characterized, you can
#> use methods 'evaluate_psi' to obtain the value of 'Psi' at
#> this law (see '?evaluate_psi') and 'evaluate_eic' to obtain
#> the efficient influence curve of 'Psi' at this law (see '?
#> evaluate_eic').
```

The law $P_{0}$ of the synthetic experiment `experiment` built by us generates
a generic observation $O$ that decomposes as \begin{equation*} O \equiv (W, A,
Y) \in [0,1] \times \{0,1\} \times [0,1].  \end{equation*} We interpret $W$ as
a real valued summary  measure of a random context where  an action $A$ chosen
among two is undertaken, leading to a real valued reward $Y$.

We can  sample from the experiment  (simply run `?sample_from` to  see the man
page of method `sample_from`). The next chunk of code runs the experiment five
times, independently:


```r
(five_obs <- sample_from(experiment, n = 5))
#>          W A     Y
#> [1,] 0.429 1 0.981
#> [2,] 0.454 1 0.855
#> [3,] 0.377 0 0.836
#> [4,] 0.461 1 0.582
#> [5,] 0.419 1 0.878
```
### Revealing `experiment` {#revealing-experiment}

Acting as oracles,  we can peek into `experiment` and  *reveal* a selection of
relevant  features  (simply run  `?reveal`  to  see  the  man page  of  method
`reveal`).  Made  by us,  the selection  exhibits features  that will  play an
important role in the text.


```r
relevant_features <- reveal(experiment)
names(relevant_features)
#> [1] "QW"          "Gbar"        "Qbar"        "qY"          "sample_from"
```

We have  an oracular knowledge of  `experiment` and can thus  comment upon the
features of $P_{0}$ revealed in `relevant_features`.

#### `QW` {-}

The `QW` feature describes the marginal law of $W$, that we call $Q_{0,W}$.^[A
summary   of   the   notation   used  throughout   the   text   is   presented
[here](#notation).]


```r
relevant_features$QW
#> function(W,
#>                     mixture_weights = c(1/10, 9/10, 0),
#>                     mins = c(0, 11/30, 0),
#>                     maxs = c(1, 14/30, 1)) {
#>         out <- sapply(1:length(mixture_weights),
#>                       function(ii){
#>                         mixture_weights[ii] *
#>                           stats::dunif(W,
#>                                        min = mins[ii],
#>                                        max = maxs[ii])
#>                       })
#>         return(rowSums(out))
#>       }
#> <environment: 0xe822610>
```

It  appears that  $Q_{0,W}$ is  a  mixture of  the uniform  laws over  $[0,1]$
(weight  $1/10$)  and  $[11/30,14/30]$  (weight  $9/10$).^[We  fine-tuned  the
marginal  law $Q_{0,W}$  of $W$  to  make it  easier  later on  to drive  home
important messages.]

#### `Gbar` {-}

The `Gbar`  feature describes the  conditional probability  of action $A  = 1$
given  $W$. For  each $a  \in  \{0,1\}$, we  denote \begin{align*}  \Gbar_0(W)
&\equiv \Pr_{P_0}(A  = 1 |  W), \\\ell\Gbar_0(a,W)  &\equiv \Pr_{P_0}(A =  a |
W).\end{align*}    Obviously,    \begin{equation*}\ell\Gbar_{0}(A,W)    \equiv
A\Gbar_{0}(W) + (1-A) (1-\Gbar_{0}(W)).\end{equation*}


```r
relevant_features$Gbar
#> function(W) {
#>         expit(1 + 2 * W - 4 * sqrt(abs((W - 5/12))))
#>       }
#> <environment: 0xe822610>
```

Note how real numbers of the form $1 + 2W - 4 * \sqrt{|W - 5/12|})$ are mapped
into the interval $[0,1]$ by the $\expit$ link function.

#### `qY` {-}

The `qY` feature  describes the conditional density of $Y$  given $A$ and $W$.
For each $y\in ]0,1[$, we denote by $q_{0,Y}(y, A, W)$ the conditional density
evaluated at $y$ of $Y$ given $A$ and $W$.  


```r
relevant_features$qY
#> function(obs, Qbar, shape10 = 2, shape11 = 3){
#>         A <- obs[, "A"]
#>         AW <- obs[, c("A", "W")]
#>         QAW <- Qbar(AW)
#>         shape1 <- ifelse(A == 0, shape10, shape11)
#>         stats::dbeta(Y,
#>                      shape1 = shape1,
#>                      shape2 = shape1 * (1 - QAW) / QAW)
#>       }
#> <environment: 0xe822610>
```

It appears that the  conditional law of $Y$ given $A$ and $W$  is the Beta law
with  conditional mean  and variance  characterized by  the `Qbar`  feature of
`experiment` (see below) and the `shape10` and `shape11` parameters.

#### `Qbar` {-}

As for the `Qbar` feature, it describes  the conditional mean of $Y$ given $A$
and $W$. 


```r
relevant_features$Qbar
#> function(AW) {
#>         A <- AW[, "A"]
#>         W <- AW[, "W"]
#>         A * (cos((-1/2 + W) * pi) * 2/5 + 1/5 +
#>              (1/3 <= W & W <= 1/2) / 5 +
#>              (W >= 3/4) * (W - 3/4) * 2) +
#>           (1 - A) * (sin(4 * W^2 * pi) / 4 + 1/2) 
#>       }
#> <bytecode: 0xf3afad8>
#> <environment: 0xe822610>
```

We denote  $\Qbar_0(A,W) =  \Exp_{P_{0}}(Y|A,W)$ the  conditional mean  of $Y$
given $A$  and $W$.  Note  how $\Qbar_0(A,W)$ does  depend heavily on  $A$ and
$W$.  We   refer  the  reader   to  Section  \@ref(exo-visualization)   for  a
visualization of $\Qbar_{0}$.

#### `sample_from` {-}

Finally,  the  `sample_from`   feature  is  the  function   called  by  method
`sample_from`  when  it  is  applied  to   an  object  of  class  `LAW`,  like
`experiment`.


```r
relevant_features$sample_from
#> function(n, ideal = FALSE) {
#>         ## preliminary
#>         n <- R.utils::Arguments$getInteger(n, c(1, Inf))
#>         ideal <- R.utils::Arguments$getLogical(ideal)
#>         ## ## 'Gbar' and 'Qbar' factors
#>         Gbar <- experiment$.Gbar
#>         Qbar <- experiment$.Qbar
#>         ## sampling
#>         ## ## context
#>         params <- formals(experiment$.QW)
#>         mixture_weights <- eval(params$mixture_weights)
#>         mins <- eval(params$mins)
#>         maxs <- eval(params$maxs)
#>         W <- sample_from_mixture_of_uniforms(n, mixture_weights,
#>                                              mins, maxs)
#>         ## ## counterfactual rewards
#>         zeroW <- cbind(A = 0, W)
#>         oneW <- cbind(A = 1, W)
#>         Qbar_zeroW <- Qbar(zeroW)
#>         Qbar_oneW <- Qbar(oneW)
#>         Yzero <- stats::rbeta(n,
#>                               shape1 = 2,
#>                               shape2 = 2 * (1 - Qbar_zeroW) / Qbar_zeroW)
#>         Yone <- stats::rbeta(n,
#>                              shape1 = 3,
#>                              shape2 = 3 * (1 - Qbar_oneW) / Qbar_oneW)
#>         ## ## action undertaken
#>         A <- stats::rbinom(n, size = 1, prob = Gbar(W))
#>         ## ## actual reward
#>         Y <- A * Yone + (1 - A) * Yzero
#>         ## ## observation
#>         if (ideal) {
#>           obs <- cbind(W = W, Yzero = Yzero, Yone = Yone, A = A, Y = Y)
#>         } else {
#>           obs <- cbind(W = W, A = A, Y = Y)
#>         }
#>         return(obs)
#>       }
#> <bytecode: 0xd2a6a80>
#> <environment: 0xe822610>
```

We will comment  upon the `ideal` argument in the  above `sample_from` feature
in Section \@ref(parameter-first-pass).


## &#9881; \gear Visualization {#exo-visualization}

1.  Run the following chunk of code.  It visualizes the conditional mean
   $\Qbar_{0}$.


```r
Gbar <- relevant_features$Gbar
Qbar <- relevant_features$Qbar
QW <- relevant_features$QW

features <- tibble(w = seq(0, 1, length.out = 1e3)) %>%
  mutate(Qw = QW(w),
         Gw = Gbar(w),
         Q1w = Qbar(cbind(A = 1, W = w)),
         Q0w = Qbar(cbind(A = 0, W = w)),
         blip_Qw = Q1w - Q0w)

features %>% select(-Qw, -Gw) %>%
  rename("Q(1,.)" = Q1w,
         "Q(0,.)" = Q0w,
         "Q(1,.) - Q(0,.)" = blip_Qw) %>%
  gather("f", "value", -w) %>%
  ggplot() +
  geom_line(aes(x = w, y = value, color = f), size = 1) +
  labs(y = "f(w)", title = bquote("Visualizing" ~ bar(Q)[0])) +
  ylim(NA, 1)
```

<img src="img/exercise:visualize-1.png" width="70%" style="display: block; margin: auto;" />

2. Adapt the  above chunk of code to visualize  the marginal density $Q_{0,W}$
   and conditional probability $\Gbar_{0}$.

## &#9881; \gear Make your own experiment {#exo-make-own-experiment}

You can easily make your own experiment. 

1. Check out the man page of method `alter` by running `?alter`. 

2. Run the following chunk of code:


```r
my_experiment <- LAW() ## creates an object of class 'LAW'
alter(my_experiment,   ## characterize its relevant features
      QW = function(W) {
        out <- rep_len(0, length(W))
        out[W == 0] <- 1/4
        out[W == 1] <- 3/4
        return(out)
      },
      Gbar = function(W) {
        out <- rep_len(0, length(W))
        out[W == 0] <- 1/3
        out[W == 1] <- 3/5
        return(out)
      },
      Qbar = function(AW) {
        probs <- matrix(c(1/2, 2/3, 7/8, 4/5), ncol = 2,
                        dimnames = list(c("A=0", "A=1"),
                                        c("W=0", "W=1")))
        probs[cbind(AW[, "A"] + 1, AW[, "W"] + 1)]
      },
      qY = function(obs) {
        probs <- matrix(c(1/2, 2/3, 7/8, 4/5), ncol = 2,
                        dimnames = list(c("A=0", "A=1"),
                                        c("W=0", "W=1")))
        probs <- probs[cbind(obs[, "A"] + 1, obs[, "W"] + 1)]
        obs[, "Y"] * probs + (1 - obs[, "Y"]) * (1 - probs)
      },
      sample_from = function(n) {
        ## preliminary
        n <- R.utils::Arguments$getInteger(n, c(1, Inf))
        ## 'QW', 'Gbar' and 'Qbar' features
        QW <- my_experiment$.QW
        Gbar <- my_experiment$.Gbar
        Qbar <- my_experiment$.Qbar
        ## sampling
        W <- rbinom(n, size = 1, prob = QW(1))
        A <- rbinom(n, size = 1, prob = Gbar(W))
        AW <- cbind(W = W, A = A)
        Y <- rbinom(n, size = 1, Qbar(AW))
        return(cbind(AW, Y = Y))
      })
```

3. What does the next chunk do?


```r
(sample_from(my_experiment, 3))
#>      W A Y
#> [1,] 0 0 1
#> [2,] 1 1 1
#> [3,] 1 0 1
```

4. Characterize entirely the law of `my_experiment`. Hint:


```r
obs <- sample_from(my_experiment, 1e4)
obs %>% as.tibble %>% group_by(W, A, Y) %>% summarize(how_many = n())
#> # A tibble: 8 x 4
#> # Groups:   W, A [?]
#>       W     A     Y how_many
#>   <int> <int> <int>    <int>
#> 1     0     0     0      826
#> 2     0     0     1      808
#> 3     0     1     0      285
#> 4     0     1     1      556
#> 5     1     0     0      419
#> 6     1     0     1     2665
#> # ... with 2 more rows
obs %>% as.tibble %>% group_by(W, A) %>% summarize(prob = mean(Y))
#> # A tibble: 4 x 3
#> # Groups:   W [?]
#>       W     A  prob
#>   <int> <int> <dbl>
#> 1     0     0 0.494
#> 2     0     1 0.661
#> 3     1     0 0.864
#> 4     1     1 0.798
```

5. Now, make your own experiment.

# The parameter of interest {#parameter}

## The parameter of interest {#parameter-first-pass}

### Definition {#definition}

It happens that we especially care for a finite-dimensional feature of $P_{0}$
that  we   denote  by  $\psi_{0}$.    Its  definition  involves  two   of  the
aforementioned infinite-dimensional  features, the  marginal law  $Q_{0,W}$ of
$W$  and  the  conditional  mean  $\Qbar_{0}$   of  $Y$  given  $A$  and  $W$:
\begin{align}  \psi_{0}  &\equiv  \int \left(\Qbar_{0}(1,  w)  -  \Qbar_{0}(0,
w)\right)    dQ_{0,W}(w)    (\#eq:psi-zero)\\     \notag    &=    \Exp_{P_{0}}
\left(\Exp_{P_0}(Y \mid  A =  1, W)  - \Exp_{P_0}(Y  \mid A  = 0,  W) \right).
\end{align}

Acting  as  oracles,  we  can   compute  explicitly  the  numerical  value  of
$\psi_{0}$.   The  `evaluate_psi`  method  makes  it  very  easy  (simply  run
`?estimate_psi` to see the man page of the method):



```r
(psi_zero <- evaluate_psi(experiment))
#> [1] 0.0832
```

### A causal interpretation {#causal-interpretation}

Our interest in $\psi_{0}$  is of causal nature.  Taking a  closer look at the
`sample_from` feature of `experiment` reveals indeed that the random making of
an  observation $O$  drawn from  $P_{0}$ can  be summarized  by the  following
causal graph:

(ref:DAG) Causal graph summarizing the inner causal mechanism at play in `experiment`. 


```r
dagify(
  Y ~ A + Y1 + Y0, A ~ W, Y1 ~ W, Y0 ~ W,
  labels = c(Y = "Actual reward",
             A = "Action",
             Y1 = "Counterfactual reward\n of action 1",
             Y0 = "Counterfactual reward\n of action 0",
             W = "Context of action"),
  coords = list(
    x = c(W = 0, A = -1, Y1 = 1.5, Y0 = 0.25, Y = 1),
    y = c(W = 0, A = -1, Y1 = -0.5, Y0 = -0.5, Y = -1)),
  outcome = "Y",
  exposure = "A",
  latent = c("Y0", "Y1")) %>% tidy_dagitty %>%
  ggdag(text = TRUE, use_labels = "label") + theme_dag_grey()
```

<div class="figure" style="text-align: center">
<img src="img/DAG-1.png" alt="(ref:DAG)" width="70%" />
<p class="caption">(\#fig:DAG)(ref:DAG)</p>
</div>

In words, the experiment unfolds like this (see also Section \@ref(npsem)):

1. a context of action $W \in [0,1]$ is randomly generated;

2.    two counterfactual  rewards $Y_{0}\in  [0,1]$ and  $Y_{1}\in [0,1]$  are
   generated conditionally on $W$;
   
3. an  action $A  \in \{0,1\}$  (among two possible  actions called  $a=0$ and
   $a=1$)   is  undertaken,   *(i)*  knowing   the  context   but  *not*   the
   counterfactual rewards,  and *(ii)*  in such  a way  that both  actions can
   always be considered;
   
4. the  action yields  a reward  $Y$, which equals  either $Y_{0}$  or $Y_{1}$
   depending on whether action $a=0$ or $a=1$ has been undertaken;
   
5. summarize  the course  of the experiment  with $O \equiv  (W, A,  Y)$, thus
   concealing $Y_{0}$ and $Y_{1}$.


The above description  of the experiment is useful to  reinforce what it means
to run the "ideal" experiment by setting  argument `ideal` to `TRUE` in a call
to  `sample_from` for  `experiment`  (see Section  \@ref(causal-computation)).
Doing so  triggers a modification of  the nature of the  experiment, enforcing
that the counterfactual rewards $Y_{0}$ and  $Y_{1}$ be part of the summary of
the   experiment   eventually.    In   light   of   the   above   enumeration,
\begin{equation*}  \bbO \equiv  (W,  Y_{0}, Y_{1},  A,  Y) \end{equation*}  is
output,  as  opposed  to  its  summary  measure  $O$.   This  defines  another
experiment and its law, that we denote $\bbP_{0}$.

It is straightforward to [show that](#identification) 

\begin{align}
\psi_{0} &= \Exp_{\bbP_{0}} \left(Y_{1} - Y_{0}\right) (\#eq:psi-zero-bis) \\
&= \Exp_{\bbP_{0}}(Y_1) - \Exp_{\bbP_{0}}(Y_0).  \notag 
\end{align} 

Thus, $\psi_{0}$  describes the average  difference of the  two counterfactual
rewards.  In other  words, $\psi_{0}$ quantifies the difference  in average of
the reward  one would  get in a  world where one  would always  enforce action
$a=1$ with the reward one would get  in a world where one would always enforce
action  $a=0$.  This  said,  it  is worth  emphasizing  that  $\psi_{0}$ is  a
well-defined parameter beyond its causal interpretation, and that it describes
a standardized association between the action $A$ and reward $Y$.

### A causal computation {#causal-computation}

We  can use  our position  as oracles  to sample  observations from  the ideal
experiment.  We call `sample_from` for  `experiment` with its argument `ideal`
set to `TRUE`  in order to numerically approximate $\psi_{0}$.   By the law of
large  numbers,  the  following  code approximates  $\psi_{0}$  and  shows  it
approximate value.


```r
B <- 1e5
ideal_obs <- sample_from(experiment, B, ideal = TRUE)
(psi_approx <- mean(ideal_obs[, "Yone"] - ideal_obs[, "Yzero"]))
#> [1] 0.0841
```

The object  `psi_approx` contains  an approximation to  $\psi_0$ based  on `B`
observations from the  ideal experiment.  The random  sampling of observations
results  in uncertainty  in  the numerical  approximation  of $\psi_0$.   This
uncertainty can be  quantified by constructing a 95\%  confidence interval for
$\psi_0$.  The central  limit theorem and Slutsky's lemma  [allow us](#clt) to
build such an interval as follows.


```r
sd_approx <- sd(ideal_obs[, "Yone"] - ideal_obs[, "Yzero"])
alpha <- 0.05
(psi_approx_CI <- psi_approx + c(-1, 1) *
   qnorm(1 - alpha / 2) * sd_approx / sqrt(B))
#> [1] 0.0822 0.0860
```

We note that the interpretation of this confidence interval is that in 95\% of 
draws of size `B` from the ideal data generating experiment, the true value of 
$\psi_0$ will be contained in the generated confidence interval. 

## &#9881; \gear An alternative parameter of interest {#exo-alternative-parameter-first-pass}

Equality    \@ref(eq:psi-zero-bis)     shows    that     parameter    $\psi_0$
\@ref(eq:psi-zero) is the  difference in average rewards if  we enforce action
$a = 1$ rather than $a = 0$.  An alternative way to describe the rewards under
different actions involves *quantiles* as opposed to *averages*.

Let \begin{equation*} Q_{0,Y}(y,  A, W) \equiv \int_{0}^y q_{0,Y}(u,  A, W) du
\end{equation*} be the conditional cumulative distribution of reward $Y$ given
$A$ and $W$, evaluated  at $y \in ]0,1[$, that is implied  by $P_0$.  For each
action $a \in \{0,1\}$ and $c \in ]0,1[$, introduce 

\begin{equation}
\gamma_{0,a,c}  \equiv  \inf  \left\{y  \in  ]0,1[ :  \int  Q_{0,Y}(y,  a,  w)
dQ_{0,W}(w) \ge c \right\}. (\#eq:def-quantile)
\end{equation}

It   is  not   very   difficult  to   check  (see   Problem   1  below)   that
\begin{equation}\gamma_{0,a,c} =  \inf\left\{y \in ]0,1[  : \Pr_{\bbP_{0}}(Y_a
\leq   y)   \geq   c\right\}.    (\#eq:alter-gamma-zero)\end{equation}   Thus,
$\gamma_{0,a,c}$ can  be interpreted  as a covariate-adjusted  $c$-th quantile
reward     when     action     $a$    is     enforced.      The     difference
\begin{equation}\delta_{0,c}    \equiv    \gamma_{0,1,c}   -    \gamma_{0,0,c}
(\#eq:def-delta)\end{equation} is the $c$-th quantile counterpart to parameter
$\psi_{0}$ \@ref(eq:psi-zero).

1.  &#9761; \stixdanger{} Prove \@ref(eq:alter-gamma-zero).

2. &#9761; \stixdanger{} Compute the numerical value of $\gamma_{0,a,c}$ for each $(a,c)
   \in \{0,1\}  \times \{1/4,  1/2, 3/4\}$ using  the appropriate  features of
   `experiment` (see `relevant_features`).  Based on these results, report the
   numerical value of $\delta_{0,c}$ for each $c \in \{1/4, 1/2, 3/4\}$.
   
3. Approximate  the numerical values  of $\gamma_{0,a,c}$ for each  $(a,c) \in
   \{0,1\}  \times \{1/4,  1/2,  3/4\}$ by  drawing a  large  sample from  the
   "ideal"  data experiment  and using  empirical quantile  estimates.  Deduce
   from these results  a numerical approximation to $\delta_{0,c}$  for $c \in
   \{1/4, 1/2, 3/4\}$. Confirm that  your results closely match those obtained
   in the previous problem.


## The statistical mapping of interest  {#parameter-second-pass}

The noble way to define a statistical parameter  is to view it as the value of
a statistical  mapping at the law  of the experiment of  interest.  Beyond the
elegance, this has paramount statistical implications.

### Opening discussion {#opening}

Oftentimes, the premise of a statistical  analysis is presented like this. One
assumes  that the  law $P_{0}$  of  the experiment  of interest  belongs to  a
statistical     model    \begin{equation*}\{P_{\theta}     :    \theta     \in
T\}.\end{equation*} The statistical model is identifiable, meaning that if two
elements $P_{\theta}$  and $P_{\theta'}$ coincide, then  necessarily $\theta =
\theta'$.   Therefore, there  exists a  unique  $\theta_{0} \in  T$ such  that
$P_{0}     =    P_{\theta_{0}}$,     and     one     wishes    to     estimate
$\theta_{0}$.\index{identifiability}

For instance,  each $P_{\theta}$ could be  the Gaussian law with  mean $\theta
\in T  \equiv \bbR$ and variance  1, and one  could wish to estimate  the mean
$\theta_{0}$ of $P_{0}$. To do so, one could rely on $n$ observations $X_{1}$,
\ldots,  $X_{n}$  drawn  independently   from  $P_{0}$.   The  empirical  mean
\begin{equation*}\theta_{n}       \equiv      \frac{1}{n}       \sum_{i=1}^{n}
X_{i}\end{equation*} estimates $\theta_{0}$.  If  we assume that $\Var_{P_{0}}
(X_{1})$ is  finite, then  $\theta_{n}$ satisfies  many useful  properties. In
particular, it can be used [to construct confidence intervals](#clt).

Of course, the mean of a law is defined beyond the small model $\{P_{\theta} :
\theta \in \bbR\}$.   Let $\calM$ be the  set of laws $P$ on  $\bbR$ such that
$\Var_{P}(X)$ is finite.  In particular, $P_{0}  \in \calM$.  For every $P \in
\calM$,  the mean  $E_{P}(X)$ is  well defined.   Thus, we  can introduce  the
*statistical    mapping*    $\Theta   :    \calM    \to    \bbR$   given    by
\begin{equation*}\Theta(P) \equiv E_{P}(X).\end{equation*}

Interestingly, the empirical measure $P_{n}$^[The empirical measure $P_{n}$ is
the law such that  *(i)* $X$ drawn from $P_{n}$ takes  its values in $\{X_{1},
\ldots,  X_{n}\}$,  and *(ii)*  $X=X_{i}$  with  probability $n^{-1}$]  is  an
element  of  $\calM$.  Therefore,  the statistical  mapping  $\Theta$  can  be
evaluated   at    $P_{n}$:   \begin{equation*}\Theta(P_{n})    =   \frac{1}{n}
\sum_{i=1}^{n} X_{i}  = \theta_{n}.\end{equation*} We *recover*  the empirical
mean, and  understand that it  is a *substitution*  estimator of the  mean: in
order to estimate $\Theta(P_{0})$, we  *substitute* $P_{n}$ for $P_{0}$ within
$\Theta$.

Substitution-based estimators are particularly  valuable notably because they,
by construction, satisfy  all the constraints to which  the targeted parameter
is  subjected.   Some of  the  estimators  that  we  will build  together  are
substitution-based, some are not. 

### The parameter as the value of a statistical mapping at the experiment {#parameter-mapping}

We now go back to our main  topic of interest. Suppose we know beforehand that
$O$ drawn from $P_{0}$ takes its  values in $\calO \equiv [0,1] \times \{0,1\}
\times [0,1]$ and that $\Gbar_{0}(W) \equiv _Pr_{P_{0}}(A=1|W)$ is bounded away from
zero and one  $Q_{0,W}$-almost surely (this is the case  indeed).  Then we can
define  model  $\calM$ as  the  set  of all  laws  $P$  on $\calO$  such  that
\begin{equation*}\Gbar(W)  \equiv \Pr_{P}(A=1|W)\end{equation*}  is bounded  away
from zero and one $Q_{W}$-almost surely,  where $Q_{W}$ is the marginal law of
$W$ under $P$.

Let us also define generically  $\Qbar$ as \begin{equation*}\Qbar (A,W) \equiv
\Exp_{P} (Y|A, W).\end{equation*}  Note how we have  suppressed the dependence
of $\Gbar$ and $\Qbar$ on $P$ for notational simplicity.

Central to our approach  is viewing $\psi_{0}$ as the value  at $P_{0}$ of the
statistical  mapping   $\Psi$  from   $\calM$  to  $[0,1]$   characterized  by
\begin{align}  \Psi(P) &\equiv  \int  \left(\Qbar(1, w)  - \Qbar(0,  w)\right)
dQ_{W}(w) (\#eq:psimap)\\ &= \Exp_{P}  \left(\Qbar(1, W) - \Qbar(0, W)\right),
\notag \end{align} a clear extension of \@ref(eq:psi-zero).

### The value of the statistical mapping at another experiment {#value-another-experiment}

When  we  ran   `example(tlrider)`  earlier,  we  created   an  object  called
`another_experiment`:


```r
another_experiment
#> A law for (W,A,Y) in [0,1] x {0,1} x [0,1].
#> 
#> If the law is fully characterized, you can use method
#> 'sample_from' to sample from it.
#> 
#> If you built the law, or if you are an _oracle_, you can
#> also use methods 'reveal' to reveal its relevant features
#> (QW, Gbar, Qbar, qY -- see '?reveal'), and 'alter' to change
#> some of them.
#> 
#> If all its relevant features are characterized, you can
#> use methods 'evaluate_psi' to obtain the value of 'Psi' at
#> this law (see '?evaluate_psi') and 'evaluate_eic' to obtain
#> the efficient influence curve of 'Psi' at this law (see '?
#> evaluate_eic').
reveal(another_experiment)
#> $QW
#> function (x, min = 1/10, max = 9/10) 
#> {
#>     stats::dunif(x, min = min, max = max)
#> }
#> <environment: 0xf61e700>
#> 
#> $Gbar
#> function (W) 
#> {
#>     sin((1 + W) * pi/6)
#> }
#> <environment: 0xf61e700>
#> 
#> $Qbar
#> function (AW, h) 
#> {
#>     A <- AW[, "A"]
#>     W <- AW[, "W"]
#>     expit(logit(A * W + (1 - A) * W^2) + h * 10 * sqrt(W) * A)
#> }
#> <environment: 0xf61e700>
#> 
#> $qY
#> function (obs, Qbar, shape1 = 4) 
#> {
#>     AW <- obs[, c("A", "W")]
#>     QAW <- Qbar(AW)
#>     stats::gdbeta(Y, shape1 = shape1, shape2 = shape1 * (1 - 
#>         QAW)/QAW)
#> }
#> <environment: 0xf61e700>
#> 
#> $sample_from
#> function (n, h) 
#> {
#>     n <- R.utils::Arguments$getInteger(n, c(1, Inf))
#>     h <- R.utils::Arguments$getNumeric(h)
#>     Gbar <- another_experiment$.Gbar
#>     Qbar <- another_experiment$.Qbar
#>     params <- formals(another_experiment$.QW)
#>     W <- stats::runif(n, min = eval(params$min), max = eval(params$max))
#>     A <- stats::rbinom(n, size = 1, prob = Gbar(W))
#>     params <- formals(another_experiment$.qY)
#>     shape1 <- eval(params$shape1)
#>     QAW <- Qbar(cbind(A = A, W = W), h = h)
#>     Y <- stats::rbeta(n, shape1 = shape1, shape2 = shape1 * (1 - 
#>         QAW)/QAW)
#>     obs <- cbind(W = W, A = A, Y = Y)
#>     return(obs)
#> }
#> <environment: 0xf61e700>
(two_obs_another_experiment <- sample_from(another_experiment, 2, h = 0))
#>          W A     Y
#> [1,] 0.585 1 0.507
#> [2,] 0.347 1 0.345
```
By taking an  oracular look at the output  of `reveal(another_experiment)`, we
discover that  the law $\Pi_{0}  \in \calM$  encoded by default  (*i.e.*, with
`h=0`) in `another_experiment` differs starkly from $P_{0}$.

However,  the  parameter  $\Psi(\Pi_{0})$ is  well  defined.   Straightforward
algebra shows  that $\Psi(\Pi_{0}) =  59/300$.  The numeric  computation below
confirms the equality.


```r
(psi_Pi_zero <- evaluate_psi(another_experiment, h = 0))
#> [1] 0.197
```



## &#9881; \gear Alternative statistical mapping {#exo-alternative-parameter-second-pass}

We        now        resume        the       exercize        of        Section
\@ref(exo-alternative-parameter-first-pass).    Like   we   did   in   Section
\@ref(parameter-second-pass), we  introduce a generic version  of the relevant
features $q_{0,Y}$  and $Q_{0,Y}$.  Specifically, we  define $q_{Y}(y,A,W)$ to
be the conditional density of $Y$ given $A$ and $W$, evaluated at $y$, that is
implied by a generic  $P \in \calM$.  Similarly, we use  $Q_{Y}$ to denote the
corresponding cumulative distribution function. 

The  covariate-adjusted $c$-th  quantile reward  for action  $a \in  \{0,1\}$,
$\gamma_{0,a,c}$ \@ref(eq:def-quantile), may be viewed as the value at $P_{0}$
of  a  mapping  $\Gamma_{a,c}$  from   $\calM$  to  $[0,1]$  characterized  by
\begin{equation*} \Gamma_{a,c}(P) = \inf\left\{y \in ]0,1[ : \int Q_{Y}(y,a,w)
dQ_W(w) \ge  c \right\}.   \end{equation*} The  difference in  $c$-th quantile
rewards, $\delta_{0,c}$  \@ref(eq:def-delta), may  similarly be viewed  as the
value  at   $P_{0}$  of  a   mapping  $\Delta_c$  from  $\calM$   to  $[0,1]$,
characterized  by  \begin{equation*}   \Delta_c(P)  \equiv  \Gamma_{1,c}(P)  -
\Gamma_{0,c}(P).  \end{equation*}

1. Compute the numerical value of $\Gamma_{a,c}(\Pi_0)$ for $(a,c) \in \{0,1\}
   \times   \{1/4,    1/2,   3/4\}$    using   the   relevant    features   of
   `another_experiment`.  Based  on these results, report  the numerical value
   of $\Delta_c(\Pi_0)$ for each $c \in \{1/4, 1/2, 3/4\}$.

2. Approximate the  value of $\Gamma_{0,a,c}(\Pi_{0})$ for  $(a,c) \in \{0,1\}
   \times \{1/4, 1/2,  3/4\}$ by drawing a large sample  from the "ideal" data
   experiment  and  using empirical  quantile  estimates.   Deduce from  these
   results a numerical  approximation to $\Delta_{0,c} (\Pi_{0})$  for each $c
   \in  \{1/4, 1/2,  3/4\}$.  Confirm  that your  results closely  match those
   obtained in the previous problem.
   
3. Building upon the code you  wrote to solve the previous problem, [construct
   a  confidence interval](#order)  with asymptotic  level
   $95\%$ for $\Delta_{0,c} (\Pi_{0})$, with $c \in \{1/4, 1/2, 3/4\}$.



## Representations {#parameter-third-pass}

In Section \@ref(parameter-second-pass), we reoriented  our view of the target
parameter to be  that of a statistical  functional of the law  of the observed
data. Specifically, we viewed the parameter as a function of specific features
of the observed data law, namely $Q_{W}$ and $\Qbar$. 

### Yet another representation {#yet-another}

It is straightforward to  [show an equivalent representation](#another-rep) of
the parameter as 

\begin{align}  \notag  \psi_{0}  &=  \int \frac{2a  -  1}{\ell\Gbar_0(a,w)}  y
dP_0(w,a,y)   \\   (\#eq:psi-zero-b)   &=   \Exp_{P_0}   \left(   \frac{2A   -
1}{\ell\Gbar_{0}(A,W)} Y \right).  \end{align}  Viewing again the parameter as
a  statistical   mapping  from  $\calM$   to  $[0,1]$,  it  also   holds  that
\begin{align} \notag  \Psi(P) &= \int \frac{2a-1}{\ell\Gbar(a,w)}  y dP(w,a,y)
\\  (\#eq:psi-zero-c)  &=  \Exp_{P}\left(\frac{2A -  1}{\ell\Gbar_{0}(A,W)}  Y
\right).  \end{align}

### From representations to estimation strategies {#rep-to-est}

Our reason for introducing this alternative  view of the target parameter will
become clear when we discuss estimation of the target parameter. Specifically,
the  representations  \@ref(eq:psi-zero)  and  \@ref(eq:psi-zero-b)  naturally
suggest different  estimation strategies  for $\psi_0$,  as hinted  in Section
\@ref(opening).  The former  suggests building an estimator  of $\psi_0$ using
estimators of  $\Qbar_0$ and  of $Q_{W,0}$.  The  latter suggests  building an
estimator of $\psi_0$ using estimators of $\ell\Gbar_0$ and of $P_0$.

We return to these ideas in later sections.

## &#9881; \gear Alternative representation {#exo-alternative-parameter-third-pass}

1. &#9761; \stixdanger{} Show that for $a' = 0,1$, $\gamma_{0,a',c}$ as defined in
\@ref(eq:def-quantile) can be  equivalently expressed as \begin{equation*}\inf
\left\{z \in ]0,1[  : \int \frac{\one\{a =  a'\}}{\ell\Gbar(a',W)} \one\{y \le
z\} dP_0(w,a,y) \ge c \right\}.\end{equation*}

# Smoothness {#smooth}



## Fluctuating  smoothly {#smooth-first-pass}

Within our view of the target  parameter as a statistical mapping evaluated at
the  law of  the  experiment, it  is  natural to  inquire  of properties  this
functional enjoys.  For example, we may  be interested in asking how the value
of $\Psi(P)$ changes as we consider laws  that *get nearer to* $P$ in $\calM$.
If small deviations from $P_0$ result in large changes in $\Psi(P_0)$, then we
might hypothesize  that it will be  difficult to produce stable  estimators of
$\psi_0$.  Fortunately, this  turns out  not to  be the  case for  the mapping
$\Psi$, and so we say that $\Psi$ is a *smooth* statistical mapping.

To discuss how $\Psi(P)$ changes for distributions that *get nearer* to $P$ in
the model, we require a more concrete notion of *getting-nearness*. The notion
hinges on fluctuations (or fluctuating models). 


### The `another_experiment` fluctuation {#fluctuations}


In  Section \@ref(value-another-experiment),  we discussed  the nature  of the
object   called   `another_experiment`   that   was  created   when   we   ran
`example(tlrider)`:
 

```r
another_experiment
#> A law for (W,A,Y) in [0,1] x {0,1} x [0,1].
#> 
#> If the law is fully characterized, you can use method
#> 'sample_from' to sample from it.
#> 
#> If you built the law, or if you are an _oracle_, you can
#> also use methods 'reveal' to reveal its relevant features
#> (QW, Gbar, Qbar, qY -- see '?reveal'), and 'alter' to change
#> some of them.
#> 
#> If all its relevant features are characterized, you can
#> use methods 'evaluate_psi' to obtain the value of 'Psi' at
#> this law (see '?evaluate_psi') and 'evaluate_eic' to obtain
#> the efficient influence curve of 'Psi' at this law (see '?
#> evaluate_eic').
```

The message  is a little  misleading. Indeed, `another_experiment` is  not *a*
law but,  rather, a *collection*  of laws  indexed by a  real-valued parameter
`h`. This oracular statement (we built the object!)  is evident when one looks
again at the `sample_from` feature of `another_experiment`:


```r
reveal(another_experiment)$sample_from
#> function(n, h) {
#>         ## preliminary
#>         n <- R.utils::Arguments$getInteger(n, c(1, Inf))
#>         h <- R.utils::Arguments$getNumeric(h)
#>         ## ## 'Gbar' and 'Qbar' factors
#>         Gbar <- another_experiment$.Gbar
#>         Qbar <- another_experiment$.Qbar
#>         ## sampling
#>         ## ## context
#>         params <- formals(another_experiment$.QW)
#>         W <- stats::runif(n, min = eval(params$min),
#>                    max = eval(params$max))
#>         ## ## action undertaken
#>         A <- stats::rbinom(n, size = 1, prob = Gbar(W))
#>         ## ## reward
#>         params <- formals(another_experiment$.qY)
#>         shape1 <- eval(params$shape1)
#>         QAW <- Qbar(cbind(A = A, W = W), h = h)
#>         Y <- stats::rbeta(n,
#>                           shape1 = shape1,
#>                           shape2 = shape1 * (1 - QAW) / QAW)
#>         ## ## observation
#>         obs <- cbind(W = W, A = A, Y = Y)
#>         return(obs)
#>       }
#> <bytecode: 0xa6a2180>
#> <environment: 0xf61e700>
```

Let us call $\Pi_{h} \in \calM$  the law encoded by `another_experiment` for a
given  `h`  taken  in   $]-1,1[$.   Note  that  \begin{equation*}\calP  \equiv
\{\Pi_h : h \in ]-1,1[\}\end{equation*}  defines a collection of laws, *i.e.*,
a statistical model.  

We say that $\calP$ is a  *submodel* of $\calM$ because $\calP \subset \calM$.
Moreover, we  say that  this submodel  is *through  $\Pi_0$* since  $\Pi_{h} =
\Pi_{0}$  when $h  =  0$. We  also  say  that $\calP$  is  a *fluctuation*  of
$\Pi_{0}$.

One could  enumerate many possible  submodels in $\calM$ through  $\Pi_0$.  It
turns out that all  that matters for our purposes is the  form of the submodel
in  a neighborhood  of $\Pi_0$.  We informally  say that  this local  behavior
describes the  *direction* of a  submodel through $\Pi_0$.  We  formalize this
notion Section \@ref(smooth-second-pass).

We now have a notion of how to  move through the model space $P \in \calM$ and
can study how  the value of the parameter  changes as we move away  from a law
$P$.  Above, we said  that $\Psi$ is a smooth parameter if  it does not change
"too much" as  we move towards $P$  in any particular direction.   That is, we
should hope that $\Psi$ is differentiable along our submodel at $P$. 

This idea too is formalized  in Section \@ref(smooth-second-pass). We now turn
to illustrating this idea numerically.

### Numerical illustration {#numerical-illus}

The code  below evaluates how the  parameter changes for laws  in $\calP$, and
approximates the  derivative of  the parameter along  the submodel  $\calP$ at
$\Pi_0$. Recall that  the numerical value of $\Psi(\Pi_{0})$  has already been
computed and is stored in object `psi_Pi_zero`.

(ref:psi-approx-psi-one) Evolution of statistical mapping $\Psi$ along fluctuation $\{\Pi_{h} : h \in H\}$.


```r
approx <- seq(-1, 1, length.out = 1e2)
psi_Pi_h <- sapply(approx, function(t) {
  evaluate_psi(another_experiment, h = t)
})
slope_approx <- (psi_Pi_h - psi_Pi_zero) / approx
slope_approx <- slope_approx[min(which(approx > 0))]
ggplot() +
  geom_point(data = data.frame(x = approx, y = psi_Pi_h), aes(x, y),
             color = "#CC6666") +
  geom_segment(aes(x = -1, y = psi_Pi_zero - slope_approx,
                   xend = 1, yend = psi_Pi_zero + slope_approx),
               arrow = arrow(length = unit(0.03, "npc")),
               color = "#9999CC") +
  geom_vline(xintercept = 0, color = "#66CC99") +
  geom_hline(yintercept = psi_Pi_zero, color = "#66CC99") +
  labs(x = "h", y = expression(Psi(Pi[h]))) 
```

<div class="figure" style="text-align: center">
<img src="img/psi-approx-psi-one-1.png" alt="(ref:psi-approx-psi-one)" width="70%" />
<p class="caption">(\#fig:psi-approx-psi-one)(ref:psi-approx-psi-one)</p>
</div>

The dotted curve  represents the function $h \mapsto  \Psi(\Pi_{h})$. The blue
line  represents the  tangent to  the previous  curve at  $h=0$, which  indeed
appears to be  differentiable around $h=0$.  In  Section \@ref(revisiting), we
derive a closed-form expression for the slope of the blue curve.


## &#9881; \gear Yet another experiment {#exo-yet-another-experiment}

1. Adapt  the code  from Problem 1  in Section  \@ref(exo-visualization) to
visualize  $w  \mapsto   \Exp_{\Pi_h}(Y  |  A  =  1,  W   =  w)$,  $w  \mapsto
\Exp_{\Pi_h}(Y | A =  0, W=w)$, and $w \mapsto \Exp_{\Pi_h}(Y | A  = 1, W=w) -
\Exp_{\Pi_h}(Y | A = 0, W=w)$, for $h \in \{-1/2, 0, 1/2\}$.

2. Run the following chunk of code.


```r
yet_another_experiment <- copy(another_experiment)
alter(yet_another_experiment,
      Qbar = function(AW, h){
        A <- AW[, "A"]
        W <- AW[, "W"]
        expit( logit( A * W + (1 - A) * W^2 ) + 
               h * (2*A - 1) / ifelse(A == 1,
                                      sin((1 + W) * pi / 6), 
                                      1 - sin((1 + W) * pi / 6)) *
               (Y - A * W + (1 - A) * W^2))
      })
```

3. Justify that `yet_another_fluctuation` characterizes another fluctuation of
$\Pi_{0}$.  Comment upon the similarities and differences between $\{\Pi_{h} :
h \in ]-1,1[\}$ and $\{\Pi_{h}' : h \in ]-1,1[\}$.


4. Repeat Problem 1 above with $\Pi_{h}'$ substituted for $\Pi_{h}$.

5. Re-produce Figure  \@ref(fig:psi-approx-psi-one) for the $\{\Pi_h'  : h \in
]-1,1[\}$ fluctuation.   Comment on  the similarities and  differences between
the resulting figure and Figure \@ref(fig:psi-approx-psi-one).  In particular,
how does the behavior  of the target parameter around $h  = 0$ compare between
laws $\Pi_0$ and $\Pi_0'$?

## &#9761; \stixdanger{} More on fluctuations and smoothness {#smooth-second-pass}

### Fluctuations

Let us now formally define what it means for statistical mapping $\Psi$ to be
smooth at every  $P \in \calM$.  For every $h  \in H \equiv ]-M^{-1},M^{-1}[$,
we can  define a  law $P_{h}  \in \calM$  by setting  $P_{h} \ll  P$^[That is,
$P_{h}$  is dominated  by $P$:  if an  event $A$  satisfies $P(A)  = 0$,  then
necessarily $P_{h} (A) = 0$ too.]  and 
\begin{equation}
\frac{dP_h}{dP} \equiv
1 + h s, (\#eq:fluct)
\end{equation} 
where   $s  :  \calO\to  \bbR$  is  a
(measurable) function of $O$ such that  $s(O)$ is not equal to zero $P$-almost
surely,  $\Exp_{P}  (s(O))  =  0$,  and  $s$ bounded  by  $M$.   We  make  the
observation that 
\begin{equation}
(i)  \quad P_h|_{h=0}  = P,\quad  (ii) \quad
\left.\frac{d}{dh}  \log \frac{dP_h}{dP}(O)\right|_{h=0}  =s(O).  (\#eq:score)
\end{equation}

Because  of *(i)*,  $\{P_{h} :  h \in  H\}$ is  a submodel  through $P$,  also
referred to as  a *fluctuation* of $P$.  The fluctuation  is a one-dimensional
submodel of $\calM$ with univariate parameter  $h \in H$.  We note that *(ii)*
indicates that  the score of this  submodel at $h =  0$ is $s$.  Thus,  we say
that the fluctuation is *in the direction* of $s$.

Fluctuations  of   $P$  do  not   necessarily  take   the  same  form   as  in
\@ref(eq:fluct).  No matter how the fluctuation is built, for our purposes the
most important feature of the fluctuation is its local shape in a neighborhood
of $P$.

### Smoothness and gradients

We  are  now  prepared  to  provide  a  formal  definition  of  smoothness  of
statistical mappings.  We say that a statistical mapping $\Psi$ is *smooth* at
every $P  \in \calM$ if  for each $P \in  \calM$, there exists  a (measurable)
function $D^{*}(P)  : \calO \to  \bbR$ such that $\Exp_{P}(D^{*}(P)(O))  = 0$,
$\Var_{P}(D^{*}(P)(O)) < \infty$, and, for  every fluctuation $\{P_{h} : h \in
H\}$  with  score  $s$  at  $h  =  0$,  the  real-valued  mapping  $h  \mapsto
\Psi(P_{h})$  is  differentiable   at  $h=0$,  with  a   derivative  equal  to
\begin{equation}  \Exp_{P}  \left(D^{*}(P)(O) s(O)\right).   (\#eq:derivative)
\end{equation}  The  object  $D^*(P)$  in  \@ref(eq:derivative)  is  called  a
gradient of $\Psi$  at $P$.^[Interestingly, if a fluctuation $\{P_{h}  : h \in
H\}$  satisfies \@ref(eq:score)  for  a  direction $s$  such  that $s\neq  0$,
$\Exp_{P}(s(O))  =  0$  and  $\Var_{P}  (s(O))  <  \infty$,  then  $h  \mapsto
\Psi(P_{h})$  is still  differentiable at  $h=0$  with a  derivative equal  to
\@ref(eq:derivative) beyond fluctuations of the form \@ref(eq:fluct).]


### A Euclidean perspective {#Euclidean-perspective}

This  terminology has  a direct  parallel  to directional  derivatives in  the
calculus  of Euclidean  geometry.   Recall  that if  $f$  is a  differentiable
mapping from $\bbR^p$ to $\bbR$, then  the directional derivative of $f$ at *a
point*  $x$ (an  element of  $\bbR^p$) in  *direction* $u$  (a unit  vector in
$\bbR^p$) is the scalar product of the gradient of $f$ and $u$.  In words, the
directional derivative of $f$ at $x$ can be represented as a scalar product of
the direction that we  approach $x$ and the change of  the function's value at
$x$.

In the present  problem, the law $P$  is *the point* at which  we evaluate the
function $\Psi$, the score $s$ of  the fluctuation is *the direction* in which
we approach the point, and the gradient describes the change in the function's
value at the point.

### The canonical gradient {#canonical-gradient}

In general, it is  possible for many gradients to exist^[This  may be at first
surprising given the parallel drawn in Section \@ref(Euclidean-perspective) to
Euclidean  geometry.  However,  it is  important  to remember  that the  model
dictates fluctuations of $P$ that are valid submodels with respect to the full
model.  In  turn, this determines  the possible  directions from which  we may
approach $P$.  Thus, depending on the direction, \@ref(eq:derivative) may hold
with different choices of $D^*$.].  Yet, in the special case that the model is
nonparametric,  only a  single gradient  exists. The  unique gradient  is then
referred to  as *the canonical  gradient* or *the efficient  influence curve*,
for reasons that will be clarified in Section \@ref(influence-curves).  In the
more general setting,  the canonical gradient may be defined  as the minimizer
of $D\mapsto \Var_{P} (D(O))$ over the set of all gradients.

It is not difficult to check that the efficient influence curve of statistical
mapping  $\Psi$  \@ref(eq:psimap)   at  $P  \in  \calM$  can   be  written  as
\begin{align}  D^{*}(P)  &  \equiv  D_{1}^{*}   (P)  +  D_{2}^{*}  (P),  \quad
\text{where} (\#eq:eif)\\  D_{1}^{*}(P) (O) &\equiv \Qbar(1,W)  - \Qbar(0,W) -
\Psi(P),  \notag\\ D_{2}^{*}(P)  (O)  &\equiv \frac{2A-1}{\ell\Gbar(A,W)}(Y  -
\Qbar(A,W)).\notag \end{align}

A `method` from package `tlrider` evaluates the efficient influence curve at a
law described  by an object of  class `LAW`. It is  called `evaluate_eic`. For
instance,  the next  chunk of  code  evaluates the  efficient influence  curve
$D^{*}(P_{0})$  of  $\Psi$  \@ref(eq:psimap)  at $P_{0}  \in  \calM$  that  is
characterized by `experiment`:


```r
eic_experiment <- evaluate_eic(experiment)
```

The  efficient influence  curve $D^{*}(P_{0})$  is a  function from  $\bbO$ to
$\bbR$.  As  such, it can  be evaluated  at the five  independent observations
drawn from  $P_{0}$ in Section  \@ref(synthetic-experiment). This is  what the
next chunk of code does:


```r
(eic_experiment(five_obs))
#> [1]  0.260  0.161 -0.387 -0.186  0.110
```

Finally, the  efficient influence curve can  be visualized as two  images that
represent $(w,y) \mapsto D^{*}(P_{0})(w,a,y)$ for $a = 0,1$, respectively:

(ref:eic-three) Visualizing the efficient influence curve $D^{*}(P_{0})$ of $\Psi$ \@ref(eq:psimap) at $P_{0}$, the law described by `experiment`. 


```r
crossing(w = seq(0, 1, length.out = 2e2),
	 a = c(0, 1),
	 y = seq(0, 1, length.out = 2e2)) %>%
  mutate(eic = eic_experiment(cbind(Y=y,A=a,W=w))) %>%
  ggplot(aes(x = w, y = y, fill = eic)) +
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = eic), color = "white") +
  facet_wrap(~ a, nrow = 1,
             labeller = as_labeller(c(`0` = "a = 0", `1` = "a = 1"))) +
  labs(fill = expression(paste(D^"*", (P[0])(w,a,y))))
```

<div class="figure" style="text-align: center">
<img src="img/eic-three-1.png" alt="(ref:eic-three)" width="80%" />
<p class="caption">(\#fig:eic-three)(ref:eic-three)</p>
</div>

## A fresh look at `another_experiment` {#revisiting}

We can give a fresh look at Section \@ref(numerical-illus) now.

### Deriving the efficient influence curve

It is not difficult (though cumbersome) to verify that, up to a constant,
$\{\Pi_{h} :  h \in [-1,1]\}$ is  a fluctuation of $\Pi_{0}$  in the direction
(in the sense of \@ref(eq:fluct)) of

\begin{align}
\notag\sigma_{0}(O) \equiv - 10 \sqrt{W} A \times \beta_{0} & (A,W)\\
&\times\left(\log(1    -     Y)    +     \sum_{k=0}^{3}    \left(k     +    \beta_{0}
(A,W)\right)^{-1}\right)     +      \text{constant},\\     \text{where}     \;
\beta_{0}(A,W)&\equiv                                                  \frac{1
-\Qbar_{\Pi_{0}}(A,W)}{\Qbar_{\Pi_{0}}(A,W)}. (\#eq:sigma0)\end{align}

Consequently,    the    slope    of     the    dotted    curve    in    Figure
\@ref(fig:psi-approx-psi-one) is equal to 

\begin{equation}
\Exp_{\Pi_{0}} (D^{*}(\Pi_{0}) (O) \sigma_{0}(O)). (\#eq:slope-Pi)
\end{equation}

Since $D^{*}(\Pi_{0})$  is centered under $\Pi_{0}$,  knowing $\sigma_{0}$ up
to a constant is not problematic.

### Numerical validation

In  the following  code, we  check the  above fact  numerically.  When  we ran
`example(tlrider)`, we  created a  function `sigma0`. The  function implements
$\sigma_{0}$ defined in \@ref(eq:sigma0):


```r
sigma0
#> function(obs, law = another_experiment) {
#>   ## preliminary
#>   Qbar <- get_feature(law, "Qbar", h = 0)
#>   QAW <- Qbar(obs[, c("A", "W")])
#>   params <- formals(get_feature(law, "qY", h = 0))
#>   shape1 <- eval(params$shape1)
#>   ## computations
#>   betaAW <- shape1 * (1 - QAW) / QAW
#>   out <- log(1 - obs[, "Y"])
#>   for (int in 1:shape1) {
#>     out <- out + 1/(int - 1 + betaAW)
#>   }
#>   out <- - out * shape1 * (1 - QAW) / QAW *
#>            10 * sqrt(obs[, "W"]) * obs[, "A"]
#>   ## no need to center given how we will use it
#>   return(out)
#>  }
```

The next chunk of code approximates
\@ref(eq:slope-Pi)  pointwise and  with  a confidence  interval of  asymptotic
level 95\%:


```r
eic_another_experiment <- evaluate_eic(another_experiment, h = 0)
obs_another_experiment <- sample_from(another_experiment, B, h = 0)
vars <- eic_another_experiment(obs_another_experiment) *
  sigma0(obs_another_experiment)

sd_hat <- sd(vars)
(slope_hat <- mean(vars))
#> [1] 1.35
(slope_CI <- slope_hat + c(-1, 1) * qnorm(1 - alpha / 2) * sd_hat / sqrt(B))
#> [1] 1.33 1.36
```

Equal  to 1.349  (rounded  to three  decimal places  ---
hereafter, all  rounding will be to  three decimal places as  well), the first
numerical approximation `slope_approx` is not too off!

## &#9761; \stixdanger{} Asymptotic linearity and statistical efficiency {#influence-curves}

### Asymptotic linearity

Suppose  that  $O_{1},  \ldots,  O_{n}$ are  drawn  independently  from  $P\in
\calM$. If an estimator $\psi_n$ of $\Psi(P)$ can be written as

\begin{equation*}  \psi_n  =  \Psi(P) +  \frac{1}{n}\sum_{i=1}^n  \IC(O_i)  +
o_{P}(1/\sqrt{n})\end{equation*}

for some function  $\IC : \calO \to  \bbR$ such that $\Exp_P(\IC(O))  = 0$ and
$\Var_{P}(\IC(O))  < \infty$,  then we  say that  $\psi_n$ is  *asymptotically
linear* with  *influence curve*  $\IC$.  Asymptotically linear  estimators are
*weakly convergent*.  Specifically, if $\psi_n$ is  asymptotically linear with
influence curve $\IC$, then

\begin{equation}
\sqrt{n}  (\psi_n  - \Psi(P))  =  \frac{1}{\sqrt{n}}  \sum_{i=1}^n \IC(O_i)  +
o_P(1) (\#eq:asymp-lin)
\end{equation}

and, by  the central  limit theorem  (recall that  $O_{1}, \ldots,  O_{n}$ are
independent), $\sqrt{n}  (\psi_n -  \Psi(P))$ converges in  law to  a centered
Gaussian distribution with variance $\Var_P(\IC(O))$.

### Influence curves and gradients

As it happens,  influence curves of regular^[We can view  $\psi_{n}$ as the by
product  of an  algorithm\index{algorithm}  $\Psihat$  trained on  independent
observations $O_{1}, \ldots, O_{n}$ drawn from $P$.  We say that the estimator
is regular at $P$ if, for any direction $s\neq 0$ such that $\Exp_{P} (s(O)) =
0$  and $\Var_{P}  (s(O)) <  \infty$  and fluctuation  $\{P_{h} :  h \in  H\}$
satisfying   \@ref(eq:score),    the   estimator    $\psi_{n,1/\sqrt{n}}$   of
$\Psi(P_{1/\sqrt{n}})$   obtained  by   training   $\Psihat$  on   independent
observations $O_{1}$, \ldots, $O_{n}$ drawn from $P_{1/\sqrt{n}}$ is such that
$\sqrt{n} (\psi_{n,1/\sqrt{n}} - \Psi(P_{1/\sqrt{n}}))$  converges in law to a
limit that  does not  depend on  $s$.]  estimators  are intimately  related to
gradients.  In fact, if $\psi_n$ is a regular, asymptotically linear estimator
of $\Psi(P)$ with influence curve $\IC$, then it must be true that $\Psi$ is a
smooth at $P$ and that $\IC$ is a gradient of $\Psi$ at $P$.

### Asymptotic efficiency

Now  recall  that,  in   Section  \@ref(canonical-gradient),  we  defined  the
canonical gradient as the minimizer of $D \mapsto \Var_{P}(D(O))$ over the set
of all gradients. Therefore, if $\psi_{n}$ is a regular, asymptotically linear
estimator of  $\Psi(P)$ (built  from $n$  independent observations  drawn from
$P$), then the  asymptotic variance of $\sqrt{n} (\psi_{n}  - \Psi(P))$ cannot
be  smaller than  the variance  of the  canonical gradient  of $\Psi$  at $P$,
*i.e.*, 

\begin{equation}
(\#eq:CR)\Var_{P}(D^{*}(P)(O)). 
\end{equation}

In other words, \@ref(eq:CR) is the  lower bound on the asymptotic variance of
*any* regular,  asymptotically linear estimator  of $\Psi(P)$.  This  bound is
referred to  as the *Cramér-Rao  bound*.  Any regular estimator  that achieves
this variance bound is said to  be *asymptotically efficient* at $P$.  Because
the canonical gradient  is the influence curve of  an asymptotically efficient
estimator, it is often referred to as the *efficient influence curve*.


## &#9881; \gear Cramér-Rao bounds {#exo-cramer-rao}


1. What does the following chunk do?


```r
obs <- sample_from(experiment, B)
(cramer_rao_hat <- var(eic_experiment(obs)))
#> [1] 0.287
```

2. Same question about this one.


```r
obs_another_experiment <- sample_from(another_experiment, B, h = 0)
(cramer_rao_Pi_zero_hat <-
   var(eic_another_experiment(obs_another_experiment)))
#> [1] 0.098
```

3. With a large independent sample drawn from $\Psi(P_0)$ (or $\Psi(\Pi_0)$),
   is it possible  to construct a regular estimator  $\psi_{n}$ of $\Psi(P_0)$
   (or $\Psi(\Pi_0)$)  such that the  asymptotic variance of  $\sqrt{n}$ times
   $\psi_{n}$ minus its target be smaller than the Cramér-Rao bound?

4. Is it easier to estimate $\Psi(P_{0})$ or $\Psi(\Pi_{0})$ (from independent
   observations drawn from either law)? In  what sense? (Hint: you may want to
   compute a ratio.)


# Double-robustness {#double-robustness}

## Linear approximations of parameters {#linear-approximation}

### From gradients to estimators

We learned in Section \@ref(smooth) that the stochastic behavior of a regular,
asymptotically  linear estimator  of  $\Psi(P)$ can  be  characterized by  its
influence curve. Moreover, we said that this influence curve must in fact be a
gradient of $\Psi$ at $P$. 

In this  section, we  show that the  converse is also  true: given  a gradient
$D^*$  of  $\Psi$ at  $P$,  under  so-called  *regularity conditions*,  it  is
possible   to  construct   an  estimator   with  influence   curve  equal   to
$D^*(P)$. This fact will suggest  concrete strategies for generating efficient
estimators  of  smooth  parameters.   We  take here  the  first  step  towards
generating such estimators: linearizing the parameter.


### A Euclidean perspective {#another-Euclidean-perspective}

As in  Section \@ref(Euclidean-perspective),  drawing a parallel  to Euclidean
geometry is  helpful. We recall that  if $f$ is a  differentiable mapping from
$\bbR^p$ to $\bbR$, then a Taylor series  approximates $f$ at a point $x_0 \in
\bbR^p$:  \begin{equation*} f(x_0)  \approx f(x)  + \langle(x_0  - x),  \nabla
f(x)\rangle,\end{equation*} where $x$ is a point in $\bbR^p$, $\nabla f(x)$ is
the gradient  of $f$ evaluated at  $x$ and $\langle u,v\rangle$  is the scalar
product of  $u,v \in  \bbR^{p}$.  As the  squared distance  $\|x-x_{0}\|^{2} =
\langle x-x_{0}, x-x_{0}\rangle$ between $x$  and $x_0$ decreases, the *linear
approximation* to $f(x_0)$ becomes more accurate.

### The remainder term

Returning to  the present  problem with this  in mind, we  find that  indeed a
similar approximation strategy may be applied. 

For clarity,  let us introduce a  new shorthand notation.  For  any measurable
function $f$ of  the observed data $O$, we  may write from now on  $P f \equiv
\Exp_P(f(O))$. One may argue that the  notation is valuable beyong the gain of
space. For instance, \@ref(eq:asymp-lin) 

\begin{equation*}
\sqrt{n}  (\psi_n  - \Psi(P))  =  \frac{1}{\sqrt{n}}  \sum_{i=1}^n \IC(O_i)  +
o_P(1) 
\end{equation*}

can be rewritten as

\begin{equation*}
\sqrt{n} (\psi_n - \Psi(P)) = \sqrt{n} (P_{n} - P) \IC + o_P(1),
\end{equation*}

thus  suggesting  more clearly  the  importance  of the  so-called  *empirical
process* $\sqrt{n} (P_{n} - P)$.

In particular,  if $\Psi$ is  smooth uniformly  over directions, then  for any
given $P \in \calM$, we can write

\begin{equation} 
\Psi(P_0)    =   \Psi(P)    +   (P_0    -   P)    D^*(P)   -    \Rem_{P_0}(P),
(\#eq:taylor-expansion) 
\end{equation}

where $\Rem_{P_0}(P)$ is a  *remainder term* satisfying that \begin{equation*}
\frac{\Rem_{P_0}(P)}{d(P,  P_0)}  \rightarrow  0   \  \mbox{as}  \  d(P,  P_0)
\rightarrow 0  , \end{equation*}  where $d$  is a  measure of  discrepancy for
distributions  in  $\calM$.   Note   that  \@ref(eq:taylor-expansion)  can  be
equivalently   written   as   \begin{equation*}    \Psi(P_0)   =   \Psi(P)   +
\Exp_{P_0}(D^*(P)(O))  - \Exp_P(D^*(P)(O))  - \Rem_{P_0}(P).   \end{equation*}
The  remainder term  formalizes the  notion that  if $P$  is *close*  to $P_0$
(*i.e.*, if $d(P,P_0)$ is small), then the linear approximation of $\Psi(P_0)$
is more accurate.

### Expressing the remainder term as a function of the relevant features

The equations  for the definition  of the parameter \@ref(eq:psimap),  form of
the  canonical   gradient  \@ref(eq:eif),   and  linearization   of  parameter
\@ref(eq:taylor-expansion) combine to determine the remainder: 

\begin{equation}
\Rem_{P_0}(P)    \equiv   \Psi(P)    -   \Psi(P_0)    -   (P_0    -   P)D^*(P)
(\#eq:remainder-one)
\end{equation}

hence 

\begin{multline} 
\Rem_{P_0}(P)=   \Exp_{P_0}   \Bigg[   \left(\Gbar_0(W)   -
\Gbar(W)\right) \\          \times          \left(\frac{\Qbar_0(1,W)           -
\Qbar(1,W)}{\ell\Gbar(1,W)} + \frac{\Qbar_0(0,W) - \Qbar(0,W)}{\ell\Gbar(0,W)}
\right) \Bigg]. (\#eq:remainder) 
\end{multline}

Acting   as  oracles,   we   can  compute   explicitly   the  remainder   term
$\Rem_{P_0}(P)$.  The  `evaluate_remainder` method makes it  very easy (simply
run `?evaluate_remainder` to see the man page of the method):


```r
(evaluate_remainder(experiment, experiment))
#> [1] 0
(rem <- evaluate_remainder(experiment, another_experiment,
                           list(list(), list(h = 0))))
#> [1] 0.199
```

We recover  the equality $\Rem_{P_{0}} (P_{0})  = 0$, which is  fairly obvious
given  \@ref(eq:taylor-expansion). In  addition, we  learn that  $\Rem_{P_{0}}
(\Pi_{0})$ equals 0.199. In  the next subsection, we invite you to
make better  acquaintance with the  remainder term  by playing around  with it
numerically.

## &#9881; \gear The remainder term {#exo-remainder-term}

1. Compute numerically $\Rem_{\Pi_0}(\Pi_h)$ for  $h \in [-1,1]$ and plot your
results. What do you notice?

2.  &#9761;  \stixdanger{} Approximate $\Rem_{P_{0}}  (\Pi_{0})$ numerically
without relying on  method `evaluate_remainder` and compare the  value you get
with that of `rem`.  (Hint: use \@ref(eq:remainder-one) and a large sample of
observations drawn independently from $P_{0}$.)

## &#9761; \stixdanger{} Double-robustness {#def-double-robustness}

### The key property

Let us  denote by  $\|f\|_{P}^{2}$ the  square of  the $L^{2}(P)$-norm  of any
function  $f$  from $\bbO$  to  $\bbR$  *i.e.*,  using a  recently  introduced
notation,  $\|f\|_{P}^{2}  \equiv  Pf^{2}$.    For  instance,  $\|\Qbar_{1}  -
\Qbar_{0}\|_{P}$ or  $\|\Gbar_{1} - \Gbar_{0}\|_{P}$ is  a distance separating
the features $\Qbar_{1}$ and $\Qbar_{0}$ or $\Gbar_{1}$ and $\Gbar_{0}$.

The efficient  influence curve  $D^{*}(P)$ at  $P \in  \calM$ enjoys  a rather
remarkable property:  it is *double-robust*.   Specifically, for every  $P \in
\calM$, the remainder term $\Rem_{P_{0}} (P)$ satisfies 

\begin{equation} 
\Rem_{P_{0}}   (P)^{2}  \leq   \|\Qbar   -
\Qbar_{0}\|_{P}^{2}  \times   \|(\Gbar  -  \Gbar_{0})/\ell\Gbar_{0}\|_{P}^{2},
(\#eq:rem-two)
\end{equation}

where $\Qbar$ and  $\Gbar$ are the counterparts under $P$  to $\Qbar$_{0}$ and
$\Gbar_{0}$.   The proof  consists  in a  straightforward  application of  the
Cauchy-Schwarz   inequality    to   the   right-hand   side    expression   in
\@ref(eq:remainder-one).

### Its direct consequence {#direct-consequence}

It may  not be clear yet  why \@ref(eq:rem-two) is an  important property, and
why  $D^{*}$ is  said *double-robust*  because of  it.  To  answer the  latter
question,  let us  consider a  law $P\in  \calM$ such  that *either*  $\Qbar =
\Qbar_{0}$ *or* $\Gbar = \Gbar_{0}$. 

It  is then  the  case that  *either*  $\|\Qbar -  \Qbar_{0}\|_{P}  = 0$  *or*
$\|\Gbar - \Gbar_{0}\|_{P} = 0$.  Therefore, in light of \@ref(eq:rem-two), it
also  holds that  $\Rem_{P_{0}} (P)  = 0$.^[This  also trivially  follows from
\@ref(eq:remainder).]    It  thus   appears  that   \@ref(eq:taylor-expansion)
simplifies to

\begin{align*} \Psi(P_0) &= \Psi(P) + (P_0 - P) D^*(P)\\
&= \Psi(P) + P_0 D^*(P),\end{align*}

where the second  equality holds because $PD^{*}(P) = 0$  for all $P\in \calM$
by definition of $D^{*}(P)$.

It is now clear  that for such a law $P\in \calM$,  $\Psi(P) = \Psi(P_{0})$ is
equivalent to

\begin{equation} 
P_{0} D^{*}(P) = 0. (\#eq:solves-eic) 
\end{equation}

Most  importantly, in  words,  if $P$  solves  the so-called  $P_{0}$-specific
efficient influence  curve equation \@ref(eq:solves-eic) and  if, in addition,
$P$  has  the  same  $\Qbar$-feature *or*  $\Gbar$-feature  as  $P_{0}$,  then
$\Psi(P) = \Psi(P_{0})$.


The conclusion is  valid no matter how $P$ may  differ from $P_{0}$ otherwise,
hence the notion  of being *double-robust*.  This property is  useful to build
consistent   estimators   of  $\Psi(P)$,   as   we   shall  see   in   Section
\@ref(inference). 




## &#9881; \gear Double-robustness {#exo-double-robustness}

1. Go  back to  Problem 1  in \@ref(exo-remainder-term).  In light  of Section
   \@ref(def-double-robustness), what is happening?

2. Create  a copy  of `experiment`  and replace its  `Gbar` feature  with some
   other  function of  $W$ (see  `?copy`, `?alter`  and Problem  2 in  Section
   \@ref(exo-yet-another-experiment)).  Call $P'$ the element of model $\calM$
   thus  characterized.   Can  you  guess the  values  of  $\Rem_{P_{0}}(P')$,
   $\Psi(P')$ and $P_{0} D^{*}(P')$? Support your argument.

3. Add more exercizes?



# Inference {#inference}

## Where we stand

In the previous sections, we analyzed our target parameter and presented
relevant theory for understanding the  statistical properties of certain types
of estimators of the parameter.  The  theory is also relevant for building and
comparing a variety of estimators.

We assume from now  on that we have available a  sample $O_{1}, \ldots, O_{B}$
of independent observations  drawn from $P_{0}$. This is  literally the case!,
and  the  observations  are  stored  in  `obs`  that  we  created  in  Section
\@ref(exo-cramer-rao). 


```r
iter <- 1e2
```

Equal to 0.1 million, the sample size `B` is very large. We will in fact
use  100 disjoint  subsamples  composed  of $n$  independent
observations among $O_{1}, \ldots, O_{B}$,  where $n$ equals `B/iter`, *i.e.*,
1000.   We will  thus be  in a  position to  investigate the
statistical  properties  of  every  estimation  procedure  by  replicating  it
independently 100 times.

## Where we go

The  following  sections  explore  different statistical  paths  to  inferring
$\psi_{0}$ or, rather (though equivalently), $\Psi(P_{0})$.

- Section \@ref(simple-strategy) present a  simple inference strategy.  It can
  be  carried out  in situations  where $\Gbar_{0}$  is already  known to  the
  statistician.

- Section    \@ref(nuisance)    discusses   the    estimation    of    some
  infinite-dimensional  features of  $P_{0}$.   The  resulting estimators  are
  later used to infer $\psi_{0}$.
  
- Section...

# A simple inference strategy {#simple-strategy}

## A cautionary detour

Let us introduce first the following estimator: 

\begin{align}
\notag \psi_{n}^{a}
&\equiv \frac{\Exp_{P_{n}} (AY)}{\Exp_{P_{n}} (A)} - \frac{\Exp_{P_{n}}
((1-A)Y)}{\Exp_{P_{n}}(1-A)} \\ 
&=          \frac{\sum_{i=1}^{n}         \one\{A_{i}=Y_{i}=1\}}{\sum_{i=1}^{n}
\one\{A_{i}=1\}}                     -                    \frac{\sum_{i=1}^{n}
\one\{A_{i}=0,Y_{i}=1\}}{\sum_{i=1}^{n}\one\{A_{i}=0\}}. (\#eq:cautionary) 
\end{align} 

It    estimates     \begin{align*}\Phi(P_{0})    &\equiv    \frac{\Exp_{P_{0}}
(AY)}{\Exp_{P_{0}} (A)} - \frac{\Exp_{P_{0}} ((1-A)Y)}{\Exp_{P_{0}} (1-A)}\\&=
\Exp_{P_{0}} (Y | A=1) - \Exp_{P_{0}} (Y | A=0).\end{align*}

We seize  this opportunity  to demonstrate numerically  the obvious  fact that
$\psi_{n}^{a}$  *does   not*  estimate  $\Psi(P_{0})$  because,   in  general,
$\Psi(P_{0})$  and $\Phi(P_{0})$  differ. This  is apparent  in the  following
alternative expression of $\Phi(P_{0})$:

\begin{align*} \Phi(P_{0}) &= \Exp_{P_{0}} \left(\Exp_{P_0}(Y \mid A, W) |A=1)
\right) -  \Exp_{P_{0}} \left(\Exp_{P_0}(Y \mid  A, W) | A=0\right)\\  &= \int
\Qbar_{0}(1, w) dP_{0,W|A=1}(w) - \int \Qbar_{0}(0, w) dP_{0,W|A=0}(w).
\end{align*}

Contrast  the above  equalities and  \@ref(eq:psi-zero).  In  the latter,  the
outer  integral is  against the  marginal  law of  $W$ under  $P_{0}$. In  the
former, the outer  integrals are respectively against the  conditional laws of
$W$ given $A=1$ and $A=0$ under $P_{0}$.

## &#9881; \gear Delta-method {#delta-method}

Consider the next chunk of code:

```r
compute_irrelevant_estimator <- function(obs) {
  Y <- pull(obs, Y)
  A <- pull(obs, A)
  psi_n <- mean(A * Y) / mean(A) - mean((1 - A) * Y) / mean(1 - A)
  Var_n <- cov(cbind(A * Y, A, (1 - A) * Y, (1 - A)))
  phi_n <- c(1 / mean(A), -mean(A * Y) / mean(A)^2,
             -1 / mean(1 - A),
             mean((1 - A) * Y) / mean(1 - A)^2)
  var_n <- as.numeric(t(phi_n) %*% Var_n %*% phi_n)
  sig_n <- sqrt(var_n / nrow(obs))
  tibble(psi_n = psi_n, sig_n = sig_n)
}
```

Function `compute_irrelevant_estimator` computes  the estimator $\psi_{n}^{a}$
\@ref(eq:cautionary) based on the data set in `obs`. 

Introduce   $X_{n}   \equiv  n^{-1}\sum_{i=1}^{n}   \left(A_{i}Y_{i},   A_{i},
(1-A_{i})Y_{i},  1-A_{i}\right)^{\top}$ and  $X  \equiv  \left(AY, A,  (1-A)Y,
1-A\right)^{\top}$.  It  happens that  $X_{n}$ is asymptotically  Gaussian: as
$n$  goes  to  infinity,\begin{equation*}\sqrt{n} \left(X_{n}  -  \Exp_{P_{0}}
(X)\right)\end{equation*} converges in  law to the centered  Gaussian law with
covariance matrix \begin{equation*}V_{0} \equiv \Exp_{P_{0}} \left((X
- \Exp_{P_{0}} (X)) \times (X- \Exp_{P_{0}} (X))^{\top}\right).\end{equation*} 

Let $f:\bbR\times \bbR^{*} \times \bbR\times \bbR^{*}$ be given by $f(r,s,t,u)
= r/s - t/u$. The function is differentiable.

1.    Check that  $\psi_{n}^{a}  = f(X_{n})$.   Point out  to  the line  where
   $\psi_{n}^{a}$ is  computed in the body  of `compute_irrelevant_estimator`.
   Also point out  to the line where the above  asymptotic variance of $X_{n}$
   is estimated with its empirical counterpart, say $V_{n}$.

2. &#9761; \stixdanger{}  Argue  how the  [delta-method](#prop-delta-method)
   yields that $\sqrt{n}(\psi_{n}^{a} - \Phi(P_{0}))$  converges in law to the
   centered  Gaussian  law  with  a   variance  that  can  be  estimated  with
   \begin{equation}  v_{n}^{a}  \equiv  \nabla f(X_{n})  \times  V_{n}  \times
   \nabla f(X_{n})^{\top}. (\#eq:v-n-a) \end{equation}

3. Check  that the  gradient $\nabla f$  of $f$ is  given by  $\nabla f(r,s,t,u)
   \equiv (1/s,  -r/s^{2}, -1/u, t/u^{2})$.  Point out  to the line  where the
   asymptotic variance of $\psi_{n}^{a}$ is estimated.

## IPTW estimator assuming the mechanism of action known {#known-gbar-first-pass}

### A simple substitution estimator

Let us assume for a moment that we know $\Gbar_{0}$.  This would have been the
case  indeed if  $P_{0}$  were a  controlled experiment.   Note  that, on  the
contrary, assuming $\Qbar_{0}$ known would be difficult to justify.


```r
Gbar <- get_feature(experiment, "Gbar")
```

The   alternative  expression   \@ref(eq:psi-zero-b)   suggests  to   estimate
$\Psi(P_{0})$ with 

\begin{align}
\psi_{n}^{b}  &\equiv \Exp_{P_{n}}  \left( \frac{2A-1}{\ell  \Gbar_{0}(A,W)} Y
\right) \\ 
&       =        \frac{1}{n}       \sum_{i=1}^{n}       \left(\frac{2A_{i}-1}{
\ell\Gbar_{0}(A_{i},W_{i})}Y_{i} \right). (\#eq:psi-n-b)
\end{align}

Note how $P_{n}$  is substituted for $P_{0}$ in  \@ref(eq:psi-n-b) relative to
\@ref(eq:psi-zero-b).    This  justifies   that  we   call  $\psi_{n}^{b}$   a
*substitution   estimator*  (for   the  same   reason,  $\psi_{n}^{a}$   is  a
substitution estimator of  $\Phi(P_{0})$).  It is also dubbed  an IPTW (inverse
probability  of  treatment weighted)  estimator  because  of the  denominators
$\ell\Gbar_{0}(A_{i},W_{i})$  in  its  definition.^[We  could  have  used  the
alternative expression IPAW, where A (like  action) is substituted for T (like
treatment).]

In Section  \@ref(known-gbar-second-pass), we  develop another  IPTW estimator
that does not assume that $\Gbar_{0}$ is known beforehand.

### Elementary statistical properties

It is easy to check  that $\psi_{n}^{b}$ estimates $\Psi(P_{0})$ consistently,
but this  is too little to  request from an estimator  of $\psi_{0}$.  Better,
$\psi_{n}^{b}$   also   satisfies   a   central   limit   theorem:   $\sqrt{n}
(\psi_{n}^{b} -  \psi_{0})$ converges in law  to a centered Gaussian  law with
asymptotic     variance     \begin{equation*}v^{b}     \equiv     \Var_{P_{0}}
\left(\frac{2A-1}{\ell\Gbar_{0}(A,W)}Y\right),\end{equation*}   where  $v^{b}$
can be consistently estimated by its empirical counterpart

\begin{align}
(\#eq:v-n-b)      v_{n}^{b}       &\equiv      \Var_{P_{n}}
\left(\frac{2A-1}{\ell\Gbar_{0}(A,W)}Y\right) \\ 
&=        \frac{1}{n}        \sum_{i=1}^{n}\left(\frac{2A_{i}-1}{\ell\Gbar_{0}
(A_{i},W_{i})} Y_{i} - \psi_{n}^{b}\right)^{2}.
\end{align}

We  investigate *empirically*  the statistical  behavior of  $\psi_{n}^{b}$ in
Section \@ref(empirical-inves-IPTW).


### Empirical investigation {#empirical-inves-IPTW}

The  next chunk  of code  investigates the  empirical behaviors  of estimators
$\psi_{n}^{a}$ and  $\psi_{n}^{b}$. As explained in  Section \@ref(inference),
we first make `iter`  data sets out of the `obs` data  set (second line), then
build the estimators on each of them (fourth and fifth lines). After the first
series of  commands the object  `psi_hat_ab`, a `tibble`, contains  200
rows and  four columns.  For each  smaller data set (identified  by its `id`),
two    rows    contain   the    values    of    either   $\psi_{n}^{a}$    and
$\sqrt{v_{n}^{a}}/\sqrt{n}$  (if  `type`  equals `a`)  or  $\psi_{n}^{b}$  and
$\sqrt{v_{n}^{b}}/\sqrt{n}$ (if  `type` equals `b`).

After  the second  series of  commands, the  object `psi_hat_ab`  contains, in
addition,  the values  of  the  recentered (with  respect  to $\psi_{0}$)  and
renormalized   $\sqrt{n}/\sqrt{v_{n}^{a}}  (\psi_{n}^{a}   -  \psi_{0})$   and
$\sqrt{n}/\sqrt{v_{n}^{b}}  (\psi_{n}^{b}  -   \psi_{0})$,  where  $v_{n}^{a}$
\@ref(eq:v-n-a)  and  $v_{n}^{b}$   \@ref(eq:v-n-b)  estimate  the  asymptotic
variances  of   $\psi_{n}^{a}$  and  $\psi_{n}^{b}$,   respectively.  Finally,
`bias_ab` reports amounts of bias (at the renormalized scale).

(ref:known-Gbar-one-b) Kernel density estimators of the law of two estimators
of $\psi_{0}$ (recentered  with respect to $\psi_{0}$,  and renormalized), one
of  them  misconceived (a),  the  other  assuming  that $\Gbar_{0}$  is  known
(b). Built based on `iter` independent realizations of each estimator.


```r
psi_hat_ab <- obs %>% as_tibble() %>%
  mutate(id = (seq_len(n()) - 1) %% iter) %>%
  nest(-id, .key = "obs") %>%
  mutate(est_a = map(obs, ~ compute_irrelevant_estimator(.)),
         est_b = map(obs, ~ compute_iptw(as.matrix(.), Gbar))) %>%
  gather(`est_a`, `est_b`, key = "type", value = "estimates") %>%
  extract(type, "type", "_([ab])$") %>%
  unnest(estimates) %>% select(-obs)

(psi_hat_ab)
#> # A tibble: 200 x 4
#>      id type  psi_n  sig_n
#>   <dbl> <chr> <dbl>  <dbl>
#> 1     0 a     0.130 0.0174
#> 2     1 a     0.126 0.0180
#> 3     2 a     0.112 0.0161
#> 4     3 a     0.116 0.0164
#> 5     4 a     0.110 0.0187
#> 6     5 a     0.140 0.0178
#> # ... with 194 more rows

psi_hat_ab <- psi_hat_ab %>% 
  group_by(id) %>%
  mutate(clt = (psi_n - psi_zero) / sig_n)

(psi_hat_ab)
#> # A tibble: 200 x 5
#> # Groups:   id [100]
#>      id type  psi_n  sig_n   clt
#>   <dbl> <chr> <dbl>  <dbl> <dbl>
#> 1     0 a     0.130 0.0174  2.71
#> 2     1 a     0.126 0.0180  2.40
#> 3     2 a     0.112 0.0161  1.78
#> 4     3 a     0.116 0.0164  2.01
#> 5     4 a     0.110 0.0187  1.42
#> 6     5 a     0.140 0.0178  3.19
#> # ... with 194 more rows

(bias_ab <- psi_hat_ab %>%
   group_by(type) %>% summarise(bias = mean(clt)))
#> # A tibble: 2 x 2
#>   type    bias
#>   <chr>  <dbl>
#> 1 a     1.53  
#> 2 b     0.0922

fig_bias_ab <- ggplot() +
  geom_line(aes(x = x, y = y), 
            data = tibble(x = seq(-3, 3, length.out = 1e3),
                          y = dnorm(x)),
            linetype = 1, alpha = 0.5) +
  geom_density(aes(clt, fill = type, colour = type),
               psi_hat_ab, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_ab, size = 1.5, alpha = 0.5)
  
fig_bias_ab +
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(a, b)})*
                            (psi[n]^{list(a, b)} - psi[0]))))
```

<div class="figure" style="text-align: center">
<img src="img/known-Gbar-one-b-1.png" alt="(ref:known-Gbar-one-b)" width="70%" />
<p class="caption">(\#fig:known-Gbar-one-b)(ref:known-Gbar-one-b)</p>
</div>

By   the  above   chunk   of  code,   the   averages  of   $\sqrt{n/v_{n}^{a}}
(\psi_{n}^{a} - \psi_{0})$ and  $\sqrt{n/v_{n}^{b}} (\psi_{n}^{b} - \psi_{0})$
computed across the realizations of  the two estimators are respectively equal
to 1.526 and 
0.092 (see `bias_ab`).  Interpreted as amounts of bias, those two
quantities    are     represented    by     vertical    lines     in    Figure
\@ref(fig:known-Gbar-one-b). The red and blue bell-shaped curves represent the
empirical laws  of $\psi_{n}^{a}$ and $\psi_{n}^{b}$  (recentered with respect
to $\psi_{0}$,  and renormalized) as  estimated by kernel  density estimation.
The latter is  close to the black curve, which  represents the standard normal
density.


# Nuisance parameters {#nuisance}

## Anatomy of an expression {#anatomy}

\index{nuisance~parameters|(}

From now, all the inference strategies that we will present unfold in two or
three  stages. For  all of  them,  the first  stage consists  in estimating  a
selection of features of the law $P_{0}$ of the experiment.  Specifically, the
features are chosen  among $Q_{0,W}$ (the marginal law of  $W$ under $P_{0}$),
$\Gbar_{0}$ (the conditional  probability that $A=1$ given  $W$ under $P_{0}$)
and $\Qbar_{0}$ (the conditional mean of $Y$ given $A$ and $W$ under $P_{0}$).

In  this context,  because  they are  not the  parameter  of primary  interest
(*i.e.*, they are  not the real-values feature $\Psi(P_{0})$),  they are often
referred to as  *nuisance parameters* of $P_{0}$.  The unflaterring expression
conveys the notion that their estimation  is merely an intermediate step along
our path towards an inference of the target parameter.

As for the reason why $Q_{0,W}$,  $\Gbar_{0}$ and $\Qbar_{0}$ are singled out,
it is  because of  their role in  the definition of  $\Psi$ and  the efficient
influence curve $D^{*}(P_{0})$.

\index{nuisance~parameters|)}

## An algorithmic stance

\index{algorithm|(}

In general, we can view an estimator of any feature $f_0$ of $P_{0}$ as the
output of an algorithm $\Algo$ that maps any element of

\begin{equation*}    \calM^{\text{empirical}}     \equiv    \left\{\frac{1}{m}
\sum_{i=1}^{m} \Dirac(o_{i}) : m \geq 1, o_{1}, \ldots, o_{m} \in [0,1] \times
\{0,1\} \times [0,1]\right\} \end{equation*}

to   the    set   $\calF$   where    $f_{0}$   is   know   to    live.   Here,
$\calM^{\text{empirical}}$  can be  interpreted  as the  set  of all  possible
empirical measures summarizing  the outcomes of any number  of replications of
the experiment $P_{0}$. In particular, $P_{n}$ belongs to this set.

The `tlrider` package includes such  template algorithms for the estimation of
$Q_{0,W}$, $\Gbar_{0}$ and $\Qbar_{0}$. We  illustrate how they work and their
use in the next sections.

## `QW` {#nuisance-QW}

For instance, `estimate_QW` is an algorithm $\Algo_{Q_{W}}$ for the estimation
of the  marginal law of  $W$ under  $P_{0}$ (to see  its man page,  simply run
`?estimate_QW`).  It  is a map  from $\calM^{\text{empirical}}$ to the  set of
laws on $[0,1]$. The following chunk  of code estimates $Q_{0,W}$ based on the
$n = 1000$ first observations in `obs`:


```r
QW_hat <- estimate_QW(head(obs, 1e3))
```

It is  easy to sample  independent observations from  `QW_hat`.  To do  so, we
create an  object of  class `LAW`  then set its  marginal law  of $W$  to that
described by `QW_hat` and specify its `sample_from` feature:

(ref:estimate-QW-two) Histogram representing 1000 observations drawn independently from `QW_hat`. The superimposed red curve is the true density of $Q_{0,W}$.


```r
empirical_experiment <- LAW()
alter(empirical_experiment, QW = QW_hat)
alter(empirical_experiment, sample_from = function(n) {
  QW <- get_feature(empirical_experiment, "QW")
  W <- sample(pull(QW, "value"), n, prob = pull(QW, "weight"))
  cbind(W = W, A = NA, Y = NA)
})
W <- sample_from(empirical_experiment, 1e3) %>% as.tibble
W %>% ggplot() +
  geom_histogram(aes(x = W, y = stat(density)), bins = 40) +
  stat_function(fun = get_feature(experiment, "QW"), col = "red")
```

<div class="figure" style="text-align: center">
<img src="img/estimate-QW-two-1.png" alt="(ref:estimate-QW-two)" width="70%" />
<p class="caption">(\#fig:estimate-QW-two)(ref:estimate-QW-two)</p>
</div>

Note that all the $W$s sampled from `QW_hat` fall in the set $\{W_{1}, \ldots,
W_{n}\}$ of  observed $W$s in `obs`  (an obvious fact given  the definition of
the `sample_from` feature of `empirical_experiment`:


```r
(length(intersect(pull(W, W), head(obs[, "W"], 1e3))))
#> [1] 1000
```

This  is   because  `estimate_QW`  estimates  $Q_{0,W}$   with  its  empirical
counterpart, *i.e.*,

\begin{equation*}\frac{1}{n} \sum_{i=1}^{n} \Dirac(W_{i}).\end{equation*}


## `Gbar` {#nuisance-Gbar}

Another template algorithm is built-in into `tlrider`: `estimate_Gbar` (to see
its   man   page,   simply  run   `?estimate_Gbar`).   Unlike   `estimate_QW`,
`estimate_Gbar` needs further specification of the algorithm. The package also
includes examples of such specifications. 

There are two  sorts of specifications, of  which we say that  they are either
*working model-based* or *machine learning-based*.  We discuss the former sort
in  the   next  subsection.    The  latter  sort   is  discussed   in  Section
\@ref(nuisance-Qbar).

### Working model-based algorithms

\index{algorithm!working~model|(}

Let us take a look at `working_model_G_one` for instance: 


```r
working_model_G_one
#> $model
#> function (...) 
#> {
#>     trim_glm_fit(glm(family = binomial(), ...))
#> }
#> <environment: 0xdea07a8>
#> 
#> $formula
#> A ~ I(W^0.5) + I(abs(W - 5/12)^0.5) + I(W^1) + I(abs(W - 5/12)^1) + 
#>     I(W^1.5) + I(abs(W - 5/12)^1.5)
#> <environment: 0xdea07a8>
#> 
#> $type_of_preds
#> [1] "response"
#> 
#> attr(,"ML")
#> [1] FALSE
```

and focus  on its `model` and  `formula` attributes. The former  relies on the
`glm` and `binomial`  functions from `base` `R`, and  on `trim_glm_fit` (which
removes information  that we do  not need from  the standard output  of `glm`,
simply run  `?trim_glm_fit` to see the  function's man page). The  latter is a
`formula` that characterizes what we call a *working model* for $\Gbar_{0}$. 

In words,  by using `working_model_G_one`  we implicitly choose  the so-called
logistic   (or   negative   binomial)   loss   function   $L_{a}$   given   by

\begin{equation} 
(\#eq:logis-loss) -L_{a}(f)(A,W) \equiv A \log f(W) + (1 - A)
\log (1 - f(W)) 
\end{equation} 

for  any  function  $f :  [0,1]  \to  [0,1]$  paired  with the  working  model
\begin{equation*}   \calF_{1}   \equiv    \left\{f_{\theta}   :   \theta   \in
\bbR^{5}\right\}  \end{equation*}  where,  for   any  $\theta  \in  \bbR^{5}$,
\begin{equation*}\logit  f_{\theta}  (W)  \equiv \theta_{0}  +  \sum_{j=1}^{4}
\theta_{j} W^{j/2}.\end{equation*}

We  acted   as  oracles   when  we   specified  the   working  model:   it  is
*well-specified*, *i.e.*, it happens that  $\Gbar_{0}$ is the unique minimizer
of the risk entailed by $L_{a}$ over $\calF_{1}$: \begin{equation*}\Gbar_{0} =
\mathop{\arg\min}_{f_{\theta}        \in        \calF_{1}}        \Exp_{P_{0}}
\left(L_{a}(f_{\theta})(A,W)\right).\end{equation*}  Therefore, the  estimator
$\Gbar_{n}$ obtained by minimizing the empirical risk\index{well/mis-specified}

\begin{equation*}
\Exp_{P_{n}} \left(L_{a}(f_{\theta})(A,W)\right)  = \frac{1}{n} \sum_{i=1}^{n}
L_{a}(f_{\theta})(A_{i},W_{i})
\end{equation*} 

over $\calF_{1}$ estimates $\Gbar_{0}$ consistently. 

Of course,  it is seldom  certain in real life  that the target  feature, here
$\Gbar_{0}$,  belongs to  the working  model.^[In fact,  if one  knows nothing
about the feature, then  it is *certain* that it does  not belong to whichever
small  finite-dimensional working  model we  may  come up  with.] Suppose  for
instance that we  choose a small finite-dimensional  working model $\calF_{2}$
without  acting as  an  oracle.   Then consistency  certainly  fails to  hold.
However,  if $\Gbar_{0}$  can nevertheless  be *projected*  unambiguously onto
$\calF_{2}$ (an assumption  that cannot be checked), then  the estimator might
converge to the projection. 

### Visualization

To illustrate the use of the algorithm $\Algo_{\Gbar,1}$ obtained by combining
`estimate_Gbar` and  `working_model_G_one`, let us estimate  $\Gbar_{0}$ based
on the first $n = 1000$ observations in `obs`:


```r
Gbar_hat <- estimate_Gbar(head(obs, 1e3), algorithm = working_model_G_one)
```

Using     `compute_Gbar_hat_W`^[See     also    the     companion     function
`compute_lGbar_hat_AW`  (run `?compute_lGbar_hat_AW`  to  see  its man  page.]
(simply run  `?compute_Gbar_hat_W` to see  its man page)  makes it is  easy to
compare visually the estimator  $\Gbar_{n} \equiv \Algo_{\Gbar,1}(P_{n})$ with
its target $\Gbar0$:

(ref:estimate-Gbar-three) Comparing $\Gbar_{n}\equiv \Algo_{\Gbar,1}(P_{n})$ and $\Gbar_{0}$. The estimator is consistent because the algorithm relies on a working model that is correctly specified.


```r
tibble(w = seq(0, 1, length.out = 1e3)) %>%
  mutate("truth" = Gbar(w),
         "estimated" = compute_Gbar_hatW(w, Gbar_hat)) %>%
  gather("f", "value", -w) %>%
  ggplot() + geom_line(aes(x = w, y = value, color = f), size = 1) +
  labs(y = "f(w)",
       title = bquote("Visualizing" ~ bar(G)[0] ~ "and" ~ hat(G)[n])) +
  ylim(NA, 1)
```

<div class="figure" style="text-align: center">
<img src="img/estimate-Gbar-three-1.png" alt="(ref:estimate-Gbar-three)" width="70%" />
<p class="caption">(\#fig:estimate-Gbar-three)(ref:estimate-Gbar-three)</p>
</div>

## &#9881; \gear `Qbar`, working model-based algorithms {#nuisance-Qbar-wm}

A third template algorithm is built-in into `tlrider`: `estimate_Qbar` (to see
its   man  page,   simply   run   `?estimate_Qbar`).   Like   `estimate_Gbar`,
`estimate_Qbar` needs further specification of the algorithm. The package also
includes examples  of such  specifications, which can  also be  either working
model-based (see Section \@ref(nuisance-Gbar))  or machine learning-based (see
Sections \@ref(nuisance-Qbar) and \@ref(nuisance-Qbar-ml-exo)). 

There are built-in specifications similar to `working_model_G_one`, *e.g.*, 


```r
working_model_Q_one
#> $model
#> function (...) 
#> {
#>     trim_glm_fit(glm(family = binomial(), ...))
#> }
#> <environment: 0xdea07a8>
#> 
#> $formula
#> Y ~ A * (I(W^0.5) + I(W^1) + I(W^1.5))
#> <environment: 0xdea07a8>
#> 
#> $type_of_preds
#> [1] "response"
#> 
#> attr(,"ML")
#> [1] FALSE
#> attr(,"stratify")
#> [1] FALSE
```

1. Drawing inspiration from Section \@ref(nuisance-Gbar), comment upon and use
   the algorithm  $\Algo_{\Qbar,1}$ obtained by combining  `estimate_Gbar` and
   `working_model_Q_one`. 

\index{algorithm!working~model|)}

## `Qbar` {#nuisance-Qbar}

### `Qbar`, machine learning-based algorithms

\index{algorithm!machine~learning|(}


We explained how algorithm $\Algo_{\Gbar,1}$ is based on a working model (and
*you* did for  $\Algo_{\Qbar,1}$). It is not the case  that all algorithms are
based on  working models  in the  same (admittedly  rather narrow)  sense.  We
propose to say that those algorithms that are not based on working models like
$\Algo_{\Gbar,1}$, for instance, are instead *machine learning-based*. 

Typically, machine learning-based algorithms are more data-adaptive; they rely
on larger working models, and/or fine-tune parameters that must be calibrated,
*e.g.*  by   cross-validation.\index{algorithm!cross-validation}  Furthermore,
they   call   for  being   stacked\index{algorithm!machine~learning!stacking},
*i.e.*,   combined   by   means   of  another   outer   algorithm   (involving
cross-validation)    into    a    more   powerful    machine    learning-based
*meta-algorithm*.                           The                          super
learning\index{algorithm!machine~learning!super~learning}  methodology   is  a
popular stacking algorithm.

We will elaborate further on this important topic in another forthcoming part.
Here, we merely  illustrate the concept with two  specifications built-in into
`tlrider`.   Based on  the *$k$-nearest  neighbors* non-parametric  estimating
methodology, the  first one  is discussed  in the  next subsection.   Based on
*boosted trees*, another non-parametric estimating methodology, the second one
is used in the exercize that follows the next subsection.

### `Qbar`, kNN algorithm {#Qbar-knn-algo}

Algorithm $\Algo_{\Qbar,\text{kNN}}$ is  obtained by combining `estimate_Qbar`
and  `kknn_algo`.  The  training  of  $\Algo_{\Qbar,\text{kNN}}$ (*i.e.*,  the
making of  the output $\Algo_{\Qbar,\text{kNN}} (P_{n})$  is implemented based
on  function  `caret::train` of  the  `caret`  (classification and  regression
training) package  (to see  its man page,  simply run  `?caret::train`).  Some
additional specifications are provided in `kknn_grid` and `kknn_control`.

In a  nutshell, $\Algo_{\Qbar,\text{kNN}}$ estimates  $\Qbar_{0}(1,\cdot)$ and
$\Qbar_{0}(0,\cdot)$ separately.   Each of them  is estimated by  applying the
$k$-nearest   neighbors  methodology   as  it   is  implemented   in  function
`kknn::train.kknn` from  the `kknn` package (to  see its man page,  simply run
`?kknn::train.kknn`).^[Specifically,  argument   `kmax`  (maximum   number  of
neighbors  considered) is  set to  5,  argument `distance`  (parameter of  the
Minkowski distance) is  set to 2, and argument `kernel`  is set to `gaussian`.
The best  value of  $k$ is chosen  between 1 and  `kmax` by  leave-one-out. No
outer  cross-validation  is  needed.]   The following  chunk  of  code  trains
algorithm $\Algo_{\Qbar,\text{kNN}}$ on $P_{n}$:


```r
Qbar_hat_kknn <- estimate_Qbar(head(obs, 1e3),
                               algorithm = kknn_algo,
                               trControl = kknn_control,
                               tuneGrid = kknn_grid)
```

Using `compute_Qbar_hat_AW` (simply run  `?compute_Qbar_hat_AW` to see its man
page) makes it is easy to compare visually the estimator $\Qbar_{n,\text{kNN}}
\equiv \Algo_{\Qbar,\text{kNN}}(P_{n})$  with its target $\Qbar0$,  see Figure
\@ref(fig:estimate-Qbar-five).


```r
fig <- tibble(w = seq(0, 1, length.out = 1e3),
              truth_1 = Qbar(cbind(A = 1, W = w)),
              truth_0 = Qbar(cbind(A = 0, W = w)),
              kNN_1 = compute_Qbar_hatAW(1, w, Qbar_hat_kknn),
              kNN_0 = compute_Qbar_hatAW(0, w, Qbar_hat_kknn))
```

### `Qbar`, boosted trees algorithm

Algorithm    $\Algo_{\Qbar,\text{trees}}$    is    obtained    by    combining
`estimate_Qbar`      and      `bstTree_algo`.        The      training      of
$\Algo_{\Qbar,\text{trees}}$    (*i.e.*,   the    making    of   the    output
$\Algo_{\Qbar,\text{trees}}   (P_{n})$  is   implemented  based   on  function
`caret::train`  of the  `caret`  package. Some  additional specifications  are
provided in `bstTree_grid` and `bstTree_control`.

In a nutshell, $\Algo_{\Qbar,\text{trees}}$ estimates $\Qbar_{0}(1,\cdot)$ and
$\Qbar_{0}(0,\cdot)$ separately.  Each  of them is estimated  by boosted trees
as  implemented in  function  `bst::bst` from  the  `bst` (gradient  boosting)
package (to see its man page, simply run `?bst::bst`).^[Specifically, argument
`mstop` (number of boosting iterations for prediction) is one among 10, 20 and
30; argument `nu`  (stepsize of the shrinkage parameter) is  one among 0.1 and
0.2; argument  `maxdepth` (maximum depth of  the base learner, a  tree) is one
among 1, 2 and 5.  An outer  10-fold cross-validation is carried out to select
the best  combination of fine-tune  parameters.]  The following chunk  of code
trains algorithm $\Algo_{\Qbar,\text{trees}}$ on $P_{n}$, and reveals what are
the optimal  fine-tune parameters  for the estimation  of $\Qbar_{0}(1,\cdot)$
and $\Qbar_{0}(0,\cdot)$:


```r
Qbar_hat_trees <- estimate_Qbar(head(obs, 1e3),
                                algorithm = bstTree_algo,
                                trControl = bstTree_control,
                                tuneGrid = bstTree_grid)

Qbar_hat_trees %>% dplyr::filter(a == "one") %>% pull(fit) %>%
  capture.output %>% tail(3) %>% str_wrap(width = 60) %>% cat
#> The final values used for the model were mstop = 20,
#> maxdepth = 1 and nu = 0.2.
                                                             
Qbar_hat_trees %>% dplyr::filter(a == "zero") %>% pull(fit) %>%
  capture.output %>% tail(3) %>% str_wrap(width = 60) %>% cat
#> The final values used for the model were mstop = 30,
#> maxdepth = 1 and nu = 0.1.
```
 
We can compare visually the estimators $\Qbar_{n,\text{kNN}}$,
$\Qbar_{n,\text{trees}}  \equiv  \Algo_{\Qbar,\text{trees}}(P_{n})$  with  its
target  $\Qbar0$,  see   Figure  \@ref(fig:estimate-Qbar-five).   In  summary,
$\Qbar_{n,\text{kNN}}$ is rather good, though  very versatile at the vincinity
of the  break points.  As for  $\Qbar_{n,\text{trees}}$, it  does not  seem to
capture the shape of its target.

(ref:estimate-Qbar-five) Comparing to their target two (machine learning-based) estimators of $\Qbar_{0}$, one based on the $k$-nearest neighbors and the other on boosted trees.


```r
fig %>%
  mutate(trees_1 = compute_Qbar_hatAW(1, w, Qbar_hat_trees),
         trees_0 = compute_Qbar_hatAW(0, w, Qbar_hat_trees)) %>%
  gather("f", "value", -w) %>%
  extract(f, c("f", "a"), "([^_]+)_([01]+)") %>%
  mutate(a = paste0("a=", a)) %>%
  ggplot +
  geom_line(aes(x = w, y = value, color = f), size = 1) +
  labs(y = "f(w)",
       title = bquote("Visualizing" ~ bar(Q)[0] ~ "and its estimators")) +
  ylim(NA, 1) +
  facet_wrap(~ a)
```

<div class="figure" style="text-align: center">
<img src="img/estimate-Qbar-five-1.png" alt="(ref:estimate-Qbar-five)" width="70%" />
<p class="caption">(\#fig:estimate-Qbar-five)(ref:estimate-Qbar-five)</p>
</div>

## &#9881; \gear &#9761; \stixdanger{} `Qbar`, machine learning-based algorithms {#nuisance-Qbar-ml-exo}

1. Using `estimate_Q`, make your  own machine learning-based algorithm for the
   estimation of $\Qbar_{0}$.
   
2. Train your algorithm on the same data set as $\Algo_{\Qbar,\text{kNN}}$ and
   $\Algo_{\Qbar,\text{trees}}$.  If,  like $\Algo_{\Qbar,\text{trees}}$, your
   algorithm  includes  a fine-tuning  procedure,  comment  upon the  optimal,
   data-driven specification.
   
3. Plot  your estimators  of $\Qbar_{0}(1,\cdot)$ and  $\Qbar_{0}(0,\cdot)$ on
   Figure \@ref(fig:estimate-Qbar-five).

\index{algorithm!machine~learning|)}

\index{algorithm|)}

# Two "naive" plug-in inference strategies 

## Why "naive"?

In  this section,  we  present  and discuss  two  plug-in  strategies for  the
inference of $\Psi(P_{0})$. In light of Section \@ref(anatomy), both unfold in
*two*  stages.   During  the  first  stage,  some  features  among  $Q_{0,W}$,
$\Gbar_{0}$  and $\Qbar_{0}$  (the  $\Psi$-specific  nuisance parameters,  see
Section \@ref(nuisance))\index{nuisance~parameters} are  estimated. During the
second   stage,  the   estimators  are   substituted  for   their  theoretical
counterparts  in the  definition  of $\Psi(P_{0})$,  thus yielding  estimators
of $\Psi(P_{0})$.

Although the strategies  sound well conceived, a  theoretical analysis reveals
that they lack a third stage trying to correct an inherent flaw. They are thus
said  *naive*. The  analysis and  a first  *modus operandi*  are presented  in
Section \@ref(analysis-of-plug-in).

## IPTW estimator {#known-gbar-second-pass}

### Construction and computation

In Section \@ref(known-gbar-first-pass), we developed an IPTW substitution
estimator,    $\psi_{n}^{b}$,   *assuming    that*    we   knew    $\Gbar_{0}$
beforehand.  What  if  we  did  not?  Obviously,  we  could  estimate  it  and
substitute   the  estimator   of   $\ell\Gbar_{0}$   for  $\ell\Gbar_{0}$   in
\@ref(eq:psi-n-b). 

Let $\Algo_{\Gbar}$ be an algorithm designed for the estimation of $\Gbar_{0}$
(see   Section  \@ref(nuisance-Gbar)).    We  denote   by  $\Gbar_{n}   \equiv
\Algo_{\Gbar}(P_{n})$ the output  of the algorithm trained on  $P_{n}$, and by
$\ell\Gbar_{n}$ the resulting (empirical) function given by

\begin{equation*}
\ell\Gbar_{n}(A,W) \equiv A \Gbar_{n}(W) + (1-A) (1 - \Gbar_{n}(W)).
\end{equation*}

In light of \@ref(eq:psi-n-b), introduce 

\begin{equation*}
\psi_{n}^{c}   \equiv   \frac{1}{n}    \sum_{i=1}^{n}   \left(\frac{2A_{i}   -
1}{\ell\Gbar_{n}(A_{i}, W_{i})} Y_{i}\right).
\end{equation*} 

From a  computational point of  view, the `tlrider`  package makes it  easy to
build $\psi_{n}^{c}$. Recall that 


```r
compute_iptw(head(obs, 1e3), Gbar)
```

implements  the computation  of  $\psi_{n}^{b}$ based  on  the $n=1000$  first
observations stored  in `obs`,  using the true  feature $\Gbar_{0}$  stored in
`Gbar`,  see  Section  \@ref(empirical-inves-IPTW)  and  the  construction  of
`psi_hat_ab`. Similarly,


```r
Ghat <- estimate_Gbar(head(obs, 1e3), working_model_G_one)
compute_iptw(head(obs, 1e3), wrapper(Ghat)) %>% pull(psi_n)
#> [1] 0.0707
```

implements *(i)*  the estimation of $\Gbar_{0}$  with $\Gbar_{n}$/`Ghat` using
algorithm  $\Algo_{\Gbar,1}$  (first  line)  then *(ii)*  the  computation  of
$\psi_{n}^{c}$ (second line), both based on the same observations as above.

Note  how we  use function  `wrapper` (simply  run `?wrapper`  to see  its man
page).


### Elementary statistical properties {#elementary-stat-prop-iptw}

Because $\Gbar_{n}$  minimizes the  empirical risk over  a finite-dimensional,
identifiable, and **well-specified** working  model, $\sqrt{n} (\psi_{n}^{c} -
\psi_{0})$ converges in law to a centered Gaussian law.\index{identifiability}
Moreover, the asymptotic  variance of $\sqrt{n} (\psi_{n}^{c}  - \psi_{0})$ is
**conservatively**^[In words,  $v_{n}^{c}$ converges to an  upper-bound of the
true        asymptotic         variance.]\index{conservative}        estimated
with\index{well/mis-specified}

\begin{align*}            v_{n}^{c}            &\equiv            \Var_{P_{n}}
\left(\frac{2A-1}{\ell\Gbar_{n}(A,W)}Y\right)      \\      &=      \frac{1}{n}
\sum_{i=1}^{n}\left(\frac{2A_{i}-1}{\ell\Gbar_{n}   (A_{i},W_{i})}   Y_{i}   -
\psi_{n}^{c}\right)^{2}.  \end{align*}

We  investigate *empirically*  the statistical  behavior of  $\psi_{n}^{c}$ in
Section  \@ref(empirical-inves-IPTW-bis). For  an analysis  of the  reason why
$v_{n}^{c}$  is  a  conservative  estimator  of  the  asymptotic  variance  of
$\sqrt{n} (\psi_{n}^{c} - \psi_{0})$, see [here](#iptw-est-var).

Before proceeding, let us touch upon what would have happened if we had used a
less amenable  algorithm $\Algo_{\Gbar}$. For instance,  $\Algo_{\Gbar}$ could
still be  well-specified^[Well-specified *e.g.* in  the sense that  the target
$\Gbar_{0}$ of $\Algo_{\Gbar}$ belongs to the closure of the algorithm's image
$\Algo_{\Gbar}(\calM^{\text{empirical}})$   or,  in   other   words,  can   be
approximated  arbitrarily  well  by  an  output of  the  algorithm.]   but  so
*versatile/complex*   (as   opposed   to    being   based   on   well-behaved,
finite-dimensional parametric  model) that  the estimator  $\Gbar_{n}$, though
still  consistent,  would  converge  slowly to  its  target.   Then,  root-$n$
consistency   would    fail   to   hold.    Or    $\Algo_{\Gbar}$   could   be
mis-specified\index{well/mis-specified} and there would be no guarantee at all
that the resulting estimator $\psi_{n}^{c}$ be even consistent.



### Empirical investigation {#empirical-inves-IPTW-bis}

Let  us compute  $\psi_{n}^{c}$ on  the same  `iter =  ` 100 independent
samples  of  independent  observations  drawn   from  $P_{0}$  as  in  Section
\@ref(known-gbar-first-pass).  As  explained in Sections  \@ref(inference) and
\@ref(empirical-inves-IPTW), we first  make `iter` data sets out  of the `obs`
data set (third line), then train  algorithm $\Algo_{\Gbar,1}$ on each of them
(fifth  to seventh  lines).  After  the first  series of  commands the  object
`learned_features_fixed_sample_size`, a `tibble`, contains 100 rows and
three columns.  

We  created `learned_features_fixed_sample_size`  to store  the estimators  of
$\Gbar_{0}$ for future  use. We will at  a later stage enrich  the object, for
instance  by adding  to  it  estimators of  $\Qbar_{0}$  obtained by  training
different algorithms on each smaller data set.

In the  second series  of commands,  the object  `psi_hat_abc` is  obtained by
adding to  `psi_hat_ab` (see Section \@ref(empirical-inves-IPTW))  an 100
by  four  `tibble`  containing  notably   the  values  of  $\psi_{n}^{c}$  and
$\sqrt{v_{n}^{c}}/\sqrt{n}$  computed by  calling  `compute_iptw`. The  object
also contains  the values of the  recentered (with respect to  $\psi_{0}$) and
renormalized $\sqrt{n}/\sqrt{v_{n}^{c}}  (\psi_{n}^{c} -  \psi_{0})$. Finally,
`bias_abc`  reports  amounts of  bias  (at the  renormalized scale).


```r
learned_features_fixed_sample_size <-
  obs %>% as_tibble() %>%
  mutate(id = (seq_len(n()) - 1) %% iter) %>%
  nest(-id, .key = "obs") %>%
  mutate(Gbar_hat =
           map(obs,
               ~ estimate_Gbar(., algorithm = working_model_G_one)))

psi_hat_abc <-
  learned_features_fixed_sample_size %>%
  mutate(est_c =
           map2(obs, Gbar_hat,
                ~ compute_iptw(as.matrix(.x), wrapper(.y, FALSE)))) %>%
  unnest(est_c) %>% select(-Gbar_hat, -obs) %>%
  mutate(clt = (psi_n - psi_zero) / sig_n,
         type = "c") %>%
  full_join(psi_hat_ab)

(bias_abc <- psi_hat_abc %>%
   group_by(type) %>% summarise(bias = mean(clt)))
#> # A tibble: 3 x 2
#>   type    bias
#>   <chr>  <dbl>
#> 1 a     1.53  
#> 2 b     0.0922
#> 3 c     0.0254
```

By the above chunk of code, the average of $\sqrt{n/v_{n}^{c}} (\psi_{n}^{c} -
\psi_{0})$ computed across the realizations is equal to 
0.025 (see  `bias_abc`). In words, the  average bias of $\psi_{n}^{c}$  is of the
same  magnitude  as   that  of  $\psi_{n}^{b}$  despite  the   fact  that  the
construction of $\psi_{n}^{c}$ hinges on  the estimation of $\Gbar_{0}$ (based
on the well-specified algorithm $\Algo_{\Gbar,1}$).

We represent the empirical laws of the recentered (with respect to $\psi_{0}$)
and renormalized $\psi_{n}^{a}$, $\psi_{n}^{b}$  and $\psi_{n}^{c}$ in Figures
\@ref(fig:unknown-Gbar-three)     (kernel      density     estimators)     and
\@ref(fig:unknown-Gbar-four) (quantile-quantile plots).
 
(ref:unknown-Gbar-three)  Kernel  density  estimators  of  the  law  of  three
estimators  of   $\psi_{0}$  (recentered  with  respect   to  $\psi_{0}$,  and
renormalized), one of them misconceived  (a), one assuming that $\Gbar_{0}$ is
known  (b) and  one that  hinges  on the  estimation of  $\Gbar_{0}$ (c).  The
present  figure includes  Figure \@ref(fig:known-Gbar-one-b)  (but the  colors
differ). Built based on `iter` independent realizations of each estimator.
 

```r
fig_bias_ab +
  geom_density(aes(clt, fill = type, colour = type), psi_hat_abc, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_abc, size = 1.5, alpha = 0.5) +
  xlim(-3, 4) + 
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(a, b, c)})*
                            (psi[n]^{list(a, b, c)} - psi[0]))))
```

<div class="figure" style="text-align: center">
<img src="img/unknown-Gbar-three-1.png" alt="(ref:unknown-Gbar-three)" width="70%" />
<p class="caption">(\#fig:unknown-Gbar-three)(ref:unknown-Gbar-three)</p>
</div>

(ref:unknown-Gbar-four)  Quantile-quantile plot  of  the  standard normal  law
against the  empirical laws  of three  estimators of  $\psi_{0}$, one  of them
misconceived (a),  one assuming  that $\Gbar_{0}$  is known  (b) and  one that
hinges  on  the  estimation  of   $\Gbar_{0}$  (c).   Built  based  on  `iter`
independent realizations of each estimator. 


```r
ggplot(psi_hat_abc, aes(sample = clt, fill = type, colour = type)) +
  geom_abline(intercept = 0, slope = 1, alpha = 0.5) +
  geom_qq(alpha = 1)
```

<div class="figure" style="text-align: center">
<img src="img/unknown-Gbar-four-1.png" alt="(ref:unknown-Gbar-four)" width="70%" />
<p class="caption">(\#fig:unknown-Gbar-four)(ref:unknown-Gbar-four)</p>
</div>

Figures \@ref(fig:unknown-Gbar-three) and \@ref(fig:unknown-Gbar-four) confirm
that $\psi_{n}^{c}$ behaves as well as $\psi_{n}^{b}$ in terms of bias --- but
remember that we  acted as oracles when we chose  the well-specified algorithm
$\Algo_{\Gbar,1}$. They  also corroborate  that $v_{n}^{c}$, the  estimator of
the  asymptotic   variance  of   $\sqrt{n}  (\psi_{n}^{c}  -   \psi_{0})$,  is
conservative: for  instance, the corresponding  bell-shaped blue curve  is too
much concentrated around its axis of symmetry. 

The actual asymptotic variance of  $\sqrt{n} (\psi_{n}^{c} - \psi_{0})$ can be
estimated  with the  empirical  variance  of the  `iter`  replications of  the
construction   of  $\psi_{n}^{c}$.


```r
(emp_sig_n <- psi_hat_abc %>% filter(type == "c") %>%
   summarize(sd(psi_n)) %>% pull)
#> [1] 0.0191
(summ_sig_n <- psi_hat_abc %>% filter(type == "c") %>% select(sig_n) %>%
   summary)
#>      sig_n       
#>  Min.   :0.0531  
#>  1st Qu.:0.0550  
#>  Median :0.0557  
#>  Mean   :0.0561  
#>  3rd Qu.:0.0570  
#>  Max.   :0.0616
```



The empirical standard deviation is approximately 
2.939  times smaller  than the  average
*estimated*  standard   deviation.  The  estimator  is   conservative  indeed!
Furthermore, note  the better fit with  the density of the  standard normal
density    of  the  kernel  density estimator  of  the  law  of  $\sqrt{n}
(\psi_{n}^{c} - \psi_{0})$ **renormalized with** `emp_sig_n`.

(ref:unknown-Gbar-seven)  Kernel  density  estimators  of  the  law  of  three
estimators  of   $\psi_{0}$  (recentered  with  respect   to  $\psi_{0}$,  and
renormalized), one of them misconceived  (a), one assuming that $\Gbar_{0}$ is
known  (b) and  one that  hinges  on the  estimation of  $\Gbar_{0}$ **and  an
estimator  of  the  asymptotic  variance computed  across  the  replications**
(c). The  present figure includes Figure  \@ref(fig:known-Gbar-one-b) (but the
colors     differ)    and     it    should     be    compared     to    Figure
\@ref(fig:unknown-Gbar-four). Built  based on `iter`  independent realizations
of each estimator.



```r
clt_c <- psi_hat_abc %>% filter(type == "c") %>%
  mutate(clt = clt * sig_n /  emp_sig_n)

fig_bias_ab +
  geom_density(aes(clt, fill = type, colour = type), clt_c, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_abc, size = 1.5, alpha = 0.5) +
  xlim(-3, 4) + 
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(a, b, c)})*
                            (psi[n]^{list(a, b, c)} - psi[0]))))
```

<div class="figure" style="text-align: center">
<img src="img/unknown-Gbar-seven-1.png" alt="(ref:unknown-Gbar-seven)" width="70%" />
<p class="caption">(\#fig:unknown-Gbar-seven)(ref:unknown-Gbar-seven)</p>
</div>

**Workaround.**  In  a  real  world   data-analysis,  one  could  correct  the
estimation of the asymptotic variance of $\sqrt{n} (\psi_{n}^{c} - \psi_{0})$.
We  could for  instance  derive  the influence  function  as  it is  described
[here](#iptw-est-var)  and  use  the  corresponding  influence  function-based
estimator of  the variance.   Or one  could rely  on the  bootstrap.^[That is,
replicate the  construction of  $\psi_{n}^{c}$ many times  based on  data sets
obtained  by  resampling  from  the  original  data  set,  then  estimate  the
asymptotic  variance with  the empirical  variance of  $\psi_{n}^{c}$ computed
across the replications.] This, however, would only make sense if one knew for
sure that the algorithm for the estimation of $\Gbar_{0}$ is well-specified.

## &#9881; \gear Investigating further the IPTW inference strategy {#exo-a-nice-title}


1. Building  upon the chunks  of code devoted  to the repeated  computation of
$\psi_{n}^{b}$ and  its companion  quantities, construct  confidence intervals
for  $\psi_{0}$ of  (asymptotic)  level  $95\%$, and  check  if the  empirical
coverage is satisfactory.  Note that if  the coverage was exactly $95\%$, then
the number of confidence intervals  that would contain $\psi_{0}$ would follow
a binomial  law with parameters  `iter` and  `0.95`, and recall  that function
`binom.test` performs  an exact  test of  a simple  null hypothesis  about the
probability of success  in a Bernoulli experiment against  its three one-sided
and two-sided alternatives.


2. Discuss what happens when the dimension of the (still well-specified)\index{well/mis-specified} working model grows.  Start with the built-in working model `working_model_G_two`.  The following chunk of code re-defines `working_model_G_two`


```r
## make sure '1/2' and '1' belong to 'powers'
powers <- rep(seq(1/4, 3, by = 1/4), each = 2) 
working_model_G_two <- list(
  model = function(...) {trim_glm_fit(glm(family = binomial(), ...))},
  formula = stats::as.formula(
    paste("A ~",
          paste(c("I(W^", "I(abs(W - 5/12)^"),
                powers, 
                sep = "", collapse = ") + "),
          ")")
  ),
  type_of_preds = "response"
)
attr(working_model_G_two, "ML") <- FALSE
```

3. Play around with argument `powers` (making sure that `1/2` and `1` belong to
it),   and   plot   graphics   similar   to   those   presented   in   Figures
\@ref(fig:unknown-Gbar-three) and \@ref(fig:unknown-Gbar-four).

4. Discuss    what     happens     when    the     working    model     is
mis-specified.\index{well/mis-specified}  You could  use the  built-in working
model `working_model_G_three`.

5. Repeat the  analysis developed  in  response to  problem 1  above but  for
$\psi_{n}^{c}$.  What  can  you  say  about the  coverage  of  the  confidence
intervals? 

6. &#9761;  \stixdanger{} (Follow-up  to problem  5). Implement  the bootstrap
procedure evoked at the end of Section \@ref(empirical-inves-IPTW-bis). Repeat
the analysis  developed in response  to problem  1. Compare your  results with
those to problem 5.

7. &#9761; \stixdanger{} Is it legitimate to infer the asymptotic variance
of   $\psi_{n}^{c}$   with   $v_{n}^{c}$   when   one   relies   on   a   very
data-adaptive/versatile algorithm to estimate $\Gbar_{0}$?



## G-computation estimator {#Gcomp-estimator}

### Construction and computation

Let $\Algo_{Q_{W}}$ be  an algorithm designed for the  estimation of $Q_{0,W}$
(see   Section   \@ref(nuisance-QW)).    We    denote   by   $Q_{n,W}   \equiv
\Algo_{Q_{W}}(P_{n})$ the output of the algorithm trained on $P_{n}$.


Let $\Algo_{\Qbar}$ be an algorithm designed for the estimation of $\Qbar_{0}$
(see   Section  \@ref(nuisance-Qbar)).    We  denote   by  $\Qbar_{n}   \equiv
\Algo_{\Qbar}(P_{n})$ the output of the algorithm trained on $P_{n}$. 

Equation \@ref(eq:psi-zero) suggests the following, simple estimator of
$\Psi(P_0)$:

\begin{equation} 
\psi_{n}   \equiv   \int   \left(\Qbar_{n}(1,   w)   -   \Qbar_{n}(0,w)\right)
dQ_{n,W}(w). (\#eq:Gcomp-estimator)
\end{equation} 

In words,  this estimator is implemented  by first regressing $Y$  on $(A,W)$,
then by marginalizing with respect to the estimated law of $W$.  The resulting
estimator  is   referred  to  as  a   *G-computation*  (or  *standardization*)
estimator.

From a  computational point of  view, the `tlrider`  package makes it  easy to
build the G-computation  estimator. Recall that we have  already estimated the
marginal  law  $Q_{0,W}$  of  $W$  under $P_{0}$  by  training  the  algorithm
$\Algo_{Q_{W}}$ as it is implemented in  `estimate_QW` on the $n = 1000$ first
observations in `obs` (see Section \@ref(nuisance-QW)):


```r
QW_hat <- estimate_QW(head(obs, 1e3))
```

Recall  that  $\Algo_{\Qbar,1}$  is  the   algorithm  for  the  estimation  of
$\Qbar_{0}$  as  it  is  implemented  in  `estimate_Qbar`  with  its  argument
`algorithm`   set  to   the   built-in   `working_model_Q_one`  (see   Section
\@ref(nuisance-Qbar-wm)). Recall  also that $\Algo_{\Qbar,\text{kNN}}$  is the
algorithm  for  the  estimation  of   $\Qbar_{0}$  as  it  is  implemented  in
`estimate_Qbar` with its argument `algorithm`  set to the built-in `kknn_algo`
(see Section \@ref(Qbar-knn-algo)).  We have already trained the latter on the
$n=1000$ first observations in `obs`. Let us train the former on the same data
set:


```r
Qbar_hat_kknn <- estimate_Qbar(head(obs, 1e3),
                               algorithm = kknn_algo,
                               trControl = kknn_control,
                               tuneGrid = kknn_grid)
```


```r
Qbar_hat_d <- estimate_Qbar(head(obs, 1e3), working_model_Q_one)
```

With  these estimators  handy,  computing the  G-computation  estimator is  as
simple as running the following chunk of code:


```r
(compute_gcomp(QW_hat, wrapper(Qbar_hat_kknn, FALSE), 1e3))
#> # A tibble: 1 x 2
#>    psi_n   sig_n
#>    <dbl>   <dbl>
#> 1 0.0722 0.00487
(compute_gcomp(QW_hat, wrapper(Qbar_hat_d, FALSE), 1e3))
#> # A tibble: 1 x 2
#>    psi_n   sig_n
#>    <dbl>   <dbl>
#> 1 0.0742 0.00215
```


Note how we use function `wrapper` again,  and that it is necessary to provide
the  number of  observations  upon which  the estimation  of  the $Q_{W}$  and
$\Qbar$ features of $P_{0}$.


### Elementary statistical properties

This subsection is very similar to its counterpart for the IPTW estimator, see
Section \@ref(elementary-stat-prop-iptw). 

Let  us denote  by  $\Qbar_{n,1}$ the  output  of algorithm  $\Algo_{\Qbar,1}$
trained on  $P_{n}$, and recall  that $\Qbar_{n,\text{kNN}}$ is the  output of
algorithm  $\Algo_{\Qbar,\text{kNN}}$ trained  on $P_{n}$.  Let $\psi_{n}^{d}$
and $\psi_{n}^{e}$  be the  G-computation estimators obtained  by substituting
$\Qbar_{n,1}$     and     $\Qbar_{n,\text{kNN}}$    for     $\Qbar_{n}$     in
\@ref(eq:Gcomp-estimator), respectively. 


If    $\Qbar_{n,\bullet}$   minimized    the    empirical    risk   over    a
finite-dimensional, identifiable,  and **well-specified** working  model, then
$\sqrt{n}  (\psi_{n}^{\bullet}  -  \psi_{0})$  would converge  in  law  to  a
centered Gaussian law (here $\psi_{n}^{\bullet}$ represents the G-computation
estimator  obtained by  substituting $\Qbar_{n,\bullet}$  for $\Qbar_{n}$  in
\@ref(eq:Gcomp-estimator)).\index{identifiability}  Moreover,  the  asymptotic
variance  of $\sqrt{n}  (\psi_{n}^{\bullet} -  \psi_{0})$ would  be estimated
**anti-conservatively**^[In words,  $v_{n}^{d}$ converges to a  lower-bound of
the true asymptotic variance.]\index{conservative} with\index{well/mis-specified}

\begin{align} 
v_{n}^{d}            &\equiv            \Var_{P_{n}}
\left(\Qbar_{n,1}(1,\cdot) - \Qbar_{n,1}(0,\cdot)\right) \\ &= \frac{1}{n}
\sum_{i=1}^{n}\left(\Qbar_{n,1}(1,W_{i})         -        \Qbar_{n,1}(0,W_{i})
-\psi_{n}^{d}\right)^{2}.  (\#eq:var-Gcomp-n) 
\end{align} 

Unfortunately,    algorithm    $\Algo_{\Qbar,1}$    is    mis-specified    and
$\Algo_{\Qbar,\text{kNN}}$  is  not  based  on  a  finite-dimensional  working
model.  Nevertheless, function  `compute_gcomp`  estimates  (in general,  very
poorly) the asymptotic variance with \@ref(eq:var-Gcomp-n).

We investigate *empirically* the statistical behavior of $\psi_{n}^{d}$ in
Section  \@ref(empirical-inves-Gcomp).   For an  analysis  of  the reason  why
$v_{n}^{d}$ is  an anti-conservative estimator  of the asymptotic  variance of
$\sqrt{n} (\psi_{n}^{d} - \psi_{0})$,  see [here](#gcomp-est-var).  We wish to
emphasize   that  anti-conservativeness   is  even   more  embarrassing   that
conservativeness (both  being contingent on  the fact that the  algorithms are
well-specified, fact  that cannot be  true in the  present case in  real world
situations).

What would happen  if we used a less amenable  algorithm $\Algo_{\Qbar}$.  For
instance,   $\Algo_{\Qbar}$    could   still   be   well-specified    but   so
*versatile/complex*   (as   opposed   to    being   based   on   well-behaved,
finite-dimensional parametric  model) that  the estimator  $\Qbar_{n}$, though
still  consistent,  would  converge  slowly to  its  target.   Then,  root-$n$
consistency would  fail to  hold.  We can  explore empirically  this situation
with     estimator     $\psi_{n}^{e}$     that     hinges     on     algorithm
$\Algo_{\Qbar,\text{kNN}}$.       Or       $\Algo_{\Qbar}$      could       be
mis-specified\index{well/mis-specified} and there would be no guarantee at all
that the resulting estimator $\psi_{n}$ be even consistent.

### Empirical investigation {#empirical-inves-Gcomp}

**Fixed sample size**

Let us compute $\psi_{n}^{d}$ and $\psi_{n}^{e}$ on the same `iter = ` 
100 independent samples of independent observations drawn from $P_{0}$ as in
Sections \@ref(known-gbar-first-pass) and \@ref(empirical-inves-IPTW-bis).  We
first enrich  object `learned_features_fixed_sample_size` that was  created in
Section   \@ref(empirical-inves-IPTW-bis),   adding   to  it   estimators   of
$\Qbar_{0}$   obtained   by    training   algorithms   $\Algo_{\Qbar,1}$   and
$\Algo_{\Qbar,\text{kNN}}$ on each smaller data set.

The second series of commands creates  object `psi_hat_de`, an 100 by six
`tibble`    containing   notably    the   values    of   $\psi_{n}^{d}$    and
$\sqrt{v_{n}^{d}}/\sqrt{n}$ computed by calling  `compute_gcomp`, and those of
the    recentered   (with    respect   to    $\psi_{0}$)   and    renormalized
$\sqrt{n}/\sqrt{v_{n}^{d}}  (\psi_{n}^{d}  -   \psi_{0})$.   Because  we  know
beforehand that $v_{n}^{d}$ under-estimates  the actual asymptotic variance of
$\sqrt{n} (\psi_{n}^{d} - \psi_{0})$, the `tibble` also includes the values of
$\sqrt{n}/\sqrt{v^{d*}}  (\psi_{n}^{d}   -  \psi_{0})$  where   the  estimator
$v^{d*}$ of  the asymptotic variance  is computed *across the  replications of
$\psi_{n}^{d}$*.   The  tibble  includes  the same  quantities  pertaining  to
$\psi_{n}^{e}$, although  there is no  theoretical guarantee that  the central
limit theorem does hold and, even  if it did, that the counterpart $v_{n}^{e}$
to  $v_{n}^{d}$ estimates  in any  way  the asymptotic  variance of  $\sqrt{n}
(\psi_{n}^{e} - \psi_{0})$.

Finally,  `bias_de` reports  amounts of  bias (at  the renormalized  scales ---
plural). There is one value of bias  for each combination of *(i)* type of the
estimator (`d`  or `e`)  and *(ii)*  how the  renormalization is  carried out,
either based on $v_{n}^{d}$ and $v_{n}^{e}$ (`auto_renormalization` is `TRUE`)
*or*  on  the  estimator  of  the  asymptotic  variance  computed  across  the
replications of  $\psi_{n}^{d}$ and $\psi_{n}^{e}$  (`auto_renormalization` is
`FALSE`).



(ref:estimating-Qbar-one-bis)  Kernel density  estimators  of the  law of  two
estimators  of   $\psi_{0}$  (recentered  with  respect   to  $\psi_{0}$,  and
renormalized).     The   estimators    respectively   hinge    on   algorithms
$\Algo_{\Qbar,1}$   (d)  and   $\Algo_{\Qbar,\text{kNN}}$   (e)  to   estimate
$\Qbar_{0}$.    Two   renormalization    schemes   are    considered,   either
based on  an estimator of the  asymptotic variance (left) or  on the empirical
variance  computed   across  the   `iter`  independent  replications   of  the
estimators  (right). We  emphasize  that the  $x$-axis  ranges differ  starkly
between the left and right plots.





```r
learned_features_fixed_sample_size <-
  learned_features_fixed_sample_size %>% 
  mutate(Qbar_hat_d =
           map(obs,
               ~ estimate_Qbar(., algorithm = working_model_Q_one)),
         Qbar_hat_e =
           map(obs,
               ~ estimate_Qbar(., algorithm = kknn_algo,
                               trControl = kknn_control,
                               tuneGrid = kknn_grid))) %>%
  mutate(QW = map(obs, estimate_QW),
         est_d =
           pmap(list(QW, Qbar_hat_d, n()),
                ~ compute_gcomp(..1, wrapper(..2, FALSE), ..3)),
         est_e =
           pmap(list(QW, Qbar_hat_e, n()),
                ~ compute_gcomp(..1, wrapper(..2, FALSE), ..3)))

psi_hat_de <- learned_features_fixed_sample_size %>%
  select(est_d, est_e) %>%
  gather(`est_d`, `est_e`, key = "type", value = "estimates") %>%
  extract(type, "type", "_([de])$") %>% 
  unnest(estimates) %>%
  group_by(type) %>%
  mutate(sig_alt = sd(psi_n)) %>%
  mutate(clt_ = (psi_n - psi_zero) / sig_n,
         clt_alt = (psi_n - psi_zero) / sig_alt) %>%
  gather(`clt_`, `clt_alt`, key = "key", value = "clt") %>%
  extract(key, "key", "_(.*)$") %>%
  mutate(key = ifelse(key == "", TRUE, FALSE)) %>%
  rename("auto_renormalization" = key)

(bias_de <- psi_hat_de %>%
   group_by(type, auto_renormalization) %>%
   summarize(bias = mean(clt)))
#> # A tibble: 4 x 3
#> # Groups:   type [?]
#>   type  auto_renormalization  bias
#>   <chr> <lgl>                <dbl>
#> 1 d     FALSE                0.356
#> 2 d     TRUE                 1.08 
#> 3 e     FALSE                0.130
#> 4 e     TRUE                 0.150

fig <- ggplot() +
  geom_line(aes(x = x, y = y), 
            data = tibble(x = seq(-4, 4, length.out = 1e3),
                          y = dnorm(x)),
            linetype = 1, alpha = 0.5) +
  geom_density(aes(clt, fill = type, colour = type),
               psi_hat_de, alpha = 0.1) +
  geom_vline(aes(xintercept = bias, colour = type),
             bias_de, size = 1.5, alpha = 0.5) +
  facet_wrap(~ auto_renormalization,
             labeller =
               as_labeller(c(`TRUE` = "auto-renormalization: TRUE",
                             `FALSE` = "auto-renormalization: FALSE")),
             scales = "free")
  
fig +
  labs(y = "",
       x = expression(paste(sqrt(n/v[n]^{list(d, e)})*
                            (psi[n]^{list(d, e)} - psi[0]))))
```

<div class="figure" style="text-align: center">
<img src="img/estimating-Qbar-one-bis-1.png" alt="(ref:estimating-Qbar-one-bis)" width="70%" />
<p class="caption">(\#fig:estimating-Qbar-one-bis)(ref:estimating-Qbar-one-bis)</p>
</div>


 
We represent the empirical laws of the recentered (with respect to $\psi_{0}$)
and    renormalized    $\psi_{n}^{d}$    and    $\psi_{n}^{e}$    in    Figure
\@ref(fig:estimating-Qbar-one-bis)    (kernel   density    estimators).    Two
renormalization schemes  are considered, either  based on an estimator  of the
asymptotic variance  (left) or on  the empirical variance computed  across the
`iter` independent replications of the  estimators (right).  We emphasize that
the $x$-axis ranges differ starkly between the left and right plots.


Two  important  comments   are  in  order.   First,  on  the   one  hand,  the
G-computation estimator  $\psi_{n}^{d}$ is biased. Specifically,  by the above
chunk of code, the averages  of $\sqrt{n/v_{n}^{d}} (\psi_{n}^{d} - \psi_{0})$
and  $\sqrt{n/v_{n}^{d*}}  (\psi_{n}^{d}  -  \psi_{0})$  computed  across  the
realizations are equal to 1.076 and 0.356 (see `bias_de`).   On  the other  hand, the  G-computation
estimator   $\psi_{n}^{e}$  is   biased   too,  though   slightly  less   than
$\psi_{n}^{d}$.  Specifically,  by the  above chunk of  code, the  averages of
$\sqrt{n/v_{n}^{e}}   (\psi_{n}^{e}   -    \psi_{0})$   and   $\sqrt{n/v^{e*}}
(\psi_{n}^{e} - \psi_{0})$ computed across the realizations are equal to 
0.15 and 0.13 (see
`bias_de`). We  can provide an oracular  explanation. Estimator $\psi_{n}^{d}$
suffers     from    the     poor    approximation     of    $\Qbar_{0}$     by
$\Algo_{\Qbar,1}(P_{n})$, a  result of  the algorithm's  mis-specification. As
for  $\psi_{n}^{e}$,  it   behaves  better  because  $\Algo_{\Qbar,\text{kNN}}
(P_{n})$  approximates $\Qbar_{0}$  better  than $\Algo_{\Qbar,1}(P_{n})$,  an
apparent consequence of the greater versatility of the algorithm.

Second,  we get  a visual  confirmation that  $v_{n}^{d}$ under-estimates  the
actual  asymptotic  variance  of  $\sqrt{n} (\psi_{n}^{d}  -  \psi_{0})$:  the
right-hand  side red  bell-shaped curve  is too  dispersed.  In  contrast, the
right-hand side blue bell-shaped curve is  much closer to the black curve that
represents the  density of the standard  normal law. Looking at  the left-hand
side plot reveals  that the empirical law of  $\sqrt{n/v^{d*}} (\psi_{n}^{d} -
\psi_{0})$, once translated to compensate for the bias, is rather close to the
black curve. This means that  the random variable is approximately distributed
like  a Gaussian  random  variable.  On  the contrary,  the  empirical law  of
$\sqrt{n/v^{e*}} (\psi_{n}^{e}  - \psi_{0})$  does not strike  us as  being as
closely Gaussian-like as that  of $\sqrt{n/v^{d*}} (\psi_{n}^{d} - \psi_{0})$.
By    being    more    data-adaptive   than    $\Algo_{\Qbar,1}$,    algorithm
$\Algo_{\Qbar,\text{kNN}}$ yields a better  estimator of $\Qbar_{0}$. However,
the rate of convergence of  $\Algo_{\Qbar,\text{kNN}}(P_{n})$ to its limit may
be slower than root-$n$, invalidating a central limit theorem.
 
How do  the estimated variances  of $\psi_{n}^{d}$ and  $\psi_{n}^{e}$ compare
with their empirical counterparts (computed  across the `iter` replications of
the construction of the two  estimators)?


```r
## psi_n^d
(psi_hat_de %>% ungroup %>%
   filter(type == "d" & auto_renormalization) %>% pull(sig_n) %>% summary)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.00269 0.00539 0.00641 0.00648 0.00753 0.01012
## psi_n^e
(psi_hat_de %>% ungroup %>%
   filter(type == "e" & auto_renormalization) %>% pull(sig_n) %>% summary)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.0133  0.0149  0.0156  0.0157  0.0163  0.0188
```




The empirical standard deviation of $\psi_{n}^{d}$ is approximately 
2.933 times larger than the average *estimated*
standard deviation.  The estimator  is anti-conservative indeed! 

As for the empirical standard deviation of $\psi_{n}^{e}$, it is approximately 
1.214 times larger than the average *estimated*
standard deviation. 

**Varying sample size**


```r
##
## not updated yet
##

sample_size <- c(2e3, 3e3)
block_size <- sum(sample_size)


learned_features_varying_sample_size <- obs %>% as.tibble %>% 
  head(n = (nrow(.) %/% block_size) * block_size) %>% 
  mutate(block = label(1:nrow(.), sample_size)) %>%
  nest(-block, .key = "obs")
```

First, we cut the  data set into independent sub-data sets  of sample size $n$
in $\{$ 2000, 3000 $\}$.  Second, we infer $\psi_{0}$ as shown two chunks
earlier.  We thus obtain 20 independent realizations
of each estimator derived on  data sets of 2, increasing
sample sizes.


```r
learned_features_varying_sample_size <-
  learned_features_varying_sample_size %>%
  mutate(Qbar_hat_d =
           map(obs,
               ~ estimate_Qbar(., algorithm = working_model_Q_one)),
         Qbar_hat_e =
           map(obs,
               ~ estimate_Qbar(., algorithm = kknn_algo,
                               trControl = kknn_control,
                               tuneGrid = kknn_grid))) %>%
  mutate(QW = map(obs, estimate_QW),
         est_d =
           pmap(list(QW, Qbar_hat_d, n()),
                ~ compute_gcomp(..1, wrapper(..2, FALSE), ..3)),
         est_e =
           pmap(list(QW, Qbar_hat_e, n()),
                ~ compute_gcomp(..1, wrapper(..2, FALSE), ..3)))

root_n_bias <- learned_features_varying_sample_size %>%
  mutate(block = unlist(map(strsplit(block, "_"), ~.x[2])),
         sample_size = sample_size[as.integer(block)]) %>%
  select(block, sample_size, est_d, est_e) %>%
  gather(`est_d`, `est_e`, key = "type", value = "estimates") %>%
  extract(type, "type", "_([de])$") %>% 
  unnest(estimates) %>%
  group_by(block, type) %>%
  mutate(sig_alt = sd(psi_n)) %>%
  mutate(clt_ = (psi_n - psi_zero) / sig_n,
         clt_alt = (psi_n - psi_zero) / sig_alt) %>%
  gather(`clt_`, `clt_alt`, key = "key", value = "clt") %>%
  extract(key, "key", "_(.*)$") %>%
  mutate(key = ifelse(key == "", TRUE, FALSE)) %>%
  rename("auto_renormalization" = key)
```
The  `tibble`  called  `root_n_bias`  reports  root-$n$  times  bias  for  all
combinations of  estimator and sample  size. The  next chunk of  code presents
visually our findings, see Figure \@ref(fig:estimating-Qbar-four). Note how we
include the  realizations of the  estimators derived earlier and  contained in
`psi_hat_de`   (thus  breaking   the   independence   between  components   of
`root_n_bias`, a small price to pay in this context).

(ref:estimating-Qbar-four) Evolution of root-$n$ times bias versus sample size for two inference methodology of $\psi_{0}$ based on the estimation of $\Qbar_{0}$.  Big dots represent the average biases and vertical lines represent twice the standard error.


```r
root_n_bias <- learned_features_fixed_sample_size %>%
   mutate(block = "0",
          sample_size = B/iter) %>%  # because *fixed* sample size
   select(block, sample_size, est_d, est_e) %>%
   gather(`est_d`, `est_e`, key = "type", value = "estimates") %>%
   extract(type, "type", "_([de])$") %>% 
   unnest(estimates) %>%
   group_by(block, type) %>%
   mutate(sig_alt = sd(psi_n)) %>%
   mutate(clt_ = (psi_n - psi_zero) / sig_n,
          clt_alt = (psi_n - psi_zero) / sig_alt) %>%
   gather(`clt_`, `clt_alt`, key = "key", value = "clt") %>%
   extract(key, "key", "_(.*)$") %>%
   mutate(key = ifelse(key == "", TRUE, FALSE)) %>%
   rename("auto_renormalization" = key) %>%
   full_join(root_n_bias)
 
root_n_bias %>%
  ggplot() +
  stat_summary(aes(x = sample_size, y = clt,
                   group = interaction(sample_size, type),
                   color = type),
               fun.data = mean_se, fun.args = list(mult = 2),
               position = position_dodge(width = 250), cex = 1) +
  stat_summary(aes(x = sample_size, y = clt,
                   group = interaction(sample_size, type),
                   color = type),
               fun.data = mean_se, fun.args = list(mult = 2),
               position = position_dodge(width = 250), cex = 1,
               geom = "errorbar", width = 750) +
  stat_summary(aes(x = sample_size, y = clt,
                   color = type),
               fun.y = mean,
               position = position_dodge(width = 250),
               geom = "polygon", fill = NA) +
  geom_point(aes(x = sample_size, y = clt,
                 group = interaction(sample_size, type),
                 color = type),
             position = position_dodge(width = 250),
             alpha = 0.1) +
  scale_x_continuous(breaks = unique(c(B / iter, sample_size))) +
  labs(x = "sample size n",
       y = expression(paste(sqrt(n) * (psi[n]^{list(d, e)} - psi[0])))) +
  facet_wrap(~ auto_renormalization,
             labeller =
               as_labeller(c(`TRUE` = "auto-renormalization: TRUE",
                             `FALSE` = "auto-renormalization: FALSE")),
             scales = "free")
```

<div class="figure" style="text-align: center">
<img src="img/estimating-Qbar-four-1.png" alt="(ref:estimating-Qbar-four)" width="70%" />
<p class="caption">(\#fig:estimating-Qbar-four)(ref:estimating-Qbar-four)</p>
</div>


# Work in progress

(...)