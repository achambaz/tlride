

# Basic results and their proofs {#proofs}

## NPSEM {#npsem}

The experiment can also be summarized by a *nonparametric system of structural
equations*:  for   some  deterministic  functions  $f_w$,   $f_a$,  $f_y$  and
independent sources of randomness $U_w$, $U_a$, $U_y$,

1. sample the context where the  counterfactual rewards will be generated, the
action  will be  undertaken  and the  actual  reward will  be  obtained, $W  =
f_{w}(U_w)$;

2.  sample the  two counterfactual  rewards  of the  two actions  that can  be
undertaken, $Y_{0} = f_{y}(0, W, U_y)$ and $Y_{1} = f_{y}(1, W, U_y)$;

3. sample  which action is carried  out in the  given context, $A =  f_{a} (W,
U_a)$;

4.  define the corresponding reward, $Y = A Y_{1} + (1-A) Y_{0}$; 

5. summarize  the course of  the experiment with the  observation $O =  (W, A,
Y)$, thus concealing $Y_{0}$ and $Y_{1}$.

## Identification {#identification}

Let $\bbP_{0}$ be an experiment that  generates $\bbO \equiv (W, Y_{0}, Y_{1},
A, Y)$.   We think of  $W$ as  the context where  an action is  undertaken, of
$Y_{0}$ and  $Y_{1}$ as  the counterfactual  (potential) rewards  that actions
$a=0$ and $a=1$ would entail, of $A$ as  the action carried out, and of $Y$ as
the  reward  received  in  response  to action  $A$.  Consider  the  following
assumptions:

1. **Randomization**:   under   $\bbP_{0}$,  the   counterfactual   rewards
   $(Y_0,Y_1)$ and action $A$ are conditionally independent given $W$, *i.e.*,
   $(Y_0,Y_1) \perp A \mid W$.

1. **Consistency**: under $\bbP_{0}$, if action $A$ is undertaken then reward
  $Y_{A}$ is received, *i.e.*, $Y = Y_{A}$ (or $Y=Y_{a}$ given that $A=a$).

1. **Positivity**:  under $\bbP_{0}$,  both actions  $a=0$ and  $a=1$ have
   ($\bbP_{0}$-almost surely)  a positive  probability to be  undertaken given
   $W$, *i.e.*, $\Pr_{\bbP_0}(\ell\Gbar_0(a,W) > 0) = 1$ for $a=0,1$.



\BeginKnitrBlock{proposition}\iffalse{-91-73-100-101-110-116-105-102-105-99-97-116-105-111-110-93-}\fi{}<div class="proposition"><span class="proposition" id="prp:unnamed-chunk-3"><strong>(\#prp:unnamed-chunk-3)  \iffalse (Identification) \fi{} </strong></span>Under  the  above assumptions,  it  holds  that \begin{equation*}  \psi_{0}  =
\Exp_{\bbP_{0}}   \left(Y_{1}   -   Y_{0}\right)  =   \Exp_{\bbP_{0}}(Y_1)   -
\Exp_{\bbP_{0}}(Y_0). \end{equation*}</div>\EndKnitrBlock{proposition}

\BeginKnitrBlock{proof}<div class="proof">\iffalse{} <span class="proof"><em>Proof. </em></span>  \fi{}Set arbitrarily $a  \in \{0,1\}$.  By the randomization assumption  on the one
hand (second  equality) and by  the consistency and positivity  assumptions on
the   other   hand   (third    equality),   it   holds   that   \begin{align*}
\Exp_{\bbP_0}(Y_a) &=  \int \Exp_{\bbP_0}(Y_a \mid  W = w) dQ_{0,W}(w)  = \int
\Exp_{\bbP_0}(Y_a \mid A = a, W =  w) dQ_{0,W}(w) \\ &= \int \Exp_{P_0}(Y \mid
A =  a, W = w)  dQ_{0,W}(w) = \int \Qbar_0(a,W)  dQ_{0,W}(w). \end{align*} The
stated result easily follows.</div>\EndKnitrBlock{proof}

**Remark.** The positivity assumption is needed  for $\Exp_{P_0}(Y \mid A = a,
W) \equiv \Qbar_{0}(a,W)$ to be well-defined.

## Building a confidence interval {#confidence-interval}

Let  $\Phi$  be  the  standard  normal  distribution  function.  Let  $X_{1}$,
$\ldots$, $X_{n}$ be independently drawn from a given law.

### CLT & Slutsky's lemma {#clt}

Assume  that  $\sigma^{2}  \equiv  \Var(X_{1})$  is  finite.   Let  $m  \equiv
\Exp(X_{1})$  be   the  mean  of   $X_{1}$  and  $\bar{X}_{n}   \equiv  n^{-1}
\sum_{i=1}^{n} X_{i}$  be the  empirical mean.  By  the central  limit theorem
(CLT), it  holds that  $\sqrt{n} (\bar{X}_{n}  - m)$ converges  in law  as $n$
grows to the centered Gaussian law with variance $\sigma^{2}$.

Moreover,  if  $\sigma_{n}^{2}$  is   a  (positive)  consistent  estimator  of
$\sigma^{2}$ then, by Slutsky's lemma, $\sqrt{n}/\sigma_{n} (\bar{X}_{n} - m)$
converges in law  to the standard normal law.  The  empirical variance $n^{-1}
\sum_{i=1}^{n}(X_{i} - \bar{X}_{n})^{2}$ is such an estimator. 

\BeginKnitrBlock{proposition}<div class="proposition"><span class="proposition" id="prp:unnamed-chunk-5"><strong>(\#prp:unnamed-chunk-5) </strong></span>Under   the  above   assumptions,   \begin{equation*}  \left[\bar{X}_{n}   \pm
\Phi^{-1}(1-\alpha)  \frac{\sigma_{n}}{\sqrt{n}}\right]  \end{equation*} is  a
confidence interval for $m$ with asymptotic level $(1-2\alpha)$.</div>\EndKnitrBlock{proposition}
    
### CLT and order statistics {#order}

Suppose  that the  law of  $X_{1}$ admits  a continuous  distribution function
$F$. Set $p \in ]0,1[$ and, assuming that  $n$ is large, find $k\geq 1$ and $l
\geq   1$    such   that    \begin{equation*}   \frac{k}{n}   \approx    p   -
\Phi^{-1}(1-\alpha)      \sqrt{\frac{p(1-p)}{n}}      \end{equation*}      and
\begin{equation*}    \frac{l}{n}     \approx    p     +    \Phi^{-1}(1-\alpha)
\sqrt{\frac{p(1-p)}{n}}.  \end{equation*} 

\BeginKnitrBlock{proposition}<div class="proposition"><span class="proposition" id="prp:unnamed-chunk-6"><strong>(\#prp:unnamed-chunk-6) </strong></span>Under the above assumptions, $[X_{(k)},X_{(l)}]$  is a confidence interval for
$F^{-1}(p)$ with asymptotic level $1 - 2\alpha$.</div>\EndKnitrBlock{proposition}

## Another representation of the parameter of interest {#another-rep}

For notational simplicitiy,  note that $(2a-1)$ equals 1 if  $a=1$ and $-1$ if
$a=0$.      Now,      for     each      $a     =      0,1$,     \begin{align*}
\Exp_{P_{0}}\left(\frac{\one\{A    =    a\}Y}{\ell\Gbar_{0}(a,W)}\right)    &=
\Exp_{P_{0}}\left(\Exp_{P_{0}}\left(\frac{\one\{A  = a\}Y}{\ell\Gbar_{0}(a,W)}
\middle|  A,  W  \right)   \right)  \\  &=  \Exp_{P_{0}}\left(\frac{\one\{A  =
a\}}{\ell\Gbar_{0}(a,W)}      \Qbar_{0}(A,      W)     \right)      \\      &=
\Exp_{P_{0}}\left(\frac{\one\{A   =    a\}}{\ell\Gbar_{0}(a,W)}   \Qbar_{0}(a,
W)\right)    \\    &=   \Exp_{P_{0}}\left(\Exp_{P_{0}}\left(\frac{\one\{A    =
a\}}{\ell\Gbar_{0}(a,W)}  \Qbar_{0}(a, W)  \middle|  W \right)  \right) \\&  =
\Exp_{P_{0}}\left(\frac{\ell\Gbar_{0}(a,W)}{\ell\Gbar_{0}(a,W)}   \Qbar_{0}(a,
W)  \middle| W  \right) \\&  =  \Exp_{P_{0}} \left(  \Qbar_{0}(a, W)  \right),
\end{align*}  where the  first, fourth  and sixth  equalities follow  from the
tower  rule^[For  any  random  variable  $(U,V)$  such  that  $\Exp(U|V)$  and
$\Exp(U)$ are well  defined, it holds that $\Exp(\Exp(U|V))  = \Exp(U)$.], and
the second and fifth hold by  definition of the conditional expectation.  This
completes the proof.

## The delta-method {#prop-delta-method}

Let  $f$  be a  map  from  $\Theta \subset  \bbR^{p}$  to  $\bbR^{q}$ that  is
differentiable at $\theta\in \Theta$. Let $X_{n}$ be a random vector taking its
values in $\Theta$.

\BeginKnitrBlock{proposition}<div class="proposition"><span class="proposition" id="prp:unnamed-chunk-7"><strong>(\#prp:unnamed-chunk-7) </strong></span>If $\sqrt{n} (X_{n} - \theta)$ converges in  law to the Gaussian law with mean
$\mu$ and covariance  matrix $\Sigma$, then $\sqrt{n}  (f(X_{n}) - f(\theta))$
converge in  law to the Gaussian  law with mean $\nabla  f(\theta) \times \mu$
and  covariance   matrix  $\nabla   f(\theta)  \times  \Sigma   \times  \nabla
f(\theta)^{\top}$.    In   addition,   if  $\Sigma_{n}$   estimates   $\Sigma$
consistently then,  by Slutsky's lemma,  the asymptotic variance  of $\sqrt{n}
(f(X_{n}) - f(\theta))$ is consistently estimated with $\nabla f(X_{n}) \times
\Sigma_{n} \times \nabla f(X_{n})^{\top}$.</div>\EndKnitrBlock{proposition}


## Asymptotic negligibility of the remainder term {#asymp-neglig-remain}

Recall that  $\|f\|_{P}^{2} \equiv  \Exp_{P} \left(  f(O)^{2} \right)$  is the
$L_2(P)$-norm of  $f$, a measurable  function from $\calO$ to  $\bbR$.  Assume
that  for $a=  0,1$,  $\ell\Gbar_{n}(a,W) \geq  \delta  > 0$  $Q_{0,W}$-almost
everywhere.

The   Cauchy-Schwarz  inequality   then   implies  that,   for   $a  =   0,1$,
\begin{equation*}\Rem_{P_0}(\hat{P}_n)   \le   \frac{2}{\delta}   \max_{a=0,1}
\left(  \|\Qbar_n   (a,\cdot)  -  \Qbar_0  (a,\cdot)\|_{P_0}   \right)  \times
\|\Gbar_n  -   \Gbar_0\|_{P_0}.\end{equation*}  Therefore,  if   for  $a=0,1$,
\begin{equation*}\|\Qbar_n(a,\cdot)      -     \Qbar_0(a,\cdot)\|_{P_0}      =
o_{P_0}(n^{-1/4})\end{equation*}     *and*    \begin{equation*}\|\Gbar_n     -
\Gbar_0\|_{P_0}        =         o_{P_0}(n^{-1/4}),\end{equation*}        then
\begin{equation*}\Rem_{P_0}(\hat{P}_n) = o_{P_0}(n^{-1/2}).\end{equation*}



# References
   
   