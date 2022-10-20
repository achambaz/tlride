
# More results and their proofs {#more-proofs}



## Estimation of the asymptotic variance of an estimator

### IPTW estimator based on a well-specified model {#iptw-est-var}

__Sketch  of proof.__  The IPTW  estimator $\psi_{n}^{b}$  relies on  algorithm
$\Algo_{\Gbar,1}$, which is  "well-specified"\index{well/mis-specified} in the
sense that  its output $\Gbar_{n}\defq \Algo_{\Gbar,1}(P_{n})$  minimizes the
empirical risk over a finite-dimensional, identifiable, well-specified working
model for $\Gbar_{0}$. If one introduces $D$ given by

\begin{equation*}
D(O) \defq \frac{(2A-1)}{\ell\Gbar_{0}(A,W)} Y,
\end{equation*}

then the influence curve of $\psi_{n}^{b}$  equals $D - \Psi(P_{0})$ minus the
projection of  $D$ onto the  tangent space of  the above parametric  model for
$\Gbar_{0}$. The variance of the influence  curve is thus smaller than that of
$D$, hence the conservativeness.


### G-computation estimator based on a well-specified model {#gcomp-est-var}

__Sketch of  proof__ (see  [@TMLEbook11] page  527). Consider  a G-computation
estimator  $\psi_{n}$ that  relies  on an  algorithm  $\Algo_{\Qbar}$ that  is
"well-specified"\index{well/mis-specified}  in  the   sense  that  its  output
$\Qbar_{n}\defq  \Algo_{\Qbar}(P_{n})$ minimizes  the empirical  risk over  a
finite-dimensional,   identifiable,    well-specified   working    model   for
$\Qbar_{0}$. If one introduces $D$ given by

\begin{equation*}
D(O) \defq \Qbar_{0}(1,W) - \Qbar_{0}(0,W)
\end{equation*}

then  the influence  curve  of  $\psi_{n}$ equals  $D  -  \Psi(P_{0})$ plus  a
function of $O$ that is orthogonal to $D - \Psi(P_{0})$. Thus the variance of
the   influence   curve   is   larger    than   that   of   $D$,   hence   the
anti-conservativeness.

## &#9761; \stixdanger{} General analysis of plug-in estimators {#app-analysis-of-plug-in}

Recall that  $\Algo_{Q_{W}}$ is  an algorithm designed  for the  estimation of
$Q_{0,W}$  (see Section  \@ref(nuisance-QW)) and  that we  denote by  $Q_{n,W}
\defq \Algo_{Q_{W}}(P_{n})$ the  output of the algorithm  trained on $P_{n}$.
Likewise,  $\Algo_{\Gbar}$  and  $\Algo_{\Qbar}$ are  two  generic  algorithms
designed for  the estimation of  $\Gbar_{0}$ and of $\Qbar_{0}$  (see Sections
\@ref(nuisance-Gbar)    and     \@ref(nuisance-Qbar)),    $\Gbar_{n}    \defq
\Algo_{\Gbar}(P_{n})$  and $\Qbar_{n}  \defq \Algo_{\Qbar}(P_{n})$  are their
outputs once trained on $P_{n}$.

Let us now introduce $\Phat_n$ a law in $\calM$ such that the $Q_{W}$, $\Gbar$
and   $\Qbar$  features   of  $\Phat_n$   equal  $Q_{n,W}$,   $\Gbar_{n}$  and
$\Qbar_{n}$,  respectively.  We  say that  any such  law is  *compatible* with
$Q_{n,W}$, $\Gbar_n$ and $\Qbar_n$.

### Main analysis {#app-analysis-of-plug-in-main}

Substituting $\Phat_n$ for $P$ in \@ref(eq:taylor-expansion) yields
\@ref(eq:hard-to-study): 
\begin{equation} 
\sqrt{n} (\Psi(\Phat_n) - \Psi(P_0)) =  - \sqrt{n} P_0 D^*(\Phat_n) + \sqrt{n}
\Rem_{P_0}(\Phat_n), 
\end{equation}

an equality that we rewrite as
\begin{align} 
\sqrt{n} (\Psi(\Phat_n) - \Psi(P_0)) = - & \sqrt{n} P_n D^*(\Phat_n) + \sqrt{n}
(P_n - P_0) D^*(P_0)\\ & + \sqrt{n}(P_n - P_0) [D^*(\Phat_n) - D^*(P_0)] +
\sqrt{n}\Rem_{P_0}(\Phat_n). 
\end{align} 

Let  us know  study  in turn  the  four  terms in  the  above right-hand  side
sum. Recall that  $X_n = o_{P_0}(1)$ means that $P_0(|X_n|  > t)$ converges to
zero for all $t>0$ as $n$ goes to infinity.

1. In view of \@ref(eq:rem-two), the fourth term is $o_{P_0}(1)$ provided that
   $\sqrt{n}\|\Qbar     -     \Qbar_{0}\|_{P_0}      \times     \|(\Gbar     -
   \Gbar_{0})/\ell\Gbar_{0}\|_{P_0} =  o_{P_0}(1)$.  This is the  case if, for
   instance,   $\ell\Gbar_{0}$   is  bounded   away   from   zero,  and   both
   $n^{1/4}\|\Qbar    -     \Qbar_{0}\|_{P_0}$    and     $n^{1/4}\|\Gbar    -
   \Gbar_{0}\|_{P_0}$ are  $o_{P_0}(1)$. What  really matters,  remarkably, is
   the  *product* of  the  two norms.   If  each  norm goes  to  zero at  rate
   $n^{1/4}$, then  their product does at  rate $\sqrt{n}$. Of course,  if one
   goes to  zero at rate  $n^{1/4 + c}$ for  some $0<c<1/4$, then  it suffices
   that  the  other go  to  zero  at rate  $n^{1/4  -  c}$. See  also  Section
   \@ref(asymp-neglig-remain). 

2. A  fundamental result from  empirical processes theory gives  us conditions
   guaranteeing  that the  third  term  is $o_{P_0}(1)$.   By  Lemma 19.24  in
   [@vdV98], this is the case indeed if $\|D^*(\Phat_n) - D^*(P_0)\|_{P_{0}} = o_{P_0}
   (1)$ (that is, if $D^*(\Phat_n)$  estimates consistently $D^*(P_0)$) and if
   $D^*(\Phat_n)$ falls (with probability tending to one) into a Donsker class
   (meaning that  the random  $D^*(\Phat_n)$ must belong  eventually to  a set
   that is  not too  large).  Requesting that  $\|D^*(\Phat_n) -  D^*(P_0)\|_{P_{0}} =
   o_{P_0}  (1)$  is  not much  if  one  is  already  willing to  assume  that
   $n^{1/4}\|\Qbar    -     \Qbar_{0}\|_{P_0}$    and     $n^{1/4}\|\Gbar    -
   \Gbar_{0}\|_{P_0}$ are $o_{P_0}(1)$. Moreover,  the second condition can be
   interpreted  as a  condition  on the  complexity/versatility of  algorithms
   $\Algo_{\Gbar}$ and $\Algo_{\Qbar}$. 
   
3. By  the central  limit theorem,  the second  term converges  in law  to the
   centered Gaussian law with variance $P_0 D^*(P_0)^2$.
   
4. As  for the first term,  all we can say  is that it is  a potentially large
   (because of the $\sqrt{n}$ renormalization factor) *bias term*.

### Estimation of the asymptotic variance

Let  us   show  now   that,  under   the  assumptions   we  made   in  Section
\@ref(app-analysis-of-plug-in-main)  and  additional  assumptions  of  similar
nature,  $P_{n}  D^{*}  (\Phat_n)^2$  estimates  consistently  the  asymptotic
variance $P_{0} D^{*}  (P_{0})^2$.  The proof hinges again  on a decomposition
of  the  difference between  the  two  quantities as  a  sum  of three  terms:
\begin{align}  P_{n} D^{*}  (\Phat_n)^2 -  P_{0} D^{*}  (P_{0})^2= &  (P_{n} -
P_{0}) \left(D^{*} (\Phat_n)^2 - D^{*}  (P_{0})^2\right)\\ & + (P_{n} - P_{0})
D^{*}  (P_{0})^2 +  P_{0}  (D^{*} \left(\Phat_n)^2  - D^{*}  (P_{0})^2\right).
\end{align}

We study  the three  terms in  turn. Recall  that $X_n  = o_{P_0}(1/\sqrt{n})$
means that  $P_0(\sqrt{n}|X_n| > t)$  converges to zero  for all $t>0$  as $n$
goes to infinity. 

1. In    light   of   the   study    of   the   third   term    in   Section
   \@ref(app-analysis-of-plug-in-main),      if       $\|D^*(\Phat_n)^2      -
   D^*(P_0)^2\|_{P_{0}} =  o_{P_0} (1)$ and if  $D^*(\Phat_n)^{2}$ falls (with
   probability tending  to one) into a  Donsker class, then the  first term is
   $o_{P_0}(1/\sqrt{n})$.   Furthermore,   if  $D^*(\Phat_n)$   falls   (with
   probability tending  to one) into  a Donsker  class, an assumption  we made
   earlier,   then    so   does    $D^*(\Phat_n)^{2}$.    In    addition,   if
   $\|D^*(\Phat_n) - D^*(P_0)\|_{P_{0}} =  o_{P_0} (1)$, another assumption we
   made   earlier,  and   if  there   exists  a   constant  $c>0$   such  that
   \begin{equation} \sup_{n \geq 1}  \|D^*(\Phat_n) + D^*(P_0)\|_{\infty} \leq
   c (\#eq:bounded-sup-norm)  \end{equation}   $P_{0}$-almost   surely,  then   $\|D^*(\Phat_n)^2   -
   D^*(P_0)^2\|_{P_{0}}   =   o_{P_0}   (1)$  too   because   \begin{equation}
   \|D^*(\Phat_n)^2   -   D^*(P_0)^2\|_{P_{0}}   \leq   c   \|D^*(\Phat_n)   -
   D^*(P_0)\|_{P_{0}}.  \end{equation} The existence of such a constant $c$ is
   granted whenever $\ell\Gbar_{0}$ and  $\ell\Gbar_{n}$ are bounded away from
   zero. Note  that the  condition on  $\ell\Gbar_{n}$ can  be inforced  by us
   through the specification of algorithm $\Algo_{\Gbar}$.

2. By the central limit theorem, $\sqrt{n}$ times the second term converges in
   law  to  the  centered  Gaussian law  with  variance  $\Var_{P_{0}}  (D^{*}
   (P_{0})(O)^2)$, which  is finite  whenever $\ell\Gbar_{0}$ is  bounded away
   from zero.  By Theorem 2.4  in [@vdV98], the  second term is  thus $O_{P_0}
   (1/\sqrt{n})$ hence $o_{P_0} (1)$. 
   
3. Finally, under assumption \@ref(eq:bounded-sup-norm), the absolute value of
   the  third term  is smaller  than \begin{equation}c  P_{0} |D^*(\Phat_n)  -
   D^*(P_0)|     \leq    c     \|D^*(\Phat_n)    -     D^*(P_0)\|_{P_{0}}    =
   o_{P_0}(1),\end{equation}   where    the   inequality   follows    from   the
   Cauchy-Schwarz inequality. 

In conclusion, $P_{n}  D^{*} (\Phat_n)^2 - P_{0} D^{*}  (P_{0})^2 = o_{P_0}(1)$,
hence the result.

## Asymptotic negligibility of the remainder term {#asymp-neglig-remain}

Recall that  $\|f\|_{P}^{2} \defq  \Exp_{P} \left(  f(O)^{2} \right)$  is the
$L_2(P)$-norm of  $f$, a measurable  function from $\calO$ to  $\bbR$.  Assume
that  for $a=  0,1$,  $\ell\Gbar_{n}(a,W) \geq  \delta  > 0$  $Q_{0,W}$-almost
everywhere.

The   Cauchy-Schwarz  inequality   then   implies  that,   for   $a  =   0,1$,
\begin{equation*}\Rem_{P_0}(\Phat_n)   \le   \frac{2}{\delta}   \max_{a=0,1}
\left(  \|\Qbar_n   (a,\cdot)  -  \Qbar_0  (a,\cdot)\|_{P_0}   \right)  \times
\|\Gbar_n  -   \Gbar_0\|_{P_0}.\end{equation*}  Therefore,  if   for  $a=0,1$,
\begin{equation*}\|\Qbar_n(a,\cdot)      -     \Qbar_0(a,\cdot)\|_{P_0}      =
o_{P_0}(n^{-1/4})\end{equation*}     *and*    \begin{equation*}\|\Gbar_n     -
\Gbar_0\|_{P_0}        =         o_{P_0}(n^{-1/4}),\end{equation*}        then
\begin{equation*}\Rem_{P_0}(\Phat_n) = o_{P_0}(n^{-1/2}).\end{equation*}

## Analysis of targeted estimators {#analysis-TMLE}

### A basic fact on the influence curve equation {#basic-eic-eq}

Recall  the  definition of  $D_{1}^{*}$  \@ref(eq:eif).   For *any*  estimator
$\Qbar_n^*$  of  $\Qbar_0$  and  a  law  $P_n^{*}$  that  is  compatible  with
$\Qbar_n^*$ and $Q_{n,W}$, it holds that
\begin{align*}
P_n D_1^*(P_n^*) 
&= \frac{1}{n} \sum_{i=1}^n D_1^{*}(P_n^*)(O_i)\\ 
&=\frac{1}{n}  \sum_{i=1}^n  \left(  \Qbar_n(1,W_i)   -  \Qbar_n(0,W_i)  -  \int
\left(\Qbar_n(1,w) - \Qbar_n(0,w)\right) dQ_{n,W}(w) \right) \\ 
&=   \frac{1}{n}\sum_{i=1}^n \left( \Qbar_n(1,W_i)  - \Qbar_n(0,W_i)\right) - \frac{1}{n}\sum_{i=1}^n
\left(\Qbar_n(1,W_i) - \Qbar_n(0,W_i)\right) = 0.
\end{align*}

### Fluctuation of the regression function along the fluctuation of a law {#fluct-reg}

Let  us  resume  the discussion  where  we  left  it  at the  end  of  Section
\@ref(smooth-second-pass-fluctuations).  Let  $\Qbar$ be the  conditional mean
of $Y$ given $(A,W)$ under $P$. Set  arbitrarily $h \in H \setminus \{0\}$ and
a   measurable   function   $(w,a)   \mapsto   f(a,w)$   taking   non-negative
values. Applying  repeatedly the tower  rule yields the  following equalities:
\begin{align*} \Exp_{P_{h}}  \left(f(A,W) Y\right) &= \Exp_{P}  \left(f(A,W) Y
(1 + h  s(O))\right) \\ &= \Exp_{P} \left(f(A,W) \Exp_{P}\left(Y  (1 + h s(O))
\middle|A,W\right)\right)  \\ &=  \Exp_{P} \left(f(A,W)  \left(\Qbar(A,W) +  h
\Exp_{P}(Ys(O)   |   A,W)   \right)\right)   \\   &=   \Exp_{P}   \left(f(A,W)
\frac{\Qbar(A,W) + h  \Exp_{P}(Ys(O) | A,W)}{1 +  h \Exp_{P}(s(O)|A,W)} \times
\left(1 + h \Exp_{P}(s(O)|A,W)\right)\right).\end{align*} Now, \@ref(eq:fluct)
implies  that  the density  of  $(A,W)$  under  $P_{h}$  equals $\left(1  +  h
\Exp_{P}(s(O)|A,W)\right)$ when  it is  evaluated at $(A,W)$.   Therefore, the
last inequality rewrites as 
\begin{equation*} 
\Exp_{P_{h}}  \left(f(A,W)
Y\right)  = \Exp_{P_{h}}  \left(f(A,W) \frac{\Qbar(A,W)  + h  \Exp_{P}(Ys(O) |
A,W)}{1 + h \Exp_{P}(s(O)|A,W)}\right).
\end{equation*} 
Since this  equality is valid
for an arbitary $(w,a) \mapsto f(a,w)$ with non-negative values, we can deduce
from it  that the  conditional mean  of $Y$ given  $(A,W)$ under  $P_h$ equals
\begin{equation*}\frac{\Qbar(A,W)   +  h   \Exp_{P}(Ys(O)   |   A,W)}{1  +   h
\Exp_{P}(s(O)|A,W)}.\end{equation*}

### Computing the score of a fluctuation of the regression function {#fluct-score}

Let us  resume the  discussion where we  left it at  the beginning  of Section
\@ref(fluct-direct).   Set $\alpha,  \beta \in  \bbR$.  The  derivative of  $h
\mapsto   \expit(\alpha   +   \beta   h)$   evaluated   at   $h=0$   satisfies
\begin{equation*}\frac{d}{dh}  \left.\expit(\alpha +  \beta h)\right|_{h=0}  =
\beta \expit(\alpha)  (1 - \expit(\alpha)).\end{equation*} Therefore,  for any
$(w,a) \in [0,1] \times \{0,1\}$, 
\begin{align*}\frac{d}{dh} \left.\Qbar_{h} (a, w)\right|_{h=0} &= \frac{2a - 1}{\ell\Gbar(a,w)} \expit\left(\logit\left(\Qbar(a,w)\right)\right) \left[1 - \expit\left(\logit\left(\Qbar(a,w)\right)\right)\right]\\&= \frac{2a - 1}{\ell\Gbar(a,w)} \Qbar(a,w) \left(1 - \Qbar(a,w)\right).\end{align*}
This justifies the last but one equality in \@ref(eq:fluct-score).

Furthermore the same derivations that led to \@ref(eq:fluct-score) also imply,
*mutatis mutandis*, that

\begin{equation*}  \frac{d}{d  h} \left.   L_{y}(\Qbar_{h})(O)\right|_{h=0}  =
\frac{2A      -      1}{\ell\Gbar(A,      W)}     \left(Y      -      \Qbar(A,
W)\right). (\#eq:fluct-score-bis)\end{equation*}

In this light,  and in view of \@ref(eq:score), we  can think of $\calQ(\Qbar,
\Gbar)$ as  a fluctuation  of $\Qbar$ in  the direction  of \begin{equation*}(w,a,y)     \mapsto    \frac{2a-1}{\ell\Gbar(a,w)}     (y     -
\Qbar(a,w)).\end{equation*}   Thus   if   $P   \in   \calM$   is   such   that
$\Exp_{P}(Y|A,W) =  \Qbar(A,W)$ and  $P(A=1|W) = \Gbar(W)$,  then we  can also
think of $\calQ(\Qbar, \Gbar)$ as a fluctuation of $\Qbar$ in the direction of
the  second   component  $D_{2}^{*}(P)$  of  the   efficient  influence  curve
$D^{*}(P)$ of $\Psi$ at $P$ \@ref(eq:eif).


   
   
