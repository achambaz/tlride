<<<<<<< HEAD

# Notation {#notation}

- $O = (W,A,Y)$, the generic summary of how one realization of the experiments
of interest unfold,  our generic observation; $W \in [0,1]$  is the context of
action, $A  \in \{0,1\}$  is the  action undertaken, and  $Y\in [0,1]$  is the
reward of action  $A$ in context $W$.  We denote by $\bbO  \equiv [0,1] \times
\{0,1\} \times [0,1]$ the set where a generic $O$ takes its values.

- $P$, $P_{0}$, $\Pi_{0}$, $\Pi_{h}$, $\Pi_{0}'$, $\Pi_{h}'$, laws (on $\bbO$)
  for $O$.

- $Pf \equiv  \Exp_{P} (f(O))$ for any  law $P$ for $O$ and  function $f$ from
$\bbO$ to $\bbR^{p}$.

- $\|f\|_{P}^{2} \equiv  Pf^{2} = \Exp_{P} (f(O)^{2}) =  \int f(o)^{2} dP(o)$,
the square of the $L^{2}(P)$-norm of $f$, a function from $\bbO$ to $\bbR$.
=======
# Notation {#notation}

- $\defq$, equal by definition to

- $\one\{S\}$, the indicator of statement $S$, which equals 1 if $S$ is true
  and 0 otherwise.

- $O \defq (W,A,Y)$, the generic summary of how one realization of the experiments
of interest unfold,  our generic observation; $W \in [0,1]$  is the context of
action, $A  \in \{0,1\}$  is the  action undertaken, and  $Y\in [0,1]$  is the
reward of action  $A$ in context $W$.  We denote by $\calO  \defq [0,1] \times
\{0,1\} \times [0,1]$ the set where a generic $O$ takes its values.

- $P$, $P_{0}$, $\Pi_{0}$, $\Pi_{h}$, $\Pi_{0}'$, $\Pi_{h}'$, laws (on $\calO$)
  for $O$.

- $Pf \defq  \Exp_{P} (f(O))$ for any  law $P$ for $O$ and  function $f$ from
$\calO$ to $\bbR^{p}$.

- $\|f\|_{P}^{2} \defq  Pf^{2} = \Exp_{P} (f(O)^{2}) =  \int f(o)^{2} dP(o)$,
the square of the $L^{2}(P)$-norm of $f$, a function from $\calO$ to $\bbR$.

- $\|f\|_{\infty} \defq \sup_{o \in  \calO} |f(o)|$, the essential supremum of
$f$, a function from $\calO$ to $\bbR$. 
>>>>>>> develop

- $P_{n}$, the empirical measure. If the observations are $O_{1}$, \ldots,
$O_{n}$, then $P_{n}$ is a law such that the generic random variable $O$ drawn
from $P_{n}$ takes its values in $\{O_{1}, \ldots, O_{n}\}$ in such a way that
$O = O_{i}$ with probability $n^{-1}$ for each $1 \leq i \leq n$.

- $\sqrt{n} (P_{n} - P)$, where $P_{n}$ is the empirical measure associated to
$O_{1}, \ldots, O_{n}$ drawn independently from $P$, the empirical process.

<<<<<<< HEAD
=======
- $X_n = o_{P_0}(1)$ if $X_n$, a random variable built from $O_{1}$, \ldots,
$O_{n}$ independently drawn from $P_0$, converges in probability to zero, that
is,  if $P_0(|X_n|  > t)$  converges to  zero  for all  $t>0$ as  $n$ goes  to
infinity.  If  $n^{c}  X_n  =  o_{P_0}(1)$,   then  one  also  writes  $X_n  =
o_{P_0}(n^{-c})$. 

- $X_{n} = O_{P_0}(1)$ if $X_n$, a random variable built from $O_{1}$, \ldots,
$O_{n}$ independently drawn from $P_0$, is bounded in probability, that
is if, for all $t>0$ there exists  $M >0$ such that $\sup_{n \geq 1} P_0(|X_n|
\geq M) \leq t$. If  $n^{c}  X_n  =  O_{P_0}(1)$,   then  one  also  writes  $X_n  =
O_{P_0}(n^{-c})$. 

>>>>>>> develop
- $\calM$, the model, that is, the collection of *all* laws from which $O$
can be drawn and that meet some constraints.

- $\calM^{\text{empirical}}$,  the collection of  all discrete laws  on $[0,1]
  \times \{0,1\} \times [0,1]$, of which $P_{n}$ is a distinguished element.

- $Q_{W}$, $Q_{0,W}$, marginal laws for $W$ (under $P$ and $P_{0}$,
respectively).
  
<<<<<<< HEAD
- $\Gbar(W) \equiv \Pr_{P}(A  = 1 | W)$, $\Gbar_0(W) \equiv  \Pr_{P_0}(A = 1 |
W)$, conditional  probabilities of  action $A  = 1$ given  $W$ (under  $P$ and
$P_{0}$,  respectively).  For  each  $a \in  \{0,1\}$, $\ell\Gbar(a,W)  \equiv
\Pr_{P}(A = a | W)$ and $\ell\Gbar_0(a,W) \equiv \Pr_{P_0}(A = a | W)$.
=======
- $\Gbar(W) \defq \Pr_{P}(A  = 1 | W)$, $\Gbar_0(W) \defq  \Pr_{P_0}(A = 1 |
W)$, conditional  probabilities of  action $A  = 1$ given  $W$ (under  $P$ and
$P_{0}$,  respectively).  For  each  $a \in  \{0,1\}$, $\ell\Gbar(a,W)  \defq
\Pr_{P}(A = a | W)$ and $\ell\Gbar_0(a,W) \defq \Pr_{P_0}(A = a | W)$.
>>>>>>> develop

- $\Qbar(A,W)  = \Exp_{P}(Y|A,W)$, $\Qbar_0(A,W) =  \Exp_{P_{0}}(Y|A,W)$, the
conditional  means  of  $Y$  given  $A$   and  $W$  (under  $P$  and  $P_{0}$,
respectively).

<<<<<<< HEAD
- $q_{Y}$, $q_{0,Y}$, conditional densities of $Y$ given $A$ and $W$ (under
$P$ and $P_{0}$, respectively).

- $\Psi : \calM \to [0,1]$, given  by $\Psi(P) \equiv \int \left(\Qbar(1, w) -
\Qbar(0, w)\right)dQ_{W}(w)$, the statistical mapping of interest.
  
- $\psi \equiv \Psi(P)$, $\psi_{0} \equiv \Psi(P_{0})$.
=======
- $\calQ  \defq \{\Qbar  :  P  \in  \calM\}$,  the space  of  regression
  functions induced   by model $\calM$.

- $\calQ(\Qbar,\Gbar)  \subset \calQ$,  $\Gbar$-specific fluctuation  model of
  $\Qbar$, see \@ref(eq:Q-fluct).

- $q_{Y}$, $q_{0,Y}$, conditional densities of $Y$ given $A$ and $W$ (under
$P$ and $P_{0}$, respectively).

- $\Psi : \calM \to [0,1]$, given  by $\Psi(P) \defq \int \left(\Qbar(1, w) -
\Qbar(0, w)\right)dQ_{W}(w)$, the statistical mapping of interest.
  
- $\psi \defq \Psi(P)$, $\psi_{0} \defq \Psi(P_{0})$.
>>>>>>> develop

- $\Algo$,  $\Algo_{\Gbar,1}$, $\Algo_{\Qbar,1}$, algorithms to  be trained on
  $P_{n}$, *i.e.*,  mappings from $\calM^{\text{empirical}}$ to  the set where
  lives   the feature targeted by the algorithm.

<<<<<<< HEAD

=======
- $\Algora_{\Gbar,s}$,  $\Algora_{\Qbar,s}$, $s$-specific  oracle algorithms
  ($s>0$) that can use the  true targeted features $\Gbar_{0}$ and $\Qbar_{0}$
  to produce  predictions that are almost  exact, up to a  $N(0,s^{2})$ random
  error term.

- $L_{a}$, the contex-specific logistic  (or negative binomial) loss function,
  given by $-L_{a}(f)(A,W)  \defq A \log f(W) + (1-A)  \log (1-f(W))$ for
  any function $f:[0,1]\to[0,1]$.

- $L_{y}$, the reward-specific logistic  (or negative binomial) loss function,
  given by $-L_{y}(f)(O) \defq Y \log f(A,W) + (1-Y) \log (1-f(A,W))$ for 
  any function $f:\{0,1\} \times [0,1] \to[0,1]$. 
>>>>>>> develop
