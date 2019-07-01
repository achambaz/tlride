
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

- $P_{n}$, the empirical measure. If the observations are $O_{1}$, \ldots,
$O_{n}$, then $P_{n}$ is a law such that the generic random variable $O$ drawn
from $P_{n}$ takes its values in $\{O_{1}, \ldots, O_{n}\}$ in such a way that
$O = O_{i}$ with probability $n^{-1}$ for each $1 \leq i \leq n$.

- $\sqrt{n} (P_{n} - P)$, where $P_{n}$ is the empirical measure associated to
$O_{1}, \ldots, O_{n}$ drawn independently from $P$, the empirical process.

- $\calM$, the model, that is, the collection of *all* laws from which $O$
can be drawn and that meet some constraints.

- $\calM^{\text{empirical}}$,  the collection of  all discrete laws  on $[0,1]
  \times \{0,1\} \times [0,1]$, of which $P_{n}$ is a distinguished element.

- $Q_{W}$, $Q_{0,W}$, marginal laws for $W$ (under $P$ and $P_{0}$,
respectively).
  
- $\Gbar(W) \equiv \Pr_{P}(A  = 1 | W)$, $\Gbar_0(W) \equiv  \Pr_{P_0}(A = 1 |
W)$, conditional  probabilities of  action $A  = 1$ given  $W$ (under  $P$ and
$P_{0}$,  respectively).  For  each  $a \in  \{0,1\}$, $\ell\Gbar(a,W)  \equiv
\Pr_{P}(A = a | W)$ and $\ell\Gbar_0(a,W) \equiv \Pr_{P_0}(A = a | W)$.

- $\Qbar(A,W)  = \Exp_{P}(Y|A,W)$, $\Qbar_0(A,W) =  \Exp_{P_{0}}(Y|A,W)$, the
conditional  means  of  $Y$  given  $A$   and  $W$  (under  $P$  and  $P_{0}$,
respectively).

- $q_{Y}$, $q_{0,Y}$, conditional densities of $Y$ given $A$ and $W$ (under
$P$ and $P_{0}$, respectively).

- $\Psi : \calM \to [0,1]$, given  by $\Psi(P) \equiv \int \left(\Qbar(1, w) -
\Qbar(0, w)\right)dQ_{W}(w)$, the statistical mapping of interest.
  
- $\psi \equiv \Psi(P)$, $\psi_{0} \equiv \Psi(P_{0})$.

- $\Algo$,  $\Algo_{\Gbar,1}$, $\Algo_{\Qbar,1}$, algorithms to  be trained on
  $P_{n}$, *i.e.*,  mappings from $\calM^{\text{empirical}}$ to  the set where
  lives   the feature targeted by the algorithm.

