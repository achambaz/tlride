

# More results and their proofs {#more-proofs}

*Written too quickly. Checks needed!*

## Estimation of the asymptotic variance of an estimator

### IPTW estimator based on a well-specified model {#iptw-est-var}

__Sketch__ (to extend later on)


The IPTW estimator $\psi_{n}^{b}$ relies on algorithm $\Algo_{\Gbar,1}$, which
is  "well-specified"\index{well/mis-specified} in  the sense  that its  output
$\Gbar_{n}\equiv \Algo_{\Gbar,1}(P_{n})$  minimizes the empirical risk  over a
finite-dimensional,   identifiable,    well-specified   working    model   for
$\Gbar_{0}$. If one introduces $D$ given by

\begin{equation*}
D(O) \equiv \frac{(2A-1)}{\ell\Gbar_{0}(A,W)} Y,
\end{equation*}

then the influence curve of $\psi_{n}^{b}$  equals $D - \Psi(P_{0})$ minus the
projection of  $D$ onto the  tangent space of  the above parametric  model for
$\Gbar_{0}$. The variance of the influence  curve is thus smaller than that of
$D$, hence the conservativeness.


### G-computation estimator based on a well-specified model {#gcomp-est-var}

__Sketch__ (to extend later on, see [@TMLEbook11] page 527)

Consider  a G-computation  estimator $\psi_{n}$  that relies  on an  algorithm
$\Algo_{\Qbar}$  that  is  "well-specified"\index{well/mis-specified}  in  the
sense  that its  output $\Qbar_{n}\equiv  \Algo_{\Qbar}(P_{n})$ minimizes  the
empirical risk over a finite-dimensional, identifiable, well-specified working
model for $\Qbar_{0}$. If one introduces $D$ given by

\begin{equation*}
D(O) \equiv \Qbar_{0}(1,W) - \Qbar_{0}(0,W)
\end{equation*}

then  the influence  curve  of  $\psi_{n}$ equals  $D  -  \Psi(P_{0})$ plus  a
function of $O$ that is orthogonal to $D - \Psi(P_{0})$. Thus the variance of
the   influence   curve   is   larger    than   that   of   $D$,   hence   the
anti-conservativeness.


# References
   
   