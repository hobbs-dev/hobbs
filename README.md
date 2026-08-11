# hobbs

### R package for high dimensional Bayesian data analysis

 - Enables **high dimensional** statistical modeling using Bayesian inference.

 - **Probabilistic programming language** for high dimensional problems.

**See more on the website: <https://hobbs.github.io>**

**Visit the community forum: <https://groups.google.com/g/hobbs-users>**

## Installation

**hobbs** requires a C compiler and Rust with Cargo.

**Install Rust: <https://rustup.rs/>**

Windows users also need Rtools: <https://cran.r-project.org/bin/windows/Rtools/>

macOS users also need the Xcode Command Line Tools. Install them by running the following command in Terminal:

```
xcode-select --install
```

Linux users need a C compiler and standard build tools. For example, on Ubuntu or Debian:

```
sudo apt install build-essential
```

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("hobbs/hobbs")
```

Once the package is installed, check your compiler toolchain, build the sampler, and check the sampler:

```r
library(hobbs)
hobbs_check_toolchain()
hobbs_build_sampler()
hobbs_check_sampler()
```

## A first block example: Gaussian linear regression

This example simulates a regression problem with an `x` matrix, estimates the regression coefficients, and estimates the residual standard deviation through `logsigma`.

```r
library(hobbs)

set.seed(1)

n <- 1000L
p <- 5L

x <- matrix(rnorm(n * p), nrow = n, ncol = p)
x <- cbind(1, x)        # first column is the intercept
p <- ncol(x)

beta_true <- c(0.5, 1.0, -0.75, 0.5, 0.0, -0.25)
sigma_true <- 0.75

y <- as.numeric(x %*% beta_true + rnorm(n, 0, sigma_true))

data <- list(
  n = n,
  p = p,
  x = x,
  y = y
)
```

The model below declares two parameter blocks:

- `beta(p)` for the intercept and regression coefficients;
- `logsigma(1)` for the log residual standard deviation.

The block `beta(j)` updates one coefficient at a time. The block `logsigma(1)` updates the residual scale.

The update rule for each block is to **evaluate the prior contribution** for the parameter 
being updated and then evaluate **all direct children** of that parameter. 
In this example, the full likelihood is the only direct child of every parameter.

```r
model_src <- '
param beta(p);
param logsigma(1);

func y_lpdf() {
  double sigma = exp(logsigma(1));

  for (i = 1:n) {
    double mu = 0.0;
    for (j = 1:p) {
      mu += beta(j) * x(i,j);
    }
    y(i) ~ dnorm(mu,sigma);
  }
}

block beta(j) {
  beta(j) ~ dnorm(0,10);
  y_lpdf();
}

block logsigma(1) {
  logsigma(1) ~ dnorm(0,2);
  y_lpdf();
}
'

fit <- hobbs(
  model = model_src,
  data = data,
  samples = 2000,
  burnin = 1000,
  out = "chain_regression.bin"
)

draws <- read_hobbs("chain_regression.bin")
head(draws)
```

Posterior means can be compared with the simulated truth:

```r
beta_cols <- paste0("beta[", seq_len(p), "]")

colMeans(draws[, beta_cols])
beta_true

exp(mean(draws[, "logsigma[1]"]))
sigma_true
```

## Optimizing the regression with a deterministic cache

In the plain model, every update to one coefficient rebuilds the whole linear predictor:

```text
mu_i = beta(1) * x(i,1) + ... + beta(p) * x(i,p)
```

That costs roughly `O(n * p)` per scalar coefficient proposal. For large `p`, this is wasteful because changing `beta(j)` only changes `mu_i` by one rank-one update:

```text
mu_i <- mu_i + (new beta_j - old beta_j) * x(i,j)
```

hobbs supports attached deterministic caches for this pattern. The next model declares a persistent cached vector `mu(n)`, initializes it once, and then updates it whenever `beta(j)` changes.

```r
model_src_cached <- '
param beta(p) save=mean;
param logsigma(1);

func y_lpdf() {
  double sigma = exp(logsigma(1));

  for (i = 1:n) {
    y(i) ~ dnorm(mu(i),sigma);
  }
}

block beta(j) {
  beta(j) ~ dnorm(0,10);
  y_lpdf();
} cache mu(n) {
  for (i = 1:n) {
    for (k = 1:p) {
      mu(i) += beta(k) * x(i,k);
    }
  }
} update mu(n) {
  for (i = 1:n) {
    mu(i) += (proposal(beta(j)) - current(beta(j))) * x(i,j);
  }
}

block logsigma(1) {
  logsigma(1) ~ dnorm(0,2);
  y_lpdf();
}
'

fit_cached <- hobbs(
  model = model_src_cached,
  data = data,
  samples = 2000,
  burnin = 1000,
  out = "chain_regression_cached.bin"
)

draws <- read_hobbs("chain_regression_cached.bin")
draws_mean <- read_hobbs("chain_regression_cached.mean.bin")
```

The cached model is mathematically the same model as the plain version. The difference is computational: `mu(n)` is maintained incrementally instead of being rebuilt from all `p` predictors after every scalar coefficient proposal.

The important cache rules are:

- `cache mu(n) { ... }` declares and initializes the persistent cached vector;
- `update mu(n) { ... }` tells hobbs how to update that cache for the attached block;
- `proposal(beta(j))` is the proposed scalar value being evaluated;
- `current(beta(j))` is the currently accepted scalar value;
- if a proposal is rejected, hobbs restores the cache automatically.

This optimization is most useful when `p` is large and parameters are updated one at a time. For small regressions, the plain model may already be fast enough.

## Next steps

The same block syntax can be used for discrete parameters, sparse variable-selection models, random effects, and other models where only part of the likelihood changes for each parameter update. See <https://hobbs.github.io> for more examples and reference material.

## Automatic and declared deterministic correlation switches

hobbs can augment the ordinary scalar random-walk Metropolis-Hastings sweep
with deterministic, Metropolis-corrected moves for correlated continuous
parameters:

```r
fit_switch <- hobbs(
  model = model_src_cached,
  data = data,
  samples = 5000,
  warmups = 2000,
  switch = TRUE,
  out = "chain_switch.bin"
)
```

hobbs now fits posterior variance-covariance geometry **once**, rather than
refreshing it online. The first 40% of adaptive warmup is scalar-only. The
first half of that stage is discarded as settling, and moments are accumulated
over the second half. At 40% of adaptive warmup, hobbs fits covariance-shaped
scalar scales and switch geometry once. It then freezes that geometry while the
ordinary scalar proposal factors continue adapting toward the default 0.44
target for the remaining 60% of adaptive warmup.

For automatic switches, hobbs greedily selects the remaining pair with the
largest absolute correlation, keeps it only when
`abs(correlation) > switch_threshold`, removes both coordinates, and repeats.
For standardized centered coordinates `z_j` and `z_k`, negative correlations
use

```text
(z_j, z_k) -> (z_k, z_j)
```

and positive correlations use

```text
(z_j, z_k) -> (-z_k, -z_j).
```

Each transformation is self-inverse and has absolute Jacobian determinant one,
so hobbs applies the exact Metropolis posterior-ratio correction.

### Declared switch cycles

A model can add any number of explicit switch pairs with top-level `switch`
blocks. For example, this declaration cycles `beta(1)` through the first random
effect from every group:

```r
model_src <- '
param beta(p);
param u(m,2);

switch {
  for (i = 1:m) {
    beta(1) ~ u(i,1)
  }
}

/* model functions and scalar blocks follow */
'
```

Nested loops and multiple `switch { ... }` declarations are allowed. Explicit
pairs are expanded in source order, may overlap, and do not need to exceed the
automatic correlation threshold. Their fixed warmup correlation sign still
chooses the ordinary or signed centered switch. Every declared pair is a
separate exact MH kernel. hobbs applies all declared pairs sequentially after
every scalar sweep, then applies any non-duplicate automatic greedy pairs.
This fixed composition remains a valid Markov transition even when a coordinate
such as `beta(1)` appears in many successive pairs. Because every declared edge
is attempted once per sweep, a very long cycle involving a globally evaluated
parameter can add substantial runtime; the switch diagnostics make that tradeoff
visible during testing.

### Derived switch coordinates

A named switch coordinate can summarize many ordinary parameters without
becoming another sampled parameter. The declaration has a getter followed by an
attached update that explains how a proposed scalar value changes the underlying
state:

```r
model_src <- '
param beta(1);
param u(m,1);

switch mn(1) {
  for (i = 1:m) {
    mn(1) += u(i,1);
  }
  mn(1) /= m;
  beta(1) ~ mn(1)
} update mn(1) {
  double delta = proposal(mn(1)) - current(mn(1));
  for (i = 1:m) {
    u(i,1) += delta;
  }
}

/* scalar blocks follow */
'
```

Here `mn(1)` is the current mean of `u(1:m,1)`. During the fixed warmup
training window, hobbs tracks the mean, variance, and covariance of `mn(1)` and
`beta(1)`. When the centered standardized pair switch proposes a new value for
`mn(1)`, the attached update shifts every random effect by

```text
proposal(mn(1)) - current(mn(1)).
```

This realizes the proposed mean while preserving every deviation
`u(i,1) - mn(1)`. hobbs then evaluates the complete transformed state and makes
one joint MH decision. In block mode the changed ordinary coordinates are
staged transactionally, and all parameter and cache changes are restored on
rejection.

The getter starts the derived coordinate at zero, so the body may build a sum
with `+=`. For a mean, divide by `m` in the getter and add the full mean delta to
each underlying value. For a sum, omit the division and add
`(proposal(sum)-current(sum))/m` to each underlying value.

The attached update is part of the mathematical proposal. It must be
deterministic, reversible, and volume preserving. Equal additive shifts of a
mean or sum satisfy these requirements. hobbs also checks at runtime that the
setter actually realizes the proposed derived value. Named coordinates are
trained sparsely and do not count toward `switch_max_dim` or appear in the saved
chain.

For large models, `switch_max_dim` limits only the dense automatic correlation
search. When the continuous dimension exceeds that limit, automatic greedy
pairing is skipped, while declared pairs are still trained sparsely. Each
distinct declared coordinate gets one mean/variance accumulator and each edge
gets one cross-moment, giving O(V + E) storage for V involved coordinates and E
declared pairs. The expanded pairs are passed to the sampler through a temporary
pair file, avoiding command-line length limits for large cycles.

Switch acceptances are **not** included in scalar 0.44 tuning. The returned run
object records the expanded declarations in `switch_declared_pairs`, and the
sampler diagnostics identify each final pair as `declared` or `greedy`:

```r
fit_switch$switch_declared_pairs
switch_info <- read.csv(fit_switch$switch_diagnostics)
switch_info
```
