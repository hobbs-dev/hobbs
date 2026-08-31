#ifndef hobbs_MODEL_H
#define hobbs_MODEL_H

#include <float.h>
#include <math.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#include <malloc.h>
#define hobbs_EXPORT __declspec(dllexport)
#if defined(_MSC_VER)
#define hobbs_RESTRICT __restrict
#define hobbs_ALWAYS_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#define hobbs_RESTRICT __restrict__
#define hobbs_ALWAYS_INLINE inline __attribute__((always_inline))
#else
#define hobbs_RESTRICT restrict
#define hobbs_ALWAYS_INLINE inline
#endif
#elif defined(__GNUC__) || defined(__clang__)
#define hobbs_EXPORT __attribute__((visibility("default")))
#define hobbs_RESTRICT __restrict__
#define hobbs_ALWAYS_INLINE inline __attribute__((always_inline))
#else
#define hobbs_EXPORT
#define hobbs_RESTRICT restrict
#define hobbs_ALWAYS_INLINE inline
#endif

#ifndef hobbs_CACHELINE_BYTES
#define hobbs_CACHELINE_BYTES 64u
#endif

/*
 * Aligned native storage used by generated data and cache arrays.  The C
 * compiler can then safely vectorize row-wise kernels without paying for
 * unknown-alignment peel loops.  The rounded C11 allocation size is required
 * to be an exact multiple of the requested alignment.
 */
static hobbs_ALWAYS_INLINE void *hobbs_aligned_malloc(size_t bytes) {
    if (bytes == 0u) bytes = 1u;
#if defined(_WIN32)
    return _aligned_malloc(bytes, (size_t)hobbs_CACHELINE_BYTES);
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__APPLE__)
    const size_t alignment = (size_t)hobbs_CACHELINE_BYTES;
    if (bytes > SIZE_MAX - (alignment - 1u)) return NULL;
    const size_t rounded = (bytes + alignment - 1u) & ~(alignment - 1u);
    return aligned_alloc(alignment, rounded);
#else
    void *ptr = NULL;
    if (posix_memalign(&ptr, (size_t)hobbs_CACHELINE_BYTES, bytes) != 0) return NULL;
    return ptr;
#endif
}

static hobbs_ALWAYS_INLINE void *hobbs_aligned_calloc(size_t count, size_t size) {
    if (size != 0u && count > SIZE_MAX / size) return NULL;
    const size_t bytes = count * size;
    void *ptr = hobbs_aligned_malloc(bytes);
    if (ptr != NULL) memset(ptr, 0, bytes);
    return ptr;
}

static hobbs_ALWAYS_INLINE void hobbs_aligned_free(void *ptr) {
#if defined(_WIN32)
    _aligned_free(ptr);
#else
    free(ptr);
#endif
}

#ifndef M_PI
#define M_PI 3.14159265358979323846264338327950288
#endif
#ifndef M_SQRT2
#define M_SQRT2 1.41421356237309504880168872420969808
#endif

/*
 * hobbs C model helper API
 *
 * Users may write only:
 *   double log_posterior(const double *theta, int dim) { ... }
 *
 * The R wrapper will expose this to the sampler as posterior_logp().  Advanced
 * users may still define posterior_logp(), posterior_logp_batch(),
 * posterior_init(), or posterior_free() themselves for full control.
 */

#ifdef hobbs_ENABLE_LOG_CACHE
/*
 * Approximate log(x) with a precomputed mantissa lookup table.
 *
 * Hot path design: no frexp(), no interpolation, no floating point index math.
 * For a positive normal double we read the IEEE-754 bits, extract the exponent
 * and the top hobbs_LOG_CACHE_BITS mantissa bits, round to the nearest table
 * cell using the next mantissa bit, and return log(m) + e * log(2).
 *
 * This is an approximation. Larger hobbs_LOG_CACHE_BITS improves accuracy but
 * can be slower if the table stops fitting cache well.
 */
#ifndef hobbs_LOG_CACHE_BITS
#define hobbs_LOG_CACHE_BITS 20
#endif
#if hobbs_LOG_CACHE_BITS < 8
#error hobbs_LOG_CACHE_BITS must be at least 8
#endif
#if hobbs_LOG_CACHE_BITS > 28
#error hobbs_LOG_CACHE_BITS must be at most 28
#endif
#define hobbs_LOG_CACHE_SIZE ((size_t)1u << hobbs_LOG_CACHE_BITS)
#define hobbs_LOG_CACHE_LN2 0.693147180559945309417232121458176568

static double *hobbs_log_lookup_table = NULL;
static int hobbs_log_lookup_ready = 0;

static inline void hobbs_log_lookup_init(void) {
    if (hobbs_log_lookup_ready) return;
    hobbs_log_lookup_table = (double *)malloc(hobbs_LOG_CACHE_SIZE * sizeof(double));
    if (hobbs_log_lookup_table == NULL) {
        hobbs_log_lookup_ready = -1;
        return;
    }

    const double inv_n = 1.0 / (double)hobbs_LOG_CACHE_SIZE;
    for (size_t i = 0; i < hobbs_LOG_CACHE_SIZE; ++i) {
        /* Table cells are centered in [0.5, 1). */
        const double m = 0.5 + 0.5 * (((double)i + 0.5) * inv_n);
        hobbs_log_lookup_table[i] = log(m);
    }
    hobbs_log_lookup_ready = 1;
}

static inline double hobbs_lookup_log(double x) {
    union { double d; uint64_t u; } v;
    v.d = x;

    const uint64_t exp_bits = (v.u >> 52) & 0x7ffULL;
    if (exp_bits == 0ULL || exp_bits == 0x7ffULL || x <= 0.0) return log(x);

    hobbs_log_lookup_init();
    if (hobbs_log_lookup_ready != 1) return log(x);

    const int exponent = (int)exp_bits - 1022; /* because mantissa is in [0.5, 1) */
    const uint64_t mant = v.u & 0x000fffffffffffffULL;

#if hobbs_LOG_CACHE_BITS >= 52
    size_t idx = (size_t)mant;
#else
    const int shift = 52 - hobbs_LOG_CACHE_BITS;
    size_t idx = (size_t)(mant >> shift);
    if (shift > 0) {
        const uint64_t round_bit = (mant >> (shift - 1)) & 1ULL;
        idx += (size_t)round_bit;
        if (idx >= hobbs_LOG_CACHE_SIZE) idx = hobbs_LOG_CACHE_SIZE - 1u;
    }
#endif

    return hobbs_log_lookup_table[idx] + ((double)exponent) * hobbs_LOG_CACHE_LN2;
}
#define log(x) hobbs_lookup_log((x))
#endif



/*
 * Cached integer log-factorials and log-choose.
 *
 * Many discrete likelihoods repeatedly evaluate lgamma(y + 1) or
 * lgamma(n + 1) - lgamma(y + 1) - lgamma(n - y + 1) for observed integer
 * data.  These are data-only terms in Poisson/binomial-like models, so a
 * small reusable cache is usually more valuable than caching an entire
 * distribution.  The table is allocated lazily and only up to the largest
 * integer requested, capped by hobbs_LFACTORIAL_CACHE_MAX.
 */
#ifndef hobbs_LFACTORIAL_CACHE_MAX
#define hobbs_LFACTORIAL_CACHE_MAX 1000000u
#endif

static double *hobbs_lfactorial_table = NULL;
static unsigned int hobbs_lfactorial_filled = 0u;
static int hobbs_lfactorial_ready = 0;

static inline void hobbs_lfactorial_init(void) {
    if (hobbs_lfactorial_ready) return;
    hobbs_lfactorial_table = (double *)malloc(((size_t)hobbs_LFACTORIAL_CACHE_MAX + 1u) * sizeof(double));
    if (hobbs_lfactorial_table == NULL) {
        hobbs_lfactorial_ready = -1;
        return;
    }
    hobbs_lfactorial_table[0] = 0.0;
    hobbs_lfactorial_filled = 0u;
    hobbs_lfactorial_ready = 1;
}

static inline double hobbs_lfactorial(unsigned int n) {
    if (n > hobbs_LFACTORIAL_CACHE_MAX) return lgamma((double)n + 1.0);
    hobbs_lfactorial_init();
    if (hobbs_lfactorial_ready != 1) return lgamma((double)n + 1.0);

    while (hobbs_lfactorial_filled < n) {
        const unsigned int next = hobbs_lfactorial_filled + 1u;
        hobbs_lfactorial_table[next] = hobbs_lfactorial_table[hobbs_lfactorial_filled] + log((double)next);
        hobbs_lfactorial_filled = next;
    }
    return hobbs_lfactorial_table[n];
}

static inline double hobbs_log_choose(unsigned int n, unsigned int k) {
    if (k > n) return -INFINITY;
    if (k > n - k) k = n - k;
    return hobbs_lfactorial(n) - hobbs_lfactorial(k) - hobbs_lfactorial(n - k);
}

static inline double dnorm_log(double x, double mean, double sd) {
    if (!(sd > 0.0)) return -INFINITY;
    const double z = (x - mean) / sd;
    return -0.5 * z * z - log(sd) - 0.5 * log(2.0 * M_PI);
}

static inline double dunif_log(double x, double min, double max) {
    if (!(max > min) || x < min || x > max) return -INFINITY;
    return -log(max - min);
}

static inline double dexp_log(double x, double rate) {
    if (!(rate > 0.0) || x < 0.0) return -INFINITY;
    return log(rate) - rate * x;
}

static inline double dgamma_log(double x, double shape, double rate) {
    if (!(shape > 0.0) || !(rate > 0.0) || x < 0.0) return -INFINITY;
    if (x == 0.0) {
        if (shape == 1.0) return log(rate);
        return (shape < 1.0) ? INFINITY : -INFINITY;
    }
    return shape * log(rate) - lgamma(shape) + (shape - 1.0) * log(x) - rate * x;
}

static inline double dinvgamma_log(double x, double shape, double rate) {
    if (!(shape > 0.0) || !(rate > 0.0) || x <= 0.0) return -INFINITY;
    return shape * log(rate) - lgamma(shape) - (shape + 1.0) * log(x) - rate / x;
}

static inline double dbeta_log(double x, double a, double b) {
    if (!(a > 0.0) || !(b > 0.0) || x < 0.0 || x > 1.0) return -INFINITY;
    if (x == 0.0) return (a < 1.0) ? INFINITY : ((a == 1.0) ? lgamma(a + b) - lgamma(a) - lgamma(b) : -INFINITY);
    if (x == 1.0) return (b < 1.0) ? INFINITY : ((b == 1.0) ? lgamma(a + b) - lgamma(a) - lgamma(b) : -INFINITY);
    return lgamma(a + b) - lgamma(a) - lgamma(b) + (a - 1.0) * log(x) + (b - 1.0) * log1p(-x);
}

static inline double dcauchy_log(double x, double location, double scale) {
    if (!(scale > 0.0)) return -INFINITY;
    const double z = (x - location) / scale;
    return -log(M_PI) - log(scale) - log1p(z * z);
}

static inline double dt_log(double x, double df, double location, double scale) {
    if (!(df > 0.0) || !(scale > 0.0)) return -INFINITY;
    const double z = (x - location) / scale;
    return lgamma((df + 1.0) / 2.0) - lgamma(df / 2.0) - 0.5 * log(df * M_PI) - log(scale)
        - ((df + 1.0) / 2.0) * log1p((z * z) / df);
}

static inline double dchisq_log(double x, double df) {
    return dgamma_log(x, df / 2.0, 0.5);
}

static inline double dlnorm_log(double x, double meanlog, double sdlog) {
    if (!(sdlog > 0.0) || x <= 0.0) return -INFINITY;
    const double lx = log(x);
    const double z = (lx - meanlog) / sdlog;
    return -0.5 * z * z - lx - log(sdlog) - 0.5 * log(2.0 * M_PI);
}

static inline double dbern_log(unsigned int x, double prob) {
    if (prob < 0.0 || prob > 1.0 || x > 1U) return -INFINITY;
    if (x == 1U) return (prob == 0.0) ? -INFINITY : log(prob);
    return (prob == 1.0) ? -INFINITY : log1p(-prob);
}

#ifdef hobbs_ENABLE_LOG_CACHE
/*
 * Cached logistic parts + cached Bernoulli-logit whole distribution.
 *
 * This is a true whole-distribution cache for the stock Bernoulli-logit
 * kernel, with the reusable parts exposed as views into the same table.
 *
 * The table is interleaved by eta cell for locality:
 *   table[2 * idx + 0] = bernoulli_logit_lpdf(0, eta_idx)
 *                      = log(1 - sigmoid(eta_idx))
 *   table[2 * idx + 1] = bernoulli_logit_lpdf(1, eta_idx)
 *                      = log(sigmoid(eta_idx))
 *
 * Therefore:
 *   bernoulli_logit_lpdf(y, eta) is a single index plus y selection;
 *   log_sigmoid_lpdf(eta) and log1m_sigmoid_lpdf(eta) reuse the same table.
 */
#ifndef hobbs_LOGIT_CACHE_BITS
#define hobbs_LOGIT_CACHE_BITS hobbs_LOG_CACHE_BITS
#endif
#ifndef hobbs_LOGIT_CACHE_LIMIT
#define hobbs_LOGIT_CACHE_LIMIT 12.0
#endif
#define hobbs_LOGIT_CACHE_SIZE ((size_t)1u << hobbs_LOGIT_CACHE_BITS)

static double *hobbs_bernoulli_logit_table = NULL;  /* length 2 * N, interleaved by eta */
static int hobbs_bernoulli_logit_ready = 0;

static inline double hobbs_log_sigmoid_exact(double eta) {
    if (eta >= 0.0) return -log1p(exp(-eta));
    return eta - log1p(exp(eta));
}

static inline double hobbs_log1m_sigmoid_exact(double eta) {
    if (eta >= 0.0) return -eta - log1p(exp(-eta));
    return -log1p(exp(eta));
}

static inline double hobbs_logit_grid_eta(size_t idx) {
    const double lo = -hobbs_LOGIT_CACHE_LIMIT;
    const double width = 2.0 * hobbs_LOGIT_CACHE_LIMIT;
    const double inv_n = 1.0 / (double)hobbs_LOGIT_CACHE_SIZE;
    return lo + width * (((double)idx + 0.5) * inv_n);
}

static inline size_t hobbs_logit_index(double eta) {
    const double width = 2.0 * hobbs_LOGIT_CACHE_LIMIT;
    double pos = (eta + hobbs_LOGIT_CACHE_LIMIT) * ((double)hobbs_LOGIT_CACHE_SIZE / width);
    size_t idx = (size_t)pos;
    if (idx >= hobbs_LOGIT_CACHE_SIZE) idx = hobbs_LOGIT_CACHE_SIZE - 1u;
    return idx;
}

static inline void hobbs_bernoulli_logit_init(void) {
    if (hobbs_bernoulli_logit_ready) return;

    hobbs_bernoulli_logit_table = (double *)malloc(2u * hobbs_LOGIT_CACHE_SIZE * sizeof(double));
    if (hobbs_bernoulli_logit_table == NULL) {
        hobbs_bernoulli_logit_ready = -1;
        return;
    }

    for (size_t i = 0; i < hobbs_LOGIT_CACHE_SIZE; ++i) {
        const double eta = hobbs_logit_grid_eta(i);
        const size_t base = i << 1;
        hobbs_bernoulli_logit_table[base + 0u] = hobbs_log1m_sigmoid_exact(eta);
        hobbs_bernoulli_logit_table[base + 1u] = hobbs_log_sigmoid_exact(eta);
    }
    hobbs_bernoulli_logit_ready = 1;
}

static inline double hobbs_log_sigmoid(double eta) {
    if (!isfinite(eta)) return -INFINITY;
    if (eta <= -hobbs_LOGIT_CACHE_LIMIT) return eta;
    if (eta >=  hobbs_LOGIT_CACHE_LIMIT) return 0.0;

    hobbs_bernoulli_logit_init();
    if (hobbs_bernoulli_logit_ready != 1) return hobbs_log_sigmoid_exact(eta);

    const size_t idx = hobbs_logit_index(eta);
    return hobbs_bernoulli_logit_table[(idx << 1) + 1u];
}

static inline double hobbs_log1m_sigmoid(double eta) {
    if (!isfinite(eta)) return -INFINITY;
    if (eta <= -hobbs_LOGIT_CACHE_LIMIT) return 0.0;
    if (eta >=  hobbs_LOGIT_CACHE_LIMIT) return -eta;

    hobbs_bernoulli_logit_init();
    if (hobbs_bernoulli_logit_ready != 1) return hobbs_log1m_sigmoid_exact(eta);

    const size_t idx = hobbs_logit_index(eta);
    return hobbs_bernoulli_logit_table[idx << 1];
}

static inline double dbern_logit(unsigned int x, double eta) {
    if (x > 1U || !isfinite(eta)) return -INFINITY;

    if (eta <= -hobbs_LOGIT_CACHE_LIMIT) return x ? eta : 0.0;
    if (eta >=  hobbs_LOGIT_CACHE_LIMIT) return x ? 0.0 : -eta;

    hobbs_bernoulli_logit_init();
    if (hobbs_bernoulli_logit_ready != 1) {
        return x ? hobbs_log_sigmoid_exact(eta) : hobbs_log1m_sigmoid_exact(eta);
    }

    const size_t idx = hobbs_logit_index(eta);
    return hobbs_bernoulli_logit_table[(idx << 1) + (size_t)x];
}
#else
static inline double hobbs_log_sigmoid(double eta) {
    if (!isfinite(eta)) return -INFINITY;
    if (eta >= 0.0) return -log1p(exp(-eta));
    return eta - log1p(exp(eta));
}

static inline double hobbs_log1m_sigmoid(double eta) {
    if (!isfinite(eta)) return -INFINITY;
    if (eta >= 0.0) return -eta - log1p(exp(-eta));
    return -log1p(exp(eta));
}

static inline double dbern_logit(unsigned int x, double eta) {
    if (x > 1U || !isfinite(eta)) return -INFINITY;
    return x ? hobbs_log_sigmoid(eta) : hobbs_log1m_sigmoid(eta);
}
#endif


/*
 * Cached bounded exp component for log-link likelihoods.
 *
 * We do not globally replace exp().  Instead, log-link distributions call this
 * component where eta is usually in a reasonable model-scale range.  Outside
 * [-hobbs_EXP_CACHE_LIMIT, hobbs_EXP_CACHE_LIMIT] it falls back to exact exp().
 */
#ifdef hobbs_ENABLE_LOG_CACHE
#ifndef hobbs_EXP_CACHE_BITS
#define hobbs_EXP_CACHE_BITS hobbs_LOG_CACHE_BITS
#endif
#ifndef hobbs_EXP_CACHE_LIMIT
#define hobbs_EXP_CACHE_LIMIT 12.0
#endif
#define hobbs_EXP_CACHE_SIZE ((size_t)1u << hobbs_EXP_CACHE_BITS)
static double *hobbs_exp_table = NULL;
static int hobbs_exp_ready = 0;

static inline void hobbs_exp_init(void) {
    if (hobbs_exp_ready) return;
    hobbs_exp_table = (double *)malloc(hobbs_EXP_CACHE_SIZE * sizeof(double));
    if (hobbs_exp_table == NULL) {
        hobbs_exp_ready = -1;
        return;
    }
    const double lo = -hobbs_EXP_CACHE_LIMIT;
    const double width = 2.0 * hobbs_EXP_CACHE_LIMIT;
    const double inv_n = 1.0 / (double)hobbs_EXP_CACHE_SIZE;
    for (size_t i = 0; i < hobbs_EXP_CACHE_SIZE; ++i) {
        const double eta = lo + width * (((double)i + 0.5) * inv_n);
        hobbs_exp_table[i] = exp(eta);
    }
    hobbs_exp_ready = 1;
}

static inline double hobbs_exp_eta(double eta) {
    if (!isfinite(eta)) return exp(eta);
    if (eta <= -hobbs_EXP_CACHE_LIMIT || eta >= hobbs_EXP_CACHE_LIMIT) return exp(eta);
    hobbs_exp_init();
    if (hobbs_exp_ready != 1) return exp(eta);
    const double width = 2.0 * hobbs_EXP_CACHE_LIMIT;
    double pos = (eta + hobbs_EXP_CACHE_LIMIT) * ((double)hobbs_EXP_CACHE_SIZE / width);
    size_t idx = (size_t)pos;
    if (idx >= hobbs_EXP_CACHE_SIZE) idx = hobbs_EXP_CACHE_SIZE - 1u;
    return hobbs_exp_table[idx];
}
#else
static inline double hobbs_exp_eta(double eta) { return exp(eta); }
#endif

/* Normal/probit link components.  These are exact by default. */
static inline double hobbs_normal_cdf(double x) {
    return 0.5 * erfc(-x / M_SQRT2);
}

static inline double hobbs_log_normal_cdf_exact(double x) {
    if (x > 8.0) return 0.0;
    if (x < -38.0) return -0.5 * x * x - log(-x) - 0.5 * log(2.0 * M_PI);
    return log(0.5 * erfc(-x / M_SQRT2));
}

static inline double hobbs_log1m_normal_cdf_exact(double x) {
    if (x < -8.0) return 0.0;
    if (x > 38.0) return -0.5 * x * x - log(x) - 0.5 * log(2.0 * M_PI);
    return log(0.5 * erfc(x / M_SQRT2));
}

#ifdef hobbs_ENABLE_LOG_CACHE
#ifndef hobbs_PROBIT_CACHE_BITS
#define hobbs_PROBIT_CACHE_BITS hobbs_LOG_CACHE_BITS
#endif
#ifndef hobbs_PROBIT_CACHE_LIMIT
#define hobbs_PROBIT_CACHE_LIMIT 8.0
#endif
#define hobbs_PROBIT_CACHE_SIZE ((size_t)1u << hobbs_PROBIT_CACHE_BITS)
static double *hobbs_probit_table = NULL; /* [log Phi, log1m Phi] interleaved */
static int hobbs_probit_ready = 0;

static inline void hobbs_probit_init(void) {
    if (hobbs_probit_ready) return;
    hobbs_probit_table = (double *)malloc(2u * hobbs_PROBIT_CACHE_SIZE * sizeof(double));
    if (hobbs_probit_table == NULL) {
        hobbs_probit_ready = -1;
        return;
    }
    const double lo = -hobbs_PROBIT_CACHE_LIMIT;
    const double width = 2.0 * hobbs_PROBIT_CACHE_LIMIT;
    const double inv_n = 1.0 / (double)hobbs_PROBIT_CACHE_SIZE;
    for (size_t i = 0; i < hobbs_PROBIT_CACHE_SIZE; ++i) {
        const double z = lo + width * (((double)i + 0.5) * inv_n);
        const size_t base = i << 1;
        hobbs_probit_table[base + 0u] = hobbs_log_normal_cdf_exact(z);
        hobbs_probit_table[base + 1u] = hobbs_log1m_normal_cdf_exact(z);
    }
    hobbs_probit_ready = 1;
}

static inline size_t hobbs_probit_index(double z) {
    const double width = 2.0 * hobbs_PROBIT_CACHE_LIMIT;
    double pos = (z + hobbs_PROBIT_CACHE_LIMIT) * ((double)hobbs_PROBIT_CACHE_SIZE / width);
    size_t idx = (size_t)pos;
    if (idx >= hobbs_PROBIT_CACHE_SIZE) idx = hobbs_PROBIT_CACHE_SIZE - 1u;
    return idx;
}

static inline double hobbs_log_normal_cdf(double z) {
    if (!isfinite(z)) return (z < 0.0) ? -INFINITY : 0.0;
    if (z <= -hobbs_PROBIT_CACHE_LIMIT || z >= hobbs_PROBIT_CACHE_LIMIT) return hobbs_log_normal_cdf_exact(z);
    hobbs_probit_init();
    if (hobbs_probit_ready != 1) return hobbs_log_normal_cdf_exact(z);
    return hobbs_probit_table[(hobbs_probit_index(z) << 1) + 0u];
}

static inline double hobbs_log1m_normal_cdf(double z) {
    if (!isfinite(z)) return (z < 0.0) ? 0.0 : -INFINITY;
    if (z <= -hobbs_PROBIT_CACHE_LIMIT || z >= hobbs_PROBIT_CACHE_LIMIT) return hobbs_log1m_normal_cdf_exact(z);
    hobbs_probit_init();
    if (hobbs_probit_ready != 1) return hobbs_log1m_normal_cdf_exact(z);
    return hobbs_probit_table[(hobbs_probit_index(z) << 1) + 1u];
}
#else
static inline double hobbs_log_normal_cdf(double z) { return hobbs_log_normal_cdf_exact(z); }
static inline double hobbs_log1m_normal_cdf(double z) { return hobbs_log1m_normal_cdf_exact(z); }
#endif

static inline double dbern_probit(unsigned int x, double eta) {
    if (x > 1U) return -INFINITY;
    return x ? hobbs_log_normal_cdf(eta) : hobbs_log1m_normal_cdf(eta);
}

/* Complementary log-log Bernoulli link components. */
static inline double hobbs_log1m_exp_neg_exp_exact(double eta) {
    if (eta > 12.0) return 0.0;
    if (eta < -12.0) return eta; /* log(1 - exp(-exp(eta))) ~= log(exp(eta)) */
    const double e = hobbs_exp_eta(eta);
    return log1p(-exp(-e));
}

static inline double hobbs_log_exp_neg_exp(double eta) {
    return -hobbs_exp_eta(eta); /* log(exp(-exp(eta))) */
}

static inline double dbern_cloglog(unsigned int x, double eta) {
    if (x > 1U) return -INFINITY;
    return x ? hobbs_log1m_exp_neg_exp_exact(eta) : hobbs_log_exp_neg_exp(eta);
}

static inline double dbinom_log(unsigned int x, unsigned int size, double prob) {
    if (prob < 0.0 || prob > 1.0 || x > size) return -INFINITY;
    if (prob == 0.0) return (x == 0U) ? 0.0 : -INFINITY;
    if (prob == 1.0) return (x == size) ? 0.0 : -INFINITY;
    return hobbs_log_choose(size, x) + ((double)x) * log(prob) + ((double)(size - x)) * log1p(-prob);
}


static inline double dpois_log(unsigned int x, double lambda) {
    if (!(lambda >= 0.0)) return -INFINITY;
    if (lambda == 0.0) return (x == 0U) ? 0.0 : -INFINITY;
    return ((double)x) * log(lambda) - lambda - hobbs_lfactorial(x);
}

static inline double dpois_log_link(unsigned int x, double eta) {
    const double lambda = hobbs_exp_eta(eta);
    return ((double)x) * eta - lambda - hobbs_lfactorial(x);
}

static inline double dbinom_logit(unsigned int x, unsigned int size, double eta) {
    if (x > size || !isfinite(eta)) return -INFINITY;
    return hobbs_log_choose(size, x)
        + ((double)x) * hobbs_log_sigmoid(eta)
        + ((double)(size - x)) * hobbs_log1m_sigmoid(eta);
}

static inline double hobbs_dnbinom_log(unsigned int x, double size, double prob) {
    if (!(size > 0.0) || prob < 0.0 || prob > 1.0) return -INFINITY;
    if (prob == 0.0) return -INFINITY;
    if (prob == 1.0) return (x == 0U) ? 0.0 : -INFINITY;
    return lgamma((double)x + size) - lgamma(size) - hobbs_lfactorial(x)
        + size * log(prob) + ((double)x) * log1p(-prob);
}

/* Negative-binomial with log mean eta and shape/size parameter. */
static inline double hobbs_dnbinom_log_mu(unsigned int x, double eta, double size) {
    if (!(size > 0.0) || !isfinite(eta)) return -INFINITY;
    const double mu = hobbs_exp_eta(eta);
    return lgamma((double)x + size) - lgamma(size) - hobbs_lfactorial(x)
        + size * (log(size) - log(size + mu))
        + ((double)x) * (eta - log(size + mu));
}

/* Common fast normal special cases. */
static inline double dnorm01_log(double x) {
    return -0.5 * x * x - 0.5 * log(2.0 * M_PI);
}

static inline double dnorm_sd1_log(double x, double mean) {
    const double z = x - mean;
    return -0.5 * z * z - 0.5 * log(2.0 * M_PI);
}

static inline double dmvn_diag_log(const double *x, const double *mean, const double *sd, int dim) {
    if (dim < 0) return -INFINITY;
    double target = 0.0;
    for (int j = 0; j < dim; ++j) target += dnorm_log(x[j], mean[j], sd[j]);
    return target;
}

/* Checked workspace sizing for dense matrix helpers. */
static inline int hobbs_matrix_workspace_elems(
    int dim,
    size_t matrices,
    size_t vectors,
    size_t *elements
) {
    if (elements == NULL || dim < 0) return 0;
    const size_t n = (size_t)dim;
    if (n != 0u && n > SIZE_MAX / n) return 0;
    const size_t nn = n * n;
    if (matrices != 0u && nn > SIZE_MAX / matrices) return 0;
    size_t total = nn * matrices;
    if (vectors != 0u) {
        if (n > SIZE_MAX / vectors) return 0;
        const size_t extra = n * vectors;
        if (total > SIZE_MAX - extra) return 0;
        total += extra;
    }
    if (total > SIZE_MAX / sizeof(double)) return 0;
    *elements = total;
    return 1;
}

static inline int hobbs_chol_logdet(const double *A, double *L, int p, double *logdet);

/*
 * Multivariate normal using a caller-supplied lower Cholesky factor and
 * scratch vector.  Reuse this form inside hot loops when the covariance is
 * unchanged by the active scalar parameter; it performs no allocation and no
 * repeated factorization.
 */
static inline double dmvn_chol_log(
    const double *x,
    const double *mean,
    const double *chol,
    double logdet,
    int dim,
    double *scratch
) {
    if (dim < 0) return -INFINITY;
    if (dim == 0) return 0.0;
    if (x == NULL || mean == NULL || chol == NULL || scratch == NULL || !isfinite(logdet)) {
        return -INFINITY;
    }

    double quad = 0.0;
    for (int i = 0; i < dim; ++i) {
        double value = x[i] - mean[i];
        for (int k = 0; k < i; ++k) value -= chol[i + k * dim] * scratch[k];
        const double diagonal = chol[i + i * dim];
        if (!(diagonal > 0.0) || !isfinite(diagonal)) return -INFINITY;
        scratch[i] = value / diagonal;
        quad += scratch[i] * scratch[i];
    }
    if (!isfinite(quad)) return -INFINITY;
    return -0.5 * ((double)dim * log(2.0 * M_PI) + logdet + quad);
}

/* Workspace length: dim * dim + dim doubles. */
static inline double dmvn_log_workspace(
    const double *x,
    const double *mean,
    const double *cov,
    int dim,
    double *workspace
) {
    if (dim < 0) return -INFINITY;
    if (dim == 0) return 0.0;
    if (x == NULL || mean == NULL || cov == NULL || workspace == NULL) return -INFINITY;

    const size_t n = (size_t)dim;
    double *chol = workspace;
    double *scratch = workspace + n * n;
    double logdet = 0.0;
    if (!hobbs_chol_logdet(cov, chol, dim, &logdet)) return -INFINITY;
    return dmvn_chol_log(x, mean, chol, logdet, dim, scratch);
}

/* Multivariate normal log density with a dense covariance matrix in R/C
 * column-major order: cov[row + col * dim], zero-based internally.
 *
 * This compatibility entry point must also be safe in scalar-MWG hot loops.
 * One- and two-dimensional densities stay entirely in registers, small dense
 * factors use stack scratch, and only larger dimensions use a heap workspace.
 * In particular, never use cache-line-aligned heap allocation per density:
 * on platforms where that maps to posix_memalign, allocator traffic can
 * dominate hierarchical models that evaluate billions of small MVN factors.
 */
static hobbs_ALWAYS_INLINE double dmvn_log(
    const double *x,
    const double *mean,
    const double *cov,
    int dim
) {
    if (dim < 0) return -INFINITY;
    if (dim == 0) return 0.0;
    if (x == NULL || mean == NULL || cov == NULL) return -INFINITY;

    /* Closed forms keep the overwhelmingly common small cases in registers. */
    if (dim == 1) {
        const double variance = cov[0];
        if (!(variance > 0.0) || !isfinite(variance)) return -INFINITY;
        const double centered = x[0] - mean[0];
        const double quadratic = centered * centered / variance;
        if (!isfinite(quadratic)) return -INFINITY;
        return -0.5 * (log(2.0 * M_PI) + log(variance) + quadratic);
    }

    if (dim == 2) {
        const double variance0 = cov[0];
        const double covariance = cov[1];
        const double variance1 = cov[3];
        if (!(variance0 > 0.0) || !(variance1 > 0.0) ||
            !isfinite(variance0) || !isfinite(covariance) || !isfinite(variance1)) {
            return -INFINITY;
        }
        const double product = variance0 * variance1;
        const double cross = covariance * covariance;
        const double determinant = product - cross;
        const double centered0 = x[0] - mean[0];
        const double centered1 = x[1] - mean[1];

        /* The direct inverse is fastest for well-conditioned 2x2 matrices. */
        if (isfinite(product) && isfinite(cross) && determinant > 0.0 &&
            determinant > product * 1.4901161193847656e-8) {
            const double quadratic =
                (variance1 * centered0 * centered0
                 - 2.0 * covariance * centered0 * centered1
                 + variance0 * centered1 * centered1) / determinant;
            if (!isfinite(quadratic)) return -INFINITY;
            return -log(2.0 * M_PI) - 0.5 * (log(determinant) + quadratic);
        }

        /* Near singularity or extreme scaling, retain Cholesky stability. */
        const double diagonal0 = sqrt(variance0);
        const double lower10 = covariance / diagonal0;
        const double schur = variance1 - lower10 * lower10;
        if (!(schur > 0.0) || !isfinite(schur)) return -INFINITY;
        const double diagonal1 = sqrt(schur);
        const double standardized0 = centered0 / diagonal0;
        const double standardized1 =
            (centered1 - lower10 * standardized0) / diagonal1;
        const double logdet = 2.0 * log(diagonal0) + 2.0 * log(diagonal1);
        const double quadratic =
            standardized0 * standardized0 + standardized1 * standardized1;
        if (!isfinite(quadratic)) return -INFINITY;
        return -0.5 * (2.0 * log(2.0 * M_PI) + logdet + quadratic);
    }

    enum { hobbs_DMVN_STACK_DIM = 8 };
    if (dim <= hobbs_DMVN_STACK_DIM) {
        double workspace[hobbs_DMVN_STACK_DIM * hobbs_DMVN_STACK_DIM +
                         hobbs_DMVN_STACK_DIM];
        return dmvn_log_workspace(x, mean, cov, dim, workspace);
    }

    size_t elements = 0u;
    if (!hobbs_matrix_workspace_elems(dim, 1u, 1u, &elements)) return -INFINITY;
    double *workspace = (double *)malloc(elements * sizeof(double));
    if (workspace == NULL) return -INFINITY;
    const double out = dmvn_log_workspace(x, mean, cov, dim, workspace);
    free(workspace);
    return out;
}

static inline double dbvn_cov_log(const double *x, const double *mean, const double *cov) {
    if (x == NULL || mean == NULL || cov == NULL) return -INFINITY;
    const double v11 = cov[0];
    const double v12 = cov[2];
    const double v22 = cov[3];
    if (!(v11 > 0.0) || !(v22 > 0.0)) return -INFINITY;
    const double det = v11 * v22 - v12 * v12;
    if (!(det > 0.0)) return -INFINITY;
    const double z1 = x[0] - mean[0];
    const double z2 = x[1] - mean[1];
    const double quad = (v22 * z1 * z1 - 2.0 * v12 * z1 * z2 + v11 * z2 * z2) / det;
    return -log(2.0 * M_PI) - 0.5 * log(det) - 0.5 * quad;
}

static inline double dbvn_log(double x1, double x2, double mean1, double mean2, double sigma1, double sigma2, double rho) {
    if (!(sigma1 > 0.0) || !(sigma2 > 0.0)) return -INFINITY;
    if (!(rho > -1.0 && rho < 1.0)) return -INFINITY;
    const double z1 = (x1 - mean1) / sigma1;
    const double z2 = (x2 - mean2) / sigma2;
    const double rho2 = rho * rho;
    const double one_minus_rho2 = 1.0 - rho2;
    const double quad = z1 * z1 - 2.0 * rho * z1 * z2 + z2 * z2;
    return -log(2.0 * M_PI) - log(sigma1) - log(sigma2)
        - 0.5 * log(one_minus_rho2) - 0.5 * quad / one_minus_rho2;
}

/*
 * Zero-mean bivariate normal log density.
 *
 *   (x1, x2) ~ Normal_2(0, Sigma)
 *
 * with standard deviations sigma1, sigma2 and correlation rho. This is useful
 * for random intercept/slope priors such as normal2_lpdf(u0(p), u1(p), ...).
 *
 * All log() calls intentionally use the public log symbol, so when
 * hobbs_ENABLE_LOG_CACHE is defined they are routed through hobbs_lookup_log().
 */
static inline double dnormal2_log(double x1, double x2, double sigma1, double sigma2, double rho) {
    if (!(sigma1 > 0.0) || !(sigma2 > 0.0)) return -INFINITY;
    if (!(rho > -1.0 && rho < 1.0)) return -INFINITY;

    const double z1 = x1 / sigma1;
    const double z2 = x2 / sigma2;
    const double rho2 = rho * rho;
    const double one_minus_rho2 = 1.0 - rho2;
    if (!(one_minus_rho2 > 0.0)) return -INFINITY;

    const double quad = z1 * z1 - 2.0 * rho * z1 * z2 + z2 * z2;

    return -log(2.0 * M_PI)
        - log(sigma1)
        - log(sigma2)
        - 0.5 * log(one_minus_rho2)
        - 0.5 * quad / one_minus_rho2;
}


/* Additional scalar and covariance-matrix log densities. */
static inline double dlogis_log(double x, double location, double scale) {
    if (!(scale > 0.0)) return -INFINITY;
    const double z = (x - location) / scale;
    if (z >= 0.0) {
        return -z - log(scale) - 2.0 * log1p(exp(-z));
    } else {
        return z - log(scale) - 2.0 * log1p(exp(z));
    }
}

static inline double dlaplace_log(double x, double location, double scale) {
    if (!(scale > 0.0)) return -INFINITY;
    return -log(2.0 * scale) - fabs(x - location) / scale;
}

static inline double dweibull_log(double x, double shape, double scale) {
    if (!(shape > 0.0) || !(scale > 0.0) || x < 0.0) return -INFINITY;
    if (x == 0.0) {
        if (shape == 1.0) return -log(scale);
        return (shape < 1.0) ? INFINITY : -INFINITY;
    }
    const double lx_s = log(x / scale);
    return log(shape) - log(scale) + (shape - 1.0) * lx_s - exp(shape * lx_s);
}

static inline double dpareto_log(double x, double xmin, double alpha) {
    if (!(xmin > 0.0) || !(alpha > 0.0) || x < xmin) return -INFINITY;
    return log(alpha) + alpha * log(xmin) - (alpha + 1.0) * log(x);
}

static inline double dhalfnorm_log(double x, double sd) {
    if (!(sd > 0.0) || x < 0.0) return -INFINITY;
    return 0.5 * log(2.0 / M_PI) - log(sd) - 0.5 * (x / sd) * (x / sd);
}

static inline double dhalfcauchy_log(double x, double scale) {
    if (!(scale > 0.0) || x < 0.0) return -INFINITY;
    const double z = x / scale;
    return log(2.0 / M_PI) - log(scale) - log1p(z * z);
}

static inline double hobbs_lmultigamma(double a, int p) {
    if (p < 1 || !(a > 0.5 * (double)(p - 1))) return INFINITY;
    double out = 0.25 * (double)p * (double)(p - 1) * log(M_PI);
    for (int j = 1; j <= p; ++j) out += lgamma(a + 0.5 * (double)(1 - j));
    return out;
}

static inline int hobbs_chol_logdet(const double *A, double *L, int p, double *logdet) {
    if (A == NULL || L == NULL || logdet == NULL || p < 1) return 0;
    const size_t n = (size_t)p;
    if (n > SIZE_MAX / n) return 0;
    const size_t nn = n * n;
    for (size_t i = 0; i < nn; ++i) L[i] = 0.0;
    *logdet = 0.0;
    for (int i = 0; i < p; ++i) {
        for (int j = 0; j <= i; ++j) {
            double sum = A[i + j * p];
            for (int k = 0; k < j; ++k) sum -= L[i + k * p] * L[j + k * p];
            if (i == j) {
                if (!(sum > 0.0) || !isfinite(sum)) return 0;
                const double diag = sqrt(sum);
                L[i + j * p] = diag;
                *logdet += 2.0 * log(diag);
            } else {
                L[i + j * p] = sum / L[j + j * p];
            }
        }
    }
    return 1;
}

/* Trace(A^-1 B) from a lower Cholesky factor A = L L'. */
static inline double hobbs_trace_inv_chol_times(
    const double *L,
    const double *B,
    int p,
    double *y,
    double *z
) {
    if (L == NULL || B == NULL || y == NULL || z == NULL || p < 1) return INFINITY;
    double tr = 0.0;
    for (int c = 0; c < p; ++c) {
        for (int i = 0; i < p; ++i) {
            double v = B[i + c * p];
            for (int k = 0; k < i; ++k) v -= L[i + k * p] * y[k];
            const double diagonal = L[i + i * p];
            if (!(diagonal > 0.0) || !isfinite(diagonal)) return INFINITY;
            y[i] = v / diagonal;
        }
        for (int i = p - 1; i >= 0; --i) {
            double v = y[i];
            for (int k = i + 1; k < p; ++k) v -= L[k + i * p] * z[k];
            z[i] = v / L[i + i * p];
        }
        tr += z[c];
    }
    return tr;
}

/* Workspace length: p * p + 2 * p doubles. */
static inline double hobbs_trace_inv_spd_times_workspace(
    const double *A,
    const double *B,
    int p,
    double *workspace
) {
    if (A == NULL || B == NULL || workspace == NULL || p < 1) return INFINITY;
    const size_t n = (size_t)p;
    double *L = workspace;
    double *y = workspace + n * n;
    double *z = y + n;
    double logdet_unused = 0.0;
    if (!hobbs_chol_logdet(A, L, p, &logdet_unused)) return INFINITY;
    return hobbs_trace_inv_chol_times(L, B, p, y, z);
}

static inline double hobbs_trace_inv_spd_times(const double *A, const double *B, int p) {
    if (A == NULL || B == NULL || p < 1) return INFINITY;
    size_t elements = 0u;
    if (!hobbs_matrix_workspace_elems(p, 1u, 2u, &elements)) return INFINITY;
    double *workspace = (double *)hobbs_aligned_malloc(elements * sizeof(double));
    if (workspace == NULL) return INFINITY;
    const double out = hobbs_trace_inv_spd_times_workspace(A, B, p, workspace);
    hobbs_aligned_free(workspace);
    return out;
}

/*
 * Allocation-free Wishart helper for a scale matrix whose Cholesky factor and
 * log determinant have already been computed. Workspace length:
 * dim * dim + 2 * dim doubles.
 */
static inline double dwish_chol_scale_log(
    const double *W,
    const double *scale_chol,
    double scale_logdet,
    double df,
    int dim,
    double *workspace
) {
    if (W == NULL || scale_chol == NULL || workspace == NULL || dim < 1 ||
        !(df > (double)(dim - 1)) || !isfinite(scale_logdet)) return -INFINITY;
    const size_t n = (size_t)dim;
    double *LW = workspace;
    double *y = workspace + n * n;
    double *z = y + n;
    double logdetW = 0.0;
    if (!hobbs_chol_logdet(W, LW, dim, &logdetW)) return -INFINITY;
    const double tr = hobbs_trace_inv_chol_times(scale_chol, W, dim, y, z);
    if (!isfinite(tr)) return -INFINITY;
    return 0.5 * (df - (double)dim - 1.0) * logdetW
        - 0.5 * tr
        - 0.5 * df * (double)dim * log(2.0)
        - 0.5 * df * scale_logdet
        - hobbs_lmultigamma(0.5 * df, dim);
}

/*
 * Allocation-free inverse-Wishart helper when the scale log determinant is
 * already available. Workspace length: dim * dim + 2 * dim doubles.
 */
static inline double dinvwish_scale_logdet_log(
    const double *W,
    const double *scale,
    double scale_logdet,
    double df,
    int dim,
    double *workspace
) {
    if (W == NULL || scale == NULL || workspace == NULL || dim < 1 ||
        !(df > (double)(dim - 1)) || !isfinite(scale_logdet)) return -INFINITY;
    const size_t n = (size_t)dim;
    double *LW = workspace;
    double *y = workspace + n * n;
    double *z = y + n;
    double logdetW = 0.0;
    if (!hobbs_chol_logdet(W, LW, dim, &logdetW)) return -INFINITY;
    const double tr = hobbs_trace_inv_chol_times(LW, scale, dim, y, z);
    if (!isfinite(tr)) return -INFINITY;
    return 0.5 * df * scale_logdet
        - 0.5 * df * (double)dim * log(2.0)
        - hobbs_lmultigamma(0.5 * df, dim)
        - 0.5 * (df + (double)dim + 1.0) * logdetW
        - 0.5 * tr;
}

static inline double dwish_log(const double *W, const double *scale, double df, int dim) {
    if (W == NULL || scale == NULL || dim < 1 || !(df > (double)(dim - 1))) return -INFINITY;
    size_t elements = 0u;
    if (!hobbs_matrix_workspace_elems(dim, 2u, 2u, &elements)) return -INFINITY;
    double *workspace = (double *)hobbs_aligned_malloc(elements * sizeof(double));
    if (workspace == NULL) return -INFINITY;
    const size_t n = (size_t)dim;
    double *LW = workspace;
    double *LS = LW + n * n;
    double *y = LS + n * n;
    double *z = y + n;
    double logdetW = 0.0, logdetS = 0.0;
    const int okW = hobbs_chol_logdet(W, LW, dim, &logdetW);
    const int okS = hobbs_chol_logdet(scale, LS, dim, &logdetS);
    if (!okW || !okS) {
        hobbs_aligned_free(workspace);
        return -INFINITY;
    }
    const double tr = hobbs_trace_inv_chol_times(LS, W, dim, y, z);
    if (!isfinite(tr)) {
        hobbs_aligned_free(workspace);
        return -INFINITY;
    }
    const double lp = 0.5 * (df - (double)dim - 1.0) * logdetW
        - 0.5 * tr
        - 0.5 * df * (double)dim * log(2.0)
        - 0.5 * df * logdetS
        - hobbs_lmultigamma(0.5 * df, dim);
    hobbs_aligned_free(workspace);
    return lp;
}

static inline double dinvwish_log(const double *W, const double *scale, double df, int dim) {
    if (W == NULL || scale == NULL || dim < 1 || !(df > (double)(dim - 1))) return -INFINITY;
    size_t elements = 0u;
    if (!hobbs_matrix_workspace_elems(dim, 2u, 2u, &elements)) return -INFINITY;
    double *workspace = (double *)hobbs_aligned_malloc(elements * sizeof(double));
    if (workspace == NULL) return -INFINITY;
    const size_t n = (size_t)dim;
    double *LW = workspace;
    double *LS = LW + n * n;
    double *y = LS + n * n;
    double *z = y + n;
    double logdetW = 0.0, logdetS = 0.0;
    const int okW = hobbs_chol_logdet(W, LW, dim, &logdetW);
    const int okS = hobbs_chol_logdet(scale, LS, dim, &logdetS);
    if (!okW || !okS) {
        hobbs_aligned_free(workspace);
        return -INFINITY;
    }
    const double tr = hobbs_trace_inv_chol_times(LW, scale, dim, y, z);
    if (!isfinite(tr)) {
        hobbs_aligned_free(workspace);
        return -INFINITY;
    }
    const double lp = 0.5 * df * logdetS
        - 0.5 * df * (double)dim * log(2.0)
        - hobbs_lmultigamma(0.5 * df, dim)
        - 0.5 * (df + (double)dim + 1.0) * logdetW
        - 0.5 * tr;
    hobbs_aligned_free(workspace);
    return lp;
}

static inline double dlkjcorr2_log(const double *R, double eta) {
    if (R == NULL || !(eta > 0.0)) return -INFINITY;
    const double r11 = R[0];
    const double r12 = R[2];
    const double r22 = R[3];
    if (fabs(r11 - 1.0) > 1e-8 || fabs(r22 - 1.0) > 1e-8) return -INFINITY;
    if (!(r12 > -1.0 && r12 < 1.0)) return -INFINITY;
    const double log_norm = lgamma(eta + 0.5) - 0.5 * log(M_PI) - lgamma(eta);
    return log_norm + (eta - 1.0) * log1p(-r12 * r12);
}


static inline double inv_logit(double eta) {
    if (eta >= 0.0) {
        const double z = exp(-eta);
        return 1.0 / (1.0 + z);
    } else {
        const double z = exp(eta);
        return z / (1.0 + z);
    }
}

/* R-like aliases for users who prefer explicit lpdf names. */
#define normal_lpdf dnorm_log
#define dnorm_lpdf dnorm_log
#define normal2_lpdf dnormal2_log
#define bivar_normal_lpdf dnormal2_log
#define dmvn_lpdf dmvn_log
#define dbvn_lpdf dbvn_log
#define dbvn_cov_lpdf dbvn_cov_log
#define uniform_lpdf dunif_log
#define dunif_lpdf dunif_log
#define exponential_lpdf dexp_log
#define dexp_lpdf dexp_log
#define gamma_lpdf dgamma_log
#define dgamma_lpdf dgamma_log
#define invgamma_lpdf dinvgamma_log
#define dinvgamma_lpdf dinvgamma_log
#define beta_lpdf dbeta_log
#define dbeta_lpdf dbeta_log
#define cauchy_lpdf dcauchy_log
#define dcauchy_lpdf dcauchy_log
#define student_t_lpdf dt_log
#define dt_lpdf dt_log
#define chisq_lpdf dchisq_log
#define dchisq_lpdf dchisq_log
#define lognormal_lpdf dlnorm_log
#define dlnorm_lpdf dlnorm_log
#define logistic_lpdf dlogis_log
#define dlogis_lpdf dlogis_log
#define laplace_lpdf dlaplace_log
#define dlaplace_lpdf dlaplace_log
#define weibull_lpdf dweibull_log
#define dweibull_lpdf dweibull_log
#define pareto_lpdf dpareto_log
#define dpareto_lpdf dpareto_log
#define halfnorm_lpdf dhalfnorm_log
#define dhalfnorm_lpdf dhalfnorm_log
#define halfcauchy_lpdf dhalfcauchy_log
#define dhalfcauchy_lpdf dhalfcauchy_log
#define bernoulli_lpdf dbern_log
#define dbern_lpdf dbern_log
#define log_sigmoid_lpdf hobbs_log_sigmoid
#define log1m_sigmoid_lpdf hobbs_log1m_sigmoid
#define bernoulli_logit_lpdf dbern_logit
#define binomial_lpdf dbinom_log
#define dbinom_lpdf dbinom_log
#define poisson_lpdf dpois_log
#define dpois_lpdf dpois_log
#define dwish_lpdf dwish_log
#define wishart_lpdf dwish_log
#define dinvwish_lpdf dinvwish_log
#define invwishart_lpdf dinvwish_log
#define inv_wishart_lpdf dinvwish_log
#define lkjcorr2_lpdf dlkjcorr2_log
#define dlkjcorr2_lpdf dlkjcorr2_log

/* Cached/component-oriented helpers for model hot paths. */
#define lfactorial_lpmf hobbs_lfactorial
#define log_factorial hobbs_lfactorial
#define log_choose hobbs_log_choose
#define choose_log hobbs_log_choose
#define exp_eta hobbs_exp_eta
#define normal_cdf hobbs_normal_cdf
#define log_normal_cdf hobbs_log_normal_cdf
#define log1m_normal_cdf hobbs_log1m_normal_cdf
#define bernoulli_probit_lpdf dbern_probit
#define bernoulli_cloglog_lpdf dbern_cloglog
#define poisson_log_lpdf dpois_log_link
#define binomial_logit_lpdf dbinom_logit
#define negbinomial_lpdf hobbs_dnbinom_log
#define dnbinom_lpdf hobbs_dnbinom_log
#define negbinomial_log_lpdf hobbs_dnbinom_log_mu
#define dnbinom_log_lpdf hobbs_dnbinom_log_mu
#define normal01_lpdf dnorm01_log
#define normal_sd1_lpdf dnorm_sd1_log

#endif
