#ifndef HOBBS_ODE_H
#define HOBBS_ODE_H

#include <math.h>
#include <float.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Dormand--Prince embedded 5(4), without sensitivities or external libraries.
 * Callbacks write all n derivatives. State is committed only on success.
 * Work and all callback context are per call; no mutable global solver state. */
typedef void (*hobbs_ode_rhs)(double, const double *, double *, void *);
typedef struct {
    const double *theta;
    const double *input;
    size_t input_len;
} hobbs_ode_context;

enum {
    HOBBS_ODE_OK = 0,
    HOBBS_ODE_INVALID = 1,
    HOBBS_ODE_ALLOC = 2,
    HOBBS_ODE_NONFINITE = 3,
    HOBBS_ODE_MAX_STEPS = 4,
    HOBBS_ODE_STEP_TOO_SMALL = 5,
    HOBBS_ODE_NEWTON_FAILED = 6
};

static inline double hobbs_ode_input(const hobbs_ode_context *ctx, int i) {
    return ctx->input && i > 0 && (size_t)i <= ctx->input_len
        ? ctx->input[i - 1] : NAN;
}

static inline int hobbs_ode_eval(hobbs_ode_rhs rhs, void *ctx, int n,
                                double t, const double *y, double *dy) {
    for (int i = 0; i < n; ++i) {
        if (!isfinite(y[i])) return HOBBS_ODE_NONFINITE;
        dy[i] = NAN;
    }
    rhs(t, y, dy, ctx);
    for (int i = 0; i < n; ++i)
        if (!isfinite(dy[i])) return HOBBS_ODE_NONFINITE;
    return HOBBS_ODE_OK;
}

static inline int hobbs_ode_rk45(hobbs_ode_rhs rhs, void *ctx, int n,
                                 double *state, double t0, double t1,
                                 double rtol, double atol, int max_steps) {
    if (!rhs || !state || n <= 0 || !isfinite(t0) || !isfinite(t1) ||
        t1 < t0 || !isfinite(t1 - t0) || !isfinite(rtol) || rtol <= 0 ||
        !isfinite(atol) || atol <= 0 || max_steps <= 0)
        return HOBBS_ODE_INVALID;
    for (int i = 0; i < n; ++i)
        if (!isfinite(state[i])) return HOBBS_ODE_NONFINITE;
    if (t1 == t0) return HOBBS_ODE_OK;
    if ((size_t)n > SIZE_MAX / (10u * sizeof(double))) return HOBBS_ODE_ALLOC;
    double *work = (double *)malloc((size_t)n * 10u * sizeof(double));
    if (!work) return HOBBS_ODE_ALLOC;
    double *y = work, *tmp = y + n, *next = tmp + n;
    double *k[7];
    for (int j = 0; j < 7; ++j) k[j] = next + (j + 1) * (size_t)n;
    memcpy(y, state, (size_t)n * sizeof(double));
    double t = t0, h = (t1 - t0) * 0.1;
    if (h == 0.0) h = t1 - t0;
    int status = hobbs_ode_eval(rhs, ctx, n, t, y, k[0]);
    if (status) goto done;
    for (int step = 0; step < max_steps; ++step) {
        const double remaining = t1 - t;
        if (h > remaining) h = remaining;
        const double end = h == remaining ? t1 : t + h;
        if (!(h > 0.0) || end == t) {
            status = HOBBS_ODE_STEP_TOO_SMALL; goto done;
        }
#define HOBBS_ODE_STAGE(slot, time, expression) \
        do { \
            for (int i = 0; i < n; ++i) tmp[i] = y[i] + h * (expression); \
            status = hobbs_ode_eval(rhs, ctx, n, time, tmp, k[slot]); \
            if (status) goto done; \
        } while (0)
        HOBBS_ODE_STAGE(1, t + h/5.0, k[0][i]/5.0);
        HOBBS_ODE_STAGE(2, t + 3.0*h/10.0,
            3.0*k[0][i]/40.0 + 9.0*k[1][i]/40.0);
        HOBBS_ODE_STAGE(3, t + 4.0*h/5.0,
            44.0*k[0][i]/45.0 - 56.0*k[1][i]/15.0 + 32.0*k[2][i]/9.0);
        HOBBS_ODE_STAGE(4, t + 8.0*h/9.0,
            19372.0*k[0][i]/6561.0 - 25360.0*k[1][i]/2187.0 +
            64448.0*k[2][i]/6561.0 - 212.0*k[3][i]/729.0);
        HOBBS_ODE_STAGE(5, end,
            9017.0*k[0][i]/3168.0 - 355.0*k[1][i]/33.0 +
            46732.0*k[2][i]/5247.0 + 49.0*k[3][i]/176.0 -
            5103.0*k[4][i]/18656.0);
        HOBBS_ODE_STAGE(6, end,
            35.0*k[0][i]/384.0 + 500.0*k[2][i]/1113.0 +
            125.0*k[3][i]/192.0 - 2187.0*k[4][i]/6784.0 + 11.0*k[5][i]/84.0);
#undef HOBBS_ODE_STAGE
        memcpy(next, tmp, (size_t)n * sizeof(double));
        double error = 0.0;
        for (int i = 0; i < n; ++i) {
            const double e = h * (71.0*k[0][i]/57600.0 - 71.0*k[2][i]/16695.0 +
                71.0*k[3][i]/1920.0 - 17253.0*k[4][i]/339200.0 +
                22.0*k[5][i]/525.0 - k[6][i]/40.0);
            const double scale = atol + rtol * fmax(fabs(y[i]), fabs(next[i]));
            if (!isfinite(e) || !isfinite(scale)) {
                status = HOBBS_ODE_NONFINITE; goto done;
            }
            error = fmax(error, fabs(e) / scale);
        }
        if (error <= 1.0) {
            memcpy(y, next, (size_t)n * sizeof(double));
            memcpy(k[0], k[6], (size_t)n * sizeof(double)); /* FSAL */
            t = end;
            if (t == t1) {
                memcpy(state, y, (size_t)n * sizeof(double));
                status = HOBBS_ODE_OK; goto done;
            }
        }
        const double factor = error == 0.0 ? 5.0 :
            fmax(0.1, fmin(5.0, 0.9 * pow(error, -0.2)));
        h *= factor;
    }
    status = HOBBS_ODE_MAX_STEPS;
done:
    free(work);
    return status;
}

/* Dense, row-major Gaussian elimination with partial pivoting. */
static inline int hobbs_ode_linear_solve(int n, double *a, double *b) {
    for (int j = 0; j < n; ++j) {
        int pivot = j;
        for (int i = j + 1; i < n; ++i)
            if (fabs(a[(size_t)i*n+j]) > fabs(a[(size_t)pivot*n+j])) pivot = i;
        const double p = a[(size_t)pivot*n+j];
        if (!isfinite(p) || p == 0.0) return HOBBS_ODE_NEWTON_FAILED;
        if (pivot != j) {
            for (int k = j; k < n; ++k) {
                const double tmp = a[(size_t)j*n+k];
                a[(size_t)j*n+k] = a[(size_t)pivot*n+k];
                a[(size_t)pivot*n+k] = tmp;
            }
            const double tmp = b[j]; b[j] = b[pivot]; b[pivot] = tmp;
        }
        for (int i = j + 1; i < n; ++i) {
            const double factor = a[(size_t)i*n+j] / a[(size_t)j*n+j];
            if (!isfinite(factor)) return HOBBS_ODE_NEWTON_FAILED;
            for (int k = j + 1; k < n; ++k)
                a[(size_t)i*n+k] -= factor * a[(size_t)j*n+k];
            b[i] -= factor * b[j];
        }
    }
    for (int i = n - 1; i >= 0; --i) {
        double value = b[i];
        for (int j = i + 1; j < n; ++j) value -= a[(size_t)i*n+j] * b[j];
        b[i] = value / a[(size_t)i*n+i];
        if (!isfinite(b[i])) return HOBBS_ODE_NEWTON_FAILED;
    }
    return HOBBS_ODE_OK;
}

static inline double hobbs_ode_scaled_residual(int n, const double *z,
        const double *base, const double *f, double gamma, const double *ref,
        double rtol, double atol, double *residual) {
    double norm = 0.0;
    for (int i = 0; i < n; ++i) {
        const double scale = atol + rtol * fmax(fabs(ref[i]), fabs(z[i]));
        residual[i] = z[i] - base[i] - gamma*f[i];
        if (!isfinite(residual[i]) || !isfinite(scale)) return INFINITY;
        norm = fmax(norm, fabs(residual[i]) / scale);
    }
    return norm;
}

/* Solve z - base - gamma*f(t,z) = 0 with damped Newton iterations.
 * The RHS Jacobian is finite-differenced; no derivatives are requested from
 * model authors. Scratch consists of five n-vectors and one n*n matrix. */
static inline int hobbs_ode_newton(hobbs_ode_rhs rhs, void *ctx, int n,
        double t, const double *base, double gamma, const double *ref,
        double rtol, double atol, double *z, double *scratch, double *a) {
    double *f = scratch, *fn = f+n, *delta = fn+n, *pert = delta+n, *trial = pert+n;
    for (int iteration = 0; iteration < 12; ++iteration) {
        int status = hobbs_ode_eval(rhs, ctx, n, t, z, f);
        if (status) return status;
        const double norm = hobbs_ode_scaled_residual(n,z,base,f,gamma,ref,rtol,atol,delta);
        if (norm <= 0.02) return HOBBS_ODE_OK;
        if (!isfinite(norm)) return HOBBS_ODE_NONFINITE;
        memcpy(pert, z, (size_t)n*sizeof(double));
        for (int j = 0; j < n; ++j) {
            double magnitude = fmax(fabs(z[j]), atol/rtol);
            if (!isfinite(magnitude)) magnitude = fmax(fabs(z[j]), 1.0);
            double shift = sqrt(DBL_EPSILON) * fmax(magnitude, 1e-8);
            pert[j] = z[j] + shift;
            if (!isfinite(pert[j])) pert[j] = z[j] - shift;
            if (pert[j] == z[j]) pert[j] = nextafter(z[j], INFINITY);
            status = hobbs_ode_eval(rhs, ctx, n, t, pert, fn);
            if (status) {
                pert[j] = z[j] - shift;
                if (pert[j] == z[j]) pert[j] = nextafter(z[j], -INFINITY);
                status = hobbs_ode_eval(rhs, ctx, n, t, pert, fn);
                if (status) return status;
            }
            shift = pert[j] - z[j];
            if (!isfinite(shift) || shift == 0.0) return HOBBS_ODE_NEWTON_FAILED;
            for (int i = 0; i < n; ++i) {
                a[(size_t)i*n+j] = (i == j ? 1.0 : 0.0) - gamma*((fn[i]-f[i])/shift);
                if (!isfinite(a[(size_t)i*n+j])) return HOBBS_ODE_NEWTON_FAILED;
            }
            pert[j] = z[j];
        }
        for (int i = 0; i < n; ++i) delta[i] = -delta[i];
        status = hobbs_ode_linear_solve(n,a,delta);
        if (status) return status;
        double damping = 1.0;
        int improved = 0;
        for (int backtrack = 0; backtrack < 10; ++backtrack) {
            for (int i = 0; i < n; ++i) trial[i] = z[i] + damping*delta[i];
            status = hobbs_ode_eval(rhs,ctx,n,t,trial,fn);
            if (!status) {
                const double trial_norm = hobbs_ode_scaled_residual(n,trial,base,fn,gamma,ref,rtol,atol,pert);
                if (trial_norm < norm || trial_norm <= 0.02) {
                    memcpy(z,trial,(size_t)n*sizeof(double));
                    improved = 1; break;
                }
            }
            damping *= 0.5;
        }
        if (!improved) return HOBBS_ODE_NEWTON_FAILED;
    }
    return HOBBS_ODE_NEWTON_FAILED;
}

/* Adaptive variable-step BDF1/BDF2. The first two accepted macro steps use
 * backward-Euler step doubling. Subsequent steps use variable-step BDF2;
 * a third divided difference estimates local truncation error. Step growth
 * is limited to 2, below the BDF2 zero-stability ratio bound 1+sqrt(2).
 * Dense finite-difference Jacobians target small/moderate stiff systems.
 * The caller's state is committed only when the whole interval succeeds. */
static inline int hobbs_ode_bdf(hobbs_ode_rhs rhs, void *ctx, int n,
        double *state, double t0, double t1, double rtol, double atol, int max_steps) {
    if (!rhs || !state || n <= 0 || !isfinite(t0) || !isfinite(t1) ||
        t1 < t0 || !isfinite(t1-t0) || !isfinite(rtol) || rtol <= 0 ||
        !isfinite(atol) || atol <= 0 || max_steps <= 0) return HOBBS_ODE_INVALID;
    for (int i = 0; i < n; ++i)
        if (!isfinite(state[i])) return HOBBS_ODE_NONFINITE;
    if (t0 == t1) return HOBBS_ODE_OK;
    const size_t count = (size_t)n;
    if (count > SIZE_MAX/sizeof(double)/(count+12u)) return HOBBS_ODE_ALLOC;
    double *work = (double *)malloc(count*(count+12u)*sizeof(double));
    if (!work) return HOBBS_ODE_ALLOC;
    double *y=work, *prev=y+n, *older=prev+n, *next=older+n;
    double *coarse=next+n, *half=coarse+n, *base=half+n, *scratch=base+n;
    double *a = scratch + 5u*count;
    memcpy(y,state,count*sizeof(double));
    int status = hobbs_ode_eval(rhs,ctx,n,t0,y,scratch);
    if (status) goto bdf_done;
    double initial_norm = 0.0;
    for (int i = 0; i < n; ++i) {
        const double scale = atol + rtol*fabs(y[i]);
        if (!isfinite(scale)) { status=HOBBS_ODE_NONFINITE; goto bdf_done; }
        initial_norm = fmax(initial_norm,fabs(scratch[i])/scale);
    }
    double t=t0, h=(t1-t0)*0.01, hp=0.0, hpp=0.0;
    if (initial_norm > 0.0) h=fmin(h,0.01/initial_norm);
    if (h == 0.0) h=nextafter(t,INFINITY)-t;
    int history=0, last_failure=HOBBS_ODE_OK;
    for (int attempt = 0; attempt < max_steps; ++attempt) {
        const double remaining=t1-t;
        if (h > remaining) h=remaining;
        if (history && h > 2.0*hp) h=2.0*hp;
        const double end=h == remaining ? t1 : t+h;
        if (!(h > 0.0) || end == t) {
            status=last_failure == HOBBS_ODE_NEWTON_FAILED ? last_failure : HOBBS_ODE_STEP_TOO_SMALL;
            goto bdf_done;
        }
        /* Use the representable time increment in the BDF coefficients. */
        h=end-t;
        double error=0.0;
        if (history < 2) {
            const double mid=t+0.5*h;
            const double h1=mid-t, h2=end-mid;
            if (!(h1 > 0.0) || !(h2 > 0.0)) { status=HOBBS_ODE_STEP_TOO_SMALL; goto bdf_done; }
            memcpy(coarse,y,count*sizeof(double));
            status=hobbs_ode_newton(rhs,ctx,n,end,y,h,y,rtol,atol,coarse,scratch,a);
            if (status) goto bdf_retry;
            memcpy(half,y,count*sizeof(double));
            status=hobbs_ode_newton(rhs,ctx,n,mid,y,h1,y,rtol,atol,half,scratch,a);
            if (status) goto bdf_retry;
            memcpy(next,half,count*sizeof(double));
            status=hobbs_ode_newton(rhs,ctx,n,end,half,h2,y,rtol,atol,next,scratch,a);
            if (status) goto bdf_retry;
            for (int i=0;i<n;++i) {
                const double scale=atol+rtol*fmax(fabs(y[i]),fabs(next[i]));
                if (!isfinite(scale)) { status=HOBBS_ODE_NONFINITE; goto bdf_retry; }
                error=fmax(error,fabs(next[i]-coarse[i])/scale);
            }
        } else {
            const double ratio=h/hp;
            const double a0=(1.0+2.0*ratio)/(1.0+ratio);
            for (int i=0;i<n;++i) {
                base[i]=((1.0+ratio)/a0)*y[i] - (ratio*ratio/((1.0+ratio)*a0))*prev[i];
                next[i]=y[i];
            }
            status=hobbs_ode_newton(rhs,ctx,n,end,base,h/a0,y,rtol,atol,next,scratch,a);
            if (status) goto bdf_retry;
            for (int i=0;i<n;++i) {
                const double dnew=(next[i]-y[i])/h;
                const double dprev=(y[i]-prev[i])/hp;
                const double dold=(prev[i]-older[i])/hpp;
                /* Algebraically h^2*(h+hp)/a0 times the third divided
                 * difference, arranged to avoid cubing tiny/huge steps. */
                const double e=(h/a0)*(h/(h+hp+hpp))*
                    ((dnew-dprev) - ((h+hp)/(hp+hpp))*(dprev-dold));
                const double scale=atol+rtol*fmax(fabs(y[i]),fabs(next[i]));
                if (!isfinite(e) || !isfinite(scale)) { status=HOBBS_ODE_NONFINITE; goto bdf_retry; }
                error=fmax(error,fabs(e)/scale);
            }
        }
        if (error <= 1.0) {
            if (history) memcpy(older,prev,count*sizeof(double));
            memcpy(prev,y,count*sizeof(double));
            memcpy(y,next,count*sizeof(double));
            hpp=hp; hp=h; t=end;
            const int order=history < 2 ? 1 : 2;
            if (history < 2) ++history;
            last_failure=HOBBS_ODE_OK;
            if (t == t1) { memcpy(state,y,count*sizeof(double)); status=HOBBS_ODE_OK; goto bdf_done; }
            const double factor=error == 0.0 ? 2.0 : fmax(0.2,fmin(2.0,0.8*pow(error,-1.0/(order+1.0))));
            h*=factor;
        } else {
            last_failure=HOBBS_ODE_OK;
            const double exponent=history < 2 ? -0.5 : -1.0/3.0;
            h*=fmax(0.1,fmin(0.8,0.8*pow(error,exponent)));
        }
        continue;
bdf_retry:
        last_failure=status;
        h*=0.25;
    }
    status=HOBBS_ODE_MAX_STEPS;
bdf_done:
    free(work);
    return status;
}

#endif
