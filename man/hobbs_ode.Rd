\name{hobbs_ode}
\alias{hobbs_ode}
\title{Ordinary differential equations in HOBBS models}
\description{
Adaptive Dormand--Prince Runge--Kutta 5(4) for non-stiff ODEs and
backward differentiation formulas of orders 1 and 2 for stiff ODEs,
implemented in plain C without additional package or system dependencies.
These constructs belong to the model language; they are not R functions.
}
\section{ODE declarations}{
Declare a system at the top level using \code{ode name(n_states) \{ ... \}}.
The dimension is a positive integer literal or a scalar data name. Inside
its body, \code{t} is the current solver time, \code{y(i)} is state component
\code{i}, and \code{dy(i)} is its derivative. Indexing is one-based.
Every derivative must be assigned on every evaluation. The body can use
ordinary C arithmetic, local variables, conditionals, and HOBBS loops.
Parameter accessors and data accessors remain available. The solver evaluates
the current parameter vector, including the proposed value during a proposal.
ODEs do not introduce separately sampled parameters or require gradients.

Local variables and block indices outside the ODE body are not captured.
Pass subject-specific values through the optional input vector; inside the
ODE, these are accessed with \code{input(i)}. An omitted or out-of-range
input produces a failed solve. State and derivative indices must be in
\code{1:n_states}, as with other model array accessors.
The ODE accessor names \code{y}, \code{dy}, \code{input}, and time name
\code{t} are reserved within an ODE body.
}
\section{Solver calls}{
Inside a posterior block or a \code{func} expanded into a posterior block,
use \code{ode_rk45(name, state, t0, t1);} or
\code{ode_rk45(name, state, t0, t1, inputs);}.
For stiff systems, replace \code{ode_rk45} with \code{ode_bdf}.
Both methods use the same ODE declaration and arguments.
Place each call on its own line as a statement. The solver overwrites the
local \code{vec} named \code{state} with its value at \code{t1}; its size
must match the ODE dimension. The optional \code{inputs} must also be a
local \code{vec}. Both vectors must be declared in the calling scope.

Default relative and absolute tolerances are \code{1e-6} and \code{1e-8}.
The step limit is \code{100000} attempted steps per call, including rejected
steps. For explicit controls, use
\code{ode_rk45_tol(name, state, t0, t1, rtol, atol, max_steps);} or append
\code{inputs} as the final argument. The stiff counterpart is
\code{ode_bdf_tol}, with exactly the same arguments. Tolerances must be finite and positive;
the step limit must be a positive integer no greater than \code{INT_MAX}.
The scaled error is the maximum across states of the local error estimate divided
by \code{atol + rtol * max(abs(old_state), abs(new_state))}.

Times must be finite with \code{t1 >= t0}. Equal times leave the state
unchanged. To evaluate multiple observation times, initialize the state once
per posterior evaluation and advance it successively to each time. The solver
clips its steps at each requested endpoint. It restarts its step-size search
at each call. For dosing, resets, or discontinuities, split integration at the
change time, modify the state explicitly, then integrate the next interval.
The solver does not implement event detection or automatic dosing.
}
\section{Failures and limitations}{
Invalid inputs, non-finite states or derivatives, unassigned derivatives,
allocation failure, step exhaustion, and step sizes too small to advance time
cause an immediate \code{-Inf} return from the enclosing posterior block.
The state is unchanged on a failed call. Invalid proposals are rejected by
the sampler; a non-finite initial posterior remains a model initialization
error. Calls are not supported in cache/update bodies or void C helpers.

RK45 is intended for non-stiff systems. BDF is intended for stiff systems
and uses backward-Euler startup followed by variable-step BDF2. Its Newton
iterations use a dense finite-difference Jacobian and partial-pivoting linear
solves. No analytic Jacobian or model gradients are required. Failed Newton
iterations retry with smaller steps, within the attempted-step budget.

BDF uses dense quadratic workspace and cubic linear-solve work in the number
of states, so it is intended for small to moderate systems. This implementation
provides orders 1 and 2, rather than the full order-1-through-5 CVODE algorithm.
Both solvers restart their internal history at each call. BDF will typically
be slower than RK45 on non-stiff systems. Neither solver provides dense output,
event detection, backward integration, or sensitivity equations.
Choose tolerances appropriate for the state scales and assess posterior
stability under tighter tolerances. A solve's local error control is not a
guarantee of its global error or of posterior accuracy.
}
\section{Plain C API}{
The installed \code{hobbs_model.h} includes \code{hobbs_ode.h}.
Advanced C callers can use
\code{hobbs_ode_rk45(rhs, context, n, state, t0, t1, rtol, atol, max_steps)}
or \code{hobbs_ode_bdf} with the same arguments.
The callback signature is
\code{void rhs(double t, const double *state, double *derivative, void *context)}.
The C arrays use zero-based indexing. The integer status is one of
\code{HOBBS_ODE_OK}, \code{HOBBS_ODE_INVALID}, \code{HOBBS_ODE_ALLOC},
\code{HOBBS_ODE_NONFINITE}, \code{HOBBS_ODE_MAX_STEPS}, or
\code{HOBBS_ODE_STEP_TOO_SMALL}, or \code{HOBBS_ODE_NEWTON_FAILED}. C callers must check the status themselves.
Workspace allocation and callback context are local to each call.
}
\seealso{\code{\link{hobbs}}}

\section{BDF error control}{
The first two accepted steps use backward-Euler step doubling for startup
error estimates. Subsequent steps use variable-step BDF2 and a third divided
difference to estimate local truncation error. Step growth is limited to a
factor of two. Newton residuals must fall below 0.02 times the state tolerance
scale; each implicit solve permits up to 12 Newton iterations with damping.
The attempted-step budget counts failed error tests and failed Newton solves
as well as accepted steps; it does not count individual Newton iterations.
}
