using Optim, LinearAlgebra, CSV, FastGaussQuadrature, QuadGK, DataFrames
using Random, Distributions, LineSearches,  SpecialFunctions, Plots, ApproxFun
import ForwardDiff
# Pkg.add( )

Random.seed!(123)

root = @__DIR__
while basename(root) != "overlappingauctions"
    global root = dirname(root)
end

# make directories
include(joinpath(root, "data.R"))

# load data for computing likelihood
df = CSV.read(joinpath(ddir, "clean_data", "lik_data_2.csv"), missingstring = "NA")

# weights for gauss-hermite integration
# bump this mumber up to increase precision
nodes, weights = gausshermite(30)

function f(s, mu, sig)
    pdf(Normal(mu, sig), s)
end

function F(s, mu, sig)
    cdf(Normal(mu, sig), s)
end

# transaction liklihood
function f_transact(t, lam, mu_e, mu_t, sig_e, sig_t)
    a1 = (lam ^ 2 * exp(-lam)) / (1 - exp(-lam) - lam * exp(-lam))
    a2 = (sqrt(2) * sig_t * sig_e) / (sqrt(sig_t ^ 2 + sig_e ^ 2))
    a3 = 1 / (2 * pi * sqrt(sig_e * sig_t))
    a4 = exp(- (t - mu_t + mu_e) ^ 2 / (2 * (sig_t ^ 2 + sig_e ^ 2)))
    a = a1 * a2 * a3 * a4

    function integrand(u)
        s = (sqrt(2) * sig_t * sig_e) / (sqrt(sig_t ^ 2 + sig_e ^ 2)) * u + (sig_e ^ 2 * (t - mu_t) + sig_t ^ 2 * mu_e) / (sig_t ^ 2 + sig_e ^ 2)
        F_e_s = F(s, mu_e, sig_e)
        G = (1 - F_e_s) * exp(lam * F_e_s)
        return G
    end

    b = sum(dot(integrand.(nodes), weights))
    return a * b
end

function make_likelihood(pars, resid_col, lam_col)
    mu_e = pars[1]
    sig_e = pars[2]
    sig_t = pars[3]
    mu_t = 0
    function local_f(r, l)
        f_transact(r, l, mu_e, mu_t, sig_e, sig_t)
    end
    out = local_f.(resid_col, lam_col)
    return - sum(log.(out))
end

function do_optimization(mod_name, colr, coll)
    function tmp(x)
        make_likelihood(x, colr, coll)
    end
    initial_x = [0, .5, .5]
    lower = [-20.0, 0, 0]
    upper = [20.0, 25, 25]
    inner_optimizer = LBFGS(linesearch=LineSearches.BackTracking())
    optimal = optimize(
        tmp,
        lower,
        upper,
        initial_x,
        Fminbox(inner_optimizer),
        Optim.Options(show_trace = true, show_every = 10);
        autodiff = :forward
    )
    hess = ForwardDiff.hessian(tmp, optimal.minimizer)
    var_cov = inv(hess)

    return DataFrame(
        Model = mod_name,
        Paramerer = ["mu_e", "sig_e", "sig_t"],
        EST = optimal.minimizer,
        SE = sqrt.(var_cov[diagind(var_cov)]),
        m2ll = 2 * optimal.minimum,
        N = length(coll)
    )
end

df1 = do_optimization(
    "Mod1 w/ N",
    df[df.resid1 .!== missing, :resid1],
    df[df.resid1 .!== missing, :Lambda]
)

df5 = do_optimization(
    "Mod5 w/ N",
    df[df.resid5 .!== missing, :resid5],
    df[df.resid5 .!== missing, :Lambda]
)

df1a = do_optimization(
    "Mod1 no N",
    df[df.resid1a .!== missing, :resid1a],
    df[df.resid1a .!== missing, :Lambda]
)

df5a = do_optimization(
    "Mod5 no N",
    df[df.resid5a .!== missing, :resid5a],
    df[df.resid5a .!== missing, :Lambda]
)

# Save the true parameterization of our models in a nice csv table
solved_params = [df1; df5; df1a; df5a]
CSV.write(joinpath(ddir, "clean_data", "solved_lik.csv"), solved_params)

# Simulate 1000 values of log t and log e each for each of the 4 models
# Save this as a table, then we can randomly sample from these values to
# generate residuals for our counterfactual estimation

function make_samples(pars, model_name)
    mu_e = pars[1]
    sig_e = pars[2]
    sig_t = pars[3]
    mu_t = 0

    function f_e_tr(x)
        f(x, mu_e, sig_e)
    end
    f_e_true_sim = Fun(f_e_tr, -10..10)
    random_samples_e = ApproxFun.sample(f_e_true_sim, 1000)

    f_plot_e = DataFrame(
        model = model_name,
        series = "f_e",
        x = [-10:.01:10;],
        y = f_e_tr.([-10:.01:10;])
    )

    function f_t_tr(x)
        f(x, mu_t, sig_t)
    end
    f_t_true_sim = Fun(f_t_tr, -10..10)
    random_samples_t = ApproxFun.sample(f_t_true_sim, 1000)

    f_plot_t = DataFrame(
        model = model_name,
        series = "f_t",
        x = [-10:.01:10;],
        y = f_t_tr.([-10:.01:10;])
    )

    samples = DataFrame(
        f_e = random_samples_e,
        f_t = random_samples_t,
        model = model_name
    )
    return (samples = samples, f_plot = [f_plot_e; f_plot_t])
end

dfs1, dfp1 = make_samples(df1.EST, "Mod1 w/ N")
dfs1a, dfp1a = make_samples(df1a.EST, "Mod1 no N")
dfs5, dfp5 = make_samples(df5.EST, "Mod5 w/ N")
dfs5a, dfp5a = make_samples(df5a.EST, "Mod5 no N")

dfs = [dfs1;dfs1a;dfs5;dfs5a]
dfp = [dfp1;dfp1a;dfp5;dfp5a]

CSV.write(joinpath(ddir, "clean_data", "samples_dat.csv"), dfs)
CSV.write(joinpath(ddir, "clean_data", "plotting_dat.csv"), dfp)
