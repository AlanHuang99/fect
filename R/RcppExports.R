# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

wE_adj <- function(E, FE, W, I) {
    .Call(`_fect_wE_adj`, E, FE, W, I)
}

loglh <- function(Y_fit, Y) {
    .Call(`_fect_loglh`, Y_fit, Y)
}

loglh_ub <- function(Y_fit, Y, I) {
    .Call(`_fect_loglh_ub`, Y_fit, Y, I)
}

data_ub_adj <- function(I_data, data) {
    .Call(`_fect_data_ub_adj`, I_data, data)
}

XXinv <- function(X) {
    .Call(`_fect_XXinv`, X)
}

wXXinv <- function(X, w) {
    .Call(`_fect_wXXinv`, X, w)
}

panel_beta <- function(X, xxinv, Y, FE) {
    .Call(`_fect_panel_beta`, X, xxinv, Y, FE)
}

wpanel_beta <- function(X, xwxinv, w, Y, FE) {
    .Call(`_fect_wpanel_beta`, X, xwxinv, w, Y, FE)
}

panel_est <- function(X, Y, MF) {
    .Call(`_fect_panel_est`, X, Y, MF)
}

inter_fe_d_qr <- function(Y, Y_fit0, FE0, factor0, xi0, X, r, force, mniter = 5000L, w = 1.0, tol = 1e-5) {
    .Call(`_fect_inter_fe_d_qr`, Y, Y_fit0, FE0, factor0, xi0, X, r, force, mniter, w, tol)
}

inter_fe_d_qr_ub <- function(Y, Y_fit0, FE0, factor0, xi0, X, I, r, force, mniter = 5000L, w = 1.0, tol = 1e-5) {
    .Call(`_fect_inter_fe_d_qr_ub`, Y, Y_fit0, FE0, factor0, xi0, X, I, r, force, mniter, w, tol)
}

qr_factor <- function(F, L) {
    .Call(`_fect_qr_factor`, F, L)
}

IND <- function(I) {
    .Call(`_fect_IND`, I)
}

subfe <- function(Y, X, I, intercept) {
    .Call(`_fect_subfe`, Y, X, I, intercept)
}

l_ub <- function(Y, F, I, r, force) {
    .Call(`_fect_l_ub`, Y, F, I, r, force)
}

f_ub <- function(Y, L, I, r, force) {
    .Call(`_fect_f_ub`, Y, L, I, r, force)
}

fe <- function(E, F_old, xi_old, force, r) {
    .Call(`_fect_fe`, E, F_old, xi_old, force, r)
}

fe_ub <- function(E, I, F_old, xi_old, force, r) {
    .Call(`_fect_fe_ub`, E, I, F_old, xi_old, force, r)
}

inter_fe_d <- function(Y, Y_fit0, FE0, X, r, force, mniter = 5000L, w = 1.0, tol = 1e-5) {
    .Call(`_fect_inter_fe_d`, Y, Y_fit0, FE0, X, r, force, mniter, w, tol)
}

inter_fe_d_ub <- function(Y, Y_fit0, FE0, X, I, r, force, mniter = 5000L, w = 1.0, tol = 1e-5) {
    .Call(`_fect_inter_fe_d_ub`, Y, Y_fit0, FE0, X, I, r, force, mniter, w, tol)
}

Y_demean <- function(Y, force) {
    .Call(`_fect_Y_demean`, Y, force)
}

Y_wdemean <- function(Y, W, force) {
    .Call(`_fect_Y_wdemean`, Y, W, force)
}

fe_add <- function(alpha_Y, xi_Y, mu_Y, T, N, force) {
    .Call(`_fect_fe_add`, alpha_Y, xi_Y, mu_Y, T, N, force)
}

panel_factor <- function(E, r) {
    .Call(`_fect_panel_factor`, E, r)
}

panel_FE <- function(E, lambda, hard) {
    .Call(`_fect_panel_FE`, E, lambda, hard)
}

ife <- function(E, force, mc, r, hard, lambda) {
    .Call(`_fect_ife`, E, force, mc, r, hard, lambda)
}

inter_fe <- function(Y, X, r, force, beta0, tol = 1e-5, max_iter = 500L) {
    .Call(`_fect_inter_fe`, Y, X, r, force, beta0, tol, max_iter)
}

inter_fe_ub <- function(Y, Y0, X, I, W, beta0, r, force, tol = 1e-5, max_iter = 1000L) {
    .Call(`_fect_inter_fe_ub`, Y, Y0, X, I, W, beta0, r, force, tol, max_iter)
}

fe_ad_iter <- function(Y, Y0, I, W, force, tolerate, max_iter = 500L) {
    .Call(`_fect_fe_ad_iter`, Y, Y0, I, W, force, tolerate, max_iter)
}

fe_ad_covar_iter <- function(XX, xxinv, Y, Y0, I, beta0, W, force, tolerate, max_iter = 500L) {
    .Call(`_fect_fe_ad_covar_iter`, XX, xxinv, Y, Y0, I, beta0, W, force, tolerate, max_iter)
}

fe_ad_inter_iter <- function(Y, Y0, I, W, force, mc, r, hard, lambda, tolerate, max_iter = 1000L) {
    .Call(`_fect_fe_ad_inter_iter`, Y, Y0, I, W, force, mc, r, hard, lambda, tolerate, max_iter)
}

fe_ad_inter_covar_iter <- function(XX, xxinv, Y, Y0, I, W, beta0, force, mc, r, hard, lambda, tolerate, max_iter = 1000L) {
    .Call(`_fect_fe_ad_inter_covar_iter`, XX, xxinv, Y, Y0, I, W, beta0, force, mc, r, hard, lambda, tolerate, max_iter)
}

beta_iter <- function(X, xxinv, Y, r, tolerate, beta0, max_iter) {
    .Call(`_fect_beta_iter`, X, xxinv, Y, r, tolerate, beta0, max_iter)
}

inter_fe_mc <- function(Y, Y0, X, I, W, beta0, r, lambda, force, tol = 1e-5, max_iter = 1000L) {
    .Call(`_fect_inter_fe_mc`, Y, Y0, X, I, W, beta0, r, lambda, force, tol, max_iter)
}

