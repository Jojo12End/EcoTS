eco_auto <- 
function (var1, var2 = NULL, name1 = NULL, name2 = NULL, max_lag = 8, run_bubbles = TRUE, plot = TRUE) 
{
    if (is.null(name1)) 
        name1 <- gsub(".*\\$", "", deparse(substitute(var1)))
    if (!is.null(var2) && is.null(name2)) 
        name2 <- gsub(".*\\$", "", deparse(substitute(var2)))
    y1 <- as.numeric(stats::na.omit(var1))
    res_out <- list()
    cat("\n====================================================================\n")
    cat("   ANALYSE ECONOMETRIQUE AUTOMATISEE (SEUIL DE SIGNIFICATIVITE : 5%)\n")
    cat("====================================================================\n")
    test_stationarity_5pct <- function(serie, sname) {
        cat(sprintf("\n--- [1] TESTS DE STATIONNARITE POUR : %s (T = %d) ---\n", sname, length(serie)))
        eff_lag <- max(1L, min(as.integer(max_lag), floor((length(serie) - 1)/3)))
        adf3 <- urca::ur.df(serie, type = "trend", lags = eff_lag, selectlags = "AIC")
        tau3 <- adf3@teststat[1, "tau3"]
        cv_tau3 <- adf3@cval["tau3", "5pct"]
        phi3 <- adf3@teststat[1, "phi3"]
        cv_phi3 <- adf3@cval["phi3", "5pct"]
        cat(sprintf("  [ADF Modele 3 - Trend] tau3 = %.4f (Seuil 5%% = %.4f) | phi3 = %.4f (Seuil 5%% = %.4f)\n", tau3, cv_tau3, phi3, cv_phi3))
        spec_retenue <- "trend"
        is_stat_adf <- FALSE
        if (tau3 < cv_tau3) {
            coefs3 <- adf3@testreg$coefficients
            t_trend <- if ("tt" %in% rownames(coefs3)) 
                coefs3["tt", "t value"]
            else 0
            if (abs(t_trend) > 1.96) {
                cat("  -> Rejet H0 (tau3 < seuil) et tendance significative (|t| > 1.96) : Serie TS.\n")
                is_stat_adf <- TRUE
            }
            else {
                cat("  -> Tendance non significative (|t| <= 1.96) -> Passage au Modele 2 (Drift).\n")
                spec_retenue <- "drift"
            }
        }
        else {
            if (phi3 > cv_phi3) {
                cat("  -> Non-rejet H0 (racine unitaire) mais phi3 significatif -> Serie I(1) avec derive.\n")
                is_stat_adf <- FALSE
            }
            else {
                cat("  -> Non-rejet H0 et tendance non significative -> Passage au Modele 2 (Drift).\n")
                spec_retenue <- "drift"
            }
        }
        adf2 <- NULL
        if (spec_retenue == "drift") {
            adf2 <- urca::ur.df(serie, type = "drift", lags = eff_lag, selectlags = "AIC")
            tau2 <- adf2@teststat[1, "tau2"]
            cv_tau2 <- adf2@cval["tau2", "5pct"]
            phi1 <- adf2@teststat[1, "phi1"]
            cv_phi1 <- adf2@cval["phi1", "5pct"]
            cat(sprintf("  [ADF Modele 2 - Drift] tau2 = %.4f (Seuil 5%% = %.4f) | phi1 = %.4f (Seuil 5%% = %.4f)\n", tau2, cv_tau2, phi1, cv_phi1))
            if (tau2 < cv_tau2) {
                coefs2 <- adf2@testreg$coefficients
                t_const <- if ("(Intercept)" %in% rownames(coefs2)) 
                  coefs2["(Intercept)", "t value"]
                else 0
                if (abs(t_const) > 1.96) {
                  cat("  -> Rejet H0 (tau2 < seuil) et constante significative : Serie I(0) avec constante.\n")
                  is_stat_adf <- TRUE
                }
                else {
                  cat("  -> Constante non significative -> Passage au Modele 1 (None).\n")
                  spec_retenue <- "none"
                }
            }
            else {
                if (phi1 > cv_phi1) {
                  cat("  -> Non-rejet H0 (racine unitaire) mais phi1 significatif -> Serie DS avec derive.\n")
                  is_stat_adf <- FALSE
                }
                else {
                  cat("  -> Non-rejet H0 et constante non significative -> Passage au Modele 1 (None).\n")
                  spec_retenue <- "none"
                }
            }
        }
        adf1 <- NULL
        if (spec_retenue == "none") {
            adf1 <- urca::ur.df(serie, type = "none", lags = eff_lag, selectlags = "AIC")
            tau1 <- adf1@teststat[1, "tau1"]
            cv_tau1 <- adf1@cval["tau1", "5pct"]
            cat(sprintf("  [ADF Modele 1 - None]  tau1 = %.4f (Seuil 5%% = %.4f)\n", tau1, cv_tau1))
            if (tau1 < cv_tau1) {
                cat("  -> Rejet H0 (tau1 < seuil) : Serie I(0) stationnaire sans constante.\n")
                is_stat_adf <- TRUE
            }
            else {
                cat("  -> Non-rejet H0 (tau1 >= seuil) : Serie NON STATIONNAIRE (DS sans derive).\n")
                is_stat_adf <- FALSE
            }
        }
        kpss_type <- if (spec_retenue == "trend") 
            "tau"
        else "mu"
        kpss_res <- urca::ur.kpss(serie, type = kpss_type, lags = "short")
        stat_kpss <- kpss_res@teststat
        cv_kpss <- kpss_res@cval[1, "5pct"]
        is_stat_kpss <- (stat_kpss <= cv_kpss)
        cat(sprintf("  [KPSS (%s, lags='short')] Stat = %.4f (Seuil 5%% = %.4f) -> %s\n", kpss_type, stat_kpss, cv_kpss, ifelse(is_stat_kpss, "Non-rejet H0 (Stationnaire)", "Rejet H0 (Non stationnaire)")))
        stationary <- is_stat_adf && is_stat_kpss
        if (!stationary) {
            cat(sprintf("  ==> DIAGNOSTIC FINAL %s : NON STATIONNAIRE (I(1)) -> Differenciation requise.\n", sname))
        }
        else {
            cat(sprintf("  ==> DIAGNOSTIC FINAL %s : STATIONNAIRE (I(0)) (spec : %s).\n", sname, spec_retenue))
        }
        list(stationary = stationary, spec = spec_retenue, adf3 = adf3, adf2 = adf2, adf1 = adf1, kpss = kpss_res)
    }
    st1 <- test_stationarity_5pct(y1, name1)
    res_out$stationarity_var1 <- st1
    if (!st1$stationary) {
        d_name1 <- paste0("d_", name1)
        y1_stat <- diff(y1)
        cat(sprintf("\n>>> Differenciation automatique appliquee : %s = diff(%s)\n", d_name1, name1))
        st1_diff <- test_stationarity_5pct(y1_stat, d_name1)
        res_out$stationarity_d_var1 <- st1_diff
        order_int1 <- 1
        stat_name1 <- d_name1
        det_var1 <- st1_diff$spec
    }
    else {
        y1_stat <- y1
        order_int1 <- 0
        stat_name1 <- name1
        det_var1 <- st1$spec
    }
    if (run_bubbles) {
        cat(sprintf("\n--- [2] TESTS DE BULLES SPECULATIVES (SADF / GSADF) SUR : %s ---\n", name1))
        if (requireNamespace("exuber", quietly = TRUE)) {
            tryCatch({
                radf_fit <- exuber::radf(y1, lag = 1)
                cv_mc <- exuber::radf_mc_cv(n = length(y1), nrep = 300, seed = 123)
                sadf_stat <- as.numeric(radf_fit$sadf)
                gsadf_stat <- as.numeric(radf_fit$gsadf)
                sadf_cv5 <- as.numeric(cv_mc$sadf_cv["95%"])
                gsadf_cv5 <- as.numeric(cv_mc$gsadf_cv["95%"])
                cat(sprintf("  SADF  = %.4f (Seuil 5%% MC = %.4f) -> %s\n", sadf_stat, sadf_cv5, ifelse(sadf_stat > sadf_cv5, "BULLE DETECTEE (Rejet H0)", "Pas de bulle (Non-rejet H0)")))
                cat(sprintf("  GSADF = %.4f (Seuil 5%% MC = %.4f) -> %s\n", gsadf_stat, gsadf_cv5, ifelse(gsadf_stat > gsadf_cv5, "BULLES MULTIPLES DETECTEES (Rejet H0)", "Pas de bulle (Non-rejet H0)")))
                if (gsadf_stat > gsadf_cv5) {
                  ds <- exuber::datestamp(radf_fit, cv = cv_mc, min_duration = exuber::psy_ds(y1), option = "gsadf")
                  cat("  Dates (indices) des episodes explosifs :\n")
                  print(ds)
                }
                res_out$bubbles_var1 <- list(radf = radf_fit, cv = cv_mc)
            }, error = function(e) {
                cat("  [Info] Test exuber ignore :", e$message, "\n")
            })
        }
        else {
            cat("  [Info] Package 'exuber' non installe sur ce poste (etape ignoree).\n")
        }
    }
    cat(sprintf("\n--- [3] MODELISATION UNIVARIEE ARMA & DIAGNOSTICS SUR : %s ---\n", stat_name1))
    if (plot) {
        op <- graphics::par(mfrow = c(2, 2))
        graphics::plot(y1, type = "l", main = paste("Niveau :", name1), ylab = name1)
        graphics::plot(y1_stat, type = "l", main = paste("Stationnaire :", stat_name1), ylab = stat_name1)
        stats::acf(y1_stat, lag.max = 24, main = paste("FAC :", stat_name1))
        stats::pacf(y1_stat, lag.max = 24, main = paste("FAP :", stat_name1))
        graphics::par(op)
    }
    best_arma <- forecast::auto.arima(y1_stat, max.p = 4, max.q = 4, d = 0, seasonal = FALSE, ic = "bic", stepwise = FALSE, approximation = FALSE)
    ord <- forecast::arimaorder(best_arma)
    cat(sprintf("  Modele optimal (BIC) : ARMA(%d, %d)\n", ord["p"], ord["q"]))
    print(lmtest::coeftest(best_arma))
    res_arma <- stats::residuals(best_arma)
    p_plus_q <- as.integer(ord["p"] + ord["q"])
    lag_lb <- max(10L, p_plus_q + 2L)
    lb <- stats::Box.test(res_arma, lag = lag_lb, type = "Ljung-Box", fitdf = p_plus_q)
    jb <- tseries::jarque.bera.test(res_arma)
    arch <- FinTS::ArchTest(res_arma, lags = 4, demean = FALSE)
    cat(sprintf("  [Ljung-Box lag=%d] Q* = %.4f, p-val = %.4f -> %s\n", lag_lb, lb$statistic, lb$p.value, ifelse(lb$p.value > 0.05, "Bruit blanc valide (p > 0.05)", "Autocorrelation residuelle (p < 0.05)")))
    cat(sprintf("  [Jarque-Bera]      JB = %.4f, p-val = %.4f (Skew=%.2f, Kurt=%.2f) -> %s\n", jb$statistic, jb$p.value, moments::skewness(res_arma), moments::kurtosis(res_arma), ifelse(jb$p.value > 0.05, "Normalite validee", "Rejet normalite (p < 0.05)")))
    cat(sprintf("  [ARCH-LM lag=4]    LM = %.4f, p-val = %.4f -> %s\n", arch$statistic, arch$p.value, ifelse(arch$p.value > 0.05, "Homoscedasticite (pas d'effet ARCH)", "Effet ARCH present (p < 0.05)")))
    fc_arma <- forecast::forecast(best_arma, h = 5)
    res_out$arma_var1 <- list(model = best_arma, lb = lb, jb = jb, arch = arch, forecast = fc_arma)
    if (!is.null(var2)) {
        y2 <- as.numeric(stats::na.omit(var2))
        st2 <- test_stationarity_5pct(y2, name2)
        res_out$stationarity_var2 <- st2
        if (!st2$stationary) {
            d_name2 <- paste0("d_", name2)
            y2_stat <- diff(y2)
            cat(sprintf("\n>>> Differenciation automatique appliquee : %s = diff(%s)\n", d_name2, name2))
            st2_diff <- test_stationarity_5pct(y2_stat, d_name2)
            res_out$stationarity_d_var2 <- st2_diff
            order_int2 <- 1
            stat_name2 <- d_name2
            det_var2 <- st2_diff$spec
        }
        else {
            y2_stat <- y2
            order_int2 <- 0
            stat_name2 <- name2
            det_var2 <- st2$spec
        }
        if (order_int1 == 1 && order_int2 == 1) {
            cat(sprintf("\n--- [4A] TEST DE COINTEGRATION DE JOHANSEN (%s et %s sont I(1)) ---\n", name1, name2))
            n_lvl <- min(length(y1), length(y2))
            Y_lvl <- cbind(utils::tail(y1, n_lvl), utils::tail(y2, n_lvl))
            colnames(Y_lvl) <- c(name1, name2)
            eff_vlag_lvl <- max(2L, min(as.integer(max_lag), floor((n_lvl - 1)/4)))
            vsel_lvl <- vars::VARselect(Y_lvl, lag.max = eff_vlag_lvl, type = "const")
            k_raw <- vsel_lvl$selection["SC(n)"]
            K_opt <- if (is.na(k_raw)) 
                2L
            else max(2L, as.integer(k_raw))
            cat(sprintf("  Retards en niveau retenus (BIC) : K = %d\n", K_opt))
            jo_trace <- urca::ca.jo(Y_lvl, type = "trace", ecdet = "const", K = K_opt, spec = "transitory")
            stat_r0 <- jo_trace@teststat[2]
            cv5_r0 <- jo_trace@cval[2, "5pct"]
            stat_r1 <- jo_trace@teststat[1]
            cv5_r1 <- jo_trace@cval[1, "5pct"]
            cat(sprintf("  Test Trace r = 0  : Stat = %.4f vs Seuil 5%% = %.4f -> %s\n", stat_r0, cv5_r0, ifelse(stat_r0 > cv5_r0, "Rejet H0 (au moins 1 relation)", "Non-rejet H0 (r = 0)")))
            cat(sprintf("  Test Trace r <= 1 : Stat = %.4f vs Seuil 5%% = %.4f -> %s\n", stat_r1, cv5_r1, ifelse(stat_r1 > cv5_r1, "Rejet H0 (r = 2)", "Non-rejet H0")))
            if (stat_r0 > cv5_r0 && stat_r1 <= cv5_r1) {
                cat("  ==> CONCLUSION JOHANSEN : 1 relation de cointegration (r = 1). Estimation du VECM :\n")
                vecm_fit <- urca::cajorls(jo_trace, r = 1)
                print(summary(vecm_fit$rlm))
                cat("  Vecteur de cointegration (beta) :\n")
                print(vecm_fit$beta)
                res_out$johansen <- list(test = jo_trace, vecm = vecm_fit, rank = 1)
            }
            else {
                cat("  ==> CONCLUSION JOHANSEN : Pas de cointegration (r = 0) -> Le VAR en differences est valide.\n")
                res_out$johansen <- list(test = jo_trace, rank = 0)
            }
        }
        cat(sprintf("\n--- [4B] MODELE VAR STATIONNAIRE (%s, %s) ---\n", stat_name1, stat_name2))
        n_stat <- min(length(y1_stat), length(y2_stat))
        Y_stat <- data.frame(utils::tail(y1_stat, n_stat), utils::tail(y2_stat, n_stat))
        colnames(Y_stat) <- c(stat_name1, stat_name2)
        var_type <- if (det_var1 == "none" && det_var2 == "none") 
            "none"
        else "const"
        eff_vlag <- max(1L, min(as.integer(max_lag), floor((n_stat - 1)/4)))
        vsel <- vars::VARselect(Y_stat, lag.max = eff_vlag, type = var_type)
        p_raw <- vsel$selection["AIC(n)"]
        p_opt <- if (is.na(p_raw)) 
            1L
        else max(1L, as.integer(p_raw))
        cat("  Selection des retards (VARselect) :\n")
        print(vsel$selection)
        cat(sprintf("  --> Estimation d'un VAR(%d) avec type = '%s'\n", p_opt, var_type))
        var_fit <- vars::VAR(Y_stat, p = p_opt, type = var_type)
        print(summary(var_fit))
        max_root <- max(vars::roots(var_fit))
        ser_var <- vars::serial.test(var_fit, lags.pt = max(10L, p_opt + 4L), type = "PT.adjusted")
        norm_var <- vars::normality.test(var_fit, multivariate.only = TRUE)
        arch_var <- vars::arch.test(var_fit, lags.multi = 4, multivariate.only = TRUE)
        cat(sprintf("  [Stabilite VAR] Max module racines = %.4f -> %s\n", max_root, ifelse(max_root < 1, "VAR STABLE (< 1)", "ATTENTION : VAR INSTABLE (>= 1)")))
        cat(sprintf("  [Portmanteau]   p-val = %.4f | [Jarque-Bera multiv.] p-val = %.4f | [ARCH multiv.] p-val = %.4f\n", ser_var$serial$p.value, norm_var$jb.mul$JB$p.value, arch_var$arch.mul$p.value))
        cat("\n  --- Tests de Causalite au sens de Granger (5%) ---\n")
        g1 <- vars::causality(var_fit, cause = stat_name1)$Granger
        g2 <- vars::causality(var_fit, cause = stat_name2)$Granger
        cat(sprintf("  %s -> %s : F = %.4f, p-val = %.4f -> %s\n", stat_name1, stat_name2, g1$statistic, g1$p.value, ifelse(g1$p.value < 0.05, "CAUSE (Rejet H0)", "Ne cause pas (Non-rejet H0)")))
        cat(sprintf("  %s -> %s : F = %.4f, p-val = %.4f -> %s\n", stat_name2, stat_name1, g2$statistic, g2$p.value, ifelse(g2$p.value < 0.05, "CAUSE (Rejet H0)", "Ne cause pas (Non-rejet H0)")))
        var_fit$call <- bquote(vars::VAR(y = .(Y_stat), p = .(p_opt), type = .(var_type)))
        set.seed(123)
        irf_var <- vars::irf(var_fit, n.ahead = 12, ortho = TRUE, ci = 0.9)
        fevd_var <- vars::fevd(var_fit, n.ahead = 12)
        if (plot) {
            op <- graphics::par(mfrow = c(2, 2))
            for (choc in c(stat_name1, stat_name2)) {
                for (rep in c(stat_name1, stat_name2)) {
                  y_irf <- irf_var$irf[[choc]][, rep]
                  inf <- irf_var$Lower[[choc]][, rep]
                  sup <- irf_var$Upper[[choc]][, rep]
                  h <- seq_along(y_irf) - 1
                  graphics::plot(h, y_irf, type = "l", ylim = range(inf, sup), main = paste("IRF VAR :", choc, "->", rep), xlab = "Horizon h", ylab = "")
                  graphics::lines(h, inf, lty = 2, col = "red")
                  graphics::lines(h, sup, lty = 2, col = "red")
                  graphics::abline(h = 0, col = "grey")
                }
            }
            graphics::par(op)
        }
        cat("\n  --- Projections locales de Jorda (lpirfs) ---\n")
        irf_lp <- tryCatch({
            lp_res <- lpirfs::lp_lin(endog_data = Y_stat, lags_endog_lin = p_opt, trend = 0, shock_type = 0, confint = 1.645, hor = 12, adjust_se = TRUE)
            if (plot) 
                plot(lp_res)
            lp_res
        }, error = function(e) {
            cat("  [Info] Projections locales ignorees :", e$message, "\n")
            NULL
        })
        res_out$var <- list(model = var_fit, granger_1to2 = g1, granger_2to1 = g2, irf = irf_var, fevd = fevd_var, lp = irf_lp)
    }
    cat("\n====================================================================\n")
    cat("   FIN DE L'ANALYSE. Tous les objets sont stockes dans le resultat.\n")
    cat("====================================================================\n")
    invisible(res_out)
}
