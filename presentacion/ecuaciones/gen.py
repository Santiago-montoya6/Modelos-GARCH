import subprocess, json, os
from PIL import Image
eqs = {
 "eq_modelo": r"r_t=\mu+\varepsilon_t,\qquad \varepsilon_t=\sigma_t\,z_t,\qquad z_t\overset{\text{iid}}{\sim}(0,1)",
 "eq_condicional": r"\mathrm{E}(\varepsilon_t\mid\mathcal{F}_{t-1})=0,\qquad \mathrm{Var}(\varepsilon_t\mid\mathcal{F}_{t-1})=\sigma_t^2",
 "eq_arch": r"\sigma_t^2=\omega+\sum_{i=1}^{q}\alpha_i\,\varepsilon_{t-i}^2",
 "eq_garch": r"\sigma_t^2=\omega+\sum_{i=1}^{q}\alpha_i\,\varepsilon_{t-i}^2+\sum_{j=1}^{p}\beta_j\,\sigma_{t-j}^2",
 "eq_archinf": r"\sigma_t^2=\frac{\omega}{1-\beta}+\alpha\sum_{k\ge 1}\beta^{\,k-1}\varepsilon_{t-k}^2",
 "eq_arma": r"\varepsilon_t^2=\omega+(\alpha+\beta)\,\varepsilon_{t-1}^2+\nu_t-\beta\,\nu_{t-1}",
 "eq_garch11": r"\sigma_t^2=\omega+\alpha\,\varepsilon_{t-1}^2+\beta\,\sigma_{t-1}^2",
 "eq_segundo": r"\alpha+\beta<1",
 "eq_estricta": r"\mathrm{E}\,\ln\!\big(\beta+\alpha z_t^2\big)<0",
 "eq_cuarto": r"(\alpha+\beta)^2+(\kappa_z-1)\,\alpha^2<1",
 "eq_varinc": r"\sigma^2=\frac{\omega}{1-\alpha-\beta}",
 "eq_vidamedia": r"h_{1/2}=\frac{\ln 0{,}5}{\ln(\alpha+\beta)}",
 "eq_pronostico": r"\mathrm{E}_t\,\sigma_{t+k}^2=\sigma^2+(\alpha+\beta)^{k-1}\big(\sigma_{t+1}^2-\sigma^2\big)",
 "eq_var": r"\mathrm{VaR}_{t+1}(a)=\hat\mu+\hat\sigma_{t+1}\,q_a",
 "eq_jensen": r"\mathrm{E}\ln(\beta+\alpha z_t^2)\le\ln(\alpha+\beta)",
}
dims = {}
for k, e in eqs.items():
    tex = r"""\documentclass[border=2pt]{standalone}
\usepackage{amsmath,amssymb,xcolor}\usepackage{lmodern}
\definecolor{t}{HTML}{1E1E1E}
\begin{document}\color{t}$\displaystyle %s$\end{document}""" % e
    open(k + ".tex", "w").write(tex)
    subprocess.run(["pdflatex", "-interaction=nonstopmode", k + ".tex"], check=True, capture_output=True)
    subprocess.run(["pdftoppm", "-png", "-r", "600", "-singlefile", k + ".pdf", k], check=True)
    im = Image.open(k + ".png"); dims[k] = im.size
    for ext in (".aux", ".log", ".tex", ".pdf"): os.remove(k + ext)
json.dump(dims, open("dims.json", "w"), indent=1)
print(dims)
