import numpy as np
import matplotlib.pyplot as plt
import os
import glob
import re

# --- CONFIGURATION ---
path_to_files = "./"
n_ls, n_hours = 360, 24
total_points = n_ls * n_hours  # 8640

# Tolerance de regroupement des latitudes (deg).
# Doit etre >> bruit de precision (~1e-5 en float simple) et << pas de grille.
# Pour une grille MCD (lat ~3.75 deg), 1e-2 laisse une marge de ~2 ordres.
lat_tol = 1e-2

# Nouveau schema d'ecriture (davinci) : Results_i_<LON>_j_<LAT>.txt
#   i = LONGITUDE (valeur), j = LATITUDE (valeur)  -> i/j inverses vs ancien code
# Colonnes du fichier (inchangees) : 0:MCD(ref) 1:Raw 2:NoSens 3:Sensible
fname_regex = re.compile(r"Results_i_(-?\d+(?:\.\d+)?)_j_(-?\d+(?:\.\d+)?)\.txt$")

variants_keys = ['raw', 'nosens', 'sensible']
metrics = ['min', 'max', 'mean']


def build_lat_grid(lat_values, tol=1e-2):
    """Regroupe des latitudes bruitees en noeuds de grille canoniques.
    Retourne (noeuds tries, (nb_valeurs_brutes, nb_noeuds, dispersion_max))."""
    uniq = np.array(sorted(set(lat_values)))
    nodes, spreads, start = [], [], 0
    for k in range(1, len(uniq) + 1):
        if k == len(uniq) or (uniq[k] - uniq[k - 1]) > tol:
            grp = uniq[start:k]
            nodes.append(round(float(np.median(grp)), 3))  # representant "propre"
            spreads.append(float(grp[-1] - grp[0]))
            start = k
    diag = (len(uniq), len(nodes), max(spreads) if spreads else 0.0)
    return np.array(nodes), diag


def analyze_all_variants():
    files = glob.glob(os.path.join(path_to_files, "Results_i_*_j_*.txt"))
    print(f"{len(files)} fichiers detectes.")

    # --- 1er passage : parsing des noms ---
    parsed = []  # (chemin, lon_brut, lat_brut)
    for f in files:
        m = fname_regex.search(os.path.basename(f))
        if m is None:
            continue
        parsed.append((f, float(m.group(1)), float(m.group(2))))

    if not parsed:
        raise FileNotFoundError(
            f"Aucun fichier 'Results_i_<lon>_j_<lat>.txt' dans '{path_to_files}'.")

    # --- Construction de l'axe latitudinal robuste au bruit ---
    lat_grid, (n_raw, n_lat, max_spread) = build_lat_grid(
        [lat for _, _, lat in parsed], tol=lat_tol)
    print(f"Latitudes : {n_raw} valeurs brutes -> {n_lat} noeuds de grille "
          f"(dispersion max intra-noeud : {max_spread:.2e} deg)")
    if max_spread > lat_tol:
        print("  /!\\ dispersion > tolerance : verifier/augmenter lat_tol.")

    def row_of(lat):  # rattache une latitude bruitee au noeud le plus proche
        return int(np.argmin(np.abs(lat_grid - lat)))

    # Sommes zonales [Lat, Ls] + comptage des longitudes presentes par latitude
    sums = {k: {m: np.zeros((n_lat, n_ls)) for m in metrics} for k in variants_keys}
    counts = np.zeros(n_lat)

    # --- 2e passage : lecture des donnees et cumul des differences vs MCD ---
    print("Traitement des fichiers (moyennes zonales)...")
    for idx, (filepath, lon, lat) in enumerate(parsed):
        if (idx + 1) % 2000 == 0:
            print(f"  {idx + 1}/{len(parsed)}")
        try:
            data = np.loadtxt(filepath)
            if data.shape[0] == total_points:
                data = data.T
            mcd_daily = data[0].reshape(n_ls, n_hours)
            variants = {'raw': data[1], 'nosens': data[2], 'sensible': data[3]}
            row = row_of(lat)
            for key, values in variants.items():
                var_daily = values.reshape(n_ls, n_hours)
                sums[key]['min'][row, :] += np.min(var_daily, axis=1) - np.min(mcd_daily, axis=1)
                sums[key]['max'][row, :] += np.max(var_daily, axis=1) - np.max(mcd_daily, axis=1)
                sums[key]['mean'][row, :] += np.mean(var_daily, axis=1) - np.mean(mcd_daily, axis=1)
            counts[row] += 1
        except Exception:
            continue

    # --- Normalisation : moyenne zonale = somme / nb de longitudes presentes ---
    safe = counts.copy()
    safe[safe == 0] = 1
    results = {k: {m: sums[k][m] / safe[:, None] for m in metrics} for k in variants_keys}

    empty = counts == 0
    if empty.any():
        print(f"Attention : {int(empty.sum())} latitude(s) sans donnee -> NaN.")
        for k in variants_keys:
            for m in metrics:
                results[k][m][empty, :] = np.nan

    return results, lat_grid


# --- EXECUTION ---
res, lat_axis = analyze_all_variants()

# --- VISUALISATION ---
fig, axes = plt.subplots(3, 3, figsize=(18, 12), sharex=True, sharey=True)
titles = ['Minimum Diurne', 'Maximum Diurne', 'Moyenne Diurne']
extent = [0, 360, lat_axis.min(), lat_axis.max()]

for row, var in enumerate(variants_keys):
    for col, met in enumerate(metrics):
        ax = axes[row, col]
        im = ax.imshow(res[var][met], extent=extent, origin='lower',
                       aspect='auto', cmap='RdBu_r', vmin=-15, vmax=15)
        if row == 0:
            ax.set_title(titles[col], fontweight='bold')
        if col == 0:
            ax.set_ylabel(f"{var.upper()}\nLatitude", fontweight='bold')
        if row == 2:
            ax.set_xlabel("Saison ($L_s$)")

fig.subplots_adjust(right=0.92)
cbar_ax = fig.add_axes([0.94, 0.15, 0.02, 0.7])
fig.colorbar(im, cax=cbar_ax, label="$\\Delta T$ (KRC - MCD) [K]")
plt.suptitle("Analyse Zonale comparative : KRC vs MCD Reference", fontsize=16)
plt.show()


def export_for_matlab(res):
    if not os.path.exists('export_matlab'):
        os.makedirs('export_matlab')
    print("Exportation des fichiers pour MATLAB...")
    for var in variants_keys:
        for met in metrics:
            filename = f"export_matlab/zonal_diff_{var}_{met}.txt"
            np.savetxt(filename, res[var][met], fmt='%.4f', delimiter='\t')
    print("Termine. Les fichiers sont dans le dossier 'export_matlab/'.")


export_for_matlab(res)
