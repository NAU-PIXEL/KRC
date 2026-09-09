#!/usr/bin/env python3

import os
import glob

# Répertoire contenant les Results_*.txt
path = "/scratch/ll2456/KRC_mcd_final_validationTESnight/"

# Dictionnaire pour stocker les lignes par MY
results = {24: [], 25: [], 26: [], 27: []}

# Lecture de tous les fichiers
for filename in sorted(glob.glob(os.path.join(path, "Results_*.txt"))):

    try:
        data = open(filename).read().split()

        if len(data) < 6:
            continue

        lat = float(data[0])
        ls = float(data[1])
        ttes = float(data[2])
        my = int(round(float(data[3])))
        tkrc_raw = float(data[4])
        tkrc = float(data[5])

        if my in results:
            results[my].append([lat, ls, ttes, tkrc_raw, tkrc])

    except Exception:
        print(f"Erreur avec {filename}")

# Écriture d'un fichier par MY
for my in results:

    outfile = os.path.join(path, f"TES_KRC_MY{my}.txt")

    with open(outfile, "w") as f:

        f.write("# Latitude Ls T8TES T_krc_raw T_krc\n")

        for row in sorted(results[my], key=lambda x: (x[1], x[0])):  # tri par Ls puis latitude
            f.write(
                f"{row[0]:8.3f} "
                f"{row[1]:8.3f} "
                f"{row[2]:8.3f} "
                f"{row[3]:8.3f} "
                f"{row[4]:8.3f}\n"
            )

    print(f"{outfile} : {len(results[my])} points")
