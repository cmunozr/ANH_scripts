"""
Crea tabla de referencia comparando contra registros previos de la zona.
"""
import pathlib

import pandas as pd

obs = "egistro"

if __name__ == "__main__":

    root = pathlib.Path(__file__).parents[1]
#,
    groups = ["peces"] ##, "arboles", "aves, "anfibios", "reptiles", "coprofagos", "larvas"]
    for group in groups:

        sheets = pd.ExcelFile(root.joinpath(f"data/{group}/plantilla.xlsx")).sheet_names
        result = [v for v in sheets if obs in v]

        output_folder = root.joinpath(f"results/{group}")
        output_folder.mkdir(parents=True, exist_ok=True)

        records = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])##.dropna(subset=['eventDate'])
        records = records[["scientificName", "taxonRank"]]

        reference = pd.read_excel(root.joinpath(f"data/referencia/{group}.xlsx"))
        reference = reference.drop_duplicates("Especie", keep="first").astype(str)


        name_col = "scientificName"
        rank_col = "taxonRank"
        names = sorted(records[records[rank_col].str.contains("pecie", na=False)][name_col].unique())
        namesLB = sorted(reference["Especie"].unique())

        df = pd.DataFrame(columns=["especie", "LBprevia", "LBmuestreo"])
        df["especie"] = pd.Series(names+namesLB, copy=False).unique()

        df["LBprevia"] = df["especie"].isin(reference["Especie"]).astype(int)
        df["LBmuestreo"] = df["especie"].isin(records["scientificName"]).astype(int)

        df.to_excel(output_folder.joinpath("referencia.xlsx"), index=False)
