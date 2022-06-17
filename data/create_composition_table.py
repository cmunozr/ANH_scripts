"""
Crea tabla de resumen de composicion para todos los sitios
"""
import pathlib
import numpy as np
import pandas as pd

event = "vent"
obs = "egistro"

if __name__ == "__main__":

    root = pathlib.Path(__file__).parents[1]

    groups = ["peces"] #'arboles', ""coprofagos", "larvas", "aves, "anfibios", "reptiles"]

    for group in groups:

        sheets = pd.ExcelFile(root.joinpath(f"data/{group}/plantilla.xlsx")).sheet_names
        result = [v for v in sheets if event in v]
        result1 = [v for v in sheets if obs in v]

        output_folder = root.joinpath(f"results/{group}")
        output_folder.mkdir(parents=True, exist_ok=True)

        events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
            ["eventID", "measurementValue (Plataforma)"]].astype(str)
        records = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result1[0])#.dropna(subset=['eventDate'])

        if group == "peces":
            grupo ="Peces"
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "measurementValue (Plataforma)", "measurementValue (Orden )"]].astype(str)
            records = records[["eventID", "organismQuantity", "scientificName", "taxonRank", "identificationQualifier",
                               "order", "family", "genus"]]
            records["scientificName"] = records["scientificName"] + records["identificationQualifier"].fillna("")

        if group == "aves":
            grupo ="Aves"
            records = records[["eventID", "organismQuantity", "scientificName", "taxonRank", "order", "family", "genus"]]

        if group == "coprofagos":
            grupo ="Escarabajos"
            records = records[["eventID", "eventID_modificado", "organismQuantity", "scientificName", "taxonRank", "order", "family", "genus"]]

        if group == "larvas":
            grupo ="Escarabajos"
            records = records[["eventID", "eventID_modificado","organismQuantity", "scientificName", "taxonRank", "order", "family", "genus"]]

        if group == "reptiles":
            grupo = "Herpetos"
            records = records[["eventID", "organismQuantity", "scientificName", "taxonRank", "eventDate", "eventTime", "order", "family", "genus"]]
            records[["1", "2", "3"]] = records["eventID"].str.split("_", n=2, expand=True)
            records["parentEventID"] = records["1"] + '_' + records["2"]
            records["eventDate"] = records["eventDate"].astype(str)
            records["eventTime"] = records["eventTime"].replace(np.nan, "00")
            records["eventTime"] = records["eventTime"].astype(str)
            records[["1", "2"]] = records["eventTime"].str.split(":", n=1, expand=True)
            records["eventDate"] = records["eventDate"] + " " + records["1"] + ":00:00"
            records = records[["eventID", "eventDate", "parentEventID", "organismQuantity", "scientificName",
                               "taxonRank", "order", "family", "genus"]]

        if group == "anfibios":
            grupo = "Herpetos"
            records = records[["eventID", "organismQuantity", "scientificName", "taxonRank", "eventDate", "eventTime", "order", "family", "genus"]]
            records[["1", "2", "3"]] = records["eventID"].str.split("_", n=2, expand=True)
            records["parentEventID"] = records["1"] + '_' + records["2"]
            records["eventDate"] = records["eventDate"].astype(str)
            records["eventTime"] = records["eventTime"].replace(np.nan, "00")
            records["eventTime"] = records["eventTime"].astype(str)
            records[["1", "2"]] = records["eventTime"].str.split(":", n=1, expand=True)
            records["eventDate"] = records["eventDate"] + " " + records["1"] + ":00:00"
            records = records[["eventID", "eventDate", "parentEventID", "organismQuantity", "scientificName",
                               "taxonRank", "order", "family", "genus"]]

        if group == "hormigas":
            grupo ="Hormigas"
            records = records[["eventID", "ID_ptotocolo", "organismQuantity", "scientificName", "taxonRank", "order", "family", "genus"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "parentEventID","measurementValue (Plataforma)"]].astype(str)

        if group == "mariposas":
            grupo ="Mariposas"
            records = records[["eventID", "ID_ptotocolo", "organismQuantity", "scientificName", "taxonRank", "order", "family", "genus"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "parentEventID","measurementValue (Plataforma)"]].astype(str)

        if group in ('zooplancton','perifiton','fitoplancton','macroinvertebrados','macrofitas') :
            grupo = "Hidrobiologicos"
            records = records[["eventID", "ID_composition", "organismQuantity", "scientificName", "taxonRank", "order", "family", "genus"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "waterBody", "measurementValue (Plataforma)"]].astype(str)
            abun = pd.read_excel(output_folder.joinpath("abundancia.xlsx"))

        if group in ('murcielagos','roedores') :
            grupo = "Mamiferos"
            records = records[["eventID", "organismQuantity", "scientificName", "taxonRank", "order", "family", "genus"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "habitat", "measurementValue (Plataforma)"]].astype(str)

        if group == "mamifero":
            grupo = "Mamiferos"
            records = records[["eventID", "organismQuantity", "scientificName", "taxonRank", "order", "family", "genus",
                               "occurrenceRemarks", "sex", "lifeStage", "reproductiveCondition"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "habitat", "measurementValue (Plataforma)"]].astype(str)

        if group in ("arboles",'epifitasnovasculares', 'epifitasvasculares') :
            grupo = "Botanica"
            records = records[["eventID","eventIDCOVER", "organismQuantity", "scientificName", "taxonRank", "order", "family", "genus"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "habitat", "measurementValue (Plataforma)"]].astype(str)


        referencetodo = pd.read_excel(root.joinpath(f"data/referencia/registros.xlsx"))[
            ["species", "UICN", "Categoria de amenaza MADS", "appendixCITES"]]

        referencetodo = referencetodo.drop_duplicates("species", keep="first")
        referencesp = pd.read_excel(root.joinpath(f"data/referencia/{group}.xlsx"))
        referencesp = referencesp.drop_duplicates("Especie", keep="first")

        reference = pd.merge(
            referencesp,
            referencetodo,
            how="left",
            left_on="Especie",
            right_on="species"
        )

        cover = pd.read_excel(r"C:\Users\gabriel.perilla\Documents\ANH\BDPuntosMuestreoANH2209.xlsx")
        cover = cover[cover['GrupoBiolo'] == grupo]

        df = pd.merge(
            records,
            events,
            how="left",
            left_on="eventID",
            right_on="eventID"
        )

        if group == "hormigas":
            df["eventID"] = df["parentEventID"] + '_' + df["ID_ptotocolo"]
        if group == "mariposas":
            df["eventID"] = df["parentEventID"] + '_' + df["ID_ptotocolo"]
        if group == "mamifero":
            cover = pd.read_excel(r"C:\Users\gabriel.perilla\Documents\ANH\BDPuntosMuestreoANH2209.xlsx")
        if group == "coprofagos":
            df = df.drop('eventID', 1)
        if group == "larvas":
            df = df.drop('eventID', 1)

        dfc = pd.merge(
            df,
            cover[["eventID", "GrupoBiolo", "Cobertura"]],
            how="left",
            left_on="eventID",
            right_on="eventID"
        )

        if group in ('zooplancton','perifiton','fitoplancton','macroinvertebrados','macrofitas') :
            dfc = pd.merge(
                df,
                cover[["eventID", "GrupoBiolo", "Cobertura", "Plataf"]] ,
                how="left",
                left_on= "ID_composition",
                right_on= "eventID"
            )
            dfc["measurementValue (Plataforma)"] = dfc["Plataf"]
            prueba = pd.merge(
                abun,
                dfc[["eventID_x", "Plataf"]],
                how="left",
                left_on="eventID",
                right_on="eventID_x"
            ).drop_duplicates("eventID", keep="first")
            prueba["measurementValue (Plataforma)"] = prueba["Plataf"]
            prueba.drop(['Plataf', 'eventID_x'], axis=1, inplace = True)
            dfc = dfc.rename(columns={"eventID_x": "eventID"})
            prueba.to_excel(output_folder.joinpath("abundancia.xlsx"), index=False)

        if group == "herpetos":
            cover = cover.drop_duplicates("parentEventID", keep="first")
            dfc = pd.merge(
                df,
                cover[["parentEventID", "GrupoBiolo", "Cobertura"]],
                how="left",
                left_on="parentEventID",
                right_on="parentEventID"
            )

        if group in ("epifitasnovasculares", "epifitasvasculares"):
            dfc = pd.merge(
                df,
                cover[["eventID", "GrupoBiolo", "Cobertura"]],
                how="left",
                left_on="eventIDCOVER",
                right_on="eventID"
            )
            dfc = dfc.rename(columns={"eventID_x": "eventID"})

        name_col = "scientificName"

        df = pd.merge(
            dfc,
            reference,
            how="left",
            left_on= name_col,
            right_on= "Especie"
        )

        reference_cols = [
        'taxonRank',
        "order",
        "family",
        "UICN",
        "Categoria de amenaza MADS",
        "appendixCITES"
        ]

        other_cols = ["measurementValue (Plataforma)", "Cobertura"]
        abundance_col = "organismQuantity"

        df[["eventID", "order", "family", "Especie", "measurementValue (Plataforma)", "Cobertura", "UICN",
            "Categoria de amenaza MADS", "appendixCITES", "taxonRank"]] = df[["eventID", "order", "family", "Especie", "measurementValue (Plataforma)", "Cobertura", "UICN",
             "Categoria de amenaza MADS", "appendixCITES", "taxonRank"]].astype(str)

        if group == "peces":
            other_cols = ["measurementValue (Plataforma)", "measurementValue (Orden )", "Cobertura"]
            adicion = ["Estatus", "Tipo de migración", "Categoría de Amenaza", "Uso"]
        if group == "aves":
            adicion = ["Altura", "CITES", "Libro Rojo", "Resolución", "UICN_v2"]
        if group == "reptiles":
            adicion = ["Herpeto", "Suborden", "eventDate","Altura", "Endemismo", "Categoria IUCN"]
        if group == "anfibios":
            adicion = ["Herpeto", "Suborden", "eventDate","Altura", "Endemismo", "Categoria IUCN"]
        if group == "coprofagos":
            adicion = ["stateProvince", "basisOfRecord", "subfamily"]
        if group == "larvas":
            adicion = ["stateProvince", "basisOfRecord", "subfamily"]
        if group == "hormigas":
            adicion = ["Subfamilia", "basisOfRecord"]
        if group == "mariposas":
            adicion = ["Subfamilia", "basisOfRecord"]
        if group in ('zooplancton','perifiton','fitoplancton') :
            other_cols = ["measurementValue (Plataforma)", "waterBody", "Cobertura"]
            adicion = ["HÁBITO DE VIDA","FUENTE"]
        if group == "macrofitas":
            other_cols = ["measurementValue (Plataforma)", "waterBody", "Cobertura"]
            adicion = ["Crecimiento", "FUENTE"]
        if group == "macroinvertebrados":
            other_cols = ["measurementValue (Plataforma)", "waterBody", "Cobertura"]
            adicion = ["HÁBITO DE VIDA", "GREMIO TRÓFICO"]
        if group == "mamifero":
            other_cols = ["measurementValue (Plataforma)", "habitat", "Cobertura"]
            adicion = ["occurrenceRemarks", "sex", "lifeStage", "reproductiveCondition", "Altitud", "IUCN", "Apéndice CITES"]
        if group in ('murcielagos', 'roedores'):
            other_cols = ["measurementValue (Plataforma)", "habitat", "Cobertura"]
            adicion = ["Altitud", "IUCN", "Apéndice CITES"]
        if group in ("arboles", "epifitasnovasculares", "epifitasvasculares"):
            other_cols = ["measurementValue (Plataforma)", "habitat", "Cobertura"]
            adicion = ["Sinonimos", "Taxones infraespecíficos", "Hábito", "Origen", "Estado de conservación", "Elevación Mínima", "Elevación Máxima"]



        df = df[[name_col] + reference_cols + other_cols + [abundance_col] + adicion]
        joined = reference_cols+adicion
        agg_funcs = {k: "first" for k in joined}
        agg_funcs.update({k: lambda x: "|".join(sorted(x.unique())) for k in other_cols})
        agg_funcs.update({abundance_col: "sum"})
        df = df.groupby(name_col, as_index=False).agg(agg_funcs)

        df = df.rename(columns={name_col: "Especie"})
        df = df.rename(columns={"order": "Orden"})
        df = df.rename(columns={"family": "Familia"})
        df["ARTotal"] = df["organismQuantity"] / df["organismQuantity"].sum()


        df.to_excel(output_folder.joinpath("composicion.xlsx"), index=False)