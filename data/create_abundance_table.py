"""
Crea tabla con abundancia de especies por unidades de muestreo por metodo.
"""
import pathlib
import numpy as np
import pandas as pd

event = "vent"
obs = "egistro"

if __name__ == "__main__":

    root = pathlib.Path(__file__).parents[1]

    groups = ["peces"] #"collembola", "coprofagos", "larvas", "peces", "aves, "anfibios", "reptiles"]
    for group in groups:

        sheets = pd.ExcelFile(root.joinpath(f"data/{group}/plantilla.xlsx")).sheet_names
        result = [v for v in sheets if event in v]
        result1 = [v for v in sheets if obs in v]

        output_folder = root.joinpath(f"results/{group}")
        output_folder.mkdir(parents=True, exist_ok=True)

        events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
            ["eventID","parentEventID", "samplingProtocol", "samplingEffort", "decimalLatitude", "decimalLongitude",
             "measurementValue (Plataforma)"]]


        records = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"),
                                result1[0])  ##.dropna(subset=['eventDate'])
        #records.to_excel(output_folder.joinpath("prueba.xlsx"), index=False)


        if group == "peces":
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID","parentEventID", "samplingProtocol", "samplingEffort", "decimalLatitude", "decimalLongitude",
                 "measurementValue (Plataforma)", "measurementValue (Orden )"]]
            #records = records[["eventID", "eventDate", "organismQuantity", "scientificName", "taxonRank", "identificationQualifier"]]
            records["scientificName"] = records["scientificName"] + records["identificationQualifier"].fillna("")
            

        #records = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result1[0]) ##.dropna(subset=['eventDate'])
        #records.to_excel(output_folder.joinpath("prueba.xlsx"), index=False)

        if group == "fitoplancton":
            events1 = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "eventDate", "eventTime"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID","parentEventID", "samplingProtocol", "samplingEffort", "decimalLatitude", "decimalLongitude",
                 "measurementValue (Plataforma)", "waterBody"]]
            records = pd.merge(
                records,
                events1,
                how="left",
                left_on="eventID",
                right_on="eventID"
            )

        if group == "macrofitas":
            events1 = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "eventDate", "eventTime"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID","parentEventID", "samplingProtocol", "samplingEffort", "decimalLatitude", "decimalLongitude",
                 "measurementValue (Plataforma)", "waterBody"]]
            records = pd.merge(
                records,
                events1,
                how="left",
                left_on="eventID",
                right_on="eventID"
            )

        if group == "macroinvertebrados":
            events1 = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "eventDate", "eventTime"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "parentEventID", "samplingProtocol", "samplingEffort", "decimalLatitude",
                 "decimalLongitude",
                 "measurementValue (Plataforma)", "waterBody"]]
            records = pd.merge(
                records,
                events1,
                how="left",
                left_on="eventID",
                right_on="eventID"
            )

        if group == "zooplancton":
            events1 = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "eventDate", "eventTime"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "parentEventID", "samplingProtocol", "samplingEffort", "decimalLatitude",
                 "decimalLongitude",
                 "measurementValue (Plataforma)", "waterBody"]]
            records = pd.merge(
                records,
                events1,
                how="left",
                left_on="eventID",
                right_on="eventID"
            )

        if group == "perifiton":
            events1 = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "eventDate", "eventTime"]]
            events = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "parentEventID", "samplingProtocol", "samplingEffort", "decimalLatitude",
                 "decimalLongitude",
                 "measurementValue (Plataforma)", "waterBody"]]
            records = pd.merge(
                records,
                events1,
                how="left",
                left_on="eventID",
                right_on="eventID"
            )

        if group == "epifitasvasculares":
            events1 = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "eventDate", "eventTime"]]
            records = pd.merge(
                records,
                events1,
                how="left",
                left_on="eventID",
                right_on="eventID"
            )

        if group == "epifitasnovasculares":
            events1 = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "eventDate", "eventTime"]]
            records = pd.merge(
                records,
                events1,
                how="left",
                left_on="eventID",
                right_on="eventID"
            )

        if group == "arboles":
            events1 = pd.read_excel(root.joinpath(f"data/{group}/plantilla.xlsx"), result[0])[
                ["eventID", "eventDate", "eventTime"]]
            records = pd.merge(
                records,
                events1,
                how="left",
                left_on="eventID",
                right_on="eventID"
            )

        records[["1", "2", "3"]] = records["eventID"].str.split("_", n=2, expand=True)
        records["parentEventID"] = records["1"] + '_' + records["2"]
        #records["eventDate"] = records["eventDate"].astype(str)
        #records["eventTime"] = records["eventTime"].replace(np.nan, "00")
        #records["eventTime"] = records["eventTime"].astype(str)
        #records[["1", "2"]] = records["eventTime"].str.split(":", n=1, expand=True)
        #records["eventDate"] = records["eventDate"] + " " + records["1"] + ":00:00"
        #records = records[["eventID","parentEventID", "eventDate", "organismQuantity", "scientificName", "taxonRank"]]
        records = records[["eventID","parentEventID", "organismQuantity", "scientificName", "taxonRank"]]

        df = pd.merge(
            records,
            events,
            how="left",
            left_on="eventID",
            right_on="eventID"
        )

        group_cols = ["eventID", "samplingProtocol"]
        name_col = "scientificName"
        reference_cols = [
            "decimalLatitude",
            "decimalLongitude",
            #"eventDate",
            "measurementValue (Plataforma)",
            "samplingEffort"
        ]
        abundance_col = "organismQuantity"
        if group == "peces":
            reference_cols.append("measurementValue (Orden )")
        if group == "fitoplancton":
            reference_cols.append("waterBody")
        if group == "macroinvertebrados":
            reference_cols.append("waterBody")
        if group == "macrofitas":
            reference_cols.append("waterBody")
        if group == "zooplancton":
            reference_cols.append("waterBody")
        if group == "perifiton":
            reference_cols.append("waterBody")

        df = df[group_cols + [name_col] + reference_cols + [abundance_col]]

        agg_funcs = {k: "first" for k in reference_cols}
        agg_funcs.update({abundance_col: "sum"})
        df = df.groupby(group_cols + [name_col], as_index=False).agg(agg_funcs)

        df = df.pivot(
            index=group_cols + reference_cols,
            columns=name_col,
            values=abundance_col
        )
        df = df.reset_index()

        df.to_excel(output_folder.joinpath("abundancia.xlsx"), index=False)
