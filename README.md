# CPI Kwartaalmutatie

R-script dat via de CBS StatLine API de kwartaalmutatie van de consumentenprijsindex berekent voor huur van woning en benzine.

## Vereisten

R 4.1 of hoger. Benodigde packages worden bovenaan het script geïnstalleerd.

## Gebruik
```r
source("cpi_kwartaalmutatie_final.R")
```

Optioneel met verslagperiode:
```r
resultaat <- main(verslagperiode = "2024")
resultaat <- main(verslagperiode = "2023KW01")
```

## Output

Na uitvoering staat het volgende in de map `output/`:

- `grafiek1_lijngrafiek.png`: kwartaalmutatie laatste 5 jaar
- `grafiek2_staafdiagram.png`: kwartaalmutatie laatste 8 kwartalen
- `cpi_resultaten.db`: SQLite database met resultaten en runlog

## Bron

CBS StatLine, tabel 83131NED: Consumentenprijzen; prijsindex 2015=100
