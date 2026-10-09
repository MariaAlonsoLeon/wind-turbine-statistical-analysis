# Ficheros de prueba (fixtures)

Pequeños CSV con el formato original, usados solo por los tests.

* `valid/data_swt_iee_usp_2017.csv`: 25 filas **reales** de 2017 (20 primeras
  y 5 alrededor del cambio de separador de milisegundos `.` → `,`).
* `valid/data_swt_iee_usp_2018.csv`: **sintético**. 20 filas reales de 2017
  con el año de `Log Time` cambiado a 2018. Solo sirve para probar la
  combinación de años; no son datos de 2018.
* `bad_schema/data_swt_iee_usp_2019.csv`: **sintético**. Le falta la columna
  `Timer`, para comprobar que la validación de esquema detiene la carga.
