# beetle_tdt
Flour beetle TDT experiment

Chinmayee's Calibration Data and Details
1. Thermocouple Calibration Data
We conducted two types of pilot runs to measure heating rates: one for loose flour and another for flour in Eppendorf tubes. We used the "flour in tube" readings as they accurately reflected the experimental setup.

Target Temp (°C) | Flour Only (min) | Flour in Tube (min)
38°C | 2.28 | 2.50
41°C | 1.69 | 2.45
44°C | 2.30 | 2.57
47°C | 3.92 | 5.11
50°C | At the 50°C incubator setting, the thermocuple reading stabilised at a maximum of 49.2°C

2. R script: 50°C prediction based on the log-transform model of the "flour in tube" data (attached as a file).  

3. Protocol Logic
As the thermocouple reached a maximum wait time of 5.11 minutes for the flour-in-tube setup at 47°C, we implemented a standardised 5-minute wait period for all batches across the temperature-time matrix. The model's prediction of the wait time for 50°C was 5.18 minutes, which supported this standardised wait time.
This ensured that samples reached the target core temperature before the experimental exposure timer officially began. While lower-temperature tubes reached equilibrium faster, this standardised buffer provided a conservative and consistent "minimum exposure" across all treatments.
