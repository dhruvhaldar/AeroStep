## 2024-05-23 - Artificial Delays in Simulations
**Learning:** The application contained a hardcoded 1-second sleep (`C$SLEEP`) to simulate "Security Access Control" processing time. While this adds "realism" to the simulation, it acts as a significant performance bottleneck during testing and automated execution.
**Action:** When optimizing simulation code, identify and remove artificial delays that impede performance unless they are strictly required for timing-dependent logic.
