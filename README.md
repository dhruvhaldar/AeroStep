# AetherFlow

A COBOL program simulating a batch-controlled workflow for aerospace component testing.  
It sequentially executes testing steps including pressure test, heat treatment, and quality inspection, using automated random input generation, condition validation, detailed logging of successes or failures, and comprehensive reporting.

---

## Project Overview

**AetherFlow** models a real-world aerospace manufacturing testing pipeline as a batch job:

- **Sequential workflow:** Runs through multiple steps in order, stopping if any step fails.
- **Randomized test values:** Each step generates a random test measurement within (or outside) specified acceptable ranges.
- **Pass/Fail evaluation:** Each test compares values to thresholds to determine pass or fail.
- **Detailed logging:** Timestamps and results for each step are logged to both console and a report file.
- **Final status:** Reports whether the overall process succeeded or failed.

This makes it a useful demonstration of batch control, input simulation, validation logic, logging, and file output in COBOL.

---

## Getting Started
1. Install 'gnucobol' package.
2. Verify by checking its version 'cobc --version'.
3. 'cobc -x aerostep.cbl' to create the executable. ('cobc -x -free aerostep.cbl -o aerostep' '-free' lets you write code in free-form layout (no strict column 7, 8, 12 rules))
4. Execute './aerostep' to run it.

---

## How It Works

1. **Initialization**  
   The program starts by logging an "Initialization started" message with the current timestamp.

2. **Pressure Test**  
   Generates a random pressure value between 80 and 120 (inclusive).  
   - Pass if within range.  
   - Fail if outside range, halting further tests.

3. **Heat Treatment**  
   Generates a random heat value between 200 and 300 (inclusive).  
   - Pass if within range.  
   - Fail if outside range, halting further tests.

4. **Quality Inspection**  
   Generates a random quality score between 0 and 100.  
   - Pass if score ≥ 70.  
   - Fail if below threshold.

5. **Finalization**  
   Logs a final message indicating whether the whole batch process succeeded or failed.

---

## Code Highlights

```cobol
* Open report file for output
OPEN OUTPUT REPORT-FILE

* Initialization step with timestamped logging
PERFORM INITIALIZATION

* Sequential tests: stop on failure
IF WS-FAILED NOT = "Y"
    PERFORM PRESSURE-TEST
END-IF
IF WS-FAILED NOT = "Y"
    PERFORM HEAT-TREATMENT
END-IF
IF WS-FAILED NOT = "Y"
    PERFORM QUALITY-INSPECTION
END-IF

* Finalize and close report
PERFORM FINALIZE
CLOSE REPORT-FILE
STOP RUN.
