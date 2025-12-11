# AetherFlow

![Language](https://img.shields.io/badge/Language-COBOL-blue)
![Standard](https://img.shields.io/badge/Standard-Mil--Std-green)
![License](https://img.shields.io/badge/License-MIT-yellow)
![Status](https://img.shields.io/badge/Status-Active-brightgreen)

**AetherFlow** is a robust, military-standard COBOL application designed to simulate a batch-controlled workflow for aerospace component testing. It processes list of components, subjecting each to a rigorous sequence of tests including pressure, heat, vibration, and quality inspection. The system features a real-time TUI (Text User Interface) dashboard, automated input generation, strictly sequential logic, and comprehensive audit logging.

---

## Project Overview

**AetherFlow** models a high-integrity aerospace manufacturing testing pipeline:

-   **Batch Processing:** Reads component IDs from an external input file (`batch_input.txt`) and processes them sequentially.
-   **Real-Time TUI:** Displays a live dashboard that updates in-place using ANSI escape sequences, showing the status of the current component and the batch overall.
-   **Sequential Workflow:** Executes tests in a strict order. If a component fails any step (Pressure, Heat, Vibration, Quality), the workflow for that component halts immediately to ensure safety.
-   **Randomized Simulation:** Generates random test measurements for each step, simulating real-world sensor data.
-   **Detailed Logging:** records timestamped results for every step to `aerostep.txt` for audit trails.
-   **Standards Compliance:** Written in GnuCOBOL free-format, adhering to strict coding standards including explicit scope terminators, file status checking, and error handling.

---

## Getting Started

### Prerequisites
*   **GnuCOBOL:** Ensure you have `gnucobol` installed.
    *   *Verify:* `cobc --version`

### Compilation
Compile the source code using the `-free` flag (required for free-format COBOL) and create the executable:

```bash
cobc -x -free aerostep.cbl -o aerostep
```

### Execution
1.  Ensure `batch_input.txt` exists and contains a list of component IDs (one per line).
2.  Run the executable:
    ```bash
    ./aerostep
    ```
3.  Observe the TUI dashboard updates in your terminal.
4.  Check `aerostep.txt` for the detailed execution log.

---

## Workflow Steps

1.  **Initialization**
    *   Logs the start of processing for the component.
    *   *Criterion:* Always Passes.

2.  **Pressure Test**
    *   Simulates pressure chamber testing.
    *   *Range:* 80 - 120 units.
    *   *Fail:* Value outside range.

3.  **Heat Treatment**
    *   Simulates thermal stress testing.
    *   *Range:* 200 - 300 degrees.
    *   *Fail:* Value outside range.

4.  **Vibration Test**
    *   Simulates structural vibration testing.
    *   *Limit:* Max 20.00 units.
    *   *Fail:* Value exceeds limit.

5.  **Quality Inspection**
    *   Simulates final visual/automated inspection.
    *   *Threshold:* Score >= 70.
    *   *Fail:* Score below threshold.

---

## Standards Compliance

This project adheres to strict software development standards suitable for high-reliability environments:

*   **Explicit Scope Terminators:** Uses `END-IF`, `END-PERFORM`, `END-READ` to prevent logic errors.
*   **Robust I/O:** Every `OPEN`, `READ`, and `WRITE` operation includes a `FILE STATUS` check to detect and handle errors gracefully.
*   **Structured Header:** Includes a standardized program identification block with security level, author, and purpose.
*   **Explicit Initialization:** All variables are initialized to known states.
*   **Clean Output:** Log files use a structured, delimited format for easy parsing.

---

## Code Highlights

**Batch Loop with Status Check:**
```cobol
PERFORM UNTIL WS-EOF = "Y"
    READ BATCH-FILE
        AT END
            MOVE "Y" TO WS-EOF
        NOT AT END
            IF WS-BATCH-STATUS NOT = "00"
                 DISPLAY "ERROR: Read failed status " WS-BATCH-STATUS
                 MOVE "Y" TO WS-EOF
            ELSE
                PERFORM PROCESS-COMPONENT
            END-IF
    END-READ
END-PERFORM
```

**Sequential Testing Logic:**
```cobol
IF WS-FAILED = "N"
    PERFORM PRESSURE-TEST
END-IF

IF WS-FAILED = "N"
    PERFORM HEAT-TREATMENT
END-IF

IF WS-FAILED = "N"
    PERFORM VIBRATION-TEST
END-IF
```
