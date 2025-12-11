# AetherFlow

![Language](https://img.shields.io/badge/Language-COBOL-blue)
![Standard](https://img.shields.io/badge/Standard-Mil--Std-green)
![License](https://img.shields.io/badge/License-MIT-yellow)
![Status](https://img.shields.io/badge/Status-Active-brightgreen)

**AetherFlow** is a robust, military-standard COBOL application designed to simulate a batch-controlled workflow for aerospace component testing. It processes a list of components, subjecting each to a rigorous sequence of tests including pressure, heat, vibration, and quality inspection. The system features a real-time TUI (Text User Interface) dashboard, automated input generation, strictly sequential logic, and comprehensive secure logging.

---

## Project Overview

**AetherFlow** models a high-integrity aerospace manufacturing testing pipeline:

-   **Batch Processing:** Reads component IDs from an external input file (`batch_input.txt`) and processes them sequentially.
-   **Configurable Parameters:** Test thresholds are defined externally in `system_config.cfg`, allowing for mission-specific adaptability without code changes.
-   **Secure Access:** Requires Operator Login (ID and Auth Code) to access the system.
-   **System Integrity:** Performs a Power-On Self-Test (POST) of sensors and memory before operation.
-   **Real-Time TUI:** Displays a live dashboard that updates in-place using ANSI escape sequences.
-   **Sequential Workflow:** Executes tests in a strict order. If a component fails any step, the workflow halts immediately.
-   **Secure Logging:** Records timestamped results with specific error codes and a cryptographic checksum for each entry to ensure audit trail integrity.
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
1.  Ensure `batch_input.txt` contains component IDs.
2.  Ensure `system_config.cfg` defines the valid test thresholds.
3.  Run the executable:
    ```bash
    ./aerostep
    ```
4.  **Login:** Enter any non-empty Operator ID and Auth Code when prompted.
5.  **Operation:** Watch the System Self-Test and then the TUI dashboard as components are processed.
6.  **Audit:** Check `aerostep.txt` for the detailed execution log, including checksums.

---

## Configuration

The system behavior is controlled by `system_config.cfg`:

```ini
MIN_PRESS=0080
MAX_PRESS=0120
MIN_HEAT=0200
...
```

## Workflow Steps

1.  **Initialization**
    *   Logs the start of processing.
2.  **Pressure Test**
    *   Simulates pressure chamber testing.
    *   *Error Code:* `ERR-PRESS`
3.  **Heat Treatment**
    *   Simulates thermal stress testing.
    *   *Error Code:* `ERR-HEAT`
4.  **Vibration Test**
    *   Simulates structural vibration testing.
    *   *Error Code:* `ERR-VIB`
5.  **Quality Inspection**
    *   Simulates final inspection.
    *   *Error Code:* `ERR-QUAL`

---

## Code Highlights

**Secure Logging with Checksum:**
```cobol
PERFORM COMPUTE-CHECKSUM
STRING REPORT-RECORD DELIMITED BY SIZE
       ";CS:" WS-CHECKSUM-HEX
       INTO REPORT-RECORD
WRITE REPORT-RECORD
```

**Configuration Loading:**
```cobol
UNSTRING CONFIG-RECORD DELIMITED BY "="
    INTO WS-CONFIG-KEY WS-CONFIG-VAL
EVALUATE WS-CONFIG-KEY
    WHEN "MIN_PRESS" MOVE WS-CONFIG-VAL TO MIN-PRESS
    ...
END-EVALUATE
```
