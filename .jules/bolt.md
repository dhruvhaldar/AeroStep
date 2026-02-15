## 2026-01-02 - [COBOL String Buffering]
**Learning:** In GnuCOBOL, consolidating multiple `DISPLAY` calls into a single statement or using direct `STRING` construction into the final buffer significantly reduces system call overhead and memory copying compared to using intermediate buffers.
**Action:** Prefer direct `STRING ... INTO FINAL-BUFFER` over constructing intermediate parts, and chain literals/variables in `DISPLAY` statements to minimize I/O operations.

## 2026-02-15 - [COBOL Static Initialization]
**Learning:** Initializing complex static UI elements (like box drawings) using `VALUE` clauses with hex literals in `WORKING-STORAGE` is measurably faster than constructing them at runtime using `STRING` and `MOVE`.
**Action:** Replace runtime initialization of constant strings with compile-time definitions using `VALUE ALL` and hex literals where possible.
