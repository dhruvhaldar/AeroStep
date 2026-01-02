## 2026-01-02 - [COBOL String Buffering]
**Learning:** In GnuCOBOL, consolidating multiple `DISPLAY` calls into a single statement or using direct `STRING` construction into the final buffer significantly reduces system call overhead and memory copying compared to using intermediate buffers.
**Action:** Prefer direct `STRING ... INTO FINAL-BUFFER` over constructing intermediate parts, and chain literals/variables in `DISPLAY` statements to minimize I/O operations.
