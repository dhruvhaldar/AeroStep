## 2025-12-25 - [Artificial UI Delays]
**Learning:** Legacy COBOL applications may contain explicit delays (e.g., `CALL "C$SLEEP"`) intended for UI pacing on slower terminals.
**Action:** When modernizing or optimizing, search for and remove these artificial bottlenecks as they provide no functional value in batch or automated contexts.
