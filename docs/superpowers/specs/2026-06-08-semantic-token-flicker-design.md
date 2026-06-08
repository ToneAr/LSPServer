# Semantic-Token Coloring Flicker — Design

- **Date:** 2026-06-08
- **Status:** Approved (design); pending implementation plan
- **Approach:** A — "never-blank" token caching + refresh discipline (targeted, evidence-driven)
- **Scope:** Eliminate momentary loss/flicker of semantic-token syntax coloring after edits, across VSCode, Zed, and other LSP clients. Keep the change surgical.

## Problem

Users report that syntax coloring "breaks momentarily" after edits and the server periodically
feels unresponsive. The headline symptom is semantic-token coloring flicker (the document briefly
goes monochrome or colors visibly change, then settle). The client set includes VSCode, Zed, and
other LSP clients, so the fix must be client-agnostic and must not rely on a specific client's
behavior of holding old coloring while a request is in flight.

A Wolfram kernel is single-threaded for evaluation, so this is **not** fixable by adding OS threads.
The server already implements cooperative concurrency on one main kernel (C-level background stdin
reader, prioritized content queue, yield points, batched background indexing, and one optional
parallel subkernel for diagnostics/hover). The flicker is a logic problem in the semantic-token
lifecycle, not a lack of threads.

## Root-cause diagnosis (with code evidence)

The flicker is caused by the server **discarding good tokens and then leaving a gap before new ones
exist**. On each keystroke:

1. **The edit wipes the token cache instantly.** `didChangeFencepost`
   (`LSPServer/Kernel/LSPServer.wl:3418-3432`) replaces the entire `$OpenFilesMap[uri]` entry with a
   fresh 6-key association, silently dropping `SemanticTokens`, `CST`, `AST`, and `ScopingData`.

2. **Nothing recomputes for ~400ms.** The reparse (`runIndexUpdate`) is debounced by
   `$DiagnosticsDelayAfterLastChange = 0.4` (`LSPServer/Kernel/LSPServer.wl:3440-3445`). During that
   window the client re-requests tokens but the entry has no CST/AST, so the request builds a parse
   pipeline or `Throw[{}]`s and waits (`LSPServer/Kernel/SemanticTokens.wl:889-916`). Result: blank
   coloring for the debounce + parse window.

3. **A second flash from the fast→full handoff.** When full scoping finishes, `runScopingData` drops
   the cache again and fires a refresh (`LSPServer/Kernel/SemanticTokens.wl:1245-1250` and
   `:1278-1283`). The interim "fast" pass also classifies locals/params as global symbols, so their
   color visibly changes when the full pass corrects them.

4. **Global refresh nukes all files.** The `workspace/semanticTokens/refresh` handler clears cached
   tokens for *every* open file (`LSPServer/Kernel/LSPServer.wl:2611-2630`) and is gated by a single
   `$PendingTokenRefresh` flag with a 10s timeout; a missed ack blocks all refreshes for 10s.

5. **`{}` / `Null` responses replace good coloring with nothing** in clients that clear on an
   empty/unanswered request.

6. **Amplifier (out of scope here):** if `LaunchKernels[1]` fails
   (`LSPServer/Kernel/LSPServer.wl:1224-1226`), diagnostics run synchronously on the main loop,
   lengthening the blank window into a visible hang. Not addressed in this change.

**Violated principle:** good tokens should never be replaced by nothing.

## Design

### Core invariant

A *live* `semanticTokens/full` request is never answered with empty/`Null` and never left to blank.
It always receives the best-available token set — **fresh if ready, otherwise the last-good (stale)
set.** `Null` is reserved only for genuinely closed / never-opened files (correct per protocol).

### Section 1 — Data model

In `didChangeFencepost` (`LSPServer/Kernel/LSPServer.wl:3418-3432`), when the edit rebuilds the
entry, **carry `SemanticTokens` forward** and add `"SemanticTokensStale" -> True`, instead of
dropping it. `CST` / `AST` / `ScopingData` still move to `Previous*` or get recomputed (they must),
but the *displayable* token set survives the edit.

- `"SemanticTokens"` — the token data last sent to the client (may be stale).
- `"SemanticTokensStale" -> True` — set when the cached tokens predate the current `Text`. While
  this flag is set, the displayed coloring is known-stale and a fresh delivery is owed.
- Fresh recompute clears `"SemanticTokensStale"`.

### Section 2 — Serving path (`semanticTokens/fullFencepost`)

In `LSPServer/Kernel/SemanticTokens.wl` (`handleContent[... "fullFencepost"]`):

- **Fresh cache** (no stale flag): serve immediately (unchanged behavior).
- **Not ready** (reindex pending / no fresh tokens) **but stale tokens exist**: serve the stale set
  (full replace; at most a sub-second few-column misalignment). Keep `SemanticTokensStale` so we know
  fresh delivery is still owed. Replaces the current `Throw[{}]` / `Null` paths for live requests.
- **Prefer stale-but-complete over fresh-but-incomplete:** do not serve the "fast" global-only pass
  when complete stale tokens are available — this removes the color-*change* flash where locals and
  parameters momentarily recolor as global symbols.
- `Null` only for: closed file, never-opened file, superseded request (existing correct paths).

### Section 3 — Recompute & fresh-token delivery

This is the crux: with the serving path now answering live requests immediately (with stale tokens
when fresh ones are not ready), there is no longer a pending request to answer late. So fresh tokens,
once computed, must be **delivered** rather than left sitting in cache. The delivery path must itself
be gap-free.

When `runIndexUpdate` (`LSPServer/Kernel/LSPServer.wl:3459-3561`) or `runScopingData`
(`LSPServer/Kernel/SemanticTokens.wl:1205-1292`) computes fresh tokens for a `uri`, in order:

1. Write the fresh tokens to the cache and clear `SemanticTokensStale`.
2. **If pending (unanswered) `fullFencepost` requests exist for this `uri`** — recover them directly
   with the fresh tokens via the existing `queuePendingSemanticTokenFenceposts` helper
   (`LSPServer/Kernel/LSPServer.wl:2549-2576`). This is gap-free and involves no global churn.
   (Pending requests can still exist for clients that send a request the server hasn't yet served.)
3. **Else, if the displayed tokens were stale** (we already answered the in-flight request with the
   stale set), queue **one coalesced** `workspace/semanticTokens/refresh` so the client re-fetches.
   Coalescing/dedupe is the existing `$PendingTokenRefresh` guard, so at most one refresh fires per
   typing pause (not per keystroke).

Supporting changes that make the above gap-free:

- **`workspace/semanticTokens/refresh` handler stops `KeyDrop`-ing caches**
  (`LSPServer/Kernel/LSPServer.wl:2615-2619`). The cache drop is precisely what creates the blank gap.
  Because the refresh is only emitted *after* fresh tokens are already in cache (step 3 above), the
  client's re-fetch is an instant `fullFencepost` cache-hit with no recompute and no gap. The handler
  still asks clients to re-fetch and still recovers pending fenceposts.
- **Remove the eager, pre-recompute, cache-dropping `queueSemanticTokensRefresh`** from
  `runScopingData` (`SemanticTokens.wl:1245-1250`, `:1278-1283`). Its job (deliver corrected tokens
  after the full scoping pass) is subsumed by the unified delivery logic above.
- Keep the single coalesced refresh in `finishWorkspaceIndexing`
  (`LSPServer/Kernel/LSPServer.wl:1457-1459`) — a legitimately global classification change — now
  gap-free because caches are no longer dropped.

Net effect: refreshes drop from roughly one-per-keystroke (each dropping all caches) to at most one
per typing-pause (cache-preserving, instant re-fetch), plus the one-shot indexing-completion refresh.

### Section 4 — Safety & edges

- **Positional staleness:** serving a stale full token set during the sub-second recompute window may
  misalign tokens by a few columns; this is accepted (far less jarring than monochrome) and corrected
  by the next response.
- **Large files** (over `$SemanticTokensScopingTextLengthLimit`): keep the existing fast-only path,
  but still serve stale rather than blank.
- **Stuck-refresh safety valve:** shorten the `$PendingTokenRefresh` recovery timeout from 10s to
  ~3s (`LSPServer/Kernel/LSPServer.wl:1605-1611`). Rarely hit once per-edit refreshes are removed.
- No changes to diagnostics / hover / indexing behavior beyond what the above requires.

## Non-goals

- No true multi-threading of WL evaluation (impossible on one kernel).
- No move of token computation onto the worker kernel (Approach B — rejected: larger, riskier, and
  still needs this caching).
- No semantic-tokens delta protocol (Approach C — rejected: uneven client support, doesn't fix the
  reparse gap).
- Worker-kernel launch-failure hardening (diagnosis point 6) is explicitly deferred.

## Risks

- Stale tokens at slightly wrong positions for a sub-second window. Mitigated: corrected by the next
  delivery (direct fencepost recovery or one coalesced refresh); only affects the transient after an
  edit.
- The coalesced post-recompute refresh (Section 3, step 3) still asks the client to re-fetch. Because
  caches are no longer dropped, the re-fetch is an instant cache-hit, so any client-side gap is a
  single fast round-trip rather than a recompute-length blank. This is strictly better than the
  current per-keystroke, cache-dropping refresh storm.

## Testing & verification

Per project memory: tests under `Tests/` load from `build/paclet/` via `PacletDirectoryLoad`, so any
edited `.wl` source must be synced to **both** `build/paclet/LSPServer/Kernel/` and the installed
paclet (`~/.Wolfram/Paclets/Repository/LSPServer--*/Kernel/`) via `cp` (do not edit `build/` directly).

- All existing `Tests/` continue to pass.
- New targeted tests:
  - `didChangeFencepost` preserves `SemanticTokens` and sets `SemanticTokensStale -> True`.
  - `fullFencepost` returns a non-empty token set when reindex is pending and stale tokens exist.
  - `workspace/semanticTokens/refresh` handler leaves cached `SemanticTokens` intact.
  - `runScopingData` no longer drops the cache or enqueues an eager refresh.
  - Fresh recompute with a pending `fullFencepost` recovers it directly (no refresh enqueued).
  - Fresh recompute with no pending request but stale displayed tokens enqueues exactly one
    coalesced refresh.
- Manual verification in VSCode and Zed: rapid typing in a `.wl` file must never go monochrome; local
  variables and parameters must not visibly recolor between fast and full passes.

## References

- `LSPServer/Kernel/SemanticTokens.wl` — token computation, `fullFencepost`, `runScopingData`.
- `LSPServer/Kernel/LSPServer.wl` — `didChangeFencepost`, `runIndexUpdate`, refresh handler,
  `finishWorkspaceIndexing`, `queueSemanticTokensRefresh`, pending-fencepost recovery helpers.
