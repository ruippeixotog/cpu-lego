# Apple IIe target specification

This is the verified specification for Phase 1 (#8): the machine we build, how it is clocked, its pin-level bus protocol, its soft switches, the video scanner, the assets it needs, and the success criteria for the milestone checks. Every other Apple IIe issue builds against it. Where it differs from the epic, the difference is called out in [§9](#9-deviations-from-8).

Each fact cites its source using the keys below. Page numbers for Sather are the book's own (`3-13` is chapter 3, page 13). PoP paths are relative to the repository root, abbreviated as `CP/` for `02 POP Disk Routines/CP.525/` and `S/` for `04 Support/MakeDisk/S/`.

| Key | Source |
|---|---|
| **[TRM]** | *Apple IIe Technical Reference Manual*, Apple Computer / Addison-Wesley, 1985, ISBN 0-201-17720-X (Internet Archive item `a2etrm`). |
| **[Sather]** | Jim Sather, *Understanding the Apple IIe*, Quality Software, 1985 (Internet Archive item `Understanding_the_Apple_IIe`). |
| **[MOS]** | MOS Technology, *MCS6500 Microprocessors* preliminary data sheet, May 1976, "Read/Write Timing" table, 1 MHz parts (Internet Archive item `mos_6500_mpu_preliminary_may_1976`). |
| **[TN7]** | Apple II Miscellaneous Technical Note #7, *Apple II Family Identification*, rev. May 1991. |
| **[PoP]** | Jordan Mechner's Prince of Persia source, [github.com/jmechner/Prince-of-Persia-Apple-II](https://github.com/jmechner/Prince-of-Persia-Apple-II) at commit `2524355`. |
| **[WAD]** | Internet Archive item `wozaday_Prince_of_Persia` (file listing only). |

## Conventions

- **1 tick = 1 ns** (#5 §B, #12). PHI0 is a `Clock` with `halfPeriod = 490` ticks.
- A **CPU cycle** starts at a PHI0 falling edge. PHI0 is low for the first half (PHI1), and high for the second (PHI2).
- The **horizontal state** `hc` runs 0–64 within a scan line. The **vertical count** `v` is a 9-bit value that runs `$0FA`–`$1FF` (§6).
- "Floating" means that nothing drives the data bus, so a read returns the video byte from the same cycle (§6.3).

---

## 1. Prince of Persia requirements

Release analysed: the 5.25" build, i.e. the `S/*525.S` variants plus the shared `S/` modules and the copy-protected boot in `CP/`. `S/VERSION.S:9` reads `"Prince of Persia 1.0  1/23/90 special"`, and `01 POP Source/Source/VERSION.S` reads `1.0 9/7/89`. `01 POP Source/Source` holds the 3.5" variants of the same modules (its `GRAFIX.S` is identical to `S/GRAFIX.S`). The source can't say which build the retail disk holds. The target media is the preserved original disk in [WAD]: **two WOZ images, `Prince of Persia side A.woz` and `side B.woz`** (233,481 bytes each; both are WOZ 1 files, with no FLUX chunk, §10).

### 1.1 Memory: 128K

- The boot rejects anything but a IIe or IIc with 128K and prints `REQUIRES A //C OR //E WITH 128K` [PoP `CP/POPBOOT0.S:189–233`]:
  - `$FBB3` must be `$06` (IIe/IIc/IIGS family byte [TN7]);
  - `$C017` (SLOTC3ROM) must read 0. The firmware sets SLOTC3ROM only when the auxiliary slot is empty [Sather 5-28];
  - a test routine copied to `$0180` writes `$EE` to `$0800` with RAMWRT=aux, then compares `$0800` and `$0C00` with RAMRD=aux. This rejects the 1K 80-column card, whose memory repeats every 1K [`CP/POPBOOT0.S:158–176`].
- The game uses the whole 128K. Main memory holds the hi-res pages and data. Code runs from **aux memory** (`grafix` at aux `$0400`) and from **both language cards**: main LC has RW18 at `$D000` bank 1, `hrtables` `$E000`, `unpack` `$EA00` (bank 2), `hires` `$EE00` and `master` `$F880`; aux LC has `bluecopy` `$D000` bank 1 [`S/EQ.S:9–31`, `S/UNPACK525.S` header].
- It switches ALTZP constantly (aux zero page and stack). It also uses all RAMRD/RAMWRT combinations, and both LC banks through the two-read write-enable (`bit RWBANK1` twice) [`S/MASTER525.S:403–404`, `CP/POPBOOT0.S:241–242`].

**Required:** main and aux 64K, both language cards, the full MMU (§5), and code fetched from aux memory.

### 1.2 65C02-only opcodes: none on a IIe

A scan of every `.S` file for 65C02/65816 code (the Merlin `xc` directive; the `STZ PHX PHY PLX PLY BRA TSB TRB` mnemonics; accumulator `INC`/`DEC`; `BIT #`; `JMP (a,X)`; and `(zp)` addressing) finds only these. All of them are behind IIGS or IIc checks:

| Where | What | Guard |
|---|---|---|
| `S/GRAFIX525.S:2077–2101` `FASTSPEED`/`NORMSPEED` | `tsb`/`trb $C036` (IIGS speed register) | `lda IIGS / beq` |
| `S/GRAFIX525.S:2111–2150` `getparam`/`setparam` | 65816 `xce`/`rep`/`jsl` (IIGS toolbox) | `lda IIGS / beq` |
| `S/UNPACK.S:659–841` `FADEIN` etc.; `S/SHSTUFF.S` | 65816 super hi-res fades | IIGS only (`$C029` and `$C035` are IIGS registers) |
| `CP/LOSHOW.S:308` | `bra :isGS` | IIGS path |

`IIGS` is set by `CHECKIIGS` [`S/GRAFIX525.S:2013–2030`]. It checks `$FBB3 = 6`, then `sec / jsr $FE1F`, which leaves carry set on every 8-bit Apple II because `$FE1F` holds an `RTS` there [TN7]. **PoP therefore runs on an NMOS 6502, and #86 stays off the critical path.**

The 5.25" **RW18** disk driver (Roland Gustafsson's 18-sector RWTS, loaded to main LC `$D000`) is **not in the repository**: only the 3.5" UniDisk version (`02 POP Disk Routines/RW1835/`) and the install and format tools are. It was checked on the disk instead (§10). Its five pages were extracted from track 0 of side A, decoded the way `POPBOOT0` does it (sectors at skew `0E 0C 0A 08 06`, XORed with page `$2F`), and disassembled from `$D000` and the eight entries of its command table at `$D3C4`. All 493 reachable instructions are documented NMOS opcodes. The unreached bytes are the 6-and-2 translate table, the head-stepping delay tables, the copyright string and the dispatch table. The behavioral CPU's opcode tripwire (§8, M3) stays as a cheap guard.

### 1.3 Video modes

- **Hi-res page flipping:** pages 1 and 2 through `$C054`/`$C055` with `$C057` HIRES on [`S/MASTER525.S`, `S/SUBS.S`].
- **Double hi-res** for the splash, prologue and epilogue screens. `SETDHIRES` [`S/UNPACK525.S:614–627`] sets RAMRD/RAMWRT=aux, waits for VBL, writes `$C00D` (80COL on), reads `$C057` (HIRES on), toggles `$C05E`/`$C05F` five times ending on `$C05E` (AN3 off = DHR on; the comment says "for old Apple RGB card"), and then writes `$C050` (TEXT off). It is called for the publisher credit, the prologue and the epilogue [`S/MASTER525.S:872, 1013, 1029`]. **Double hi-res (#66) is therefore required for PoP**, not optional (§9).
- **40-column text** for prompts and the boot (`jsr text`, `home`) [`CP/POPBOOT0.S:66–70`]. The boot turns off 80STORE, 80COL and ALTCHARSET [`CP/POPBOOT0.S:57–63`].
- **No lo-res, no MIXED mode** (only `$C052` MIXED off is used), and **no 80-column text** in the game.

### 1.4 Soft switches used

Accesses found by symbol and literal address in the 5.25" files (`S/` and `CP/`):

- **MMU:** `$C000`/`$C001`, `$C002`–`$C005`, `$C008`/`$C009`, `$C00C`/`$C00D`, `$C00E`; `$C081`–`$C083` and `$C08B` (language card); status `$C017`.
- **IOU:** `$C050`–`$C057` (no `$C053`), `$C05E`/`$C05F`.
- **Keyboard and game I/O:** `$C000`, `$C010`; `$C061`/`$C062` (buttons); `$C064`/`$C065` and `$C070` (paddles 0 and 1 via its own `PREAD`, `S/GRAFIX525.S:1199–1225`).
- **Speaker:** `$C030` (`spkr`, `S/SOUND.S:16`).
- **Disk II:** `$C080,X`–`$C08E,X` (phases, motor, Q6/Q7, data register) in the boot and the protection checks.
- **VBL:** `$C019` (`VBLANK`, `S/GRAFIX525.S:1941–1946`).
- **IIc and IIGS only:** `$C05B`, `$C078`/`$C079`, `$C07E`/`$C07F`, `$C022`, `$C029`, `$C034`–`$C036`. The IIc VBL-interrupt path is selected only when `$FBC0 = $00` (IIc ID byte [TN7]) [`S/GRAFIX525.S:1984–1999`].

### 1.5 VBL and the floating bus

- `VBLANK` loops while `$C019` bit 7 is 0, then while it is 1. It returns at the **start** of vertical blank, so it depends on the IIe polarity (**bit 7 = 0 during VBL**, §5.2) [`S/GRAFIX525.S:1941–1946`]. The rotating-cube easter egg does the same thing [`CP/POPBOOT0.S`, `rotcube`].
- **The game never reads the floating bus.** The only floating-bus reads are `eor $C020` "a little randomness" in an easter egg random generator [`CP/POPBOOT0.S:876, 927`]. A floating bus is still needed for a correct IIe (§6.3), but PoP doesn't depend on its exact values.

### 1.6 Input, sound and interrupts

- **Keyboard:** polled at `$C000`, strobe cleared at `$C010`.
- **Buttons:** `$C061`/`$C062` (open/closed apple and joystick buttons).
- **Joystick:** optional, read through PoP's own `PREAD`. It counts loop passes and calibrates a centre (`SETCENTER`), so it only needs monotonic paddle timing.
- **Sound:** speaker toggles from timed loops.
- **Interrupts:** none on a IIe. `cli` appears only in the IIc VBL path [`S/GRAFIX525.S:1953–1966`].

### 1.7 Disk access and copy protection

- **Boot.** The Disk II slot ROM loads track 0 sector 0 to `$0800`. The boot code then reads 15 more sectors by calling **the slot ROM's sector-read routine at `$Cn5C`**: `:rdsect jsr $005c` with the high byte patched to `$Cn` [`CP/POPBOOT0.S:84–96`]. It decodes them, copies RW18 to main LC bank 1 [`moverw18`, `:240–262`], and jumps to stage 3 at `$EE00` [`:136–153`]. **The real 16-sector slot ROM is required.**
- **RW18:** Gustafsson's 18-sector format with a Brøderbund ID byte per disk side: side A `$A9`, side B `$AD` [`S/MASTER525.S:45–46`]. Game code and levels are spread across **both sides**. When it reads a side that doesn't match the ID it expects, the game prompts the user to insert the other side [`checkdisk`/`error`, `S/MASTER525.S:502–515, 1480`]. It uses one drive (drive 1).
- **Writes:** **SAVEGAME writes track 23 of side B** [`S/MASTER525.S:523–546`], so saving needs Disk II write support (#72) and image write-back.
- **Copy protection.** The HLS checks (`CheckCP` in the boot [`CP/POPBOOT0.S:314–416`], and `YELLOW`/`PURPLE` during play [`CP/YELLOW.MAIN.S`, `CP/PURPLE.MAIN.S`]) find physical sector 7 of track 0. They read `$D5 $E7 $E7 $E7`, then **access `$C08D,X` in the middle of the nibble stream** (Q6 high, load or sense mode), wait with a cycle-counted `bit $6 ; 3 US`, and resynchronise to "1/2 nibbles" [`CP/POPBOOT0.S:358–375`; `CP/YELLOW.MAIN.S:120–122`; `CP/PURPLE.MAIN.S:104–106`]. **This needs a Disk II modelled at the Logic State Sequencer level, driven by the real sequencer ROM, reading a bitstream image (WOZ)**, with CPU and LSS timing exact to the cycle (§3.1–§3.3). Nibble-level images (NIB) can't reproduce it.

### 1.8 ROM routines

The boot calls Monitor routines in ROM: `TEXT` `$FB2F`, `HOME` `$FC58`, `NORMAL`, `PR#0` and `IN#0` [`CP/POPBOOT0.S:12–19, 66–72`]; its error path uses `VTAB` and `COUT`, and the easter eggs use Applesoft's `HGR`/`HPLOT` (`$F3E2`, `$F457`) [`:576–654`]. **The real IIe ROM is required** (§7).

---

## 2. Machine model

| Item | Choice | Justification |
|---|---|---|
| Motherboard | **Apple IIe, Revision B, unenhanced ROM, NMOS 6502** | Rev A can't display double hi-res: it has no FRCTXT' line, so AN3 can't enable DOUBLE-RES timing [Sather 8-12, Table 8.6 note]. PoP needs DHR (§1.3) and no 65C02 opcodes (§1.2). The unenhanced ROM has ID `$FBC0 = $EA` [TN7]. |
| Auxiliary slot | **Extended 80-Column Text Card (64K)** | PoP needs 128K and rejects the 1K card (§1.1). On a Rev B board, the 64K card also links AN3 to FRCTXT' for DHR [Sather Table 8.6 note]. |
| Slot 6 | **Disk II controller, 16-sector ROMs, one drive** (optional second) | PoP boots from it and needs LSS-level behaviour (§1.7). Slot 6 is the conventional boot slot. PoP itself works from any slot, since it uses the slot left by the boot ROM [`CP/POPBOOT0.S:74–84`]. |
| Game port | 2 paddles (joystick) and 3 buttons | §1.6 |
| Other slots | empty | Nothing else is needed. With nothing in the slots, **IRQ' and NMI' are never asserted**: on the IIe both are connected only to the peripheral slots [Sather 4-15]. |

An enhanced IIe (65C02 + enhanced ROMs) becomes a configuration option with #86, but it is not part of Phase 1's success criteria.

---

## 3. Timing model

### 3.1 Confirmed facts

- **Master clock:** 14.31818 MHz. A normal CPU cycle is 14 master periods (977.8 ns), and the **long cycle** is 16 (1117.5 ns). The composite rate is **1.02048 MHz**, and the average cycle is 979.93 ns, with an average half-cycle of 489.96 ns [Sather Table 3.1, p. 3-5].
- **The long cycle stretches PHI2** (PHI0 high) by 140 ns [Sather pp. 4-5–4-6]. It occurs once per scan line, straddling horizontal states `$40` and `$41` on the IIe (on the Apple II it is at `$00`) [Sather Appendix I, p. I-3].
- **Scan structure:** a 65-state horizontal counter (`0000000`, then `1000000`–`1111111`). Picture data is output during 40 states and blanked during 25. A 262-state vertical counter (`011111010`–`111111111`) has 192 displayed lines and 70 blanked. That gives **17,030 cycles per frame** (16.688 ms, 59.92 Hz) [Sather pp. 3-13–3-14].
- **Disk II:** the sequencer is clocked by Q3 falling, which is **2 MHz, so 2 steps per CPU cycle** [Sather pp. 9-14–9-15]. The data register shifts every eighth sequencer clock, **once every four CPU cycles, i.e. 4 µs bit cells** [Sather Ch. 9, *The WRITE Sequence*]. Q3 is one of the signals stretched by the long cycle [Sather 3-5], so sequencer steps stay locked at 2 per CPU cycle even then.

### 3.2 Decision: uniform PHI0 is enough

We keep #8 §C's default: **a uniform PHI0 with `halfPeriod = 490` ticks**, 980 ns per cycle, i.e. **1.020408 MHz**. This is 0.007% slower than the real composite rate. Video counts PHI0 cycles in the 65 × 262 structure.

Reasons:

- PoP synchronises to the display only by polling `$C019` (§1.5). It has no raster tricks, so it can't observe where the long cycle falls within a line.
- The CPU-to-LSS ratio (2 steps per cycle) is exact in both models (§3.1). The cycle-counted protection code (§1.7) therefore sees the same sequencer behaviour.
- Only the speed of the disk rotation relative to the CPU changes. That stays within 0.01% because the average cycle length is preserved (§3.3), which is far inside the tolerance of real drive speed.

The optional clock-generator device of #8 §C (exact 14/16-period cycles) is **not required** for any Phase 1 milestone. If it is ever built, it must place the stretch on PHI2 at horizontal states `$40`/`$41`.

### 3.3 Disk rotation timing

The drive device must advance the bitstream by **simulated time, not by sequencer steps**:

- One bit cell is 4 µs (WOZ 1), or `optimalBitTiming × 125 ns` (WOZ 2 INFO chunk).
- With 980 ns cycles, a bit cell lasts 4.0816 CPU cycles, i.e. 8.16 sequencer steps.
- Devices can't read ticks (#5 §C). The drive therefore takes the PHI0 half-period as a constructor parameter and keeps a **fractional accumulator** in ns, advanced by 490 on each PHI0 edge it counts.
- Advancing one bit per 8 sequencer steps would spin the disk 2% too fast (306 rpm). Avoid it.

### 3.4 Power-up and reset timing

- At power-up the video scanner presets to all zeros. The IOU holds RESET' low until the scanner first overflows: **512 horizontal scans, 33,280 cycles, 32.6 ms**. After that the vertical counter follows its normal `$0FA`–`$1FF` cycle [Sather p. 3-19, Table 3.3].
- The Disk II controller extends the power-up reset to about 100 ms [Sather 9-13]. With the controller in slot 6, the machine therefore holds RES low for **102,000 cycles** at power-on. That is a fixed constant, so golden runs are deterministic. The real value comes from an RC timer, not a counter.
- A Ctrl+Reset from the keyboard drives RESET' while it is held [Sather 1-9–1-10; §5.3].

---

## 4. Pin-level bus protocol

This confirms #8 §D and makes it precise. Times are in ticks (ns) relative to PHI0 edges. The CPU limits are from [MOS] for 1 MHz parts.

### 4.1 CPU side (the contract any 6502 implementation must meet)

| Signal | Rule | Source |
|---|---|---|
| `A0–A15`, `R/W`, `SYNC` | Change only in **(fall, fall + 300]**. Hold the previous value for ≥ 30 after the fall, and stay stable until the next fall. | T<sub>ADS</sub>, T<sub>RWS</sub> ≤ 300; address and R/W hold ≥ 30 |
| `D0–D7`, write cycle | Driven by **rise + 200** at the latest, held until **fall + 30**, then released (Z) before the next rise. | T<sub>MDS</sub> ≤ 200; T<sub>HW</sub> ≥ 30 |
| `D0–D7`, read cycle | **Sampled at the PHI0 falling edge.** Data must be valid ≥ 100 before it. | T<sub>DSU</sub> ≥ 100 |
| Inputs `RDY`, `IRQ`, `NMI`, `RES`, `SO` | Board-driven. They change only shortly after a PHI0 rise (see 4.2), so they are stable around the CPU's sampling points. | T<sub>RDY</sub> setup ≥ 100 |

Every bus cycle appears on the pins, **including dummy reads and writes**, exactly as in SingleStepTests' per-cycle traces (#8 §D, §H).

**Reset fingerprint (part of the contract).** On RESET the 6502 performs three reads at descending page-1 addresses (`$01xx`, R/W high), then reads `$FFFC`/`$FFFD` [Sather 4-14]. **The MMU detects a system reset from this address sequence, not from the RESET' line** (§5.3). An implementation that skips or reorders these cycles breaks the MMU reset. #57 and #82 must test this at the pins.

### 4.2 Board side (the backplane device)

| Event | Action |
|---|---|
| PHI0 **rise** | Read `A` and `R/W` (stable since fall + 300). Dispatch the bus cycle once. On a read, **drive `D` with delay `t_acc = 100`**; it must be ≤ 390 (490 − T<sub>DSU</sub>). Read side effects happen here, and the value returned is the one from before the side effect (e.g. `$C010` returns the key, then clears the strobe). |
| PHI0 **fall** | On a write, read `D` and commit it; write side effects happen here. On a read, **release `D` with delay `t_hold = 30`**. The datasheet minimum is T<sub>HR</sub> ≥ 10; 30 leaves margin for gate-level CPUs, whose input latches close a few gate delays after the edge (about 2 ns per level in RefSim, #5 §B). |
| Inputs | `RES`, `IRQ`, `NMI`, `RDY` and `SO` are driven with delay 1 after a PHI0 rise. |

- There is no bus contention by construction. The board releases at fall + 30, and the earliest the CPU drives write data is in the next PHI2, ≥ 490 later.
- When nothing responds to a read, the board **still drives `D`** with the floating-bus value (§6.3). The CPU-facing bus is therefore never `None` on a read, as in hardware, where the bus capacitance holds the last value.

### 4.3 Recommended delays for the behavioral CPU device (#59)

- `A`, `R/W`, `SYNC`: fall + 100.
- Write data: rise + 100.
- Release `D`: fall + 30.

These are mid-window values, so the pin-level harness can detect implementations that drift toward the limits. The harness should also check every 6502 implementation against the windows in 4.1. A STA run (#23) on the hand-written CPU must show its critical paths fit within them at 490 ns per half-cycle.

---

## 5. Soft switches

### 5.1 Write-activated and read/write switches

"W" means a write activates the switch; "R/W" means any access does. MMU = memory management unit; IOU = I/O unit.

| Address | Access | Switch / function | Chip | RESET clears? |
|---|---|---|---|---|
| `$C000` / `$C001` | W | 80STORE off / on | MMU + IOU | yes |
| `$C002` / `$C003` | W | RAMRD off (main) / on (aux) | MMU | yes |
| `$C004` / `$C005` | W | RAMWRT off (main) / on (aux) | MMU | yes |
| `$C006` / `$C007` | W | INTCXROM off (slot ROM) / on (internal ROM at `$C100–$CFFF`). Apple calls it SLOTCXROM; the sense is the same. | MMU | yes |
| `$C008` / `$C009` | W | ALTZP off / on (aux `$0000–$01FF` and aux LC) | MMU | yes |
| `$C00A` / `$C00B` | W | SLOTC3ROM off (internal `$C3xx`) / on (slot 3 ROM) | MMU | yes |
| `$C00C` / `$C00D` | W | 80COL off / on | IOU | yes |
| `$C00E` / `$C00F` | W | ALTCHARSET off / on | IOU | yes |
| `$C010`–`$C01F` | W | Clear KEYSTROBE (any write) | IOU | — |
| `$C010` | R | Any-key-down in bit 7, ASCII in bits 0–6; **clears KEYSTROBE** | IOU | — |
| `$C020`–`$C02F` | R/W | Toggle cassette output | IOU | — |
| `$C030`–`$C03F` | R/W | **Toggle speaker** | IOU | — |
| `$C040`–`$C04F` | R/W | Utility strobe output pulse | IOU | — |
| `$C050` / `$C051` | R/W | TEXT off / on | IOU | **no** |
| `$C052` / `$C053` | R/W | MIXED off / on | IOU | **no** |
| `$C054` / `$C055` | R/W | PAGE2 off / on | MMU + IOU | yes |
| `$C056` / `$C057` | R/W | HIRES off / on | MMU + IOU | yes |
| `$C058`–`$C05F` | R/W | AN0–AN3 off / on (pairs). **AN3 off (`$C05E`) enables double hi-res** with 80COL on and HIRES on. | IOU | yes (all annunciators) |
| `$C070`–`$C07F` | R/W | Trigger all four paddle timers | game I/O | — |
| `$C080`–`$C08F` | R/W | Language card (§5.4) | MMU | yes |
| `$C080 + 16n` – `$C08F + 16n`, n = 1–7 | R/W | Slot n DEVICE SELECT' (slot 6: `$C0E0`–`$C0EF`, Disk II) | slots | card-specific |
| `$C3xx` access with SLOTC3ROM off | R/W | Set INTC8ROM (internal ROM at `$C800–$CFFF`) | MMU | yes |
| `$CFFF` | R/W | Clear INTC8ROM; slot cards release `$C800–$CFFF` | MMU + slots | — |

Sources: [TRM] Tables 2-2, 2-10, 2-11, 2-12, 4-6 (p. 82), 4-7 (p. 87), 6-5 (p. 142); [Sather] MMU switch table (Chapter 5), p. 5-28 (INTCXROM/SLOTC3ROM/INTC8ROM truth table and `$CFFF`), Ch. 7 keyboard circuitry (KEYSTROBE), Table 8.6.

Notes:

- `$C000–$C00F` switches respond to **writes only**. Reading them returns the keyboard [TRM Ch. 6 note on Table 6-5].
- `$C050–$C05F`, `$C030` and `$C070` respond to **every** access, dummy cycles included (#8 §D).
- **Slot `$C3xx`/`$C800` truth table** [Sather 5-28]: `$C100–$C2FF` and `$C400–$C7FF` are internal iff INTCXROM. `$C300–$C3FF` is internal iff INTCXROM **or** ¬SLOTC3ROM. `$C800–$CFFF` is internal iff INTCXROM **or** INTC8ROM.
- **Not on the IIe:** [TRM] Table 2-10 lists IOUDIS/RDIOUDIS/RDDHIRES at `$C07E`/`$C07F`, and the IIc VBL interrupt uses `$C05A`/`$C05B` and `$C078`/`$C079`. Sather's description of the IIe IOU (Table 8.6, Chapter 7) has no such switches, and PoP uses them only on its IIc path (§1.4). **We don't implement them.** On our IIe, `$C07x` only triggers the paddles, and `$C05A`/`$C05B` are AN1.

### 5.2 Read addresses

| Address | Bit 7 | Bits 0–6 |
|---|---|---|
| `$C000`–`$C00F` | KEYSTROBE | ASCII of the last key |
| `$C010` | AKD (any key down) | ASCII (and the read clears KEYSTROBE) |
| `$C011`–`$C01F` | status (below) | **ASCII of the last key** |
| `$C060`–`$C067` | input (below) | **floating** |
| `$C068`–`$C06F` | same as `$C060`–`$C067` (the multiplexer decodes A0–A2) | floating |
| all other `$C020`–`$C0FF` with no responder, `$C100`–`$CFFF` with no ROM selected | floating | floating |

Status bits (1 = on), [TRM] Tables 2-10, 4-6, 4-7, 6-5:

| Address | Bit 7 | Address | Bit 7 |
|---|---|---|---|
| `$C011` | LC bank 2 selected | `$C019` | **VBL' — 0 during vertical blank** |
| `$C012` | LC RAM read enabled | `$C01A` | TEXT |
| `$C013` | RAMRD | `$C01B` | MIXED |
| `$C014` | RAMWRT | `$C01C` | PAGE2 |
| `$C015` | INTCXROM | `$C01D` | HIRES |
| `$C016` | ALTZP | `$C01E` | ALTCHARSET |
| `$C017` | SLOTC3ROM | `$C01F` | 80COL |
| `$C018` | 80STORE | | |

| Address | Bit 7 | Address | Bit 7 |
|---|---|---|---|
| `$C060` | cassette input | `$C064`–`$C067` | paddle timer 0–3 running |
| `$C061`–`$C063` | push buttons 0–2 | | |

- **`$C01x` low bits come from the keyboard.** The MMU enables the keyboard ROM onto the data bus for every read of `$C000–$C01F` [Sather 5-29], and the keyboard pin-out table lists `R$C011–$C01F` as "Read IOU or MMU soft switch **and ASCII**" [Sather Ch. 7]. This contradicts #61, which says floating bus (§9).
- **`$C019` polarity:** the register reads VBL', the inverse of vertical blanking. Bit 7 is 0 from just after the last displayed byte of line 191 until the same horizontal point at the end of the last blanked line [Sather Ch. 3 application notes, p. 3-23ff: `MINUS => VBL'`, `PLUS => VBL`]. PoP relies on this (§1.5).
- `$C06x`: only D7 is driven, by the serial-input multiplexer; D0–D6 float [Sather 5-40].
- After an MMU reset, every MMU status bit reads 0 **except `$C011`**, which reads 1 because bank 2 is selected [Sather, MMU functional diagram notes, Chapter 5].

### 5.3 RESET

There are two reset mechanisms. Model both.

1. **MMU reset by address fingerprint.** "Any time the address bus contains three Page 1 addresses in sequence followed by `$FFFC`, the MMU assumes that a system reset is occurring, and resets all of its soft switches." The MMU doesn't tell ascending from descending page-1 accesses, so a program running in page 1 can trigger it too [Sather 4-14]. The backplane implements this on its stream of bus cycles. False triggers included: that is hardware behaviour. The reset clears:
   - 80STORE, RAMRD, RAMWRT, INTCXROM, ALTZP, SLOTC3ROM and INTC8ROM;
   - the language card: BANK1 = 0 (bank 2), HRAMRD = 0 (ROM read), HRAMWRT' = 0 (RAM **write-enabled**), PRE-WRITE = 0 [Sather pp. 5-23–5-24; TRM p. 81].
2. **IOU reset on RESET' falling.** All IOU display switches except **TEXT and MIXED** are reset: 80STORE, 80COL, ALTCHARSET, PAGE2, HIRES and **AN0–AN3** [Sather Table 8.6 note; annunciators: Sather Ch. 7 app. note]. The firmware then sets text mode and, per Sather, AN2/AN3.

Not reset: RAM contents, the video scanner and flash counter (both free-running), and KEYSTROBE (cleared only at power-up or by `$C010` access / `$C01x` write [Sather Ch. 7 keyboard circuitry, item 8]). The **Disk II controller**: RESET' forces all its switches off (phases, motor, drive 1 selected, Q6, Q7), and the motor stops **immediately**, without the 1 s spin-down [Sather 9-12–9-13].

RESET' is driven by the power-up timer (§3.4) and by Ctrl+Reset (the keyboard device, #68).

### 5.4 Language card (`$C080–$C08F`)

Per [Sather pp. 5-23–5-24] (more precise than [TRM] Table 4-6):

- **A3** selects the `$D000` bank: `$C080–$C087` → **bank 2**, `$C088–$C08F` → bank 1. [TRM] p. 81's prose gives the opposite sense, but its own table and Sather agree on the above.
- **A0 and A1** set HRAMRD: access to `$C080/3/4/7/8/B/C/F` enables RAM read; `$C081/2/5/6/9/A/D/E` select ROM.
- **Write enable (two-read rule):**
  - an **odd read** sets PRE-WRITE; an odd read while PRE-WRITE is already set enables RAM writes (clears HRAMWRT');
  - an **even access** (read or write) disables writes (sets HRAMWRT') and clears PRE-WRITE;
  - **any write** clears PRE-WRITE, and an odd write leaves HRAMWRT' as it was;
  - so writing to an odd address changes HRAMRD without touching write enable.
- **Dummy cycles count** (6502 RMW and indexed dummy reads).
- Reading RAM selects the same bank for writing. There is no split.
- With ALTZP on, `$D000–$FFFF` RAM comes from the aux card [TRM p. 87].
- Reset state: §5.3.

### 5.5 Memory banking (MMU)

- `$0000–$01FF`: main or aux by **ALTZP**.
- `$0200–$BFFF`: reads by **RAMRD** and writes by **RAMWRT**, **except** when 80STORE is on. Then `$0400–$07FF` is selected by **PAGE2** (aux if on) regardless of RAMRD/RAMWRT, and so is `$2000–$3FFF` if HIRES is also on [TRM pp. 86–87].
- `$C000–$CFFF`: I/O and ROM (§5.1). RAM never appears there [Sather 5-24].
- `$D000–$FFFF`: ROM or LC RAM (§5.4), main or aux by ALTZP.

---

## 6. Video scanner and floating bus

### 6.1 Counters

- **Horizontal:** `hc` = 0–64. `hc = 0` is the preset state (HPE' low, H = 0). For `hc = 1–64`, `H = hc − 1` (0–63), with bits H0–H5.
- **Vertical:** `v` = `$0FA`–`$1FF`, nine bits named, from LSB: VA, VB, VC, V0, V1, V2, V3, V4, V5. `v` increments after `hc = 64`, and `$1FF` wraps to `$0FA` [Sather 3-13].
- **Display window:** displayed bytes are `hc = 25–64` (H = 24–63, i.e. H5H4H3 ≥ `011`) on lines `v = $100–$1BF`. Horizontal blanking is the other 25 states, including `hc = 0`, so the first HBL address is scanned **twice** [Sather 5-10]. **VBL ⇔ V4 ∧ V3**: lines `$1C0–$1FF` and `$0FA–$0FF`, 70 lines [Sather 5-8].

### 6.2 Scanner address (every cycle, including blanking)

From [Sather] Table 5.1 (p. 5-8) and the IOU adder (p. 5-9). The scanner fetches during PHI1 of each cycle:

```
A0–A2   = H0–H2
A3–A6   = SUM, where SUM = ( (¬H5,¬H5,H4,H3) + (V4,V3,V4,V3) + 1 ) mod 16
A7–A9   = V0–V2
P2      = PAGE2 ∧ ¬80STORE
if HIRESTIME:  A10–A12 = VA,VB,VC;  A13 = ¬P2;  A14 = P2;  A15 = 0
else:          A10 = ¬P2;  A11 = P2;  A12–A15 = 0
HIRESTIME = HIRES ∧ ¬TEXT ∧ ¬(MIXED ∧ V4 ∧ V2)
```

HIRESTIME is from [Sather pp. 5-5–5-7, 5-13]. In mixed mode, `V4 ∧ V2` is true for the four text rows at the bottom of the screen, and also during part of VBL, where it doesn't matter.

I checked this numerically against Sather's facts:

- text row bases are `$400 + 128·(row mod 8) + 40·(row div 8)`;
- hi-res line bases are `$2000 + 1024·(y mod 8) + 128·((y div 8) mod 8) + 40·(y div 64)`;
- HBL scans the 24 bytes before the display address, with 128-byte wrap (line 0: `$468`, `$468`, `$469`, …, `$47F`);
- VBL scans "top third minus eight bytes" (`$478`…);
- lines `$0FA–$0FF` scan the same addresses as `$1FA–$1FF` [Sather pp. 5-10, 5-13].

#63 must include these as unit tests.

In 80-column and double hi-res modes, the scanner reads the **same address from main and aux** in each cycle, into separate video latches [Sather Appendix I, "RAM Output Data Distribution"].

### 6.3 Floating bus

- When a read gets no response, the data bus keeps **the motherboard (main RAM) byte the scanner fetched during PHI1 of the same cycle** [Sather p. 5-40; Ch. 7 *I/O Timing* (p. 7-23ff), Fig. 7.7].
- This applies to non-responding addresses in `$C020–$CFFF`: the cassette, speaker, strobe, screen-switch, annunciator, paddle-trigger and `$C08x` language-card switches, and empty slot ranges [Sather pp. 5-40–5-41; Ch. 7 *I/O Timing*], plus D0–D6 of `$C06x` (§5.2).
- It does **not** apply to `$C000–$C01F`, which return keyboard data (§5.2).
- The value returned is always the main-RAM byte, even in 80-column modes.

Model: the video chip keeps `lastScannedMain` from the current cycle's scanner address. The backplane drives it whenever no `BusTarget` claims a read (§4.2).

### 6.4 Frames, flash and auto-repeat

- **Frame boundary** for frame hashes and frame-buffer publication: **the start of VBL**, i.e. the `v` transition `$1BF → $1C0`.
- **Flash counter:** a 5-bit counter (F0–F4) of frames, incremented at scanner overflow (`$1FF → $0FA`). Flashing text uses F4, so it toggles every 16 frames (1.87 Hz) [Sather Table 3.3, p. 3-19; 8-14].
- **Keyboard auto-repeat is in hardware**, in the IOU, timed by frames:
  - after a key has been held for **32–48 frames** (534–801 ms; the exact count depends on F3's phase at the keypress), AUTOSTRB sets KEYSTROBE **every 4 frames** (15 Hz);
  - any new key or key-up clears the delay [Sather Table 3.3; Ch. 7 keyboard items 6–8].
  - #68 implements it with these frame counts.

### 6.5 Text modes (unenhanced character ROM)

From [Sather Tables 8.2–8.3, pp. 8-12–8-14]:

- **ALTCHARSET off:**
  - `$00–$3F` inverse;
  - `$40–$7F` **flashing** (no flashing patterns are stored; flashing alternates the inverse and normal glyphs of the upper-case/special set using F4);
  - `$80–$FF` normal, with lower case at `$E0–$FF`.
- **ALTCHARSET on:** `$00–$7F` inverse (including inverse lower case), `$80–$FF` normal. No MouseText; that needs the enhanced ROM.

---

## 7. Assets and configuration

### 7.1 Files

None of these are committed or available in CI (#8 §B). The reviewer has them.

| Asset | File (under `$CPU_LEGO_ASSETS/apple2e/`) | Size | Layout | Validation |
|---|---|---|---|---|
| System ROM, unenhanced IIe | `apple2e.rom` | 16,384 | `$C000–$FFFF` image: file offset 0 = `$C000`. The `$C000–$C0FF` bytes are unused (I/O). The two 8K chips, "CD" 342-0135-A (`$C000–$DFFF`) and "EF" 342-0134-A (`$E000–$FFFF`), concatenated in that order. | `$FBB3 = $06`, `$FBC0 = $EA`, `$FE1F = $60` (RTS) [TN7] |
| Video (character) ROM | `apple2e-video.rom` | 4,096 | Raw chip dump: the US 342-0133-A, or the **second 4 KiB** (US set) of the 8 KiB German/US 341-0161-A. Address bits: A0–A2 = SEGA–SEGC (glyph row), A3–A8 = VID0–VID5, A9 = RA9, A10 = RA10, A11 = GR+2 (text in the low 2 KiB, graphics in the high 2 KiB) [Sather 8-11–8-14; confirmed on a dump, §10]. Bit 0 is the **leftmost** dot, bit 7 is unused. Glyphs for RA10 = 1 (normal text) are stored **inverted** (1 = dark); those for RA10 = 0 (inverse and flashing) are stored lit. | Size; the `A` glyph (`08 14 22 22 3E 22 22 00`) at offset `$008` (lit) and `$408` (dark) |
| Disk II boot ROM (16-sector) | `disk2-boot.rom` | 256 | P5A 341-0027-A, mapped at `$Cn00`; contains the `$Cn5C` read routine PoP calls (§1.7). **Some circulating dumps are scrambled** by the reader's pin mapping: in the `ROMS.ZIP` set, data bits 4–7 and address bits 5–7 are reversed. Unscramble them before use (§10). | Bytes `$00–$07` = `A2 20 A0 00 A2 03 86 3C`; bytes `$5C–$60` = `18 08 BD 8C C0`; `4C 01 08` (JMP `$0801`) at `$F8` |
| Disk II sequencer ROM (16-sector / DOS 3.3) | `disk2-sequencer.rom` | 256 | P6A 342-0028-A, raw dump in **physical address order**. The device applies Sather's input wiring: the next-state outputs **D4, D5, D6, D7 feed back to A5, A0, A6, A7** (in that order), QA on A1, SHIFT/LOAD on A2, READ/WRITE on A3, and the read pulse on A4, **active low** (A4 = 0 while a pulse is present). D0–D3 hold the data-register command: `0–7` CLR, `8`/`C` NOP, `9` SL0, `A`/`E` SR, `B`/`F` LD, `D` SL1 [Sather 9-14–9-15, Table 9.3; wiring and polarity confirmed by simulation, §10]. | Size; bytes `$00–$07` = `18 D8 18 08 0A 0A 0A 0A` |
| PoP disks | `pop-side-a.woz`, `pop-side-b.woz` | 233,481 each (WOZ 1) | [WAD]. Side A is write-protected; side B (which holds the saved game, §1.7) is not. | Header `WOZ1`/`WOZ2`; content hash recorded in scenarios |
| ProDOS boot disk | `prodos.woz` (or `.po`/`.dsk`) | — | ProDOS 8 **1.9 or earlier** (2.0.x needs a 65C02) | — |
| Commercial app | `appleworks.woz` (or `.po`) | — | AppleWorks **2.x** (unenhanced IIe, 128K) | — |

The keyboard ROM (matrix to ASCII) is **not** needed. The keyboard device receives ASCII codes from the host (#68).

### 7.2 Mechanism

- **Location:** the assets helper from #56 (`util.ExternalAssets`, PR #95) resolves `$CPU_LEGO_ASSETS` (default `~/.cache/cpu-lego`). Copyrighted assets **have no download URL**. Add a lookup that only checks for a local file, for example `ExternalAssets.local("apple2e/apple2e.rom"): Either[String, Path]`. It must never fetch. The host frontend (#67) accepts the same names as overrides on the command line (`--rom`, `--video-rom`, `--disk1`, …).
- **Validation:** the checks in the table run on load. A wrong file is an error that names the check, **not a skip**.
- **Skip-when-absent:** a test that needs an asset calls the lookup. On `Left` it reports the test as **skipped with the missing path** (specs2 `skipped(...)`). It never passes silently or fails.
- Golden scenarios refer to media **by SHA-256** (#8 §G) and record the ROM hashes. A run with different ROMs is reported, not mis-compared.

---

## 8. Success criteria for the milestone checks

Each scenario is replayed with the golden-run tooling (#74), and frame checkpoints are taken at the VBL boundary (§6.4). Media swaps are scenario events (§9).

### M3 (#75): behavioral CPU

1. **ProDOS 8 boot.** Power on with the ProDOS disk in drive 1 and reach the `BASIC.SYSTEM` `]` prompt or the launcher. The checkpoint frame shows the prompt. ProDOS loads into the language card, so this also exercises §5.4.
2. **Commercial app.** Boot AppleWorks 2.x, create a word-processor document, type a fixed sentence and save it. Checkpoint frames on the main menu, the typed text and after the save. The saved file is read back in the same run, and write-back to the image is verified.
3. **Prince of Persia.** Boot side A:
   - the copy-protection check passes (the game continues past the boot instead of hanging or rebooting);
   - the double hi-res title, credits and prologue display (checkpoint frames);
   - the scripted input starts the game (the side-swap prompt, if it appears, is answered by a scripted media swap);
   - level 1 is played for at least 3 minutes of simulated time, with movement, a sword fight and a potion;
   - the cumulative audio hash is checked at every checkpoint.
4. **Opcode tripwire.** During all three scenarios, the behavioral CPU executes no opcode outside the NMOS 6502 set (§1.2).

### M4 (#84): hand-written 6502

Every checkpoint hash of the three M3 scenarios matches, frame and audio. The reset fingerprint (§4.1) is exercised by every scenario's power-on.

### M5 (#85): real time

- Sustained simulated clock **≥ 1.0204 MHz**, i.e. ≥ 100% of real time for `halfPeriod = 490`: 17,030 cycles per 16.6894 ms frame. #85 currently says 1.0205 MHz, which a 490-tick PHI0 can't reach by definition (§9).
- Measured during PoP gameplay on the reference machine, with zero audio underruns over a 10-minute session.
- The golden hashes still match.

---

## 9. Deviations from #8

Proposed epic and sub-issue updates:

1. **Double hi-res is required** (§1.3). Mark #66 as not optional, and add it to #75's "Depends on".
2. **`$C011–$C01F` bits 0–6 are keyboard ASCII, not floating** (§5.2). Fix #61's "status reads … return floating-bus data in bits 0–6".
3. **`$C019` reads VBL'** (bit 7 = 0 during blanking) (§5.2). #8 §F lists it as "VBL". Clarify the polarity there and in #63.
4. **MMU reset by address fingerprint**, the IOU by RESET' (§5.3). Add both to #61, #62 and #63. Add the reset bus sequence to the CPU pin contract (#57, #59, #82).
5. **The Disk II needs the sequencer ROM as a second asset**, and the LSS-level model (§1.7, §7.1). #8 §B says "the Disk II slot ROM" in the singular.
6. **Disk rotation by simulated time with a fractional accumulator**, not 8 sequencer steps per bit (§3.3). Update #70.
7. **Scenarios need media-swap events.** PoP spans two disk sides (§1.7). #8 §G lists only input events. Update #74, and give the frontend a disk-swap action (#67).
8. **No FLUX support needed:** both preserved PoP images are WOZ 1 files, which have no FLUX chunks (§10). Close the question in #71.
9. **The M5 target is 1.0204 MHz** (100% of the 490-tick clock), not 1.0205 MHz (§8).
10. **Power-on RES is held 102,000 cycles** (Disk II 100 ms extension; IOU 512 lines without the card) (§3.4). Update #60.
11. **Long cycle position:** if it is ever modelled, it stretches PHI2 at horizontal states `$40`/`$41`, not "every 65th cycle" at an unspecified point (§3.1).
12. **Not implemented on our IIe:** IOUDIS, RDDHIRES and the VBL interrupt (IIc only) (§5.1).

Paddle timing, for #73: a timer pulse lasts `(R + 100) × 0.022 µs` for a paddle of R Ω (150 kΩ pots: 2–3,302 µs). The Monitor's `PREAD` waits 10 cycles, then polls in an **11-cycle loop** (not 12, as its comment says) and saturates at 255 [Sather pp. 7-7–7-8, 7-29]. Model a paddle value p ∈ 0–255 as a pulse of `11·p + k` cycles. Calibrate `k` in #73 so that `PREAD` on the real ROM returns p for p ≤ 254.

## 10. Asset checks

These were run on 2026-10-25 against the reviewer's dumps. The scripts are not committed; the results are:

| Check | Result | Issue |
|---|---|---|
| ROM ID bytes | CD 342-0135-A + EF 342-0134-A give `$FBB3 = $06`, `$FBC0 = $EA` and an `RTS` at `$FE1F`, as [TN7] says for an unenhanced IIe. The reset vector is `$FA62`. | #60 |
| Video ROM layout | On 341-0161-A, the second 4 KiB holds the US set (`[`, `@` and `{` where the first holds `Ä`, `§` and `ä`). `A` (code `$01`/`$41`) is at `$008`/`$208` stored lit and at `$408`/`$608` stored dark, so A3–A8 carry the character code and A10 = RA10. The top 2 KiB of each half hold graphics patterns. | #63 |
| Sequencer ROM order | A sequencer simulation reads a synthetic 6-and-2 bitstream (sync bytes, then `D5 AA 96 …`) correctly with the raw P6A dump and Sather's wiring taken literally (D4–D7 → A5, A0, A6, A7) with an active-low read pulse. No other wiring or polarity works. This dump is **not** scrambled. | #70 |
| Boot ROM | The P5A dump in the same set is scrambled. After reversing data bits 4–7 and address bits 5–7, it disassembles as coherent code: every branch lands on an instruction, with the `$Cn5C` read routine and `JMP $0801` at the end. The other address-bit permutations don't. The 13-sector (DOS 3.2) ROMs in the set are not usable. | #70 |
| RW18 opcodes | No 65C02 opcodes (§1.2). All 16 sectors of track 0 decode with valid checksums, and the disk's boot sector holds the same skew and page tables as `POPBOOT0.S`. | #75 |
| WOZ version | Both sides are `WOZ1`, INFO version 1, 35 tracks, no FLUX chunk. Track 0 sector 7 carries the HLS protection data: a long run of `$E7` nibbles in its data field (§1.7). | #71 |
