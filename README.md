# `approx`: A Library of Approximate Arithmetic Units in Chisel

[![Actions Status](https://github.com/aproposorg/approx/actions/workflows/ci.yml/badge.svg)](https://github.com/aproposorg/approx/actions)

This repository contains a collection of approximate arithmetic units for use in various digital designs. The units are written in [Chisel](https://github.com/chipsalliance/chisel3) with tests written for the exact units using [ChiselSim](https://www.chisel-lang.org/docs/explanations/testing). Currently only a selection of adders and multipliers and a simple sequential division unit are included - more designs to come!

When you use this library in a research project, please cite it as:

```
@misc{damsgaard2022approx,
  title={{approx: A Library of Approximate Arithmetic Units in Chisel}},
  author={Damsgaard, Hans Jakob},
  year={2022},
  howpublished={\url{https://github.com/aproposorg/approx}}}
```

This README only contains a brief overview of the library's current contents. All units are commented to be reasonably understandable for people with prior knowledge of Chisel and Scala. Refer to the [Digital Design with Chisel](https://github.com/schoeberl/chisel-book/) book for more details on these topics.

***
# Requirements

Utilizing Chisel and ChiselSim, `approx` requires a suitable installation of Scala. For this purpose, we use the Scala Build Tool (`sbt`) for which we provide a suitable build script. The provided tests require a recent version of Verilator.

This library is tested in Ubuntu 24.04 with Verilator 5.032. Note that the default Verilator version (5.020) available through `apt` in Ubunty 24.04 is _not_ new enough. If you wish to have VCD dumps from the simulations, pass the `emitVcd` flag to `testOnly`, for example:

```bash
sbt "testOnly approx.addition.RCASpec -- -DemitVcd=1"
```

***
# Adders

The `approx.addition` library contains a vast number of approximate and exact adder designs that are parameterized to be reasonably flexible. The lists below specify which designs are included currently. We gladly accept requests for other designs as issues in this repository.

## Exact designs

All exact designs are based on descriptions in [Ercegovac and Lang](https://www.sciencedirect.com/book/9781558607989/digital-arithmetic)'s book on digital arithmetic.

| Type                                            | Name             | Code location                                                                           |
|-------------------------------------------------|------------------|-----------------------------------------------------------------------------------------|
| Half adder                                      | `HalfAdder`      | [approx.addition.HalfAdder](./src/main/scala/approx/addition/Exact.scala#L8)            |
| Full adder                                      | `FullAdder`      | [approx.addition.FullAdder](./src/main/scala/approx/addition/Exact.scala#L14)           |
| Ripple-carry adder                              | `RCA`            | [approx.addition.RCA](./src/main/scala/approx/addition/Exact.scala#L23)                 |
| Adaptive optimized lower-part constant-OR adder | `AdaptiveOFLOCA` | [approx.addition.AdaptiveOFLOCA](./src/main/scala/approx/addition/AdaptiveOFLOCA.scala) |
| Carry-lookahead adder                           | `CLA`            | [approx.addition.CLA](./src/main/scala/approx/addition/Exact.scala#L84)                 |
| Two-layer carry-lookahead adder                 | `CLA2`           | [approx.addition.CLA2](./src/main/scala/approx/addition/Exact.scala#L118)               |
| Carry-select adder                              | `CSA`            | [approx.addition.CSA](./src/main/scala/approx/addition/Exact.scala#L178)                |
| Parallel prefix adder                           | `PPA`            | [approx.addition.PPA](./src/main/scala/approx/addition/Exact.scala#L222)                |
| Self-timed adder                                | `STA`            | [approx.addition.STA](./src/main/scala/approx/addition/ExactSelfTimed.scala#L13)        |
| Parallel carry-completion sensing adder         | `CCA`            | [approx.addition.CCA](./src/main/scala/approx/addition/ExactSelfTimed.scala#L58)        |

## Approximate designs

| Type                                                    | Name(s)                   | Code location                                                                                      | Reference                                                                                |
|---------------------------------------------------------|---------------------------|----------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------|
| Full adder                                              | `AXA1`, `AXA2`, `AXA3`    | [approx.addition.AXA](./src/main/scala/approx/addition/AXA.scala)                                  | [Yang et al.](https://ieeexplore.ieee.org/document/6720793)                              |
| Full adder                                              | `AFA`                     | [approx.addition.AFA](./src/main/scala/approx/addition/ErrorResilient.scala#L9)                    | [Dutt et al.](https://dl.acm.org/doi/10.1145/3131274)                                    |
| Full adder                                              | `InXA1`, `InXA2`, `InXA3` | [approx.addition.InXA](./src/main/scala/approx/addition/InXA.scala)                                | [Almurib et al.](https://ieeexplore.ieee.org/document/7459392)                           |
| Full adder                                              | `SESA1`, `SESA2`, `SESA3` | [approx.addition.SESA](./src/main/scala/approx/addition/SESA.scala)                                | [Jha et al.](https://ieeexplore.ieee.org/document/10113797)                              |
| Full adder                                              | `TCAA`                    | [approx.addition.TCAA](./src/main/scala/approx/addition/TCAA.scala)                                | [Yang and Thapliyal](https://ieeexplore.ieee.org/document/9154922)                       |
| Full adder                                              | `TSAA`                    | [approx.addition.TSAA](./src/main/scala/approx/addition/TSAA.scala)                                | [Yang and Thapliyal](https://ieeexplore.ieee.org/document/9154922)                       |
| Accuracy-configurable adder                             | `ACA`                     | [approx.addition.ACA](./src/main/scala/approx/addition/ACA.scala)                                  | [Kahng and Kang](https://dl.acm.org/doi/10.1145/2228360.2228509)                         |
| Approximate parallel prefix adder                       | `AxPPA`                   | [approx.addition.AxPPA](./src/main/scala/approx/addition/AxPPA.scala)                              | [da Rosa et al.](https://ieeexplore.ieee.org/document/9956923)                           |
| Carry cut-back adder                                    | `CCBA`                    | [approx.addition.CCBA](./src/main/scala/approx/addition/CCBA.scala)                                | [Camus et al.](https://dl.acm.org/doi/10.1145/2897937.2897964)                           |
| Carry estimating simultaneous adder                     | `CESA_PERL`               | [approx.addition.CESA_PERL](./src/main/scala/approx/addition/CESA_PERL.scala)                      | [Bhattacharjya et al.](https://dl.acm.org/doi/10.1145/3386263.3406928)                   |
| Dual-mode ripple-carry adder                            | `DualModeRCA`             | [approx.addition.DualModeRCA](./src/main/scala/approx/addition/DualMode.scala#L43)                 | [Raha et al.](https://ieeexplore.ieee.org/document/7106512)                              |
| Dual-mode carry-lookahead adder                         | `DualModeCLA`             | [approx.addition.DualModeCLA](./src/main/scala/approx/addition/DualMode.scala#L89)                 | [Raha et al.](https://ieeexplore.ieee.org/document/7106512)                              |
| Error-resilient adder w/o correction                    | `ErrorResilient`          | [approx.addition.ErrorResilient](./src/main/scala/approx/addition/ErrorResilient.scala#L21)        | [Dutt et al.](https://dl.acm.org/doi/10.1145/3131274)                                    |
| Error-resilient adder w/ correction                     | `ErrorResilientCorrect`   | [approx.addition.ErrorResilientCorrect](./src/main/scala/approx/addition/ErrorResilient.scala#L49) | [Dutt et al.](https://dl.acm.org/doi/10.1145/3131274)                                    |
| Error-tolerant adder I                                  | `ETAI`                    | [approx.addition.ETAI](./src/main/scala/approx/addition/ETAI.scala)                                | [Zhu et al.](https://ieeexplore.ieee.org/document/5286230)                               |
| Error-tolerant adder II                                 | `ETAII`                   | [approx.addition.ETAII](./src/main/scala/approx/addition/ETAII.scala#L12)                          | [Zhu et al.](https://ieeexplore.ieee.org/document/5403865)                               |
| Modified error-tolerant adder II                        | `ETAIIM`                  | [approx.addition.ETAIIM](./src/main/scala/approx/addition/ETAII.scala#L48)                         | [Zhu et al.](https://ieeexplore.ieee.org/document/5403865)                               |
| FAU LUT-based adder                                     | `FAU`                     | [approx.addition.FAU](./src/main/scala/approx/addition/FAU.scala)                                  | [Echavarria et al.](https://ieeexplore.ieee.org/document/7929536)                        |
| Generic accuracy-configurable adder                     | `GeAr`                    | [approx.addition.GeAr](./src/main/scala/approx/addition/GeAr.scala)                                | [Shafique et al.](https://dl.acm.org/doi/10.1145/2744769.2744778)                        |
| Hybrid error reduction lower-part OR adder              | `HERLOA`                  | [approx.addition.HERLOA](./src/main/scala/approx/addition/HERLOA.scala)                            | [Seo et al.](https://www.mdpi.com/2079-9292/9/3/471)                                     |
| Hardware-optimized adder with normal error distribution | `HOAANED`                 | [approx.addition.HOAANED](./src/main/scala/approx/addition/HOAANED.scala)                          | [Balasubramian et al.](https://ieeexplore.ieee.org/document/9309208)                     |
| Lower-part OR adder                                     | `LOA`                     | [approx.addition.LOA](./src/main/scala/approx/addition/LOA.scala)                                  |                                                                                          |
| Lower-part constant-OR adder                            | `LOCA`                    | [approx.addition.LOCA](./src/main/scala/approx/addition/LOCA.scala)                                | [Dalloo](https://ieeexplore.ieee.org/document/8821112)                                   |
| Lu's adder                                              | `LUA`                     | [approx.addition.LUA](./src/main/scala/approx/addition/LUA.scala)                                  | [Lu](https://ieeexplore.ieee.org/document/1274006)                                       |
| LUT-based adder                                         | `LutBased`                | [approx.addition.LUTBased](./src/main/scala/approx/addition/LUTBased.scala)                        | [Becher et al.](https://ieeexplore.ieee.org/document/7544739)                            |
| Optimized lower-part constant-OR adder                  | `OFLOCA`                  | [approx.addition.OFLOCA](./src/main/scala/approx/addition/OFLOCA.scala)                            | [Dalloo](https://ieeexplore.ieee.org/document/8821112)                                   |
| Reconfigurable carry-lookahead adder                    | `RAP_CLA`                 | [approx.addition.RAP_CLA](./src/main/scala/approx/addition/RAP_CLA.scala)                          | [Akbari et al.](https://ieeexplore.ieee.org/document/7762134)                            |
| Speculative carry-skip adder                            | `SCSkip`                  | [approx.addition.SCSkip](./src/main/scala/approx/addition/SCSkip.scala)                            | [Kim et al.](https://dl.acm.org/doi/10.5555/2561828.2561854)                             |
| Self-adaptive adder                                     | `SelfAdaptive`            | [approx.addition.SelfAdaptive](./src/main/scala/approx/addition/SelfAdaptive.scala)                | [Liu et al.](https://dl.acm.org/doi/abs/10.1145/3386263.3407589)                         |
| Static segment adder                                    | `SSA`                     | [approx.addition.SSA](./src/main/scala/approx/addition/SSA.scala)                                  | [Jothin and Vasanthanayaki](https://link.springer.com/article/10.1007/s10836-016-5634-9) |

Beware that some approximate adders (e.g., `DualModeRCA`, `GeAr`) do not extend the abstract `Adder` base class as their IOs do not match those of the basic adder. Almost all designs are purely combinational; only `GeAr` has an option for adding a register to support flagging errors.

***
# Multipliers

Like above, the `approx.multiplication` library contains several approximate and exact multiplier designs that are also parameterized. The list below specifies which designs are included currently. In addition to these, the tool also includes a generic compressor tree generator for ASIC, Xilinx 7-Series/UltraScale or Versal FPGA (supported by the primitives in [approx.util.Xilinx](./src/main/scala/approx/util/Xilinx.scala)), and Intel FPGA flows. The generator supports fully exact compression as well as approximation by column or row truncation, OR-based column compression, and _miscounting_ (i.e., inexact compression). Multiple of these approximations can also be applied concurrently.

## Exact designs

All exact designs are based on descriptions in [Ercegovac and Lang](https://www.sciencedirect.com/book/9781558607989/digital-arithmetic)'s book on digital arithmetic. Note that some exact multiplier implementations, specifically `Radix2Multiplier`, `Radix4Multiplier`, `RecursiveMultiplier`, and `AdaptiveRadix2Multiplier`, permit approximation through their arguments.

| Type                              | Signed/unsigned | Name                                  | Code location                                                                                                           |
|-----------------------------------|-----------------|---------------------------------------|-------------------------------------------------------------------------------------------------------------------------|
| Compressor 2:2                    |                 | `Compressor2to2`                      | [approx.multiplication.Compressor3to2](./src/main/scala/approx/multiplication/Exact.scala#L10)                          |
| Compressor 3:2                    |                 | `Compressor3to2`                      | [approx.multiplication.Compressor3to2](./src/main/scala/approx/multiplication/Exact.scala#L19)                          |
| Compressor 4:2                    |                 | `Compressor4to2`, `Compressor4to2Opt` | [approx.multiplication.Compressor4to2](./src/main/scala/approx/multiplication/Exact.scala#L29)                          |
| Compressor 5:3                    |                 | `Compressor5to3`                      | [approx.multiplication.Compressor5to3](./src/main/scala/approx/multiplication/Exact.scala#L56)                          |
| Compressor 7:3                    |                 | `Compressor7to3`                      | [approx.multiplication.Compressor7to3](./src/main/scala/approx/multiplication/Exact.scala#L74)                          |
| 2x2-bit multiplier                |                 | `TwoxTwo`                             | [approx.multiplication.TwoxTwo](./src/main/scala/approx/multiplication/Exact.scala#L97)                                 |
| Radix-2 array multiplier          | Both            | `Radix2Multiplier`                    | [approx.multiplication.Radix2Multiplier](./src/main/scala/approx/multiplication/Exact.scala#L119)                       |
| Adaptive radix-2 array multiplier | Both            | `AdaptiveRadix2Multiplier`            | [approx.multiplication.AdaptiveRadix2Multiplier](./src/main/scala/approx/multiplication/AdaptiveRadix2Multiplier.scala) |
| Radix-4 array multiplier          | Both            | `Radix4Multiplier`                    | [approx.multiplication.Radix4Multiplier](./src/main/scala/approx/multiplication/Exact.scala#L230)                       |
| Recursive multiplier              | Both            | `RecursiveMultiplier`                 | [approx.multiplication.RecursiveMultiplier](./src/main/scala/approx/multiplication/Exact.scala#L431)                    |
| Alphabet-set multiplier           | Both            | `AlphabetSetMultiplier`               | [approx.multiplication.AlphabetSetMultiplier](./src/main/scala/approx/multiplication/Exact.scala#L517)                  |
| Radix-2 sequential multiplier     | Unsigned        | `Radix2SeqMultiplier`                 | [approx.multiplication.Radix2SeqMultiplier](./src/main/scala/approx/multiplication/ExactSequential.scala)               |

## Approximate designs

| Type                                           | Signed/unsigned | Name                                                                  | Code location                                                                                              | Reference                                                                      |
|------------------------------------------------|-----------------|-----------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------|
| Reduced compressor 4:2                         |                 | `Compressor4to2D1`, `Compressor4to2D2`                                | [approx.multiplication.Compressor4to2D1](./src/main/scala/approx/multiplication/Compressors.scala#L14)     | [Momeni et al.](https://ieeexplore.ieee.org/document/6748013)                  |
| Modified compressor 4:2                        |                 | `Compressor4to2CV1`, `Compressor4to2CV2`                              | [approx.multiplication.Compressor4to2CV1](./src/main/scala/approx/multiplication/Compressors.scala#L36)    | [Zanandrea and Meinhardt](https://ieeexplore.ieee.org/document/10261949)       |
| Majority-based compressor 4:2                  |                 | `Compressor4to2Maj`                                                   | [approx.multiplication.Compressor4to2Maj](./src/main/scala/approx/multiplication/Compressors.scala#L58)    | [Moaiyeri et al.](https://link.springer.com/article/10.1007/s00542-017-3587-2) |
| Compressor 8:3                                 |                 | `Compressor8to3`, `Compressor8to3SevenSeries`, `Compressor8to3Versal` | [approx.multiplication.Compressor8to3](./src/main/scala/approx/multiplication/Compressors.scala#L239)      | [Moaiyeri et al.](https://link.springer.com/article/10.1007/s00542-017-3587-2) |
| Kulkarni-style 2x2-bit multiplier              |                 | `Kulkarni`                                                            | [approx.multiplication.Kulkarni](./src/main/scala/approx/multiplication/Kulkarni.scala)                    | [Kulkarni et al.](https://ieeexplore.ieee.org/document/5718826)                |
| Rehman-style 2x2-bit multiplier                |                 | `ApproxMul2`, `ApproxMul3`, `ApproxMul4`, `ApproxMul5`                | [approx.multiplication.Rehman](./src/main/scala/approx/multiplication/Rehman.scala)                        | [Rehman et al.](https://ieeexplore.ieee.org/document/7827657)                  |
| Configurable partial error recovery multiplier | Unsigned        | `CPER`                                                                | [approx.multiplication.CPER](./src/main/scala/approx/multiplication/CPER.scala)                            | [Liu et al.](https://ieeexplore.ieee.org/document/6800309)                     |
| Dynamic range unbiased multiplier              | Both            | `DRUM`                                                                | [approx.multiplication.DRUM](./src/main/scala/approx/multiplication/DRUM.scala)                            | [Hashemi et al.](https://ieeexplore.ieee.org/document/7372600)                 |
| Error-tolerant multiplier                      | Both            | `ETM`                                                                 | [approx.multiplication.ETM](./src/main/scala/approx/multiplication/ETM.scala)                              | [Kyaw et al.](https://ieeexplore.ieee.org/document/5713751)                    |
| Low-power small-area multiplier                | Unsigned        | `LPSA`                                                                | [approx.multiplication.LPSA](./src/main/scala/approx/multiplication/LPSA.scala)                            | [Baba et al.](https://ieeexplore.ieee.org/document/8429430)                    |
| Minimally-biased multiplier                    | Unsigned        | `MBM`                                                                 | [approx.multiplication.MBM](./src/main/scala/approx/multiplication/MBM.scala)                              | [Saadat et al.](https://ieeexplore.ieee.org/document/8493590)                  |
| Approximate radix-2 sequential multiplier      | Unsigned        | `ApproxRadix2SeqMultiplier`                                           | [approx.multiplication.ApproxRadix2SeqMultiplier](./src/main/scala/approx/multiplication/Sequential.scala) | [Mannepalli et al.](https://dl.acm.org/doi/10.1145/3453688.3461482)            |

***
# Dividers

The `approx.division` library currently only contains an exact, sequential radix-2 divider. We hope to extend this collection in the future.

## Exact designs

| Type                       | Signed/unsigned | Name               | Code location                                                                                  |
|----------------------------|-----------------|--------------------|------------------------------------------------------------------------------------------------|
| Radix-2 sequential divider | Unsigned        | `Radix2SeqDivider` | [approx.division.Radix2SeqDivider](./src/main/scala/approx/division/ExactSequential.scala#L15) |

## Approximate designs

N/A

***
# Accumulators

The `approx.accumulation` library currently only contains a number of exact, non-pipelined single-lane and parallel accumulators with options for the parallel designs to be approximated with the custom compressor trees from `approx.multiplication.comptree`. We also hope to extend this collection in the future.

## Exact designs

| Type                          | Signed/unsigned | Name                          | Code location                                                                                            |
|-------------------------------|-----------------|-------------------------------|----------------------------------------------------------------------------------------------------------|
| Simple accumulator            | Both            | `SimpleAccumulator`           | [approx.accumulation.SimpleAccumulator](./src/main/scala/approx/accumulation/Exact.scala#L14)            |
| Multiply accumulator          | Both            | `MultiplyAccumulator`         | [approx.accumulation.MultiplyAccumulator](./src/main/scala/approx/accumulation/Exact.scala#L34)          |
| Bit matrix accumulator        |                 | `BitMatrixAccumulator`        | [approx.accumulation.BitMatrixAccumulator](./src/main/scala/approx/accumulation/Exact.scala#L58)         |
| Parallel simple accumulator   | Both            | `ParallelSimpleAccumulator`   | [approx.accumulation.ParallelSimpleAccumulator](./src/main/scala/approx/accumulation/Exact.scala#L108)   |
| Parallel multiply accumulator | Both            | `ParallelMultiplyAccumulator` | [approx.accumulation.ParallelMultiplyAccumulator](./src/main/scala/approx/accumulation/Exact.scala#L156) |

## Approximate designs

N/A
