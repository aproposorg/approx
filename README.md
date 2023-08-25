# `approx`: A Library of Approximate Arithmetic Units in Chisel

[![Actions Status](https://github.com/aproposorg/approx/actions/workflows/ci.yml/badge.svg)](https://github.com/aproposorg/approx/actions)

This repository contains a collection of approximate arithmetic units for use in various digital designs. The units are written in [Chisel](https://github.com/chipsalliance/chisel3) with tests written for the exact units using [ChiselTest](https://github.com/ucb-bar/chiseltest). Currently only a selection of adders and multipliers and a simple sequential division unit are included - more designs to come!

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

Utilizing Chisel and ChiselTest, `approx` requires a suitable installation of Scala. For this purpose, we use the Scala Build Tool (`sbt`) for which we provide a suitable build script. 

Moreover, some tests run too slow in ChiselTest's built-in Treadle simulator, so we have instead decided to run them using Verilator (see e.g., `Radix2MultiplierSpec` [here](./src/test/scala/approx/multiplication/ExactMultiplierSpec.scala#L165)). Thus, to run all provided tests, one must have a suitable installation of Verilator available. Note that only [specific versions of Verilator](https://github.com/ucb-bar/chiseltest#verilator-versions) are officially supported.

This library is tested in Ubuntu 20.04 with Verilator 4.028.

***
# Adders

The `approx.addition` library contains a vast number of approximate and exact adder designs that are parameterized to be reasonably flexible. The lists below specify which designs are included currently. We gladly accept requests for other designs as issues in this repository.

## Exact designs

All exact designs are based on descriptions in [Ercegovac and Lang](https://www.sciencedirect.com/book/9781558607989/digital-arithmetic)'s book on digital arithmetic. All designs are purely combinational.

| Type                                    | Name        | Code location                                                                    |
|-----------------------------------------|-------------|----------------------------------------------------------------------------------|
| Half adder                              | `HalfAdder` | [approx.addition.HalfAdder](./src/main/scala/approx/addition/Exact.scala#L8)     |
| Full adder                              | `FullAdder` | [approx.addition.FullAdder](./src/main/scala/approx/addition/Exact.scala#L14)    |
| Ripple-carry adder                      | `RCA`       | [approx.addition.RCA](./src/main/scala/approx/addition/Exact.scala#L23)          |
| Carry-lookahead adder                   | `CLA`       | [approx.addition.CLA](./src/main/scala/approx/addition/Exact.scala#L84)          |
| Two-layer carry-lookahead adder         | `CLA2`      | [approx.addition.CLA2](./src/main/scala/approx/addition/Exact.scala#L118)        |
| Carry-select adder                      | `CSA`       | [approx.addition.CSA](./src/main/scala/approx/addition/Exact.scala#L178)         |
| Self-timed adder                        | `STA`       | [approx.addition.STA](./src/main/scala/approx/addition/ExactSelfTimed.scala#L13) |
| Parallel carry-completion sensing adder | `CCA`       | [approx.addition.CCA](./src/main/scala/approx/addition/ExactSelfTimed.scala#L58) |

## Approximate designs

| Type                                                    | Name(s)                   | Code location                                                                                      | Reference                                                                                |
|---------------------------------------------------------|---------------------------|----------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------|
| Full adder                                              | `AXA1`, `AXA2`, `AXA3`    | [approx.addition.AXA](./src/main/scala/approx/addition/AXA.scala)                                  | [Yang et al.](https://ieeexplore.ieee.org/document/6720793)                              |
| Full adder                                              | `AFA`                     | [approx.addition.AFA](./src/main/scala/approx/addition/ErrorResilient.scala#L9)                    | [Dutt et al.](https://dl.acm.org/doi/10.1145/3131274)                                    |
| Full adder                                              | `InXA1`, `InXA2`, `InXA3` | [approx.addition.InXA](./src/main/scala/approx/addition/InXA.scala)                                | [Almurib et al.](https://ieeexplore.ieee.org/document/7459392)                           |
| Full adder                                              | `TCAA`                    | [approx.addition.TCAA](./src/main/scala/approx/addition/TCAA.scala)                                | [Yang and Thapliyal](https://ieeexplore.ieee.org/document/9154922)                       |
| Full adder                                              | `TSAA`                    | [approx.addition.TSAA](./src/main/scala/approx/addition/TSAA.scala)                                | [Yang and Thapliyal](https://ieeexplore.ieee.org/document/9154922)                       |
| Accuracy-configurable adder                             | `ACA`                     | [approx.addition.ACA](./src/main/scala/approx/addition/ACA.scala)                                  | [Kahng and Kang](https://dl.acm.org/doi/10.1145/2228360.2228509)                         |
| Carry cut-back adder                                    | `CCBA`                    | [approx.addition.CCBA](./src/main/scala/approx/addition/CCBA.scala)                                | [Camus et al.](https://dl.acm.org/doi/10.1145/2897937.2897964)                           |
| Dual-mode ripple-carry adder                            | `DualModeRCA`             | [approx.addition.DualModeRCA](./src/main/scala/approx/addition/DualMode.scala#L43)                 | [Raha et al.](https://ieeexplore.ieee.org/document/7106512)                              |
| Dual-mode carry-lookahead adder                         | `DualModeCLA`             | [approx.addition.DualModeCLA](./src/main/scala/approx/addition/DualMode.scala#L89)                 | [Raha et al.](https://ieeexplore.ieee.org/document/7106512)                              |
| Error-resilient adder w/o correction                    | `ErrorResilient`          | [approx.addition.ErrorResilient](./src/main/scala/approx/addition/ErrorResilient.scala#L20)        | [Dutt et al.](https://dl.acm.org/doi/10.1145/3131274)                                    |
| Error-resilient adder w/ correction                     | `ErrorResilientCorrect`   | [approx.addition.ErrorResilientCorrect](./src/main/scala/approx/addition/ErrorResilient.scala#L45) | [Dutt et al.](https://dl.acm.org/doi/10.1145/3131274)                                    |
| Error-tolerant adder I                                  | `ETAI`                    | [approx.addition.ETAI](./src/main/scala/approx/addition/ETAI.scala)                                | [Zhu et al.](https://ieeexplore.ieee.org/document/5286230)                               |
| Error-tolerant adder II                                 | `ETAII`                   | [approx.addition.ETAII](./src/main/scala/approx/addition/ETAII.scala#L11)                          | [Zhu et al.](https://ieeexplore.ieee.org/document/5403865)                               |
| Modified error-tolerant adder II                        | `ETAIIM`                  | [approx.addition.ETAIIM](./src/main/scala/approx/addition/ETAII.scala#L42)                         | [Zhu et al.](https://ieeexplore.ieee.org/document/5403865)                               |
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

Like above, the `approx.multiplication` library contains several approximate and exact multiplier designs that are also parameterized. The list below specifies which designs are included currently. In addition to these, the tool also includes a generic compressor tree generator for ASIC, Xilinx 7-Series/UltraScale or Versal FPGA (supported by the primitives in [approx.util.Xilinx](./src/main/scala/approx/util/Xilinx.scala)), and Intel FPGA flows. The generator supports fully exact compression as well as approximation by (column) truncation and _miscounting_ (i.e., inexact compression).

## Exact designs

All exact designs are based on descriptions in [Ercegovac and Lang](https://www.sciencedirect.com/book/9781558607989/digital-arithmetic)'s book on digital arithmetic. 

| Type                          | Signed/unsigned | Name                                  | Code location                                                                                             |
|-------------------------------|-----------------|---------------------------------------|-----------------------------------------------------------------------------------------------------------|
| Compressor 2:2                |                 | `Compressor2to2`                      | [approx.multiplication.Compressor3to2](./src/main/scala/approx/multiplication/Exact.scala#L10)            |
| Compressor 3:2                |                 | `Compressor3to2`                      | [approx.multiplication.Compressor3to2](./src/main/scala/approx/multiplication/Exact.scala#L19)            |
| Compressor 4:2                |                 | `Compressor4to2`, `Compressor4to2Opt` | [approx.multiplication.Compressor4to2](./src/main/scala/approx/multiplication/Exact.scala#L29)            |
| Compressor 5:3                |                 | `Compressor5to3`                      | [approx.multiplication.Compressor5to3](./src/main/scala/approx/multiplication/Exact.scala#L56)            |
| Compressor 7:3                |                 | `Compressor7to3`                      | [approx.multiplication.Compressor7to3](./src/main/scala/approx/multiplication/Exact.scala#L74)            |
| 2x2-bit multiplier            |                 | `TwoxTwo`                             | [approx.multiplication.TwoxTwo](./src/main/scala/approx/multiplication/Exact.scala#L88)                   |
| Radix-2 array multiplier      | Both            | `Radix2Multiplier`                    | [approx.multiplication.Radix2Multiplier](./src/main/scala/approx/multiplication/Exact.scala#L106)         |
| Recursive multiplier          | Both            | `RecursiveMultiplier`                 | [approx.multiplication.RecursiveMultiplier](./src/main/scala/approx/multiplication/Exact.scala#L460)      |
| Alphabet-set multiplier       | Both            | `AlphabetSetMultiplier`               | [approx.multiplication.AlphabetSetMultiplier](./src/main/scala/approx/multiplication/Exact.scala#L559)    |
| Radix-2 sequential multiplier | Unsigned        | `Radix2SeqMultiplier`                 | [approx.multiplication.Radix2SeqMultiplier](./src/main/scala/approx/multiplication/ExactSequential.scala) |

## Approximate designs

| Type                                      | Signed/unsigned | Name                                                                  | Code location                                                                                              | Reference                                                                      |
|-------------------------------------------|-----------------|-----------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------|
| Reduced compressor 4:2                    |                 | `Compressor4to2D1`, `Compressor4to2D2`                                | [approx.multiplication.Compressor4to2](./src/main/scala/approx/multiplication/Compressors.scala#L14)       | [Momeni et al.](https://ieeexplore.ieee.org/document/6748013)                  |
| Majority-based compressor 4:2             |                 | `Compressor4to2Maj`                                                   | [approx.multiplication.Compressor4to2Maj](./src/main/scala/approx/multiplication/Compressors.scala#L29)    | [Moaiyeri et al.](https://link.springer.com/article/10.1007/s00542-017-3587-2) |
| Compressor 8:3                            |                 | `Compressor8to3`, `Compressor8to3SevenSeries`, `Compressor8to3Versal` | [approx.multiplication.Compressor8to3](./src/main/scala/approx/multiplication/Compressors.scala#L215)      | [Moaiyeri et al.](https://link.springer.com/article/10.1007/s00542-017-3587-2) |
| Kulkarni-style 2x2-bit multiplier         |                 | `Kulkarni`                                                            | [approx.multiplication.Kulkarni](./src/main/scala/approx/multiplication/Kulkarni.scala)                    | [Kulkarni et al.](https://ieeexplore.ieee.org/document/5718826)                |
| Rehman-style 2x2-bit multiplier           |                 | `ApproxMul2`, `ApproxMul3`, `ApproxMul4`, `ApproxMul5`                | [approx.multiplication.Rehman](./src/main/scala/approx/multiplication/Rehman.scala)                        | [Rehman et al.](https://ieeexplore.ieee.org/document/7827657)                  |
| Compressed radix-2 array multiplier       | Unsigned        | `CompressedMultiplier`                                                | [approx.multiplication.CompressedMultiplier](./src/main/scala/approx/multiplication/Compressed.scala)      | [Yang et al.](https://dl.acm.org/doi/10.1145/3299874.3317975)                  |
| Error-tolerant multiplier                 | Both            | `ETM`                                                                 | [approx.multiplication.ETM](./src/main/scala/approx/multiplication/ETM.scala)                              | [Kyaw et al.](https://ieeexplore.ieee.org/document/5713751)                    |
| Approximate radix-2 sequential multiplier | Unsigned        | `ApproxRadix2SeqMultiplier`                                           | [approx.multiplication.ApproxRadix2SeqMultiplier](./src/main/scala/approx/multiplication/Sequential.scala) | [Mannepalli et al.](https://dl.acm.org/doi/10.1145/3453688.3461482)            |

***
# Dividers

The `approx.division` library currently only contains an exact, sequential radix-2 divider. We hope to extend this collection in the future.

## Exact designs

| Type            | Signed/unsigned | Name            | Code location                                                                     |
|-----------------|-----------------|-----------------|-----------------------------------------------------------------------------------|
| Radix-2 divider | Unsigned        | `Radix2Divider` | [approx.division.Radix2Divider](./src/main/scala/approx/division/Exact.scala#L15) |

## Approximate designs

N/A
