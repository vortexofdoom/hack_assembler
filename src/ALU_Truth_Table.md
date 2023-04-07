The Hack ALU performs a number of binary operations on some combination of the constants `0`, `-1` (two's complement representation) and the two inputs (CPU Registers `D` and `A`).

The behavior of the CPU around the ALU also depends on the 'a' bit of the instruction, but as this is used only to determine whether the CPU reads the A register as an address or a value (When set, we are reading `M[A]` (or `M`) and when unset, we are reading `A`), this table will stick to the representation of `A`.

The ALU uses six control bits to determine which binary operation is computed as the output, in the form (*x* `operation` *y*) from most significant to least, are as follows:

1. *zd* (Zero/D): Determines whether to use `0` or `D` as the first argument. (`true` for `0`, `false` for `D`)
2. *nd* (Not/D):  If true, uses the bitwise NOT of the first argument in-place (dependent on ZD flag)
3. *za* (Zero/A): Same as *zd* except for the second argument, uses `A` if false
4. *na* (Not/A): Same as *nd* except for the second argument
5. *f* (Add/And): If true, computes `x + y`, otherwise `x & y`
6. *no* (!out/out): If true, outputs the bitwise NOT of the result rather than the result itself.

The resulting truth table, starting with the official bit configurations as set in the hack specification:

|zd |nd |za |na | f |no | Literal       | Result    |
|:-:|:-:|:-:|:-:|:-:|:-:|---------------|-----------|
| 1 | 0 | 1 | 0 | 1 | 0 | `0 + 0`       | `0`       |
| 1 | 1 | 1 | 1 | 1 | 1 | `!(-1 + -1)`  | `1`       |
| 1 | 1 | 1 | 0 | 1 | 0 | `-1 + 0`      | `-1`      |
| 0 | 0 | 1 | 1 | 0 | 0 | `D & -1`      | `D`       |
| 1 | 1 | 0 | 0 | 0 | 0 | `-1 & A`      | `A`       |
| 0 | 0 | 1 | 1 | 0 | 1 | `!(D & -1)`   | `!D`      |
| 1 | 1 | 0 | 0 | 0 | 1 | `!(-1 & A)`   | `!A`      |
| 0 | 0 | 1 | 1 | 1 | 1 | `!(D + -1)`   | `-D`      |
| 1 | 1 | 0 | 0 | 1 | 1 | `!(-1 + A)`   | `-A`      |
| 0 | 1 | 1 | 1 | 1 | 1 | `!(!D + -1)`  | `D + 1`   |
| 1 | 1 | 0 | 1 | 1 | 1 | `!(-1 + !A)`  | `A + 1`   |
| 0 | 0 | 1 | 1 | 1 | 0 | `D + -1`      | `D - 1`   |
| 1 | 1 | 0 | 0 | 1 | 0 | `-1 + A`      | `A - 1`   |
| 0 | 0 | 0 | 0 | 1 | 0 | `D + A`       | `D + A`   |
| 0 | 1 | 0 | 0 | 1 | 1 | `!(!D + A)`   | `D - A`   |
| 0 | 0 | 0 | 1 | 1 | 1 | `!(D + !A)`   | `A - D`   |
| 0 | 1 | 0 | 1 | 0 | 1 | `!(!D & !A)`  | `D | A`   |
| 0 | 0 | 0 | 0 | 0 | 0 | `D & A`       | `D & A`   |

# Unofficial duplicate bit configurations

There is really no reason to use these over the official ones, although it could make for a simple method to detect piracy?

|zd |nd |za |na | f |no | Literal       | Result    |
|:-:|:-:|:-:|:-:|:-:|:-:|---------------|-----------|
| 1 | 1 | 1 | 0 | 1 | 1 | `!(-1 + 0)`   | `0`       |
| 0 | 0 | 1 | 0 | 0 | 0 | `D & 0`       | `0`       |
| 0 | 1 | 1 | 0 | 0 | 0 | `!D & 0`      | `0`       |
| 1 | 0 | 0 | 0 | 0 | 0 | `0 & A`       | `0`       |
| 1 | 0 | 0 | 1 | 0 | 0 | `0 & !A`      | `0`       |
| 1 | 0 | 1 | 0 | 0 | 0 | `0 & 0`       | `0`       |
| 1 | 0 | 1 | 1 | 0 | 0 | `0 & -1`      | `0`       |
| 1 | 0 | 1 | 1 | 1 | 1 | `!(0 + -1)`   | `0`       |
| 1 | 1 | 1 | 0 | 0 | 0 | `-1 & 0`      | `0`       |
| 1 | 1 | 1 | 1 | 0 | 1 | `!(-1 & -1)`  | `0`       |
| 0 | 0 | 1 | 0 | 0 | 1 | `!(D & 0)`    | `-1`      |
| 0 | 1 | 1 | 0 | 0 | 1 | `!(!D & 0)`   | `-1`      |
| 1 | 0 | 0 | 0 | 0 | 1 | `!(0 & A)`    | `-1`      |
| 1 | 0 | 0 | 1 | 0 | 1 | `!(0 & !A)`   | `-1`      |
| 1 | 0 | 1 | 0 | 0 | 1 | `!(0 & 0)`    | `-1`      |
| 1 | 0 | 1 | 0 | 1 | 1 | `!(0 + 0)`    | `-1`      |
| 1 | 0 | 1 | 1 | 0 | 1 | `!(0 & -1)`   | `-1`      |
| 1 | 0 | 1 | 1 | 1 | 0 | `0 + -1`      | `-1`      |
| 1 | 1 | 1 | 0 | 0 | 1 | `!(-1 & 0)`   | `-1`      |
| 1 | 1 | 1 | 1 | 0 | 0 | `-1 & -1`     | `-1`      |
| 0 | 0 | 1 | 0 | 1 | 0 | `D + 0`       | `D`       |
| 0 | 1 | 1 | 0 | 1 | 1 | `!(!D + 0)`   | `D`       |
| 0 | 1 | 1 | 1 | 0 | 1 | `!(!D & -1)`  | `D`       |
| 1 | 0 | 0 | 0 | 1 | 0 | `0 + A`       | `A`       |
| 1 | 0 | 0 | 1 | 1 | 1 | `!(0 + !A)`   | `A`       |
| 1 | 1 | 0 | 1 | 0 | 1 | `!(-1 & !A)`  | `A`       |
| 0 | 0 | 1 | 0 | 1 | 1 | `!(D + 0)`    | `!D`      |
| 0 | 1 | 1 | 1 | 0 | 0 | `!D & -1`     | `!D`      |
| 0 | 1 | 1 | 0 | 1 | 0 | `!D + 0`      | `!D`      |
| 1 | 0 | 0 | 0 | 1 | 1 | `!(0 + A)`    | `!A`      |
| 1 | 0 | 0 | 1 | 1 | 0 | `0 + !A`      | `!A`      |
| 1 | 1 | 0 | 1 | 0 | 0 | `-1 & !A`     | `!A`      |

# TODO: Math these out

These bit configurations could conceivably be used for niche assembly optimizations in actual hardware.
It would not be wise to assume they could be used in any given emulation platform (even the official one)

|zd |nd |za |na | f |no | Literal       | Result    |
|:-:|:-:|:-:|:-:|:-:|:-:|---------------|-----------|
| 0 | 0 | 0 | 0 | 0 | 1 | `!(D & A)`    |           |
| 0 | 0 | 0 | 0 | 1 | 1 | `!(D + A)`    |           |
| 0 | 0 | 0 | 1 | 0 | 0 | `D & !A`      |           |
| 0 | 0 | 0 | 1 | 0 | 1 | `!(D & !A)`   |           |
| 0 | 0 | 0 | 1 | 1 | 0 | `D + !A`      |           |
| 0 | 1 | 0 | 0 | 0 | 0 | `!D & A`      |           |
| 0 | 1 | 0 | 0 | 0 | 1 | `!(!D & A)`   |           |
| 0 | 1 | 0 | 0 | 1 | 0 | `!D + A`      |           |
| 0 | 1 | 0 | 1 | 0 | 0 | `!D & !A`     |           |
| 0 | 1 | 0 | 1 | 1 | 0 | `!D + !A`     |           |
| 0 | 1 | 1 | 1 | 1 | 0 | `!D + -1`     |           |
| 0 | 1 | 0 | 1 | 1 | 1 | `!(!D + !A)`  |           |
| 1 | 1 | 0 | 1 | 1 | 0 | `-1 + !A`     |           |
| 1 | 1 | 1 | 1 | 1 | 0 | `-1 + -1`     |  `32767`  | // this could be useful




