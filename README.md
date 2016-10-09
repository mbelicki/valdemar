valdemar
========

valdemar is a small experimental programming language, here is a short code
snippet:

```lua
-- comments start with dobule dash
ext_c sin (a double_t) -> double_t -- declare external C function
ext_c cos (a double_t) -> double_t
ext_c sqrt (a double_t) -> double_t

-- fn denotes a function declaration
-- 'compute' takes one double precision floating point value
-- and returns anonymous tuple with two doubles
fn compute (a double_t) -> (double_t, double_t) {
    -- val introduces new variable, variables are immutable by defualt
    val radius double_t = 5
    return (sin(a) * radius, cos(a) * radius)
}

-- here is a named tuple delcaration:
tuple point_t { -- all types in valdemar have to end with '_t' suffix
    x, y double_t
}

fn length (p point_t) -> double_t {
    -- tuples can be destrucutred to assign filed values to multiple variables
    val (x, y double_t) = p 
    return sqrt(x * x + y * y)
}

-- main function is entry point of the program
fn main () -> unit_t {
    -- the array below is allocated on stack and is immutable,
    -- type of 'numbers' is: pointer to array of double_t
    val numbers ^[double_t] = [1.0, 2.0, 4.0, 8.0]
    -- to create mutable variable simply put exclamation sign after val,
    -- if the type of a variable is not specified then compiler will assume
    -- that the new variable has the type of right hand side expression
    val !i = 0 while i < #numbers; i = i + 1 { -- # operator returns length of array
        -- anonymous tuple returned by 'compute' can be assigned
        -- to named tuple as long as layout of both tuples is the same
        val p point_t = compute(numbers[i])
    }
}
```

building
--------

Current version of compiler is developed in Haskell, project can be compiled on
GNU/Linux (tested on Ubuntu 16.04 LTS, 64-bit) and macOS (tested on 10.11 and
10.12).

Building valdemar on Ubuntu requires following packages installed:

```shell
apt-get install gcc llvm llvm-3.5 libz-dev libedit-dev
```

On macOS you will need command line tools provided by Apple (compiler, linker etc.) and similar set of libraries as on Ubunut (zlib, llvm-3.5, libedit) you can get them from brew or other package manager.

To compile use cabal:

```shell
cabal sandbox init
cabal install --only-dependencies 
cabal build
```

license
-------

valdemar is licensed under *GNU General Public License v3*, full text can be
found [here](gpl.md).

    Copyright (C) 2016 Mateusz Belicki

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
