# hxxd - A Simple Hex Dump Utility in Haskell

`hxxd` is a minimalist command-line utility, similar to the standard `xxd` tool, written in Haskell. It reads binary data from a specified file or standard input and prints a hexadecimal and ASCII representation of that data to standard output.


## Features

*   Reads from a file or standard input.
*   Outputs data in a standard `xxd`-like format:
    `OFFSET: HEX_GROUP1 HEX_GROUP2 ... HEX_GROUP8  ASCIICHARS`
*   **Offset:** 8-digit hexadecimal.
*   **Hex Groups:** 8 groups, each representing 2 bytes (e.g., `aabb`), separated by single spaces.
*   **ASCII Characters:** 16 characters, with non-printable bytes replaced by a dot (`.`).
*   Each line represents 16 bytes of input.


## No Options

This version of `hxxd` is intentionally simple and has **no command-line options** beyond specifying an input file. It uses sane defaults for its output format.

## Building

This project uses Cabal.

1.  **Clone the repository (if applicable) or ensure you have the source code.**
2.  **Navigate to the project directory:**
    ```bash
    cd /path/to/hxxd
    ```
3.  **Build the project:**
    ```bash
    cabal build
    ```
    This will create an executable, typically in a path like `dist-newstyle/build/<arch-os>/ghc-<version>/hxxd-<version>/x/hxxd/build/hxxd/hxxd`.

## Usage

**From a file:**

```bash
cabal run hxxd -- yourfile.bin
```


```
% cabal run hxxd /tmp/ahh

00000000: 6d6f 6475 6c65 204d 6169 6e20 7768 6572  module Main wher
00000010: 650a 0a6d 6169 6e20 3a3a 2049 4f20 2829  e..main :: IO ()
00000020: 0a6d 6169 6e20 3d20 646f 0a20 2020 7075  .main = do.   pu
00000030: 7453 7472 4c6e 2022 4865 6c6c 6f21 2121  tStrLn "Hello!!!
00000040: 220a 2020 2070 7574 5374 724c 6e20 2257  ".   putStrLn "W
00000050: 6861 7420 6973 2079 6f75 7220 6e61 6d65  hat is your name
00000060: 3f20 220a 2020 206e 616d 6520 3c2d 2067  ? ".   name <- g
00000070: 6574 4c69 6e65 0a20 2020 7075 7453 7472  etLine.   putStr
00000080: 4c6e 2024 2022 4865 6c6c 6f20 2220 2b2b  Ln $ "Hello " ++
00000090: 206e 616d 6520 2b2b 2022 2121 220a        name ++ "!!".  

```