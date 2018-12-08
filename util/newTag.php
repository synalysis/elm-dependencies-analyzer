#!/usr/bin/env php
<?php

/*
 * Get 4-digit rangom number which is not yet seen in code.
 */

# ======================================================================
# CONSTANTS

const GLOB_SOURCE_FILES = "src/*.elm";

# ======================================================================
# MAIN

$seen = [];

foreach (glob(GLOB_SOURCE_FILES) as $file) {
    $data = file_get_contents($file);
    if ($data === false) throw new Exception();

    if (preg_match_all("/(\d{4,})/", $data, $m) === false) throw new Exception();
    foreach ($m[1] as $number) {
        for ($n = 0; $n <= strlen($number) - 4; $n++) {
            $seen[substr($number, $n, 4)] = true;            
        }
    }
}

$new = null;
do {
    $new = rand(1000, 9999);
} while (array_key_exists($new, $seen));

printf("Seen %d. New: %d\n", count($seen), $new);
