#!/bin/bash
# JES 実行に必要なバイナリ (LNKLST) を一括ビルド
set -u
cd "$(dirname "$0")/.."
mkdir -p tests/bin tests/cpy-portable
for f in app/cpy/*.cpy; do
  sed 's/PIC G(\([0-9]*\)) DISPLAY-1/PIC N(\1)      /' "$f" \
    > tests/cpy-portable/$(basename "$f")
done
C="cobc -std=ibm -x -I tests/cpy-portable"
$C -o tests/bin/testims  tests/TESTIMS.cbl app/cbl/RBMTR00C.cbl \
   tests/RXIMSTB.cbl 2>/dev/null
$C -o tests/bin/mkkenin  tests/MKKENIN.cbl 2>/dev/null
$C -o tests/bin/rbken01c app/cbl/RBKEN01C.cbl app/cbl/RUTLDTC.cbl 2>/dev/null
$C -o tests/bin/testsrt  tests/TESTSRT.cbl app/cbl/RXSRT15C.cbl \
   app/cbl/RXSRT35C.cbl 2>/dev/null
$C -o tests/bin/rbken02c app/cbl/RBKEN02C.cbl app/cbl/RUTLDTC.cbl 2>/dev/null
$C -o tests/bin/rbryo01c app/cbl/RBRYO01C.cbl app/cbl/RUTLDTC.cbl 2>/dev/null
$C -o tests/bin/rbstm01  app/cbl/RBSTM01C.cbl app/cbl/RBSTM02C.cbl \
   app/cbl/RUTLDTC.cbl 2>/dev/null
$C -o tests/bin/rbshu01c app/cbl/RBSHU01C.cbl app/cbl/RUTLDTC.cbl 2>/dev/null
$C -o tests/bin/rbtai01c app/cbl/RBTAI01C.cbl app/cbl/RUTLDTC.cbl 2>/dev/null
$C -o tests/bin/rbget01c app/cbl/RBGET01C.cbl app/cbl/RUTLDTC.cbl 2>/dev/null
$C -o tests/bin/rbcnv01c app/cbl/RBCNV01C.cbl app/cbl/RUTLDTC.cbl 2>/dev/null
mkdir -p tests/cics-gen
for p in RASGN00C RAMEN01C RAKYK01C RAKYK02C RAKEN01C RARYO01C; do
  python3 tools/cicsprep.py app/cbl/$p.cbl tests/cics-gen/$p.cbl \
    > /dev/null
done
$C -o tests/bin/testcics tests/TESTCICS.cbl tests/cics-gen/*.cbl \
   tests/RXCICSTB.cbl 2>/dev/null
echo "LNKLST READY: $(ls tests/bin | wc -l) programs"
