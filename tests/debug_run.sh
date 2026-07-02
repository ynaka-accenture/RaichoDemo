#!/bin/bash
# debug_run.sh — デバッグモード実行 (文トレース+GTF 相関)
#   使い方: bash tests/debug_run.sh {cics|auth|ims} [--prog PGMNAME]
#   出力:   spool/gtf/annotated.txt (統合デバッグビュー)
set -eu
cd "$(dirname "$0")/.."
TGT=${1:-cics}; shift || true
mkdir -p tests/bin tests/cpy-portable spool/gtf
rm -f spool/gtf/*.trc spool/gtf/annotated.txt
for f in app/cpy/*.cpy; do
  sed 's/PIC G(\([0-9]*\)) DISPLAY-1/PIC N(\1)      /' "$f" \
    > tests/cpy-portable/$(basename "$f")
done
C="cobc -std=ibm -x -ftraceall -I tests/cpy-portable"
E="env RACS_GTF=1 COB_SET_TRACE=Y COB_TRACE_FILE=spool/gtf/cobol.trc"
case "$TGT" in
cics)
  mkdir -p tests/cics-gen
  for p in RASGN00C RAMEN01C RAKYK01C RAKYK02C RAKEN01C RARYO01C; do
    python3 tools/cicsprep.py app/cbl/$p.cbl tests/cics-gen/$p.cbl \
      > /dev/null
  done
  $C -o tests/bin/dbg tests/TESTCICS.cbl tests/cics-gen/*.cbl \
     tests/RXCICSTB.cbl 2>/dev/null
  rm -f app/data/portable/CICSOUT.txt app/data/portable/CICSUPD.dat
  $E tests/bin/dbg > /dev/null 2>&1
  ;;
auth)
  mkdir -p tests/sql-gen
  python3 tools/sqlprep.py app/cbl/RAAUP00C.cbl \
      tests/sql-gen/RAAUP00C.cbl > /dev/null
  $C -o tests/bin/dbg tests/sql-gen/RAAUP00C.cbl app/cbl/RAAUP01C.cbl \
     app/cbl/RUMQSUB.cbl tests/RXDB2TB.cbl 2>/dev/null
  rm -f app/data/portable/MQOUT.dat app/data/portable/DB2LOG.dat
  $E tests/bin/dbg > /dev/null 2>&1
  ;;
ims)
  $C -o tests/bin/dbg tests/TESTIMS.cbl app/cbl/RBMTR00C.cbl \
     tests/RXIMSTB.cbl 2>/dev/null
  $E tests/bin/dbg > /dev/null 2>&1
  ;;
*) echo "target: cics|auth|ims"; exit 8 ;;
esac
python3 tools/gtf_view.py --out spool/gtf/annotated.txt "$@"
echo "less spool/gtf/annotated.txt で参照 (--prog PGM で絞込み可)"
