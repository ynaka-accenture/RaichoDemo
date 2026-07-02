#!/bin/bash
# perf_m.sh — M スケール性能実測 (データは gen_scale_m.py で生成)
# 使い方: python3 tools/gen_scale_m.py && bash tests/perf_m.sh
set -u
cd "$(dirname "$0")/.."
M=/tmp/racs-m
P=app/data/portable
mkdir -p tests/bin tests/cpy-portable
for f in app/cpy/*.cpy; do
  sed 's/PIC G(\([0-9]*\)) DISPLAY-1/PIC N(\1)      /' "$f" \
    > tests/cpy-portable/$(basename "$f")
done

echo "== M-1: RBRYO01C 料金計算 (検針 720,000 件読み/当月抽出) =="
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/rbryo01c \
    app/cbl/RBRYO01C.cbl app/cbl/RUTLDTC.cbl 2>/dev/null
cp "$P/KENFILE.dat" /tmp/KENFILE.keep
cp "$M/KENFILE-M.dat" "$P/KENFILE.dat"
S=$(date +%s.%N)
tests/bin/rbryo01c 202406 > /tmp/perf_ryo.out 2>/dev/null
E=$(date +%s.%N)
cp /tmp/KENFILE.keep "$P/KENFILE.dat"
T1=$(echo "$E - $S" | bc)
grep -E "KENSU|GOKEI" /tmp/perf_ryo.out | head -2
echo "M-1 elapsed: ${T1}s  (720,000 recs -> throughput: \
$(echo "720000 / $T1" | bc) recs/s)"

echo "== M-2: RAAUP00C デマンド判定 (電文 1,000,000 件) =="
mkdir -p tests/sql-gen
python3 tools/sqlprep.py app/cbl/RAAUP00C.cbl \
    tests/sql-gen/RAAUP00C.cbl > /dev/null
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/raaup \
    tests/sql-gen/RAAUP00C.cbl app/cbl/RAAUP01C.cbl \
    app/cbl/RUMQSUB.cbl tests/RXDB2TB.cbl 2>/dev/null
cp "$P/MQIN.dat" /tmp/MQIN.keep
cp "$M/MQIN-M.dat" "$P/MQIN.dat"
rm -f "$P/MQOUT.dat" "$P/DB2LOG.dat"
S=$(date +%s.%N)
tests/bin/raaup > /tmp/perf_aup.out 2>/dev/null
E=$(date +%s.%N)
T2=$(echo "$E - $S" | bc)
cat /tmp/perf_aup.out
echo "M-2 elapsed: ${T2}s  (1,000,000 msgs -> throughput: \
$(echo "1000000 / $T2" | bc) msgs/s)"
cp /tmp/MQIN.keep "$P/MQIN.dat"
rm -f "$P/MQOUT.dat" "$P/DB2LOG.dat"
rm -rf tests/bin tests/cpy-portable tests/sql-gen
echo "== 復元完了 (portable データは N スケールに戻した) =="
