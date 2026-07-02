#!/bin/bash
# 日次サイクル一気通し (JCL相当のRC意味論で連結)
#   D010受信 -> D020選別 -> D025検証 -> D030料金 -> D040検針票
#   -> D060収納 -> D070滞納(RC=4が正常) -> M010集計 -> 変換
set -u
cd "$(dirname "$0")/.."
B=tests/bin
step() {  # step 名 期待RC群 コマンド...
  NAME=$1; OKRC=$2; shift 2
  OUT=$("$@" 2>&1); RC=$?
  if echo ",$OKRC," | grep -q ",$RC,"; then
    echo "STEP OK  $NAME (RC=$RC)"
  else
    echo "STEP NG  $NAME (RC=$RC expected $OKRC)"; echo "$OUT"; exit 1
  fi
}
printf '20260701' > app/data/portable/DATECTL.dat
step D005-MTRDB    0   $B/testims
step D010-JYUSHIN  0   $B/mkkenin
step D010-HENSHU   0   $B/rbken01c 202406
step D020-SORT     0   $B/testsrt
step D025-KENSHO   0   $B/rbken02c 202406
step D030-RYOKIN   0,4 $B/rbryo01c 202406
step D040-KENSHINH 0   $B/rbstm01
step D060-SHUNO    0   $B/rbshu01c 202406
step D070-TAINO    4   $B/rbtai01c
step M010-SYUKEI   0   $B/rbget01c
step CNV-KYKMAST   0   $B/rbcnv01c
echo "DAILY CYCLE COMPLETE"
