#!/bin/bash
# RACS-DEMO 品質ゲート: バッチ系COBOLの全数コンパイル+ゴールデン突合
set -e
cd "$(dirname "$0")/.."
mkdir -p tests/bin
PASS=0; FAIL=0
# コピー句の可搬前処理 (PIC G -> PIC N)
mkdir -p tests/cpy-portable
for f in app/cpy/*.cpy; do
  sed 's/PIC G(\([0-9]*\)) DISPLAY-1/PIC N(\1)      /' "$f" \
      > "tests/cpy-portable/$(basename "$f")"
done
for src in app/cbl/RB*.cbl app/cbl/RU*.cbl app/cbl/RX*.cbl; do
  [ -e "$src" ] || continue
  cobc -std=ibm -c -I tests/cpy-portable \
      -o "tests/bin/$(basename "$src" .cbl).o" "$src" 2>/dev/null \
    && echo "COMPILE OK  $src" || { echo "COMPILE NG  $src"; FAIL=$((FAIL+1)); continue; }
  PASS=$((PASS+1))
done
# コピー句ゲート: 構文/サイズ + データ整合
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/rchkcpy tests/RCHKCPY.cbl 2>/dev/null
if diff <(tests/bin/rchkcpy) tests/expected/RCHKCPY.golden > /dev/null; then
  echo "GOLDEN OK   RCHKCPY (copybook sizes)"; PASS=$((PASS+1))
else
  echo "GOLDEN NG   RCHKCPY"; FAIL=$((FAIL+1))
fi
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/rchkdat tests/RCHKDAT.cbl 2>/dev/null
if diff <(tests/bin/rchkdat) tests/expected/RCHKDAT.golden > /dev/null; then
  echo "GOLDEN OK   RCHKDAT (data-copybook byte consistency)"; PASS=$((PASS+1))
else
  echo "GOLDEN NG   RCHKDAT"; FAIL=$((FAIL+1))
fi
# データ保証ゲート (docs/07 潜在バグ台帳)
if python3 tools/gen_data.py --verify; then
  echo "DATA OK     (潜在バグ引き金の不在保証)"; PASS=$((PASS+1))
else
  echo "DATA NG"; FAIL=$((FAIL+1))
fi
# 機能ゲート: ドライバ実行→ゴールデン突合
cobc -std=ibm -x -o tests/bin/testdtc tests/TESTDTC.cbl app/cbl/RUTLDTC.cbl
if diff <(tests/bin/testdtc) tests/expected/TESTDTC.golden > /dev/null; then
  echo "GOLDEN OK   RUTLDTC (24 cases)"; PASS=$((PASS+1))
else
  echo "GOLDEN NG   RUTLDTC"; FAIL=$((FAIL+1))
fi
# 中核バッチ実走ゲート: RBRYO01C 202406 -> ゴールデン突合 + 対照実装検算
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/rbryo01c \
    app/cbl/RBRYO01C.cbl app/cbl/RUTLDTC.cbl 2>/dev/null
if diff <(tests/bin/rbryo01c 202406) tests/expected/RBRYO01C.golden \
    > /dev/null; then
  echo "GOLDEN OK   RBRYO01C (202406 full run, 5968 bills)"; PASS=$((PASS+1))
else
  echo "GOLDEN NG   RBRYO01C"; FAIL=$((FAIL+1))
fi
if python3 tools/xcheck_ryo.py 202406 > /dev/null; then
  echo "XCHECK OK   RBRYO01C (対照実装: 全5968件・全金額項目一致)"; PASS=$((PASS+1))
else
  echo "XCHECK NG   RBRYO01C"; FAIL=$((FAIL+1))
fi
# 第2波バッチ実走ゲート (RBKEN02C -> RBSHU01C -> RBTAI01C の依存順)
printf '20260701' > app/data/portable/DATECTL.dat
for PG in rbken02c rbshu01c rbtai01c; do
  UP=$(echo "$PG" | tr 'a-z' 'A-Z')
  cobc -std=ibm -x -I tests/cpy-portable -o "tests/bin/$PG" \
      "app/cbl/$UP.cbl" 2>/dev/null
  case "$PG" in
    rbtai01c) OUTPUT=$("tests/bin/$PG"; echo "rc=$?");;
    *)        OUTPUT=$("tests/bin/$PG" 202406; echo "rc=$?");;
  esac
  if [ "$OUTPUT" = "$(cat tests/expected/$UP.golden)" ]; then
    echo "GOLDEN OK   $UP"; PASS=$((PASS+1))
  else
    echo "GOLDEN NG   $UP"; FAIL=$((FAIL+1))
  fi
done
# 受信系ゲート: VB生成 -> RBKEN01C -> ゴールデン / SORT出口ドライバ
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/mkkenin \
    tests/MKKENIN.cbl 2>/dev/null && tests/bin/mkkenin > /dev/null
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/rbken01c \
    app/cbl/RBKEN01C.cbl 2>/dev/null
if [ "$(tests/bin/rbken01c 202406; echo rc=$?)" = \
     "$(cat tests/expected/RBKEN01C.golden)" ]; then
  echo "GOLDEN OK   RBKEN01C (VB受信 H/D/K/T 300件)"; PASS=$((PASS+1))
else
  echo "GOLDEN NG   RBKEN01C"; FAIL=$((FAIL+1))
fi
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/testsrt \
    tests/TESTSRT.cbl app/cbl/RXSRT15C.cbl app/cbl/RXSRT35C.cbl \
    2>/dev/null
if diff <(tests/bin/testsrt) tests/expected/TESTSRT.golden \
    > /dev/null; then
  echo "GOLDEN OK   TESTSRT (E15/E35 出口: 除外254/部門付与)"; PASS=$((PASS+1))
else
  echo "GOLDEN NG   TESTSRT"; FAIL=$((FAIL+1))
fi
# 第3波ゲート: 帳票親子 / 月次集計 / 新旧変換 -> ゴールデン + 対照検算
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/rbstm01 \
    app/cbl/RBSTM01C.cbl app/cbl/RBSTM02C.cbl 2>/dev/null
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/rbget01c \
    app/cbl/RBGET01C.cbl 2>/dev/null
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/rbcnv01c \
    app/cbl/RBCNV01C.cbl 2>/dev/null
for GP in rbstm01:RBSTM01C rbget01c:RBGET01C rbcnv01c:RBCNV01C; do
  BIN=${GP%%:*}; UP=${GP##*:}
  if diff <(tests/bin/$BIN) tests/expected/$UP.golden > /dev/null; then
    echo "GOLDEN OK   $UP"; PASS=$((PASS+1))
  else
    echo "GOLDEN NG   $UP"; FAIL=$((FAIL+1))
  fi
done
if python3 tools/xcheck_wave3.py > /dev/null; then
  echo "XCHECK OK   wave3 (按分残差/帳票行数/変換等価)"; PASS=$((PASS+1))
else
  echo "XCHECK NG   wave3"; FAIL=$((FAIL+1))
fi
if python3 tools/xcheck_wave2.py 202406 > /dev/null; then
  echo "XCHECK OK   wave2 (KEN/SHU/TAI 対照検算)"; PASS=$((PASS+1))
else
  echo "XCHECK NG   wave2"; FAIL=$((FAIL+1))
fi
# 第4波ゲート: 旧版/計器/単価点検/退避 + 化石(翻訳不能の確認)
for GP in rbryo00c:RBRYO00C:202406 rbmtr01c:RBMTR01C: \
          rbtbl01c:RBTBL01C: rbbku01c:RBBKU01C:; do
  BIN=$(echo "$GP" | cut -d: -f1); UP=$(echo "$GP" | cut -d: -f2)
  ARG=$(echo "$GP" | cut -d: -f3)
  cobc -std=ibm -x -I tests/cpy-portable -o "tests/bin/$BIN" \
      "app/cbl/$UP.cbl" 2>/dev/null
  if diff <(tests/bin/$BIN $ARG) tests/expected/$UP.golden \
      > /dev/null; then
    echo "GOLDEN OK   $UP"; PASS=$((PASS+1))
  else
    echo "GOLDEN NG   $UP"; FAIL=$((FAIL+1))
  fi
done
if cobc -std=ibm -fsyntax-only app/cbl/fossil/RYOKYU68.cbl \
    2>/dev/null; then
  echo "FOSSIL NG   RYOKYU68 が翻訳できてしまう"; FAIL=$((FAIL+1))
else
  echo "FOSSIL OK   RYOKYU68 (OS/VS化石: 翻訳不能を確認 K-06)"
  PASS=$((PASS+1))
fi
if python3 tools/xcheck_wave4.py > /dev/null; then
  echo "XCHECK OK   wave4 (外税再計算/窓割り/退避同一)"; PASS=$((PASS+1))
else
  echo "XCHECK NG   wave4"; FAIL=$((FAIL+1))
fi
# 第5波ゲート: 媒体作成 / 一括異動 / 契約棚卸し
for GP in rbryo02c:RBRYO02C rbjyu01c:RBJYU01C rbkyk01c:RBKYK01C; do
  BIN=${GP%%:*}; UP=${GP##*:}
  cobc -std=ibm -x -I tests/cpy-portable -o "tests/bin/$BIN" \
      "app/cbl/$UP.cbl" 2>/dev/null
  if diff <(tests/bin/$BIN) tests/expected/$UP.golden > /dev/null; then
    echo "GOLDEN OK   $UP"; PASS=$((PASS+1))
  else
    echo "GOLDEN NG   $UP"; FAIL=$((FAIL+1))
  fi
done
if python3 tools/xcheck_wave5.py > /dev/null; then
  echo "XCHECK OK   wave5 (媒体CORR転記/異動30件/棚卸同一)"; PASS=$((PASS+1))
else
  echo "XCHECK NG   wave5"; FAIL=$((FAIL+1))
fi
# CICSゲート: 変換器->翻訳->疑似会話実走->セッションログ照合
python3 tools/cicsprep.py app/cbl/RASGN00C.cbl \
    tests/cics-gen/RASGN00C.cbl > /dev/null
python3 tools/cicsprep.py app/cbl/RAMEN01C.cbl \
    tests/cics-gen/RAMEN01C.cbl > /dev/null
python3 tools/cicsprep.py app/cbl/RAKYK01C.cbl \
    tests/cics-gen/RAKYK01C.cbl > /dev/null
python3 tools/cicsprep.py app/cbl/RAKYK02C.cbl \
    tests/cics-gen/RAKYK02C.cbl > /dev/null
python3 tools/cicsprep.py app/cbl/RAKEN01C.cbl \
    tests/cics-gen/RAKEN01C.cbl > /dev/null
python3 tools/cicsprep.py app/cbl/RARYO01C.cbl \
    tests/cics-gen/RARYO01C.cbl > /dev/null
rm -f app/data/portable/CICSOUT.txt app/data/portable/CICSUPD.dat
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/testcics \
    tests/TESTCICS.cbl tests/cics-gen/RASGN00C.cbl \
    tests/cics-gen/RAMEN01C.cbl tests/cics-gen/RAKYK01C.cbl \
    tests/cics-gen/RAKYK02C.cbl tests/cics-gen/RAKEN01C.cbl \
    tests/cics-gen/RARYO01C.cbl tests/RXCICSTB.cbl 2>/dev/null
DRV=$(tests/bin/testcics 2>/dev/null)
if [ "$DRV" = "$(cat tests/expected/TESTCICS.golden)" ] && \
   diff <(sed 's/[0-9][0-9]\.[0-9][0-9]\.[0-9][0-9]/YY.MM.DD/g' \
       app/data/portable/CICSOUT.txt) \
       tests/expected/CICSOUT.golden > /dev/null; then
  echo "CICS OK     疑似会話 34イベント (照会4種->更新->検針->料金)"
  PASS=$((PASS+1))
else
  echo "CICS NG     疑似会話"; FAIL=$((FAIL+1))
fi
if python3 tools/xcheck_cics.py > /dev/null; then
  echo "XCHECK OK   cics (更新ジャーナル/履歴退避/マスタ不変)"; PASS=$((PASS+1))
else
  echo "XCHECK NG   cics"; FAIL=$((FAIL+1))
fi
# 3270端末ゲート: BMS描画->対話セッション11画面->更新到達
rm -rf spool/term; mkdir -p spool/term
rm -f app/data/portable/CICSOUT.txt app/data/portable/CICSUPD.dat
( nohup timeout 60 python3 tools/term3270.py \
    --script tests/term_script.txt \
    --snap spool/term/screens.txt --no-ansi > /dev/null 2>&1 & )
env RACS_TERM=1 timeout 60 tests/bin/testcics > /dev/null 2>&1
T1=$?
sleep 1
SCR=$(grep -c "===== SCREEN" spool/term/screens.txt 2>/dev/null)
if [ "$T1" = "0" ] && [ "$SCR" = "11" ] && \
   grep -q "TOZAI DENRYOKU" spool/term/screens.txt && \
   grep -q "59,463" spool/term/screens.txt && \
   grep -q "KOSHIN KANRYO" spool/term/screens.txt && \
   grep -q "GO-RIYOU ARIGATOU" spool/term/screens.txt && \
   [ -s app/data/portable/CICSUPD.dat ]; then
  echo "TERM OK     3270端末 11画面 (BMS描画/照会59,463/更新REWRITE到達)"
  PASS=$((PASS+1))
else
  echo "TERM NG     3270端末 (rc=$T1 screens=$SCR)"; FAIL=$((FAIL+1))
fi
rm -f app/data/portable/CICSUPD.dat
# デマンド監視ゲート: SQL変換->翻訳->MQ12電文->判定照合+対照検算
python3 tools/sqlprep.py app/cbl/RAAUP00C.cbl \
    tests/sql-gen/RAAUP00C.cbl > /dev/null
rm -f app/data/portable/MQOUT.dat app/data/portable/DB2LOG.dat
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/raaup \
    tests/sql-gen/RAAUP00C.cbl app/cbl/RAAUP01C.cbl \
    app/cbl/RUMQSUB.cbl tests/RXDB2TB.cbl 2>/dev/null
AUP=$(tests/bin/raaup 2>/dev/null)
if [ "$AUP" = "$(cat tests/expected/RAAUP00C.golden)" ] && \
   diff app/data/portable/MQOUT.dat tests/expected/MQOUT.golden \
       > /dev/null && \
   python3 tools/xcheck_auth.py > /dev/null; then
  echo "AUTH OK     デマンド12電文 (TJ07境界2件=W側/DB2履歴7行)"
  PASS=$((PASS+1))
else
  echo "AUTH NG     デマンド監視"; FAIL=$((FAIL+1))
fi
# JESゲート: 本物のJCLを解釈実行 (割当ログ/スプール/COND意味論)
bash tests/jes_build.sh > /dev/null 2>&1
rm -rf spool
set +e
python3 tools/jes.py app/jcl/RJDAILY.jcl > /tmp/jes-daily.out 2>&1
J1=$?
grep -q "GOKEI=+0000072053623" spool/RJDAILY/D030RYO.SYSPRINT 2>/dev/null
J2=$?
python3 tools/jes.py tests/RJCOND.jcl 2>/dev/null | \
    grep -q "IEF202I.*S2SKIP"
J3=$?
set -e
if [ $J1 -eq 0 ] && [ $J2 -eq 0 ] && [ $J3 -eq 0 ] && \
   grep -q "STEPS=11 SKIP=0" /tmp/jes-daily.out; then
  echo "JES OK      RJDAILY 11ステップ MAXCC=4 / COND旧様式スキップ実証"
  PASS=$((PASS+1))
else
  echo "JES NG      JCL解釈実行"; FAIL=$((FAIL+1))
fi
# 日次サイクル一気通しゲート (JCL相当のRC意味論)
# IMS正本ゲート: DL/I抽出がMTRMASTをバイト等価再生成すること
cobc -std=ibm -x -I tests/cpy-portable -o tests/bin/testims \
    tests/TESTIMS.cbl app/cbl/RBMTR00C.cbl tests/RXIMSTB.cbl 2>/dev/null
SHA_B=$(sha256sum app/data/portable/MTRMAST.dat | cut -c1-16)
tests/bin/testims > /tmp/ims-gate.out 2>/dev/null
SHA_A=$(sha256sum app/data/portable/MTRMAST.dat | cut -c1-16)
if [ "$SHA_B" = "$SHA_A" ] && \
   grep -q "SEIGO-NG=0000000" /tmp/ims-gate.out && \
   python3 tools/xcheck_ims.py > /dev/null; then
  echo "IMS OK      DL/I抽出 6500計器/12982ツイン バイト等価 (正本性)"
  PASS=$((PASS+1))
else
  echo "IMS NG      DL/I抽出"; FAIL=$((FAIL+1))
fi
if bash tests/run_daily.sh > /dev/null 2>&1; then
  echo "DAILY OK    日次サイクル 11ステップ (D005 IMS抽出->D010->CNV)"
  PASS=$((PASS+1))
else
  echo "DAILY NG"; FAIL=$((FAIL+1))
fi
# デバッグ相関ゲート: 文トレース x GTF の全件対応
set +e
bash tests/debug_run.sh cics > /tmp/dbg.out 2>&1
D1=$?
set -e
if [ $D1 -eq 0 ] && \
   grep -q "GTF 102/102 対応" /tmp/dbg.out && \
   grep -A1 "CALL 'RXCICSTB' USING DFHEIBLK CICS-PARM SGN00MO" \
       spool/gtf/annotated.txt | grep -q "GTF#000000001 CICS SEND-MAP"; then
  echo "DEBUG OK    文トレースxGTF相関 102/102 (CALL->API 単一絵巻)"
  PASS=$((PASS+1))
else
  echo "DEBUG NG    デバッグ相関"; FAIL=$((FAIL+1))
fi
# レガシー度ゲート: 複雑度下限+命名揺れ前提(docs/06 と同期)
if python3 tools/cobmetrics.py --gate app/cbl/*.cbl; then
  echo "METRICS OK  (レガシー複雑度 下限クリア)"; PASS=$((PASS+1))
else
  echo "METRICS NG  (複雑さ不足のプログラムあり)"; FAIL=$((FAIL+1))
fi
echo "---- PASS=$PASS FAIL=$FAIL ----"
exit $FAIL
