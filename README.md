# RaichoDemo — 東西電力 料金計算システム RACS (架空・完全オリジナル)

**A fully original, self-contained Japanese IBM Z legacy sample system
for modernization practice — batteries (and traps) included.**

> **Named after the raichō (雷鳥, rock ptarmigan) — the "thunder bird"
> of the Japanese Alps.** A bird of the power domain by name, and a
> survivor from the ice age by nature: it has endured in harsh alpine
> terrain for tens of thousands of years — much like the systems this
> repository portrays. Sibling project of TSUBAME-BENCH (the swallow).

リポジトリ名は **RaichoDemo**、作中の行内システム名は **RACS**
(Rates And Customer-billing System) — 「対外名称と行内名称が違う」
という実際の現場の二層構造をそのまま採っています。

日本の電力会社「東西電力株式会社」(架空) の料金計算システム RACS を、
IBM Z 純正構成 (COBOL/JCL/CICS/BMS/MQ/IMS/Db2/HLASM) の**本物の作法**で
書き起こした、モダナイゼーション演習用のサンプルシステムです。
AWS CardDemo と同じ目的を、日本市場の実相 (和暦・全銀・検針・
多段委託・Galapagos 事情) で。

## なにが入っているか

| 層 | 資産 | 検証 |
|----|------|------|
| バッチ | COBOL 19 本 (料金計算・検針受信・全銀・媒体・世代管理ほか) | GnuCOBOL 実走 + golden + Python 対照検算 |
| オンライン | CICS 6 本 (サインオン->照会->更新 3 画面 REWRITE) + BMS 6 面 + CSD | ミニ CICS エミュレータで疑似会話 34 イベント実走 |
| 常駐 | デマンド監視 (MQ 電文 -> 判定 -> Db2 履歴) | 12 電文 golden + 全件独立再判定 |
| IMS/DB | 計器設備DB (階層: 計器ルート+取付履歴ツイン) — **乗率と和暦検定満期の正本**。DL/I 抽出 (GU/GN/GNP) が日次先頭で計器マスタをバイト等価再生成 | CBLTDLI スタブ実走 + byte-equal + Python 独立再導出 |
| 資産のみ | DMDDBD/PSB (デマンド当日値), Db2 DDL, HLASM 2 本 (検査数字の「原典」), JCL, 化石 1 本 (翻訳不能) | 目録 + 負のゲート |
| 罠 | 11 カテゴリ 85 パターンの台帳 + 潜在バグ 10 件 (LB-001..010) | 挑発データと golden で「罠が生きている」ことを保証 |

## 仮想メインフレーム (JES2 模擬)

本物の JCL をそのまま投入できます:

```bash
bash tests/jes_build.sh                    # LNKLST 構築
python3 tools/jes.py app/jcl/RJDAILY.jcl   # 日次 11 ステップ投入
cat spool/RJDAILY/JESMSGLG                 # ジョブログ (IEF/HASP)
cat spool/RJDAILY/D030RYO.SYSPRINT         # 料金計算の SYSOUT
```

割当 (IEF237I)・RC (IEF142I)・COND スキップ (IEF202I)・MAXCC
($HASP395) まで JES2 の顔つきで動きます。IDCAMS の SYSIN 解釈、
DFSRRC00 (IMS バッチ域)、SORT+MODS (E15/E35) にも対応。

## 3270 端末で操作する

本物の BMS マクロを解析して 24x80 画面を描画し、RACS オンラインを
キーボードで操作できます (2 つの端末窓で):

```bash
# 窓1: 3270 端末
python3 tools/term3270.py
# 窓2: CICS 起動 (端末モード)
bash tests/jes_build.sh   # 初回のみ (testcics も LNKLST に入る)
RACS_TERM=1 tests/bin/testcics
```

サインオン画面 (** TOZAI DENRYOKU **) が現れたら USER0001 /
PASS0001 で入場。メニューから契約照会・更新・検針・料金へ。
非対話の台本実行は `--script tests/term_script.txt --snap 保存先`。

## デバッグモード (文トレース x GTF 相関)

「どの COBOL 文の CALL で、どの CICS/DL-I/SQL/MQ が走ったか」を
単一の時系列絵巻で読めます:

```bash
bash tests/debug_run.sh cics          # ims / auth も可
less spool/gtf/annotated.txt
python3 tools/gtf_view.py --prog RAKYK02C   # 1本に絞込み
```

```
RASGN00C   56    MOVE 'SEND-MAP   ' TO CP-CMD
RASGN00C   59    CALL 'RXCICSTB' USING DFHEIBLK CICS-PARM SGN00MO.
      >>>>> GTF#000000001 CICS SEND-MAP MAP=RSGN00M/SGN00M TRN=RSGN
```

仕組み: cobc -ftraceall の文トレースと、4 スタブが書く GTF
(プロセス共有 EXTERNAL 通番) を、単一スレッドの順序保存で突合。
CALL の呼び先はトレースに出ないため、Line 番号で生成ソース原文を
引いて判定・注釈します。

## 30 秒で動かす

```bash
apt-get install gnucobol   # 3.x/4.x
bash tests/build_all.sh    # 53 ゲート全 green
bash tests/perf_m.sh       # (任意) M スケール性能実測
```

## 評価演習での配布方針

ベンダ・AI ツールの移行評価に使う場合は、本番の RFP と同様に
**被評価者へは `app/` のみを渡し**、`tests/`(golden・xcheck・
品質ゲート)と `docs/`(罠台帳・採点シート)は評価者側が保持して
ください。採点方法は docs/10 を参照。なお app/cbl/RUMQSUB.cbl の
GTF 書出し節は模擬環境用フック(ソース内に注記あり)で、評価対象
ロジックには含めません。

## 設計原則

1. **全プログラム実走**: 動かないサンプルは腐る。全 COBOL は
   GnuCOBOL で実走し、出力は golden 固定 + Python 独立実装で検算。
2. **罠は台帳に登録**: 偶然のバグではなく、docs/03 の 85 パターン
   と docs/07 の潜在バグ台帳に意図を記録。移行採点に使える。
3. **変換器の向こうに本物**: CICS/Db2 のソースは EXEC CICS /
   EXEC SQL の本物構文。実機の変換器を模した cicsprep/sqlprep が
   スタブ呼出しへ展開する — 「変換前ソースが資産」という実機と
   同じ構図。
4. **複製には系譜**: 検査数字 mod 97 は S58 の HLASM (app/asm/
   RASMCHK.asm) が原典で、COBOL 側 5 箇所 + 簡易版 1 箇所へ複製
   されている (I-01)。仕様復元で遡ると assembler に行き着く。

## ライセンスと免責

Apache-2.0。東西電力株式会社は実在しません。データはすべて
seed 固定の擬似乱数による生成物で、実在の人物・団体・供給地点
とは無関係です。詳細は DISCLAIMER.md。

---

# RaichoDemo (English)

A fully original Japanese-market mainframe legacy sample —
**RaichoDemo** — a utility-billing system (in-fiction name: RACS) for
the fictional **Tozai Denryoku** (East-West Electric Power), written
in authentic IBM Z style:
COBOL, JCL, CICS/BMS, MQ, IMS, Db2 and HLASM.

Same purpose as AWS CardDemo, but built around realities of
Japanese legacy estates: Japanese calendar (wareki) windowing,
Zengin bank-transfer media, meter-reading workflows, layered
outsourcing fingerprints, and era-mixed coding styles.

Everything executes: all 25+ COBOL programs run under GnuCOBOL
with golden outputs and independent Python cross-checks; the CICS
layer runs pseudo-conversations on a mini-CICS emulator behind a
faithful command-translator mock; the demand-monitoring resident
consumes MQ-style messages and writes Db2-style history — all
gated by `tests/build_all.sh` (53 checks). The hierarchical IMS meter-asset DB is a first-class system of record: a DL/I batch sweep regenerates the meter master byte-for-byte.

85 cataloged trap patterns and 10 latent defects (with triggering
data) make it a scoring rig for modernization tooling.
Apache-2.0. The company, people and data are fictional.
