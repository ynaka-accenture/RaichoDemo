# 05. 構築計画 — フェーズ分割と品質ゲート

総量(COBOL 46本 約44,000行+周辺資産)を一気に生成せず、**フェーズごとに
「正しく動く」を確定させてから積み上げる**。cob2j / CardDemoJ で実証済みの
インクリメンタル方式を踏襲する。

## フェーズ

| Phase | 内容 | 完了条件(品質ゲート) |
|---|---|---|
| **0(本セッション)** | 全体設計・罠カタログ85・資産一覧・データ設計・骨格・**参照実装1本**(RUTLDTC) | 設計文書一式+RUTLDTC が GnuCOBOL で全テストケース green |
| 1 | コピー句34本+VSAM DEFINE 一式+データ生成ツール(S スケール)+dirty_ledger | 生成データがレイアウト検証ツールを通過(ゴミは台帳どおりの位置のみ) |
| 2 | バッチ17本+SORT出口2本+日次/月次 JCL | **GnuCOBOL 全数コンパイル0警告方針**+日次サイクル(D010→D090)を S データで通し実行し期待結果一致。ゴミデータ全件が「MF想定挙動」で処理されること |
| 3 | CICS オンライン23本+BMS 20面+CSD | 静的検証(EXEC CICS 構文・COMMAREA 整合・画面項目対応表)+疑似会話トレース表 |
| 4 | オーソリ相当(RAAUP/RBAUD)+IMS DBD/PSB+Db2 DDL+MQ 電文+ASM 2本 | 判定ロジックは GnuCOBOL ドライバでリプレイ検証(判定コード分布一致)。ASM は HLASM 構文検証+機能等価の COBOL リファレンス実装を併置 |
| 5 | M スケールデータ生成+性能基準測定+README 最終化+採点シート | D030 とリプレイのベースライン計測値を記録。カタログ85パターン全てに埋込確認チェック✓ |

## 品質ゲートの実装(tests/)

1. **コンパイルゲート**: `tests/build_all.sh` — バッチ系を `cobc -std=ibm` 系
   設定でコンパイル。メインフレーム固有(EBCDIC 照合順・NUMPROC 挙動)は
   `-fsign-ebcdic` 等の互換オプション+検証時のみ有効化する分岐で吸収し、
   **ソース自体は IBM Enterprise COBOL 準拠**を維持する。
2. **機能ゲート**: プログラムごとに `tests/cases/<PGM>/` に入力・期待出力を置き、
   実行結果をバイト比較。**罠を踏むデータは「MF 想定挙動」を期待値とする**
   (例: B-02 スペース検針員 → 集計 0 扱いで正常終了、が green 条件)。
3. **移行シミュレーションゲート**(答え合わせ用): 同じ入力に対し「移行後に
   起きがちな挙動」を再現する対照実装(Python)を tools/ に置き、差分が
   dirty_ledger.csv の症状記述と一致することを確認 — 罠が「ちゃんと罠として
   機能する」ことの検証。

## ライセンス・独自性の担保

- CardDemo(Apache 2.0)のソース・コピー句・データ・画面・命名は**不使用**。
  業務ドメイン(電力)、命名体系(RA/RB/RU/RX + 日本語業務略号)、
  レイアウト、ロジックはすべて新規設計。
- 参照するのは公開情報としての「規模・技術範囲・オーソリを含む構成概念」のみ。

## 進捗状況

- [x] Phase 0: 設計文書一式・カタログ85パターン・RUTLDTC(テスト green)
- [x] Phase 1: コピー句12本(主要罠実装)・データ生成ツール・Sスケール
      データ 95,120件(13.9MB)・ゴミデータ台帳3,784行・挑発データ・
      翻訳記録簿・外字コード表・JCL 3本(D-02/03/05/06 実装)
      品質ゲート6項目 green(コピー句サイズ/データ整合/保証/機能/複雑度)
- [ ] Phase 1 残: コピー句拡充(BMS用・COMMAREA各種)、RYOFILE/SMTIN 生成
- [~] Phase 2 第1波: 中核 RBRYO01C 完成(642行, CC=127, MI=0.0,
      TDI=220.8 で複雑度ゲート PASS / 罠 F-01,02,03,05,07 / C-02,06,08 /
      B-03,09,12 / D-01,09 / E-02,03 / I-02,03 / K-03 実装)。
      Sスケール実走 1.0秒/72,000件走査/5,968請求、対照実装(Python)で
      全件・全金額項目一致を確認。品質ゲートは8項目に拡張。
- [x] Phase 2 第2波: RBKEN02C(F-04満了補正181件全一致/B-02・G-08物語/
      I-05 D欄/番兵探索/CC=45,TDI=90.0)、RBSHU01C(B-01完全復号ラダー
      MINUS=55台帳一致/B-12旧口座+4/CC=48,TDI=96.2)、RBTAI01C(D-09
      RC=4正常/I-01日付ローカル複製/K-02/C-03流れ落ち/CC=45,TDI=90.0)。
      3本とも実走+ゴールデン+対照検算(xcheck_wave2)通過。
- [x] Phase 2 第2波残: RBKEN01C(VB受信 H/D/K/T 状態機械/B-05 FD多重01/
      B-06 ODO集計+D欄の48固定版/D-07/CC=50,TDI=90.0)、SORT出口
      RXSRT15C(制御カード駆動の選別・除外254件/CC=20)+RXSRT35C
      (部門付与・例外地区対応/CC=22)、受信VB生成基盤 MKKENIN、
      ドライバ TESTSRT。第2波はこれで完結。
- [x] Phase 2 第3波: 帳票親子 RBSTM01C(D-10宣言部/I-01元号複製5/1境界/
      CC=46)+RBSTM02C(C-01 ALTER×2頁制御/A-11 16進見出し/A-06姓12byte/
      C-07未クリアワーク/外字代替置換/編集子区分新設 CC=30,REFMOD=37)、
      RBGET01C(F-08按分残差7円発現/B-10 COMP-2構成比/F-07/G-07/G-08
      TRUNC(OPT)/E-04注記/CC=58)、RBCNV01C(B-12新旧変換660件等価/CC=51)、
      日次サイクル一気通し run_daily.sh(10ステップ・RC意味論込み)。
      全て実走+ゴールデン+対照検算(xcheck_wave3)通過。
- [x] Phase 2 第4波: RBRYO00C(K-01廃要素高濃度: LABEL RECORDS/
      MEMORY SIZE/セクション優先番号/外税3%現役=RBRYO01Cの封印の原典/
      加算ループ検算NG=0/CC=50)、fossil/RYOKYU68(K-06: NOTE/EXAMINE/
      TRANSFORM/READY TRACE入り・翻訳不能を負のゲートで担保)、
      RBMTR01C(E-01窓割り実走: 元号空白900→S441/H459/CC=49)、
      RBTBL01C(F-03内蔵=正の突合: 一致3種別/旧世代117/CC=45)、
      RBBKU01C(D-04世代DS/D-10宣言部/検査和剰余方式/退避バイト同一/
      CC=52)。全てゴールデン+対照検算(xcheck_wave4)通過。
- [x] Phase 2 第5波 — **バッチ層完結**: RBRYO02C(B-11 MOVE
      CORRESPONDING×2の本丸/A-10予備域二重利用ライブ: 大口203=PACKED・
      小口5,765=文字/B-08下4桁仕様+減算ループ検算/J-02媒体20桁切詰/
      CC=45)、RBJYU01C(B-11第2サイト/異動30件全適用/CC=53)、
      RBKYK01C(E-02センチネル三様解釈: 5,749/125/124/B-03第2サイト
      120件/B-12判別分岐/CC=55)。対照検算 xcheck_wave5 全OK。
      バッチCOBOL 19本+化石1本+SORT出口2本+共通サブ1本 = 全て
      実走・ゴールデン・複雑度ゲート通過。
- [~] Phase 3 着手 — **CICS疑似実行基盤が完成・フロー動作確認済**:
      tools/cicsprep.py(CICS変換器の模擬: EXEC CICS→RXCICSTBスタブ
      CALL展開・DFHEIBLK注入・PROC DIVISION USING書換え)、
      tests/RXCICSTB.cbl(ミニCICS: SEND/RECEIVE MAP・RETURN TRANSID
      擬似会話・XCTL・READ・ASKTIME・ABEND、端末スクリプトCICSIN.txt
      駆動、セッションログCICSOUT.txt、非印字バイトのダンプ風置換)、
      tests/TESTCICS.cbl(疑似端末ドライバ: TRANSID再入とXCTL即時遷移)。
      RASGN00C(サインオン: K-02 2桁年ACCEPT — ログに「26.07.02」が
      実際に出る/内蔵8ユーザ表/試行制限)+RAMEN01C(メニュー:
      A-11 16進見出しがダンプ風に可視/G-01 EIBCALEN=120旧経路)+
      BMS 2面(RSGN00M/RMEN01M マクロ資産+シンボリックマップcpy)。
      実走: 誤PW→エラー→正PW→XCTL→メニュー→選択1→XCTL RAKYK01C
      (未実装検出)の8イベント完走。
      RA*ゲート充足済(RASGN00C CC=38/TDI=71.2: 端末許可・PW規則・
      予約ID検査、RAMEN01C CC=46/TDI=70.4: COMMAREA検証・権限表・
      保守メニュー可否)。build_all統合済(変換->翻訳->実走->
      2桁年マスクdiffの4段ゲート)。
      RAKYK01C 完成(CC=45/TDI=73.0): READ DATASET+RESP(NOTFND)、
      B-12旧レイアウト表示分岐がセッションログで可視(59,463円=新
      PACKED / 32,483円KY=旧ゾーン+KYU-LAYOUT表示)、検査数字検算の
      第4複製(I-01)、結果整合検査・状態別メッセージ。疑似会話は
      16イベント(サインオン->メニュー->照会4パターン: 新/旧/
      NOTFND/検査数字NG)。RCKYKMAP+RKYK01M.bms追加。
      開発中スタブ自身のC-08(段落フォールスルー)を踏み修正 —
      罠台帳の実演事例として記録。
      RAKYK02C 完成 — **専用ゲートPASS(CC=146/GOTO=80/REFMOD=41/
      TDI=200.4/606行)**: 3画面疑似会話(キー->変更->確認)+REWRITE、
      スタブは更新ジャーナルCICSUPD.dat方式(KYKMAST.dat不変を
      xcheck_cicsでsha検証)。実装トラップ: G-01(EIBCALEN=0/120/200
      三分岐)/K-03(旧H07検証とH26新検証の並走・不一致時は旧優先)/
      K-05(H07ピリオド様式とH26 END-IF様式の節単位混在)/B-12連動
      (旧レイアウトは画面更新不可)/G-07(初期値なし更新通番)。
      業務内容: 権限表・二重更新防止・種別別規則4本・終了日整合
      (最終検針YYMMDDの50窓割り比較)・停止/再開規則・大口10万円超
      端末制限(5段ネスト)・楽観再確認・レコード内履歴退避。
      開発中の実地事故2件を記録: 大口検査の種別全面誤発動、
      W-KYK-R写像の8バイトずれ(検証側も同じ誤りで「一致して
      見えた」— 対照検算の独立性の重要性を実証)。
      RAKEN01C(検針照会/CC=50: 簡易版spt検査=検査数字を見ない
      「劣化複製」をコメント宣言/交換月・概算・再検針の状態別警告/
      備考制御文字走査)+RARYO01C(料金照会/CC=55: A-10予備域の
      読み側=大口なら調整額解釈/画面側粗検算 基本+税≦合計/RYO
      正本コピー句参照)完成 — 手書き写像のオフセットずれを2度
      踏み、「正本コピー句をCOPYし写しを作らない」原則へ転換
      (I-01の教訓の自己適用)。72桁あふれ・置換侵食も実地で記録。
      スタブREADを多データセット化(KENFILE/RYOFILE 最新月検索の
      簡易VSAM代替索引)。CSD定義 app/csd/RACSCSD.txt(PROGRAM6/
      TRANSACTION6/MAPSET6/FILE3)+BMS 6面完備。
      疑似会話34イベント: SGN->照会4種->M->更新3画面->M->検針照会
      ->M->料金照会。**Phase 3 完了**(RAJYU01CはB-11バッチ側で
      充足済みのため計画から除外し docs/02 を整合)。
- [x] Phase 4 完了: デマンド監視オーソリ相当 —
      tools/sqlprep.py(Db2プリコンパイラ模擬: INCLUDE SQLCA展開/
      INSERT->RXDB2TBスタブ変換)+RXDB2TB(履歴DB2LOG.dat)+
      RUMQSUB(社内MQラッパ: MQI隠蔽の共通サブという現実的設定、
      GETB/CMIT/CLOS/理由2005/2009/2033/2067/2085/2110/2195、
      CC=33 PASS)。RAAUP00C(受付常駐: 理由コード別処置・接続断
      1回再試行・連続受信検出・予備域走査・応答自己検証・
      SQLCODE -803/-911別処置、CC=38 PASS)+RAAUP01C(判定子:
      **TJ07実証 — 100.00%ジャストはW扱い「取決めNo.31」**、
      時間帯別基準90/95/正午92%、比率逆算検算、契約帯50-2000kW、
      CC=53 PASS)。MQIN 12電文fixture(N3/W4うちJ100=2/C3/E2)、
      xcheck_auth.py(全件再判定一致+TJ07明示+DB2履歴件数)。
      資産: app/ims/DMDDBD.dbd+DMDPSB.psb(HDAM/OSAM、Db2移行後の
      縮退運用注記)、app/ddl/RACSDB2.sql(RACS.DMD_RIREKI、
      取決めNo.31をCOMMENT化)、app/asm/RASMCHK.asm
      (**S58原典: 検査数字mod97の最古層=I-01系譜の根**)+
      RASMKNJ.asm(SO/SI整合検査、H02)。
      実地事故: 固定100バイト読みへの改行混入ズレ(LINE SEQUENTIAL
      化で解決)、fixture値設計ミス1件(80.00%を100.00%と誤認)。
- [x] Phase 5 完了: tools/gen_scale_m.py(検針72万/電文100万、
      seed固定、削除済みNULスロットの下流漏れを意図的に保存)+
      tests/perf_m.sh(退避->計測->復元方式)。実測: 料金計算
      52万件/s、デマンド9.3万電文/s(docs/09)。Mスケール限定の
      発見2件を教材として仕様化: 統計カウンタ9(5)桁あふれ
      (GET=00000)、NULスロット由来0.033%エラー電文と「エラー応答
      がエラーで送れない」(PUTNG=333)。GitHub体裁: README英日/
      DISCLAIMER/LICENSE(Apache-2.0本文)/.gitattributes/
      .github/workflows/ci.yml(51ゲート+縮小Mスモーク)/
      docs/09性能基準/docs/10採点シート(100点法、合格70)。
      **全5フェーズ完了 — v1.0** (公開名: RaichoDemo)
- [x] v1.1: **IMS/DB を計器資産の正本へ昇格** — MTRDBD (計器ルート
      KEIKISEG + 取付履歴ツイン TRTUSEG、乗率と和暦検定満期は IMS
      にのみ存在)。tests/RXIMSTB.cbl (CBLTDLI スタブ: GU/GN/GNP/
      ISRT/REPL、修飾SSA、GE/GB/GP/DJ)、tools/gen_mtrdb.py (unload
      形式・MTRMAST から導出=正本関係の成立)、RBMTR00C (S58系譜の
      抽出: DL/I 全走査+現行ツイン合成 -> MTRMAST **バイト等価**
      再生成、整合検査群、CC=55/TDI=96 PASS)。日次サイクルは D005
      先頭組込みで 11 ステップ。xcheck_ims.py (Python 独立再導出
      byte-equal + 階層規則: 現行丁度1・取付日昇順・期間連鎖)。
      実地事故 3 件を記録: gen_mtrdb の R セグ切出しオフセットずれ
      (通算4度目 -> RBMTR01C golden が汚染を検出)、OPEN OUTPUT に
      よる正本 MTRMAST 破壊と誤復元の連鎖 (seed 固定 gen_data.py
      による正規復旧が機能)、スタブ自身の C-08 段落フォールスルー
      **2 度目** (GO TO 範囲外脱出 -> ISRT-R 末尾の MOVE SPACES が
      GE を上書きし無限ループ 44M 行)。
      [残] 計器交換業務 RBMTR03C (ISRT/REPL 更新系)、CICS 計器照会
- [x] P1 仮想メインフレーム第1段: **JES2 模擬 (tools/jes.py)** —
      本物の JCL を解釈実行 (JOB/EXEC PGM=,PARM=,COND=/DD DSN=,
      DISP=,SYSOUT=*,DUMMY,インライン SYSIN/継続行)。MVS カタログ
      模擬 (tools/catalog.json: DSN->実体パス、VSAM/PS/PDS 種別)、
      LNKLST 表 (PGM->実行体: DFSRRC00->IMS バッチ、SORT->E15/E35、
      IDCAMS->AMS ビルトイン解釈)。スプール spool/<JOB>/ に
      JESMSGLG (IEF403I/237I/142I/202I/404I/$HASP395) と各ステップ
      SYSPRINT を捕捉。日次 11 ステップを新造 RJDAILY.jcl で投入し
      MAXCC=0004 (D070 滞納あり=正常/D-04)、D030 SYSPRINT に
      GOKEI=+72,053,623 を捕捉 (業務結果は従来経路と同一)。
      COND 旧様式の意味論 (D-03) を tests/RJCOND.jcl で実行実証
      (RC=4 の次の COND=(4,LE) が IEF202I でスキップ)。
      制約明記: DD 割当はカタログ検査とログ (実 I/O はプログラム内
      割当 — 電算部標準 7.2 相当の注記)。
- [x] P2 完了: **3270 端末エミュレータ (tools/term3270.py)** —
      本物の BMS マクロ 6 面を解析 (DFHMDI/DFHMDF、POS/LENGTH/
      ATTRB/INITIAL、72桁継続と末尾X剥ぎの両対応) して 24x80 を
      ANSI 描画 (BRT=強調/DRK=伏字/UNPROT=下線入力域)。CICS スタブ
      に端末モード追加 (RACS_TERM=1): SEND=画面スナップ+READY フラグ
      を都度 open/close (確実フラッシュ)、RECEIVE=INPUT.dat を
      CBL_GC_NANOSLEEP ポーリング。シンボリックマップ対応表で
      入出力フィールド<->画面位置を結線。対話モードとスクリプト
      モード (--script/--snap)。端末ゲート: 台本 11 画面完走、
      TOZAI 見出し/照会 59,463/KOSHIN KANRYO/GO-RIYOU を画面上で
      確認、端末対話由来の REWRITE がジャーナル到達。端末モード
      OFF では既存 34 イベント golden 完全無傷。
      実地事故: 端末起動時クリーンアップが先着 READY.flg を削除し
      デッドロック / sh の & 結合で rm~mkdir ごと背景化 (3敗) /
      pkill -f が自分のシェルを殺す — すべて docs 行きの教訓。
- [x] P3 完了: **GTF 統合トレース+文レベル相関デバッグ** —
      4 スタブ (CICS/DLI/DB2/MQ) に GTF 計装 (RACS_GTF=1、
      01 GTF-SEQ EXTERNAL でプロセス共有通番、各層別 .trc、
      WRITE 前サニタイズ)。tools/gtf_view.py が cobc -ftraceall の
      文トレースと突合: 単一スレッドの順序保存を相関原理とし、
      CALL 文の呼び先は Line 番号で生成ソース原文を引いて判定、
      スタブ CALL の直下に GTF を注釈。tests/debug_run.sh で
      cics/auth/ims をワンコマンド化。実証: cics=GTF 102/102、
      auth=32/32 (MQ GET/PUT+SQL INSERT)、ims=**25,983/25,983**
      (DL/I 全呼出しが GNP->STAT 判定文まで一続き)。既定表示は
      スタブ/ドライバ内部を折畳み、--prog で 1 本に絞込み可。
      実地事故: GTF レコードに COMP バイナリ (CP-RESP) を STRING
      して ST=71 行落ち — CICSOUT で踏んだ制御文字混入の**再演**
      (表示項目化+サニタイズで解決)。OPEN EXTEND 新規 35 の
      フォールバックも整備。
      **仮想メインフレーム P1-P3 完成: JES + 3270 + デバッグ相関**
- [ ] Phase 4: オーソリ相当(MQ/IMS/Db2)+ASM / Phase 5: Mスケール+
      性能基準+GitHub体裁(README/LICENSE/CI)
- [ ] Phase 2 第3波: 残バッチ+日次サイクル通し実行
- [ ] Phase 3: CICS 23本+BMS / Phase 4: オーソリ+IMS/Db2/ASM /
      Phase 5: Mスケール+性能基準+最終README
