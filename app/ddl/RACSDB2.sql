-- -------------------------------------------------------------------
--  RACS Db2 定義                          東西電力 電算部  H24.10
--  デマンド判定履歴 (RAAUP00C が INSERT する)
-- -------------------------------------------------------------------
CREATE TABLESPACE TSDMD01 IN DBRACS
  USING STOGROUP SGRACS PRIQTY 7200 SECQTY 720
  LOCKSIZE PAGE BUFFERPOOL BP1;

CREATE TABLE RACS.DMD_RIREKI
( SPT_NO     CHAR(22)      NOT NULL
, JIKOKU     CHAR(4)       NOT NULL
, HANTEI     CHAR(1)       NOT NULL
, RITU       DECIMAL(5,2)  NOT NULL
, TOROKU_TS  TIMESTAMP     NOT NULL WITH DEFAULT
, PRIMARY KEY (SPT_NO, JIKOKU)
) IN DBRACS.TSDMD01;

CREATE UNIQUE INDEX RACS.XDMD01
  ON RACS.DMD_RIREKI (SPT_NO, JIKOKU)
  USING STOGROUP SGRACS PRIQTY 720;

COMMENT ON TABLE RACS.DMD_RIREKI IS
  'デマンド判定履歴 W/C のみ記録 (N は書かない -- H24 設計)';
-- 注意: HANTEI に格納されるのは W/C のみ.
--       100.00 ちょうどは W (営業部取決め No.31)
