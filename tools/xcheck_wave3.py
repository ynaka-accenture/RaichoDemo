#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""xcheck_wave3.py — RBSTM01/02, RBGET01C(F-08按分), RBCNV01C の対照検算"""
import sys, os
P = os.path.join(os.path.dirname(__file__), '..', 'app', 'data', 'portable')

def unpack(b):
    h = b.hex(); v = int(h[:-1])
    return -v if h[-1] in 'db' else v

def main():
    ok = True
    # ---- RBGET01C: 按分の独立再計算 (F-08) ----
    ryo = open(f'{P}/RYOFILE.dat', 'rb').read()
    ken_by = {}
    for r in range(0, len(ryo), 256):
        c = int(ryo[r+2:r+4]); ken_by[c] = ken_by.get(c, 0) + 1
    total = sum(ken_by.values())
    exp = {c: 100000 * n // total for c, n in ken_by.items()}
    zan = 100000 - sum(exp.values())
    gs = open(f'{P}/GETSUM.dat', 'rb').read()
    got, got_zan = {}, None
    for r in range(0, len(gs), 80):
        b = gs[r:r+80]; c = int(b[0:2])
        w = unpack(b[20:24])
        if c == 99: got_zan = w
        else: got[c] = w
    ok1 = got == exp and got_zan == zan
    ok &= ok1
    print(f'XCHECK GET: 地区数={len(got)} 按分一致={got == exp} '
          f'残差={got_zan}(期待{zan}) -> {"OK" if ok1 else "NG"}')
    # ---- RBSTM01/02: 行数・頁数・合計行 ----
    st = open(f'{P}/STMLST.dat', 'rb').read()
    lines = len(st) // 132
    heads = sum(1 for r in range(0, len(st), 132)
                if st[r+39:r+40] == b'\x0e')
    gokei = sum(1 for r in range(0, len(st), 132)
                if st[r:r+13] == b'*** GOKEI ***')
    exp_lines = total + (total + 59) // 60 + 1
    ok2 = lines == exp_lines and heads == (total + 59) // 60 \
        and gokei == 1
    ok &= ok2
    print(f'XCHECK STM: LINES={lines}(期待{exp_lines}) 頁={heads} '
          f'合計行={gokei} -> {"OK" if ok2 else "NG"}')
    # ---- RBCNV01C: 旧->新変換のフィールド等価 ----
    old = open(f'{P}/KYKMAST.dat', 'rb').read()
    new = open(f'{P}/KYKNEW.dat', 'rb').read()
    ng = 0
    for r in range(0, len(old), 320):
        o, n = old[r:r+320], new[r:r+320]
        kaisi = int(o[42:50])
        if kaisi >= 19930101:
            if o != n: ng += 1
            continue
        if unpack(n[59:64]) != int(o[59:68]): ng += 1
        if n[64:79] != o[68:83]: ng += 1          # 振替域
        if n[79:109] != o[83:113]: ng += 1        # カナ
        if n[109:121] != o[113:125]: ng += 1      # 検針/指示
    ok3 = ng == 0
    ok &= ok3
    print(f'XCHECK CNV: 等価NG={ng} -> {"OK" if ok3 else "NG"}')
    return 0 if ok else 1

if __name__ == '__main__':
    sys.exit(main())
