#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""xcheck_wave5.py — RBRYO02C(媒体), RBJYU01C(異動), RBKYK01C(棚卸)の検算"""
import sys, os
P = os.path.join(os.path.dirname(__file__), '..', 'app', 'data', 'portable')

def unpack(b):
    h = b.hex(); v = int(h[:-1])
    return -v if h[-1] in 'db' else v

def main():
    ok = True
    # ---- RBRYO02C: 媒体全件再計算 (CORR転記/POINT/予備二重利用) ----
    ryo = open(f'{P}/RYOFILE.dat', 'rb').read()
    kyk = open(f'{P}/KYKMAST.dat', 'rb').read()
    sei = open(f'{P}/SEIKYU.dat', 'rb').read()
    koza = {}
    for r in range(0, len(kyk), 320):
        b = kyk[r:r+320]
        off = 4 if int(b[42:50]) < 19930101 else 0
        koza[b[0:22]] = b[64+off:79+off]
    ng = 0
    for i in range(len(ryo) // 256):
        b = ryo[i*256:(i+1)*256]; s = sei[i*120:(i+1)*120]
        gokei = unpack(b[65:70])
        if s[16:36] != b[0:20]: ng += 1            # J-02: 20桁切詰
        if s[1:16] != koza[b[0:22]]: ng += 1       # CORR 口座
        if int(s[42:51]) != gokei: ng += 1         # CORR 金額
        if int(s[57:61]) != (gokei * 3) % 10000:   # B-08 下4桁
            ng += 1
        yobi = s[61:69]
        if gokei > 20000:
            if unpack(yobi[:8]) != int(gokei * 0.05 * 100): ng += 1
        else:
            if yobi != b'MAIL OK ': ng += 1        # A-10 二重利用
    ok1 = ng == 0 and len(sei) // 120 == len(ryo) // 256
    ok &= ok1
    print(f'XCHECK SEI: RECS={len(sei)//120} NG={ng} '
          f'-> {"OK" if ok1 else "NG"}')
    # ---- RBJYU01C: 差分は異動30件のフィールドのみ ----
    j1 = open(f'{P}/JYUMAST.dat', 'rb').read()
    j2 = open(f'{P}/JYUMST2.dat', 'rb').read()
    ido = {int(open(f'{P}/IDOIN.dat','rb').read()[r:r+10]):
           open(f'{P}/IDOIN.dat','rb').read()[r:r+40]
           for r in range(0, 1200, 40)}
    diff = apply_ok = 0
    for r in range(0, len(j1), 256):
        a, b = j1[r:r+256], j2[r:r+256]
        jno = int(a[0:10])
        if a == b:
            if jno in ido: diff += 1  # 適用漏れ
            continue
        if jno not in ido:
            diff += 1; continue
        w = ido[jno]
        if b[130:141] == w[10:21] and b[161:162] == w[21:22]:
            apply_ok += 1
        else:
            diff += 1
    ok2 = diff == 0 and apply_ok == 30
    ok &= ok2
    print(f'XCHECK JYU: 適用一致={apply_ok}/30 不整合={diff} '
          f'-> {"OK" if ok2 else "NG"}')
    # ---- RBKYK01C: 停止0件 -> バイト同一 ----
    k2 = open(f'{P}/KYKMST2.dat', 'rb').read()
    ok3 = k2 == kyk
    ok &= ok3
    print(f'XCHECK KYK: 出力同一={ok3} -> {"OK" if ok3 else "NG"}')
    return 0 if ok else 1

if __name__ == '__main__':
    sys.exit(main())
