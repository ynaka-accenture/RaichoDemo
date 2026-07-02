#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""xcheck_auth.py — デマンド判定の対照検算 (TJ07 境界含む)"""
import sys, os
P = os.path.join(os.path.dirname(__file__), '..', 'app', 'data', 'portable')

def judge(rec):
    if rec[0:2] != 'D1': return 'E', 0.0
    spt, jk, val, key = rec[2:24], rec[24:28], rec[28:33], rec[33:37]
    if spt[0:2] != '03' or not spt[2:4].isdigit() \
       or not ('01' <= spt[2:4] <= '47') or not spt[4:20].isdigit() \
       or not spt[20:22].isdigit():
        return 'E', 0.0
    if not (jk.isdigit() and jk[0:2] <= '23' and jk[2:4] in ('00', '30')):
        return 'E', 0.0
    if not val.isdigit() or val == '00000' or not key.isdigit() \
       or int(key) == 0:
        return 'E', 0.0
    k = int(key)
    if k < 50 or k > 2000: return 'E', 0.0
    demand = int(val) / 5.0
    if demand > 9999: return 'C', 999.99
    ritu = round(demand * 100 / k + 1e-9, 2)
    if ritu > 999.99: return 'C', 999.99
    kijun = 90.0
    hh = int(jk[0:2])
    if hh < 6 or hh > 21: kijun = 95.0
    if hh == 12 and jk[2:4] == '00': kijun = 92.0
    if ritu > 100.0: return 'C', ritu
    if ritu >= kijun: return 'W', ritu   # 100.00 ちょうどは W (TJ07)
    return 'N', ritu

def main():
    ok = True
    src = [l.rstrip('\n').ljust(100)
           for l in open(f'{P}/MQIN.dat')]
    out = [l.rstrip('\n') for l in open(f'{P}/MQOUT.dat')]
    j100 = 0
    for i, rec in enumerate(src):
        h, r = judge(rec)
        o = out[i]
        oh, orv = o[28], float(o[29:35])
        if h == 'W' and abs(r - 100.0) < 1e-9: j100 += 1
        if oh != h or abs(orv - (r if h != 'E' else 0.0)) > 0.005:
            print(f'XCHECK AUTH NG line {i+1}: {oh}/{orv} != {h}/{r}')
            ok = False
    n2 = sum(1 for l in open(f'{P}/DB2LOG.dat'))
    expect_db2 = sum(1 for rec in src if judge(rec)[0] in ('W', 'C'))
    if n2 != expect_db2:
        print(f'XCHECK AUTH NG DB2LOG {n2} != {expect_db2}')
        ok = False
    print(f'XCHECK AUTH: 12電文全件再判定一致 / TJ07境界 {j100} 件が'
          f' W 側 / DB2LOG {n2} 行 -> {"OK" if ok else "NG"}')
    return 0 if ok else 1

if __name__ == '__main__':
    sys.exit(main())
