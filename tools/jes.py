#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
jes.py — JES2 模擬: JCL を解釈しジョブを実行する
  対応: JOB / EXEC PGM=,PARM=,COND= / DD DSN=,DISP=,SYSOUT=*,DUMMY,
        DD * (インライン SYSIN) / 継続行 / コメント //*
  出力: spool/<JOBNAME>/JESMSGLG (IEF メッセージ風ジョブログ)
        spool/<JOBNAME>/<STEP>.SYSPRINT (各ステップの標準出力捕捉)
  意味論: COND=(n,OP) 旧様式 = 「n OP 前段最大RC が真ならスキップ」
  制約: 模擬環境の DD 割当はカタログ検査とログのみ (実 I/O は
        プログラム内割当). DSN はカタログ (tools/catalog.json) で
        実体パスへ解決し存在検査する — 電算部標準 7.2 相当
使い方: python3 tools/jes.py app/jcl/RJDAILY.jcl
"""
import json, os, re, subprocess, sys, time

BASE = os.path.join(os.path.dirname(__file__), '..')
CATALOG = json.load(open(f'{BASE}/tools/catalog.json'))
BIN = f'{BASE}/tests/bin'

# PGM 名 -> 実行体 (LNKLST 相当)
PGMTBL = {
    'DFSRRC00': 'testims',      # IMS バッチ域制御
    'SORT':     'testsrt',      # DFSORT (E15/E35 MODS 込み)
    'IDCAMS':   '@IDCAMS',      # AMS はビルトイン解釈
    'MKKENIN':  'mkkenin',
    'RBKEN01C': 'rbken01c', 'RBKEN02C': 'rbken02c',
    'RBRYO01C': 'rbryo01c', 'RBSTM01C': 'rbstm01',
    'RBSHU01C': 'rbshu01c', 'RBTAI01C': 'rbtai01c',
    'RBGET01C': 'rbget01c', 'RBCNV01C': 'rbcnv01c',
}
COND_OP = {'GT': lambda n, rc: n > rc, 'GE': lambda n, rc: n >= rc,
           'EQ': lambda n, rc: n == rc, 'NE': lambda n, rc: n != rc,
           'LT': lambda n, rc: n < rc, 'LE': lambda n, rc: n <= rc}

def parse(path):
    """JCL -> [(kind, name, params, inline)] kind=JOB/EXEC/DD"""
    stmts, cur, inline, in_inline = [], None, None, False
    for raw in open(path, encoding='utf-8'):
        line = raw.rstrip('\n')
        if in_inline:
            if line.startswith('/*'):
                in_inline = False
            else:
                inline.append(line)
            continue
        if line.startswith('//*') or line.strip() == '':
            continue
        if line == '//':
            break
        body = line[2:]
        if body[:1] == ' ':               # 継続行
            cur[2] += body.strip()
            continue
        m = re.match(r'(\S+)\s+(\S+)\s*(.*)', body)
        if not m:
            continue
        name, verb, rest = m.groups()
        if verb == 'JOB':
            cur = ['JOB', name, rest, None]
        elif verb == 'EXEC':
            cur = ['EXEC', name, rest, None]
        elif verb == 'DD':
            if rest.strip() == '*':
                inline = []
                cur = ['DD', name, '*', inline]
                in_inline = True
            else:
                cur = ['DD', name, rest, None]
        stmts.append(cur)
    return stmts

def split_params(s):
    """カンマ区切り (括弧・引用内は保護)"""
    out, buf, depth, q = [], '', 0, False
    for ch in s:
        if ch == "'" and not q: q = True
        elif ch == "'" and q: q = False
        if not q:
            if ch == '(': depth += 1
            if ch == ')': depth -= 1
            if ch == ',' and depth == 0:
                out.append(buf); buf = ''
                continue
        buf += ch
    if buf: out.append(buf)
    return out

def run_idcams(sysin, log):
    rc = 0
    for stmt in re.findall(r'DEFINE\s+CLUSTER\s*\(\s*NAME\((\S+?)\)',
                           ' '.join(sysin)):
        ent = CATALOG.get(stmt)
        if ent and os.path.exists(f'{BASE}/{ent["path"]}'):
            log.append(f'IDC0508I DATA ALLOCATION STATUS FOR '
                       f'{stmt} IS 0 (既存実体あり)')
        else:
            log.append(f'IDC0512I NAME {stmt} カタログ実体なし (定義のみ)')
            rc = max(rc, 0)
    log.append(f'IDC0002I IDCAMS PROCESSING COMPLETE. '
               f'MAX CONDITION CODE {rc}')
    return rc

def main(jcl_path):
    stmts = parse(jcl_path)
    jobname = next(s[1] for s in stmts if s[0] == 'JOB')
    spool = f'{BASE}/spool/{jobname}'
    os.makedirs(spool, exist_ok=True)
    log = [f'1                   J E S 2  J O B  L O G',
           f' IEF403I {jobname} - STARTED - TIME={time.strftime("%H.%M.%S")}']
    maxcc, prevmax, skipped = 0, 0, 0
    step = None
    steps = []
    for s in stmts:
        if s[0] == 'EXEC':
            steps.append({'name': s[1], 'params': s[2], 'dds': []})
        elif s[0] == 'DD' and steps:
            steps[-1]['dds'].append(s)
    for st in steps:
        p = {k: v for k, v in
             (kv.split('=', 1) for kv in split_params(st['params'])
              if '=' in kv)}
        pgm = p.get('PGM', '?')
        parm = p.get('PARM', '').strip("'")
        # COND 判定 (旧様式)
        if 'COND' in p:
            m = re.match(r'\((\d+),(\w+)\)', p['COND'])
            n, op = int(m.group(1)), m.group(2)
            if COND_OP[op](n, prevmax):
                log.append(f' IEF202I {jobname} {st["name"]} - STEP WAS '
                           f'NOT RUN BECAUSE OF CONDITION CODES '
                           f'COND=({n},{op}) 前段MAXCC={prevmax}')
                skipped += 1
                continue
        # DD 割当 (カタログ検査)
        sysin = None
        for _, dd, rest, inline in st['dds']:
            if rest == '*':
                sysin = inline
                log.append(f' IEF237I JES2 ALLOCATED TO {dd} (インライン)')
                continue
            dp = {k: v for k, v in
                  (kv.split('=', 1) for kv in split_params(rest)
                   if '=' in kv)}
            if 'DSN' in dp:
                dsn = dp['DSN']
                ent = CATALOG.get(dsn)
                disp = dp.get('DISP', '(NEW)').strip('()').split(',')[0]
                if ent is None:
                    log.append(f' IEF212I {jobname} {st["name"]} {dd} - '
                               f'DATA SET NOT FOUND: {dsn} (未カタログ)')
                    if disp in ('SHR', 'OLD'):
                        log.append(f' IEF272I {jobname} {st["name"]} - '
                                   f'STEP WAS NOT EXECUTED.')
                        maxcc = max(maxcc, 12)
                        prevmax = 12
                        break
                else:
                    ex = os.path.exists(f'{BASE}/{ent["path"]}')
                    if disp in ('SHR', 'OLD') and not ex:
                        log.append(f' IEF212I {jobname} {st["name"]} {dd}'
                                   f' - {dsn} 実体なし')
                        maxcc = max(maxcc, 12); prevmax = 12
                        break
                    log.append(f' IEF237I {ent["type"]:4} ALLOCATED TO '
                               f'{dd:8} DSN={dsn}')
            elif 'SYSOUT' in dp:
                log.append(f' IEF237I JES2 ALLOCATED TO {dd} (SYSOUT)')
            elif rest.startswith('DUMMY'):
                log.append(f' IEF237I DMY  ALLOCATED TO {dd} (DUMMY)')
        else:
            # 実行
            if pgm == 'IDCAMS':
                sp = []
                rc = run_idcams(sysin or [], sp)
                out = '\n'.join(sp) + '\n'
            else:
                exe = PGMTBL.get(pgm)
                if exe is None or not os.path.exists(f'{BIN}/{exe}'):
                    log.append(f' IEF453I {jobname} - JOB FAILED - '
                               f'JCL ERROR (PGM={pgm} NOT IN LNKLST)')
                    maxcc = 16
                    break
                args = [f'{BIN}/{exe}'] + ([parm] if parm else [])
                r = subprocess.run(args, capture_output=True, text=True,
                                   cwd=BASE)
                rc, out = r.returncode, r.stdout + r.stderr
            open(f'{spool}/{st["name"]}.SYSPRINT', 'w').write(out)
            log.append(f' IEF142I {jobname} {st["name"]} - STEP WAS '
                       f'EXECUTED - COND CODE {rc:04d}')
            prevmax = max(prevmax, rc)
            maxcc = max(maxcc, rc)
            continue
        break
    log.append(f' IEF404I {jobname} - ENDED - TIME='
               f'{time.strftime("%H.%M.%S")}')
    log.append(f' $HASP395 {jobname:8} ENDED - RC={maxcc:04d}'
               f' (STEPS={len(steps)} SKIP={skipped})')
    open(f'{spool}/JESMSGLG', 'w').write('\n'.join(log) + '\n')
    print('\n'.join(log))
    return maxcc

if __name__ == '__main__':
    sys.exit(0 if main(sys.argv[1]) <= 4 else 1)
