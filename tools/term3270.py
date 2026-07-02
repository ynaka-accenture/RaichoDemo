#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
term3270.py — 3270 端末エミュレータ (RACS オンライン用)
  ・app/bms/*.bms (本物の BMS マクロ) を解析して 24x80 画面を構成
  ・CICS スタブの端末モード (RACS_TERM=1) とファイル握手で対話
      spool/term/SCREEN.dat + READY.flg  <- CICS が画面送信
      spool/term/INPUT.dat               -> 端末が入力送信
  ・属性: BRT=強調 / DRK=非表示 / UNPROT=入力域 (下線表示)
  ・シンボリックマップ対応表: DFHMSD が生成する順序に基づく
    フィールド<->データ域オフセット (RC*MAP.cpy 準拠)

使い方:
  対話:      python3 tools/term3270.py
  スクリプト: python3 tools/term3270.py --script tests/term_script.txt
             (各行=1入力. 'フィールド名=値' を空白区切り, /END で切断)
  画面保存:  --snap spool/term/screens.txt (全画面を追記保存)
"""
import argparse, os, re, sys, time

BASE = os.path.join(os.path.dirname(__file__), '..')
TERM = f'{BASE}/spool/term'

# --- シンボリックマップ対応表 (RC*MAP.cpy の入出力レイアウト準拠) ---
INFLD = {   # map -> [(field, offset0, len)]
    'SGN00M': [('USERID', 0, 8), ('PASSWD', 8, 8)],
    'MEN01M': [('SENTAKU', 0, 1)],
    'KYK01M': [('SPT', 0, 22)],
    'KY201M': [('SPT', 0, 22), ('SYURYO', 22, 8),
               ('TEISI', 30, 1), ('KAKUNIN', 31, 1)],
    'KEN01M': [('SPT', 0, 22)],
    'RYO01M': [('SPT', 0, 22)],
}
OUTFLD = {  # map -> [(BMSフィールド名, offset0, len)]
    'SGN00M': [('USERID', 0, 8), ('PASSWD', 8, 8), ('MSG', 16, 40),
               ('HIDUKE', 56, 10)],
    'MEN01M': [('TITLE', 0, 20), ('USER', 20, 8), ('MSG', 28, 40)],
    'KYK01M': [('SPT', 0, 22), ('JYU', 22, 10), ('SYU', 32, 2),
               ('KAISI', 34, 8), ('KIHON', 42, 9), ('FLG', 51, 1),
               ('MSG', 52, 28)],
    'KY201M': [('SPT', 0, 22), ('SYURYO', 22, 8), ('TEISI', 30, 1),
               ('KYUSYUR', 31, 8), ('KYUTEI', 39, 1), ('MSG', 40, 40)],
    'KEN01M': [('SPT', 0, 22), ('YM', 22, 6), ('BI', 28, 6),
               ('ZEN', 34, 6), ('KON', 40, 6), ('SIYO', 46, 7),
               ('KBN', 53, 1), ('MSG', 54, 26)],
    'RYO01M': [('SPT', 0, 22), ('YM', 22, 6), ('GOKEI', 28, 11),
               ('KIHON', 39, 8), ('ZEI', 47, 8), ('FLG', 55, 1),
               ('MSG', 56, 24)],
}

def parse_bms(path):
    """BMS マクロ -> {mapname: [field]}  field={name,row,col,len,attrs,init}"""
    maps, cur, fields = {}, None, None
    pend = ''
    for raw in open(path, encoding='utf-8'):
        line = raw.rstrip('\n')
        if line.startswith('*'):
            continue
        body = (pend + line[:71].rstrip()).rstrip()
        pend = ''
        if body.endswith(' X'):
            pend = body[:-1].rstrip()
            continue
        if len(line) > 71 and line[71:72].strip() != '':
            pend = body
            continue
        if body.endswith(','):
            pend = body
            continue
        m = re.match(r'(\S*)\s+(DFHMSD|DFHMDI|DFHMDF)\s+(.*)', body)
        if not m:
            continue
        label, macro, ops = m.groups()
        if macro == 'DFHMDI':
            cur, fields = label, []
            maps[cur] = fields
        elif macro == 'DFHMDF' and cur:
            pm = re.search(r'POS=\((\d+),(\d+)\)', ops)
            lm = re.search(r'LENGTH=(\d+)', ops)
            am = re.search(r'ATTRB=\(([A-Z,]+)\)', ops) or \
                 re.search(r'ATTRB=([A-Z]+)', ops)
            im = re.search(r"INITIAL='([^']*)'", ops)
            fields.append({
                'name': label, 'row': int(pm.group(1)),
                'col': int(pm.group(2)), 'len': int(lm.group(1)),
                'attrs': (am.group(1).split(',') if am else []),
                'init': (im.group(1) if im else None)})
    return maps

def load_all_bms():
    allmaps = {}
    for f in sorted(os.listdir(f'{BASE}/app/bms')):
        if f.endswith('.bms'):
            allmaps.update(parse_bms(f'{BASE}/app/bms/{f}'))
    return allmaps

BOLD, DIM, UL, RST = '\033[1m', '\033[2m', '\033[4m', '\033[0m'
GRN = '\033[32m'

def render(mapname, data, bms, use_ansi=True):
    """24x80 画面文字列を構成"""
    grid = [[' '] * 80 for _ in range(24)]
    marks = {}
    fields = bms.get(mapname, [])
    out = {n: (o, l) for n, o, l in OUTFLD.get(mapname, [])}
    for f in fields:
        r, c, ln = f['row'] - 1, f['col'] - 1, f['len']
        text = None
        if f['name'] and f['name'] in out:
            o, l = out[f['name']]
            text = data[o:o + l].ljust(ln)[:ln]
            if 'DRK' in f['attrs'] and text.strip():
                text = '*' * len(text.strip()) + ' ' * (ln - len(text.strip()))
        elif f['init'] is not None:
            text = f['init'].ljust(ln)[:ln]
        if text is None:
            if 'UNPROT' in f['attrs']:
                text = '_' * ln
            else:
                continue
        if 'UNPROT' in f['attrs'] and not text.strip():
            text = '_' * ln
        for i, ch in enumerate(text):
            if c + i < 80:
                grid[r][c + i] = ch
                if use_ansi:
                    if 'BRT' in f['attrs']:
                        marks[(r, c + i)] = BOLD
                    elif 'UNPROT' in f['attrs']:
                        marks[(r, c + i)] = UL + GRN
    lines = []
    top = '+' + '-' * 80 + '+'
    lines.append(top)
    for r in range(24):
        row = ''
        for c in range(80):
            ch = grid[r][c]
            mk = marks.get((r, c))
            row += (mk + ch + RST) if (mk and use_ansi) else ch
        lines.append('|' + row + '|')
    lines.append(top)
    return '\n'.join(lines)

def wait_screen(timeout=30):
    t0 = time.time()
    while time.time() - t0 < timeout:
        if os.path.exists(f'{TERM}/READY.flg'):
            lines = open(f'{TERM}/SCREEN.dat').read().split('\n')
            os.remove(f'{TERM}/READY.flg')
            head = lines[0]
            m = re.search(r'MAP=(\S+)\s*/\s*(\S+)', head.replace(' ', ''))
            mm = re.match(r'MAP=(\S+)/(\S+)\sSEQ', head)
            mapname = (mm.group(2) if mm else head[4:20].split('/')[1].split()[0]).strip()
            data = lines[1].ljust(80) if len(lines) > 1 else ' ' * 80
            return mapname, data
        time.sleep(0.1)
    return None, None

def compose_input(mapname, kv):
    """{field:value} -> 80桁入力行"""
    line = [' '] * 80
    for name, off, ln in INFLD.get(mapname, []):
        v = kv.get(name, '')
        for i, ch in enumerate(v[:ln]):
            line[off + i] = ch
    return ''.join(line).rstrip() or ' '

def send_input(text):
    tmp = f'{TERM}/INPUT.tmp'
    open(tmp, 'w').write(text + '\n')
    os.replace(tmp, f'{TERM}/INPUT.dat')

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--script')
    ap.add_argument('--snap')
    ap.add_argument('--no-ansi', action='store_true')
    a = ap.parse_args()
    os.makedirs(TERM, exist_ok=True)
    p = f'{TERM}/INPUT.dat'
    if os.path.exists(p):
        os.remove(p)
    bms = load_all_bms()
    script = None
    if a.script:
        script = [l.rstrip('\n') for l in open(a.script)
                  if l.strip() and not l.startswith('#')]
    snap = open(a.snap, 'w') if a.snap else None
    use_ansi = (not a.no_ansi) and (script is None) and sys.stdout.isatty()
    n = 0
    print('RACS 3270 -- セッション待機 (RACS_TERM=1 で CICS を起動)')
    while True:
        mapname, data = wait_screen()
        if mapname is None:
            print('** 画面待ちタイムアウト / セッション終了 **')
            break
        n += 1
        scr = render(mapname, data, bms, use_ansi)
        plain = render(mapname, data, bms, False)
        if snap:
            snap.write(f'===== SCREEN {n} MAP={mapname} =====\n'
                       + plain + '\n')
            snap.flush()
        print(f'\n----- 画面 {n}: {mapname} -----')
        print(scr if use_ansi else plain)
        infields = INFLD.get(mapname, [])
        kv = {}
        if script is not None:
            if not script:
                send_input('/END')
                mapname, data = wait_screen(5)
                break
            entry = script.pop(0)
            if entry == '/END':
                send_input('/END')
                break
            for tok in entry.split():
                if '=' in tok:
                    k, v = tok.split('=', 1)
                    kv[k] = v
            print(f'>> 入力: {entry}')
        else:
            print('(値を入力. 空=そのまま送信, /END=切断)')
            try:
                for name, off, ln in infields:
                    v = input(f'  {name} ({ln}桁): ')
                    if v == '/END':
                        send_input('/END')
                        return
                    kv[name] = v
            except (EOFError, KeyboardInterrupt):
                send_input('/END')
                return
        send_input(compose_input(mapname, kv))
    if snap:
        snap.close()

if __name__ == '__main__':
    main()
