// Copyright (c) 2010-2026 Zhang Weidong <zwd@funlang.org>
// SPDX-License-Identifier: MIT

use "lib-cwinapi.fun";
use "lib-cstruct.fun";
use 'lib-time.fun';
use 'lib-os.fun';
var tk = tick();

var getLen  = CWin32API('user32.dll', 'int GetWindowTextLength(HWND hWnd);');
var getText = CWin32API('user32.dll', 'int GetWindowText(HWND hWnd, LPTSTR lpString, int nMaxCount);');
var enumWin = CWin32API('user32.dll', 'BOOL EnumWindows(BOOL CALLBACK (HWND, LPARAM) lpEnumFunc, LPARAM lParam);');
var showWin = 'user32'.getapi('ShowWindow', 'ii:i');

var cmds;
enumWin.args.lpEnumFunc = enumProc;
enumWin.args.lParam = 0;
loop
  cmds = new [];
  enumWin.call();
  var ii = 0;
  for i = 0 to 9 loop
    tk.get();
    sleep(100); //超时未醒, 系统慢
    if tk.get() > 150 and cmds.@count() > 0 then
      ii += 1;
      if ii > 5 then //放过黑窗切换
        for k: v in cmds do
          showWin(k * 1, 2); //最小化之
        end do;
        exit;
      end if;
    end if;
  end loop;
end loop;

fun enumProc(hWnd, lParam)
  getLen.args.hWnd = hWnd;
  var len = getLen.call();
  if len <= 0 then return 1; end if;

  var buf = 0.toChar().x(len + 1);
  getText.args.hWnd = hWnd;
  getText.args.lpString = buf;
  getText.args.nMaxCount = len + 1;
  var l = getText.call();
  var title = getText.args.lpString.substr(len: l); //?. title;
  if title =~ /\bcmd\.exe\b/i then
    cmds.[hWnd] = title;
  end if;
  return 1;
end fun;
