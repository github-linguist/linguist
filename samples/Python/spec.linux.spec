a = Analysis(['portablizer.pyqt4.py'],
             hiddenimports=[],
             hookspath=None,
             runtime_hooks=None)
pyz = PYZ(a.pure)
exe = EXE(pyz,
          a.scripts,
          exclude_binaries=True,
          name='Portablizer',
          debug=False,
          strip=None,
          upx=True,
          console=False)
node = Tree('node', prefix='node')
collect = COLLECT(exe,
                  a.binaries,
                  a.zipfiles,
                  a.datas,
                  node,
                  strip=None,
                  upx=True,
                  name='Portablizer')
