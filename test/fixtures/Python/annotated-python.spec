# These package fields are used to label the executable.
Name: str = "metrics-agent"
Version: str = "2.4"
Release: int = 1

# %description belongs in the RPM package, not this PyInstaller build.
a = Analysis(["metrics_agent.py"], datas=[])
pyz = PYZ(a.pure)
exe = EXE(pyz, a.scripts, name=Name, console=True)
