@echo off
for %%F in (*.R) do (
echo Running %%F
Rscript "%%F"
if errorlevel 1 (
echo Error running %%F
pause 
)
)

PAUSE
