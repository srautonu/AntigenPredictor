@echo off
rem for /f %%f in ('dir /b %1/*.fasta') do type %%f >> %1.fasta
for %%f in (%1\*.fasta) do (
	type %%f >> %1.fasta
	echo[ >> %1.fasta
)