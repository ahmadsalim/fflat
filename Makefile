FSLEX := $(wildcard packages/FsLexYacc.*/bin/fslex.exe)
FSYACC := $(wildcard packages/FsLexYacc.*/bin/fsyacc.exe)
FSLEXYACCRUNTIMEPATH := $(wildcard packages/FsLexYacc.Runtime.*/lib/net40)
FSLEXYACCRUNTIME := $(FSLEXYACCRUNTIMEPATH)/FsLexYacc.Runtime.dll

FSC := fsharpc
FSI := fsharpi
FSLEX := mono $(FSLEX)
FSYACC := mono $(FSYACC)
RM := rm -rf

OUTPUTS = FbLex.fs FbPar.fs FbLang.dll

build: $(OUTPUTS)

FbLex.fs: FbLex.fsl
	$(FSLEX) --unicode $<

FbPar.fs: FbPar.fsy
	$(FSYACC) --module $(basename $<) $<

FbLang.dll: LazyList.fs FbAst.fs FbPar.fs FbLex.fs FbCheck.fs FbType.fs FbEval.fs FbInterpret.fs
	$(FSC) -r $(FSLEXYACCRUNTIME) $^ --target:library -o:$@

run: FbLang.dll
	$(FSI) -r:$(FSLEXYACCRUNTIME) --lib:$(FSLEXYACCRUNTIMEPATH) -r $^

.PHONY:
clean:
	$(RM) $(OUTPUTS)
