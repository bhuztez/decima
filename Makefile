all: x.elf

%.elf: %.bc asm/main.bc asm/write.bc
	llvm-link $< asm/write.bc | opt -Os | llvm-link -internalize asm/main.bc - | opt -Os | llc | gcc -o $@ -x assembler -

%.bc: %.dec
	./bin/decimac $< | opt -Os -o $@

asm/%.bc: asm/%.ll
	opt -Os -o $@ $<

clean:
	rm -rf *.elf asm/*.bc *.bc
