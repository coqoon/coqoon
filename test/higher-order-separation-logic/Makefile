MODULES := Util Stack Expr Subst Lang Pwf LiftOp Pure \
           AbstractSepAlg AbstractUPred HeapSepAlg AbstractAsn HeapAsn \
           SubstAbstractAsn Spec SemCmd SemCmdRules \
           Semantics SemRules Tactics Cell Fac Lists

VS := $(MODULES:%=%.v)

.PHONY: coq clean

coq: Makefile.coq
	$(MAKE) -j2 -f Makefile.coq

Makefile.coq: Makefile $(VS)
	coq_makefile $(VS) -o Makefile.coq

clean:: Makefile.coq
	$(MAKE) -f Makefile.coq clean
	rm -f Makefile.coq

