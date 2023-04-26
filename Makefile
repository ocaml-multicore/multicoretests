all:
	dune build

coverage-run:
	dune runtest -f src/ --instrument-with bisect_ppx

coverage-report:
	bisect-ppx-report html
	@echo "Report should be available in file://$(shell pwd)/_coverage/index.html"

coverage-summary:
	bisect-ppx-report summary --per-file

clean:
	dune clean
	rm -f *~ src/*~ issues/*~
