-c /var/services/homes/spparmar/spec2006/benchspec/CPU2006/470.lbm/run/run_base_test_elider-amd64-m64-gcc42-nn.0000 -o lbm.out.cmp specperl /var/services/homes/spparmar/spec2006/bin/specdiff -m -l 10  --abstol 1e-07 /var/services/homes/spparmar/spec2006/benchspec/CPU2006/470.lbm/data/test/output/lbm.out lbm.out